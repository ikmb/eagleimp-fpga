--
--    Copyright (C) 2018-2021 by Lars Wienbrandt,
--    Institute of Clinical Molecular Biology, Kiel University
--    
--    This file is part of EagleImp-FPGA.
--
--    EagleImp-FPGA is free software: you can redistribute it and/or modify
--    it under the terms of the GNU General Public License as published by
--    the Free Software Foundation, either version 3 of the License, or
--    (at your option) any later version.
--
--    EagleImp-FPGA is distributed in the hope that it will be useful,
--    but WITHOUT ANY WARRANTY; without even the implied warranty of
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--    GNU General Public License for more details.
--
--    You should have received a copy of the GNU General Public License
--    along with EagleImp-FPGA. If not, see <https://www.gnu.org/licenses/>.
--

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- To use with a (simple) dual port BRAM. The BRAM contents are read as from 
-- an FWFT FIFO starting with fifo_start_addr. The empty signal is assigned when
-- the fifo_stop_addr is reached, i.e. when the data at the previous address was read.
-- If reverse is asserted, the contents are read backwards, i.e. the first data is read
-- from fifo_stop_addr-1 down to fifo_start_addr.
-- Further read requests are discarded until a reset is assigned. This will also reset 
-- the empty flag.
-- NOTE: The entity will pre-read a number of values. Before the first datum is
-- available, the empty flag will be asserted as well.
-- Be sure to reset the entity before starting to read in order to ensure the correct data
-- will be pre-read! 
-- Start/stop addresses and the reverse signal are initialized only when the reset signal is
-- asserted. These values are ignored otherwise.
entity BRAMFIFORdInterface is
generic (
  DATA_WIDTH   : integer := 16;
  ADDR_WIDTH   : integer := 16;
  BRAM_LATENCY : integer := 2
);
port (
  clk        : in  std_logic;
  reset      : in  std_logic; -- Note, that this does NOT reset the RAM contents!!! Only FIFO pointer is set to the beginning.
  -- FIFO's start and stop addresses (expected to be constant for one FIFO use)
  fifo_start_addr : in unsigned(ADDR_WIDTH downto 0); -- inclusive
  fifo_stop_addr  : in unsigned(ADDR_WIDTH downto 0); -- exclusive
  reverse         : in std_logic; -- indicates if the FIFO should be read backwards (which makes it a stack)
  -- FIFO Rd Interface
  fifo_dout  : out std_logic_vector(DATA_WIDTH-1 downto 0);
  fifo_en    : in  std_logic;
  fifo_am_empty : out std_logic;
  fifo_empty : out std_logic;
  -- BRAM Rd Port
  bram_req_addr : out std_logic_vector(ADDR_WIDTH-1 downto 0);
  bram_req_en   : out std_logic;
  bram_data     : in  std_logic_vector(DATA_WIDTH-1 downto 0);
  bram_valid    : in  std_logic
);
end entity BRAMFIFORdInterface;

architecture Behaviour of BRAMFIFORdInterface is

signal next_addr : std_logic_vector(ADDR_WIDTH-1 downto 0) := (others => '0');
signal empty_int : std_logic;
signal req_empty : std_logic := '1'; -- if no more requests can be made
signal init_empty : std_logic := '1'; -- during initialization
signal prefetch_en : std_logic := '0';
signal bram_req_en_int : std_logic := '0';
signal level : integer range 0 to BRAM_LATENCY+1 := 0;
type buf_type is array (natural range <>) of std_logic_vector(DATA_WIDTH-1 downto 0);
signal buf : buf_type(BRAM_LATENCY downto 0) := (others => (others => '0'));
type fifo_state_t is (PREFETCH, WAITNONEMPTY);
signal fifo_state : fifo_state_t := PREFETCH;
signal reset_del : std_logic_vector(BRAM_LATENCY downto 0) := (others => '0'); -- this indicates the number of prefetch cycles and how long BRAM answers must be blocked from pre-reset requests

begin

-- the FIFO is empty if all data has been requested from BRAM and the buffers for prefetching are empty as well
empty_int <= '1' when level = 0 else '0';
fifo_empty <= empty_int or init_empty or reset;

-- the FIFO is almost empty if the last datum is currently presented
fifo_am_empty <= '1' when req_empty = '1' and level = 1 and bram_valid = '0' else '0';

bram_req_addr <= next_addr;
bram_req_en_int <= ((fifo_en and not init_empty and not empty_int) or prefetch_en) and not req_empty;
bram_req_en <= bram_req_en_int;

fifo_dout <= buf(0) when level = 0 else buf(level-1);

bram_req_p: process
  variable level_pre : integer range 0 to BRAM_LATENCY+1 := 0;
  variable tmp_addr : unsigned(ADDR_WIDTH downto 0);
  variable stop_addr : unsigned(ADDR_WIDTH downto 0);
  variable curr_reverse : std_logic := '0';
begin
  wait until rising_edge(clk);
  
  prefetch_en <= '0';
  reset_del <= reset_del(BRAM_LATENCY-1 downto 0) & '0'; -- left shift
  
  if reset = '1' then
    curr_reverse := reverse;
    if reverse = '0' then -- forward 
      tmp_addr := fifo_start_addr;
      stop_addr := fifo_stop_addr;
    else -- reverse NOTE: stop addr is excluding and start address is including!
      tmp_addr := fifo_stop_addr-1;
      stop_addr := fifo_start_addr-1;
    end if;
    level_pre := 0;
    init_empty <= '1';
    fifo_state <= PREFETCH;
    reset_del <= (0 => '1', others => '0');
  else
    case fifo_state is
      
    when PREFETCH =>
      prefetch_en <= '1';
      init_empty <= '1';
      if reset_del(BRAM_LATENCY) = '1' then -- last prefetch cycle
        fifo_state <= WAITNONEMPTY;
      end if; 
     
    when WAITNONEMPTY => -- @suppress "Dead state 'WAITNONEMPTY': state does not have outgoing transitions"
      -- NOTE: It's normally safer to wait for bram_valid = '1', but to be one cycle faster, we know that the
      -- first BRAM answer will arrive in the next cycle.
      init_empty <= '0';
      
    end case;
    
    if bram_req_en_int = '1' then -- data request was sent -> increment address
      if curr_reverse = '0' then
        tmp_addr := tmp_addr + 1;
      else
        tmp_addr := tmp_addr - 1;
      end if;
    end if;
    
    if fifo_en = '1' and empty_int = '0' and init_empty = '0' then -- data was fetched from FIFO
      -- potentially select data from lower level now (consumed one datum from buffer)
      level_pre := level_pre - 1;
    end if;
    
    if bram_valid = '1' and reset_del = (reset_del'range => '0') then -- got valid data from BRAM
      -- shift data into buffer
      for I in BRAM_LATENCY downto 1 loop
        buf(I) <= buf(I-1);
      end loop;
      buf(0) <= bram_data;
      level_pre := level_pre + 1; -- potentially select data from higher level now (more data is buffered)
    end if;
    
  end if;
  
  if tmp_addr = stop_addr then 
    req_empty <= '1';
  else
    req_empty <= '0';
  end if;
  
  next_addr <= std_logic_vector(tmp_addr(ADDR_WIDTH-1 downto 0));
  
  level <= level_pre; -- data selector

end process bram_req_p;
    
end Behaviour;

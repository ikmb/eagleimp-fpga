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

-- To use with a (simple) dual port BRAM. The BRAM is filled as a FIFO
-- starting with fifo_start_addr. The full signal is assigned when the
-- fifo_stop_addr is reached, i.e. the addr before was written to.
-- Further data will be discarded until a reset is assigned. This will 
-- also reset the full flag.
-- Start/stop addresses are initialized only when the reset signal is
-- asserted. These values are ignored otherwise. 
entity BRAMFIFOWrInterface is
generic (
  DATA_WIDTH   : integer := 16;
  ADDR_WIDTH   : integer := 16  -- address width for each BRAM part
);
port (
  clk        : in  std_logic;
  reset      : in  std_logic; -- Note, that this does NOT reset the RAM contents!!! Only FIFO pointer is set to the beginning.
  -- FIFO's start and stop addresses (expected to be constant for one FIFO use)
  fifo_start_addr : in unsigned(ADDR_WIDTH downto 0); -- inclusive
  fifo_stop_addr  : in unsigned(ADDR_WIDTH downto 0); -- exclusive
  -- FIFO Wr Interface
  fifo_din   : in  std_logic_vector(DATA_WIDTH-1 downto 0);
  fifo_en    : in  std_logic;
  fifo_full  : out std_logic;
  -- BRAM Wr Port
  bram_data  : out std_logic_vector(DATA_WIDTH-1 downto 0);
  bram_addr  : out std_logic_vector(ADDR_WIDTH-1 downto 0);
  bram_wr_en : out std_logic
);
end entity BRAMFIFOWrInterface;

architecture Behaviour of BRAMFIFOWrInterface is

signal next_addr : std_logic_vector(ADDR_WIDTH-1 downto 0) := (others => '0');
signal bram_wr_en_int : std_logic := '0';
--signal full_int : std_logic := '0';
signal req_full : std_logic := '0';
--signal init_full : std_logic := '0';

begin

bram_wr_en <= bram_wr_en_int;
fifo_full <= req_full or reset;

fifo_p: process
  variable tmp_addr : unsigned(ADDR_WIDTH downto 0);
  variable stop_addr : unsigned(ADDR_WIDTH downto 0);
begin
  wait until rising_edge(clk);

  bram_wr_en_int <= '0';
  
  -- delay writing to relax timing  
  bram_data <= fifo_din;
  bram_addr <= next_addr;
  
  if reset = '1' then
    tmp_addr := fifo_start_addr;
    stop_addr := fifo_stop_addr;
  else
    if fifo_en = '1' and req_full = '0' then -- will write into BRAM now
      bram_wr_en_int <= '1';
      tmp_addr := tmp_addr + 1;
    end if;
  end if;
  
  if tmp_addr = stop_addr then 
    req_full <= '1';
  else
    req_full <= '0';
  end if;
  
  next_addr <= std_logic_vector(tmp_addr(ADDR_WIDTH-1 downto 0));

end process fifo_p;
    
end Behaviour;

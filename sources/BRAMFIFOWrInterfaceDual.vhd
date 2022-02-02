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

-- To use with a true dual port BRAM (connected to the write ports only).
-- The BRAM is filled as a FIFO
-- starting with fifo_start_addr. The full signal is assigned when the
-- fifo_stop_addr is reached, i.e. the addr before was written to.
-- Further data will be discarded until a reset is assigned. This will 
-- also reset the full flag.
-- Start/stop addresses are initialized only when the reset signal is
-- asserted. These values are ignored otherwise.
-- Due to the dual port BRAM this FIFO can store two data words at once
-- in one cycle, which will be the case if fifo_enA and fifo_enB are enabled.
-- At first, the data at port A will be written followed by the data at port B.
-- If only one fifo_enX is enabled, only one word will be written.
entity BRAMFIFOWrInterfaceDual is
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
  fifo_dinA   : in  std_logic_vector(DATA_WIDTH-1 downto 0);
  fifo_enA    : in  std_logic;
  fifo_dinB   : in  std_logic_vector(DATA_WIDTH-1 downto 0);
  fifo_enB    : in  std_logic;
  fifo_full  : out std_logic;
  -- BRAM Wr Port
  bram_dataA  : out std_logic_vector(DATA_WIDTH-1 downto 0);
  bram_addrA  : out std_logic_vector(ADDR_WIDTH-1 downto 0);
  bram_wr_enA : out std_logic;
  bram_dataB  : out std_logic_vector(DATA_WIDTH-1 downto 0);
  bram_addrB  : out std_logic_vector(ADDR_WIDTH-1 downto 0);
  bram_wr_enB : out std_logic
);
end entity BRAMFIFOWrInterfaceDual;

architecture Behaviour of BRAMFIFOWrInterfaceDual is

signal next_addr : unsigned(ADDR_WIDTH-1 downto 0) := (others => '0');
signal req_full : std_logic := '0';

begin

fifo_full <= req_full or reset;

fifo_p: process
  variable tmp_addr : unsigned(ADDR_WIDTH downto 0);
  variable stop_addr : unsigned(ADDR_WIDTH downto 0);
  variable tmp_req_full : std_logic := '0';
begin
  wait until rising_edge(clk);

  bram_wr_enA <= '0';
  bram_wr_enB <= '0';
  
  -- delay writing to relax timing  
  bram_dataA <= fifo_dinA;
  bram_dataB <= fifo_dinB;
  bram_addrA <= std_logic_vector(next_addr);
  if fifo_enA = '1' and fifo_enB = '1' then
    bram_addrB <= std_logic_vector(next_addr+1);
  else
    bram_addrB <= std_logic_vector(next_addr);
  end if;
  
  tmp_req_full := '0';
  
  if reset = '1' then
    tmp_addr := fifo_start_addr;
    stop_addr := fifo_stop_addr;
  else
    if fifo_enA = '1' and req_full = '0' then -- will write into BRAM now
      bram_wr_enA <= '1';
      tmp_addr := tmp_addr + 1;
      -- disabled here since this is a timing issue and 
      -- this FIFO is used for splitting the 0 and 1 bit origins,
      -- so if count0 has been determined correctly in advance,
      -- an overflow will never occur
--      if tmp_addr = stop_addr then 
--        tmp_req_full := '1';
--      end if;
    end if;
    if fifo_enB = '1' and req_full = '0' then --and tmp_req_full = '0' then -- will write into BRAM now
      bram_wr_enB <= '1';
      tmp_addr := tmp_addr + 1;
    end if;
  end if;
  
  if tmp_addr = stop_addr then 
    tmp_req_full := '1';
  end if;
  req_full <= tmp_req_full;
  
  next_addr <= tmp_addr(ADDR_WIDTH-1 downto 0);

end process fifo_p;
    
end Behaviour;

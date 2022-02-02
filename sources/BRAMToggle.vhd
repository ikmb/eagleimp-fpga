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

-- Divides the attached simple dual-port BRAM into several partial BRAMs
-- according to the most significant address bits.
-- Write accesses on port A are processed immediately. Note, that usually BRAM_LATENCY cycles are required
-- until the written data can be read from port B.
-- Read accesses from port B are processed immediately though the data comes available BRAM_LATENCY cycles 
-- after the request. However, requests can be made in each cycle and results are pipelined and
-- flagged accordingly. The requests can be accompanied by arbitrary metadata, which is
-- pipelined as well.
entity BRAMToggle is
generic (
  DATA_WIDTH   : integer := 16;
  ADDR_WIDTH   : integer := 16; -- address width for each BRAM part
  SELECT_WIDTH : integer := 1;  -- width for the BRAM part selection address
  META_WIDTH   : integer := 1;  -- width of metadata that accompanies read requests
  BRAM_LATENCY : integer := 2
);
port (
  clk        : in  std_logic;
  reset      : in  std_logic; -- Note, that this does NOT reset the RAM contents!!!
  -- Port A: write
  wr_din     : in  std_logic_vector(DATA_WIDTH-1 downto 0);
  wr_addr    : in  std_logic_vector(ADDR_WIDTH-1 downto 0);
  wr_sel     : in  std_logic_vector(SELECT_WIDTH-1 downto 0);
  wr_en      : in  std_logic;
  -- Port B: read
  rd_req_addr : in  std_logic_vector(ADDR_WIDTH-1 downto 0);
  rd_req_sel  : in  std_logic_vector(SELECT_WIDTH-1 downto 0);
  rd_req_meta : in  std_logic_vector(META_WIDTH-1 downto 0);
  rd_req_en   : in  std_logic;
  rd_dout     : out std_logic_vector(DATA_WIDTH-1 downto 0);
  rd_meta     : out std_logic_vector(META_WIDTH-1 downto 0);
  rd_valid    : out std_logic;
  -- ports for attached BRAM entity
  bram_wea    : out std_logic_vector(0 downto 0);
  bram_addra  : out std_logic_vector(ADDR_WIDTH+SELECT_WIDTH-1 downto 0);
  bram_dina   : out std_logic_vector(DATA_WIDTH-1 downto 0);
  bram_addrb  : out std_logic_vector(ADDR_WIDTH+SELECT_WIDTH-1 downto 0);
  bram_doutb  : in  std_logic_vector(DATA_WIDTH-1 downto 0)
);
end entity BRAMToggle;

architecture Behaviour of BRAMToggle is

type meta_vector is array (natural range <>) of std_logic_vector(META_WIDTH-1 downto 0);
signal meta_buf : meta_vector(BRAM_LATENCY-1 downto 0) := (others => (others => '0'));
signal rd_en_buf : std_logic_vector(BRAM_LATENCY-1 downto 0);

begin

-- port A
bram_wea(0) <= wr_en;
bram_addra <= wr_sel & wr_addr;
bram_dina <= wr_din;

-- port B
bram_addrb <= rd_req_sel & rd_req_addr;

meta_p: process
begin
  wait until rising_edge(clk);
  if reset = '1' then
    meta_buf <= (others => (others => '0'));
    rd_en_buf <= (others => '0');
  else
    rd_en_buf(BRAM_LATENCY-1) <= rd_req_en;
    rd_en_buf(BRAM_LATENCY-2 downto 0) <= rd_en_buf(BRAM_LATENCY-1 downto 1);
    meta_buf(BRAM_LATENCY-1) <= rd_req_meta;
    meta_buf(BRAM_LATENCY-2 downto 0) <= meta_buf(BRAM_LATENCY-1 downto 1);
  end if;
end process meta_p;

rd_dout <= bram_doutb;
rd_meta <= meta_buf(0);
rd_valid <= rd_en_buf(0);

end Behaviour;

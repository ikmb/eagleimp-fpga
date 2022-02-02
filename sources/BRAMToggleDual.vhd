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

-- Multiplexes two attached true dual-port BRAMs by the xxx_sel ports.
-- Write accesses are processed immediately. Note, that usually BRAM_LATENCY cycles are required
-- until the written data can be read.
-- Read accesses are processed immediately though the data comes available BRAM_LATENCY cycles 
-- after the request. However, requests can be made in each cycle and results are pipelined and
-- flagged accordingly. The requests can be accompanied by arbitrary metadata, which is
-- pipelined as well.
-- Note, that it is not possible to have a write and read request to the same selection address at the same port (A or B).
-- If applied anyway, the write request has precedence.
--
-- NOTE: This entity adds one cycle to the BRAM latency when reading!!!
--
entity BRAMToggleDual is
generic (
  DATA_WIDTH   : integer := 16;
  ADDR_WIDTH   : integer := 16; -- address width for each BRAM part
  META_WIDTH   : integer := 1;  -- width of metadata that accompanies read requests
  BRAM_LATENCY : integer := 2
);
port (
  clk        : in  std_logic;
  reset      : in  std_logic; -- Note, that this does NOT reset the RAM contents!!!
  -- Port A0: write
  wrA_din     : in  std_logic_vector(DATA_WIDTH-1 downto 0);
  wrA_addr    : in  std_logic_vector(ADDR_WIDTH-1 downto 0);
  wrA_sel     : in  std_logic;
  wrA_en      : in  std_logic;
  -- Port A1: read
  rdA_req_addr : in  std_logic_vector(ADDR_WIDTH-1 downto 0);
  rdA_req_sel  : in  std_logic;
  rdA_req_meta : in  std_logic_vector(META_WIDTH-1 downto 0);
  rdA_req_en   : in  std_logic;
  rdA_dout     : out std_logic_vector(DATA_WIDTH-1 downto 0);
  rdA_meta     : out std_logic_vector(META_WIDTH-1 downto 0);
  rdA_valid    : out std_logic;
  -- Port B0: write
  wrB_din     : in  std_logic_vector(DATA_WIDTH-1 downto 0);
  wrB_addr    : in  std_logic_vector(ADDR_WIDTH-1 downto 0);
  wrB_sel     : in  std_logic;
  wrB_en      : in  std_logic;
  -- Port B0: read
  rdB_req_addr : in  std_logic_vector(ADDR_WIDTH-1 downto 0);
  rdB_req_sel  : in  std_logic;
  rdB_req_meta : in  std_logic_vector(META_WIDTH-1 downto 0);
  rdB_req_en   : in  std_logic;
  rdB_dout     : out std_logic_vector(DATA_WIDTH-1 downto 0);
  rdB_meta     : out std_logic_vector(META_WIDTH-1 downto 0);
  rdB_valid    : out std_logic;
  -- ports for attached BRAM entities
  bramA_wea    : out std_logic_vector(0 downto 0);
  bramA_addra  : out std_logic_vector(ADDR_WIDTH-1 downto 0);
  bramA_dina   : out std_logic_vector(DATA_WIDTH-1 downto 0);
  bramA_douta  : in  std_logic_vector(DATA_WIDTH-1 downto 0);
  bramA_web    : out std_logic_vector(0 downto 0);
  bramA_addrb  : out std_logic_vector(ADDR_WIDTH-1 downto 0);
  bramA_dinb   : out std_logic_vector(DATA_WIDTH-1 downto 0);
  bramA_doutb  : in  std_logic_vector(DATA_WIDTH-1 downto 0);
  bramB_wea    : out std_logic_vector(0 downto 0);
  bramB_addra  : out std_logic_vector(ADDR_WIDTH-1 downto 0);
  bramB_dina   : out std_logic_vector(DATA_WIDTH-1 downto 0);
  bramB_douta  : in  std_logic_vector(DATA_WIDTH-1 downto 0);
  bramB_web    : out std_logic_vector(0 downto 0);
  bramB_addrb  : out std_logic_vector(ADDR_WIDTH-1 downto 0);
  bramB_dinb   : out std_logic_vector(DATA_WIDTH-1 downto 0);
  bramB_doutb  : in  std_logic_vector(DATA_WIDTH-1 downto 0)
);
end entity BRAMToggleDual;

architecture Behaviour of BRAMToggleDual is

type meta_vector is array (natural range <>) of std_logic_vector(META_WIDTH-1 downto 0);
signal metaA_buf : meta_vector(BRAM_LATENCY downto 0) := (others => (others => '0'));
signal rdA_en_buf : std_logic_vector(BRAM_LATENCY downto 0);
signal rdA_sel_buf : std_logic_vector(BRAM_LATENCY downto 0);
signal metaB_buf : meta_vector(BRAM_LATENCY downto 0) := (others => (others => '0'));
signal rdB_en_buf : std_logic_vector(BRAM_LATENCY downto 0);
signal rdB_sel_buf : std_logic_vector(BRAM_LATENCY downto 0);

begin

process
begin
  wait until rising_edge(clk);
  
  -- port A, write
  if wrA_sel = '0' then
    bramA_wea(0) <= wrA_en  ;
    bramA_addra  <= wrA_addr;
    bramB_wea(0) <= '0';
    bramB_addra  <= rdA_req_addr;
  else
    bramA_wea(0) <= '0';
    bramA_addra  <= rdA_req_addr;
    bramB_wea(0) <= wrA_en  ;
    bramB_addra  <= wrA_addr;
  end if;
  bramA_dina   <= wrA_din;
  bramB_dina   <= wrA_din;
  
  -- port B, write
  if wrB_sel = '0' then
    bramA_web(0) <= wrB_en  ;
    bramA_addrb  <= wrB_addr;
    bramB_web(0) <= '0';
    bramB_addrb  <= rdB_req_addr;
  else
    bramA_web(0) <= '0';
    bramA_addrb  <= rdB_req_addr;
    bramB_web(0) <= wrB_en  ;
    bramB_addrb  <= wrB_addr;
  end if;
  bramA_dinb   <= wrB_din;
  bramB_dinb   <= wrB_din;
  
  -- port A+B, read (handle rd requests and meta data)
  if reset = '1' then
    metaA_buf <= (others => (others => '0'));
    rdA_en_buf <= (others => '0');
    rdA_sel_buf <= (others => '0');
    metaB_buf <= (others => (others => '0'));
    rdB_en_buf <= (others => '0');
    rdB_sel_buf <= (others => '0');
  else
    rdA_en_buf(BRAM_LATENCY) <= rdA_req_en;
    rdA_en_buf(BRAM_LATENCY-1 downto 0) <= rdA_en_buf(BRAM_LATENCY downto 1);
    rdA_sel_buf(BRAM_LATENCY) <= rdA_req_sel;
    rdA_sel_buf(BRAM_LATENCY-1 downto 0) <= rdA_sel_buf(BRAM_LATENCY downto 1);
    metaA_buf(BRAM_LATENCY) <= rdA_req_meta;
    metaA_buf(BRAM_LATENCY-1 downto 0) <= metaA_buf(BRAM_LATENCY downto 1);
    rdB_en_buf(BRAM_LATENCY) <= rdB_req_en;
    rdB_en_buf(BRAM_LATENCY-1 downto 0) <= rdB_en_buf(BRAM_LATENCY downto 1);
    rdB_sel_buf(BRAM_LATENCY) <= rdB_req_sel;
    rdB_sel_buf(BRAM_LATENCY-1 downto 0) <= rdB_sel_buf(BRAM_LATENCY downto 1);
    metaB_buf(BRAM_LATENCY) <= rdB_req_meta;
    metaB_buf(BRAM_LATENCY-1 downto 0) <= metaB_buf(BRAM_LATENCY downto 1);
  end if;
end process;

rdA_dout <= bramA_douta when rdA_sel_buf(0) = '0' else bramB_douta;
rdA_meta <= metaA_buf(0);
rdA_valid <= rdA_en_buf(0);
rdB_dout <= bramA_doutb when rdB_sel_buf(0) = '0' else bramB_doutb;
rdB_meta <= metaB_buf(0);
rdB_valid <= rdB_en_buf(0);

end Behaviour;

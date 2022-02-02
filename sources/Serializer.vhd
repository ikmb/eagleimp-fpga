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

-- Serializes from LSBs to MSBs
-- meta data is taken together with a valid DIN word and is provided together with each DOUT word repetitively.
-- if skip_rem is set and the unit has already presented the first bit of the current word, the remainder is skipped
-- and the unit is ready to load a new word.
entity Serializer is
generic (
  DIN_WIDTH  : integer := 16; -- has to be a multiple of DOUT_WIDTH
  DOUT_WIDTH : integer := 1;
  META_WIDTH : integer := 1
);
port (
  clk        : in  std_logic;
  reset      : in  std_logic;
  din        : in  std_logic_vector(DIN_WIDTH-1 downto 0);
  meta_in    : in  std_logic_vector(META_WIDTH-1 downto 0);
  din_ready  : out std_logic;
  din_valid  : in  std_logic;
  dout       : out std_logic_vector(DOUT_WIDTH-1 downto 0);
  meta_out   : out std_logic_vector(META_WIDTH-1 downto 0);
  dout_ready : in  std_logic;
  dout_valid : out std_logic;
  dout_last  : out std_logic;
  skip_rem   : in  std_logic
);
end entity Serializer;

architecture Behaviour of Serializer is

signal buf : std_logic_vector(DIN_WIDTH-1 downto 0) := (others => '0');
signal meta : std_logic_vector(META_WIDTH-1 downto 0) := (others => '0');
signal last : std_logic := '0';
signal empty : std_logic := '1';
signal valid_int : std_logic := '0';
signal din_ready_int : std_logic := '0';

begin

buf_p: process
  variable buf_tmp : std_logic_vector(DIN_WIDTH-1 downto 0) := (others => '0');
  variable fill_cnt : integer range 0 to DIN_WIDTH/DOUT_WIDTH := 0;
begin
  wait until rising_edge(clk);
  
  if reset = '1' then
    buf_tmp := (others => '0');
    fill_cnt := 0;
  else
  
    if valid_int = '1' then -- fetched one bit
      -- right shift buffer
      buf_tmp(DIN_WIDTH-DOUT_WIDTH-1 downto 0) := buf_tmp(DIN_WIDTH-1 downto DOUT_WIDTH);
      fill_cnt := fill_cnt - 1;
    end if; -- END if valid_int = 1
    
    if din_valid = '1' and din_ready_int = '1' then -- load
      buf_tmp := din;
      fill_cnt := DIN_WIDTH/DOUT_WIDTH;
      meta <= meta_in;
    end if;
    
    -- skip remaining bits (only if the buffer is not completely filled, i.e. if the first bit has not been read yet) 
    if skip_rem = '1' and fill_cnt /= DIN_WIDTH/DOUT_WIDTH then
      fill_cnt := 0;
    end if; 
    
  end if;
  
  buf <= buf_tmp;
  if fill_cnt = 1 then
    last <= '1';
  else
    last <= '0';
  end if;
  if fill_cnt = 0 then
    empty <= '1';
  else
    empty <= '0';
  end if;
  
end process buf_p;

din_ready_int <= (empty or (last and dout_ready)) and not reset; -- ready if empty or presenting the last word
din_ready <= din_ready_int;

valid_int <= dout_ready and not empty;
dout_valid <= valid_int;
dout_last  <= dout_ready and last;
dout <= buf(DOUT_WIDTH-1 downto 0);
meta_out <= meta;
  
end Behaviour;
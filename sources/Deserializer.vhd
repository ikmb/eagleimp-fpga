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

-- Deserializes from LSBs to MSBs
-- cnt0 provides the number of input bits for the current output word that were equal to 0.
-- meta data is taken from the last valid DIN word and is provided together with DOUT.
-- if at least one bit was already loaded into the unit and skip_rem is set, the remainder is skipped and the current buffer is sent out.
-- in this case, the remainder is padded with SKIP_PAD_VALUE
entity Deserializer is
generic (
  DIN_WIDTH  : integer := 2;
  DOUT_WIDTH : integer := 16; -- has to be a multiple of DIN_WIDTH
  CNT0_WIDTH : integer := 5; -- ideally log2(DOUT_WIDTH/DIN_WIDTH)+1, a value too small may result in an overflow, which will not be signalled!
  META_WIDTH : integer := 1;
  SKIP_PAD_VALUE : std_logic_vector := (others => '0')
);
port (
  clk        : in  std_logic;
  reset      : in  std_logic;
  din        : in  std_logic_vector(DIN_WIDTH-1 downto 0);
  meta_in    : in  std_logic_vector(META_WIDTH-1 downto 0);
  din_ready  : out std_logic;
  din_valid  : in  std_logic;
  skip_rem   : in  std_logic;
  dout       : out std_logic_vector(DOUT_WIDTH-1 downto 0);
  cnt0       : out std_logic_vector(CNT0_WIDTH-1 downto 0);
  meta_out   : out std_logic_vector(META_WIDTH-1 downto 0);
  dout_ready : in  std_logic;
  dout_valid : out std_logic
);
end entity Deserializer;

architecture Behaviour of Deserializer is

signal buf : std_logic_vector(DOUT_WIDTH-1 downto 0) := (others => '0');
signal meta : std_logic_vector(META_WIDTH-1 downto 0) := (others => '0');
signal full : std_logic := '0';
signal valid_int : std_logic := '0';
signal din_ready_int : std_logic := '0';

begin

buf_p: process
  variable buf_tmp : std_logic_vector(DOUT_WIDTH-1 downto 0);
  variable curr_mask : std_logic_vector(DOUT_WIDTH-1 downto 0) := (DIN_WIDTH-1 downto 0 => '1', others => '0');
  variable curr_mult : std_logic_vector(DOUT_WIDTH-1 downto 0);
  variable free_cnt : integer range 0 to DOUT_WIDTH/DIN_WIDTH := 0;
  variable cnt0_int : unsigned(CNT0_WIDTH-1 downto 0) := (others => '0');
begin
  wait until rising_edge(clk);
  
  if reset = '1' then
    for I in 1 to DOUT_WIDTH/DIN_WIDTH loop
      buf_tmp(I*DIN_WIDTH-1 downto (I-1)*DIN_WIDTH) := SKIP_PAD_VALUE;
    end loop;
    curr_mask := (DIN_WIDTH-1 downto 0 => '1', others => '0');
    free_cnt := DOUT_WIDTH/DIN_WIDTH;
    cnt0_int := (others => '0');
  else
  
    if valid_int = '1' then -- fetched the word, reset buffer
      for I in 1 to DOUT_WIDTH/DIN_WIDTH loop
        buf_tmp(I*DIN_WIDTH-1 downto (I-1)*DIN_WIDTH) := SKIP_PAD_VALUE;
      end loop;
      curr_mask := (DIN_WIDTH-1 downto 0 => '1', others => '0');
      free_cnt := DOUT_WIDTH/DIN_WIDTH;
      cnt0_int := (others => '0');
    end if; -- END if valid_int = 1
    
    if din_valid = '1' and din_ready_int = '1' then -- add one word
--      -- right shift buffer
--      buf_tmp(DOUT_WIDTH-DIN_WIDTH-1 downto 0) := buf_tmp(DOUT_WIDTH-1 downto DIN_WIDTH);
--      buf_tmp(DOUT_WIDTH-1 downto DOUT_WIDTH-DIN_WIDTH) := din;
      -- multiply din to all possible parts in dout
      for I in 1 to DOUT_WIDTH/DIN_WIDTH loop
        curr_mult(I*DIN_WIDTH-1 downto (I-1)*DIN_WIDTH) := din;
      end loop;
      -- due to the variable padding value, we need to ensure that ones and also zeros are explicitly set
      buf_tmp := buf_tmp or (curr_mult and curr_mask);     -- set ones of current din, keep the rest as is
      buf_tmp := buf_tmp and (curr_mult or not curr_mask); -- set zeros of current din, keep the rest as is
      -- left shift current mask
      curr_mask(DOUT_WIDTH-1 downto DIN_WIDTH) := curr_mask(DOUT_WIDTH-DIN_WIDTH-1 downto 0);
      curr_mask(DIN_WIDTH-1 downto 0) := (others => '0');
      free_cnt := free_cnt - 1;
      meta <= meta_in;
--      if din = (din'range => '0') then
--        cnt0_int := cnt0_int + 1;
--      end if;
      for I in din'range loop
        if din(I) = '0' then
          cnt0_int := cnt0_int + 1;
        end if;
      end loop;
    end if;
    
    if skip_rem = '1' and free_cnt /= DOUT_WIDTH/DIN_WIDTH then
      free_cnt := 0;
    end if;
    
  end if;
  
  buf <= buf_tmp;
  cnt0 <= std_logic_vector(cnt0_int);
  
  if free_cnt = 0 then
    full <= '1';
  else
    full <= '0';
  end if;
  
end process buf_p;

din_ready_int <= (not full or dout_ready) and not reset; -- ready if not full or, if full, the data is already been fetched 
din_ready <= din_ready_int;

valid_int <= dout_ready and full;
dout_valid <= valid_int;
dout <= buf;
meta_out <= meta;
  
end Behaviour;
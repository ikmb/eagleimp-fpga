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

-- takes a reference site-by-site and provides it as output in parts of DOUT_WIDTH, LSB to MSB
entity ReferenceBuffer is
generic(
  DIN_WIDTH : integer := 256; -- multiple of DOUT_WIDTH!!
  DOUT_WIDTH : integer := 16;
  COUNT_WIDTH : integer := 20
);
port (
  clk   : in  std_logic;
  reset : in  std_logic;
  -- Constants: set to TIG!
  last_site_idx      : in  unsigned(COUNT_WIDTH-1 downto 0);
  last_word_per_site : in  unsigned(COUNT_WIDTH-1 downto 0);
  -- Input
  reference_in      : in  std_logic_vector(DIN_WIDTH-1 downto 0);
  reference_valid   : in  std_logic;
  reference_ready   : out std_logic;
  -- Output         
  rout_data           : out std_logic_vector(DOUT_WIDTH-1 downto 0);
  rout_ready          : in  std_logic;
  rout_valid          : out std_logic;
  rout_last_from_site : out std_logic;
  rout_last           : out std_logic
);
end ReferenceBuffer;

architecture Behavioral of ReferenceBuffer is
  
signal ref_buf : std_logic_vector(DIN_WIDTH-1 downto 0);
signal ref_last : std_logic := '0';
signal ref_empty : std_logic := '1';
signal rout_valid_int : std_logic := '0';
signal rout_last_from_site_int : std_logic := '0';
signal rout_last_int : std_logic := '0';

begin

-- TODO could use the Serializer entity here

buf_p: process
  variable ref_buf_tmp : std_logic_vector(DIN_WIDTH-1 downto 0) := (others => '0');
  variable fill_cnt : integer range 0 to DIN_WIDTH/DOUT_WIDTH := 0;
  variable site_cnt : unsigned(COUNT_WIDTH-1 downto 0) := (others => '0');
  variable word_cnt : unsigned(COUNT_WIDTH-1 downto 0) := (others => '0');
begin
  wait until rising_edge(clk);
  
  rout_last_from_site_int <= '0';
  rout_last_int <= '0';

  if reset = '1' then
    ref_buf_tmp := (others => '0');
    fill_cnt := 0;
    site_cnt := (others => '0');
    word_cnt := (others => '0');
  else
  
    if rout_valid_int = '1' then -- fetched one gt
      -- right shift buffers
      ref_buf_tmp(DIN_WIDTH-DOUT_WIDTH-1 downto 0) := ref_buf_tmp(DIN_WIDTH-1 downto DOUT_WIDTH);
      fill_cnt := fill_cnt - 1;
      if ref_last = '1' then
        if rout_last_from_site_int = '1' then
          word_cnt := (others => '0');
          if rout_last_int = '1' then
            site_cnt := (others => '0');
          else
            site_cnt := site_cnt + 1;
          end if;
        else
          word_cnt := word_cnt + 1;
        end if;
      end if; -- END if ref_last = 1
    end if; -- END if rout_valid_int = 1
    
    if reference_valid = '1' then -- load
      ref_buf_tmp := reference_in;
      fill_cnt := DIN_WIDTH/DOUT_WIDTH;
    end if;
    
  end if;
  
  ref_buf <= ref_buf_tmp;
  if fill_cnt = 1 then
    ref_last <= '1';
    if word_cnt = last_word_per_site then
      rout_last_from_site_int <= '1';
      if site_cnt = last_site_idx then
        rout_last_int <= '1';
      end if;
    end if;
  else
    ref_last <= '0';
  end if;
  if fill_cnt = 0 then
    ref_empty <= '1';
  else
    ref_empty <= '0';
  end if;
  
end process buf_p;

reference_ready <= ref_empty or (ref_last and rout_ready); -- ready if empty or presenting the last word

rout_valid_int <= rout_ready and not ref_empty;
rout_valid <= rout_valid_int;
rout_data <= ref_buf(DOUT_WIDTH-1 downto 0);
rout_last_from_site <= rout_valid_int and rout_last_from_site_int;
rout_last <= rout_valid_int and rout_last_int;

end Behavioral;


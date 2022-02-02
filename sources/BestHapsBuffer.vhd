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

-- output behaviour similar to FWFT FIFO
entity BestHapsBuffer is
generic(
  DIN_WIDTH : integer := 128;
  DOUT_WIDTH : integer := 32
);
port (
  clk   : in  std_logic;
  reset : in  std_logic;
  -- Input           
  besthaps_in         : in  std_logic_vector(DIN_WIDTH-1 downto 0);
  besthaps_valid      : in  std_logic;
  besthaps_ready      : out std_logic;
  -- Output           
  curr_besthaps       : out std_logic_vector(DOUT_WIDTH-1 downto 0); 
  curr_besthaps_valid : out std_logic;
  curr_besthaps_ready : in  std_logic
);
end BestHapsBuffer;

architecture Behavioral of BestHapsBuffer is

signal bh_buf : std_logic_vector(DIN_WIDTH-1 downto 0);
signal bh_last : std_logic := '0';
signal bh_empty : std_logic := '1';
signal curr_besthaps_valid_int : std_logic := '0';

begin

buf_p: process
  variable bh_buf_tmp : std_logic_vector(DIN_WIDTH-1 downto 0) := (others => '0');
  variable fill_cnt : integer range 0 to DIN_WIDTH/DOUT_WIDTH := 0;
begin
  wait until rising_edge(clk);

  if reset = '1' then
    bh_buf_tmp := (others => '0');
    fill_cnt := 0;
  else
  
    if curr_besthaps_valid_int = '1' then -- fetched one gt
      -- right shift buffers
      bh_buf_tmp(DIN_WIDTH-DOUT_WIDTH-1 downto 0) := bh_buf_tmp(DIN_WIDTH-1 downto DOUT_WIDTH);
      fill_cnt := fill_cnt - 1;
    end if; 
    
    if besthaps_valid = '1' then -- load
      bh_buf_tmp := besthaps_in;
      fill_cnt := DIN_WIDTH/DOUT_WIDTH;
    end if;
    
  end if;
  
  bh_buf <= bh_buf_tmp;
  if fill_cnt = 1 then
    bh_last <= '1';
  else
    bh_last <= '0';
  end if;
  if fill_cnt = 0 then
    bh_empty <= '1';
  else
    bh_empty <= '0';
  end if;
  
end process buf_p;

besthaps_ready <= bh_empty or (bh_last and curr_besthaps_ready); -- ready if empty or presenting the last word

curr_besthaps_valid_int <= curr_besthaps_ready and not bh_empty;
curr_besthaps_valid <= curr_besthaps_valid_int;
curr_besthaps <= bh_buf(DOUT_WIDTH-1 downto 0);

end Behavioral;


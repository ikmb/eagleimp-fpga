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

-- higher bits (MSBs) carry the _smaller_ data!!!
entity SelectSet is
generic (
  DATA_WIDTH : integer := 16 -- needs to be an even number!
);
port (
--  clk : in std_logic;
  data_in       : in  std_logic_vector(DATA_WIDTH-1 downto 0);
  selection_in  : in  std_logic_vector(DATA_WIDTH-1 downto 0);
  data_out      : out std_logic_vector(DATA_WIDTH-1 downto 0);
  selection_out : out std_logic_vector(DATA_WIDTH-1 downto 0)
);
end entity SelectSet;

architecture Behavioral of SelectSet is

type data_array is array (natural range <>) of std_logic_vector(DATA_WIDTH-1 downto 0); 
signal data_tmp      : data_array(0 to DATA_WIDTH);
signal selection_tmp : data_array(0 to DATA_WIDTH);

begin

data_tmp(0) <= data_in;
selection_tmp(0) <= selection_in;

-- Odd-Even-Transposition Sort
oets_g: for I in 0 to DATA_WIDTH/2-1 generate

  -- even step: sort 0-1, 2-3, 4-5, etc
  even_g: for P in 0 to DATA_WIDTH/2-1 generate
  
    sbase_even_i: entity work.SelectSetBase
    port map(
--      clk => clk,
      a_label => selection_tmp(2*I)(2*P+1),
      a_data  => data_tmp     (2*I)(2*P+1),
      b_label => selection_tmp(2*I)(2*P),
      b_data  => data_tmp     (2*I)(2*P),
      l_label => selection_tmp(2*I+1)(2*P+1),
      l_data  => data_tmp     (2*I+1)(2*P+1),
      h_label => selection_tmp(2*I+1)(2*P), -- higher data to lower bits (LSBs)!!!
      h_data  => data_tmp     (2*I+1)(2*P)
    );
  
  end generate even_g;
  
  -- odd step: sort 1-2, 3-4, 5-6, etc
  -- 0 and DATA_WIDTH-1 are routed through (with buffer)
--  odd_buf_p: process
--  begin
--    wait until rising_edge(clk);
    selection_tmp(2*I+2)(0) <= selection_tmp(2*I+1)(0);
    data_tmp     (2*I+2)(0) <= data_tmp     (2*I+1)(0);
    selection_tmp(2*I+2)(DATA_WIDTH-1) <= selection_tmp(2*I+1)(DATA_WIDTH-1);
    data_tmp     (2*I+2)(DATA_WIDTH-1) <= data_tmp     (2*I+1)(DATA_WIDTH-1);
--  end process odd_buf_p;
--
  odd_g: for P in 1 to DATA_WIDTH/2-1 generate
  
    sbase_even_i: entity work.SelectSetBase
    port map(
--      clk => clk,
      a_label => selection_tmp(2*I+1)(2*P),
      a_data  => data_tmp     (2*I+1)(2*P),
      b_label => selection_tmp(2*I+1)(2*P-1),
      b_data  => data_tmp     (2*I+1)(2*P-1),
      l_label => selection_tmp(2*I+2)(2*P),
      l_data  => data_tmp     (2*I+2)(2*P),
      h_label => selection_tmp(2*I+2)(2*P-1), -- higher data to lower bits (LSBs)!!!
      h_data  => data_tmp     (2*I+2)(2*P-1)
    );
  
  end generate odd_g;
  
end generate oets_g;

data_out <= data_tmp(DATA_WIDTH);
selection_out <= selection_tmp(DATA_WIDTH);
  
end Behavioral;
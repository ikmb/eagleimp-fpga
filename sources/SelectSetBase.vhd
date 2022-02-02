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

-- the label is sorted: l: lower, h: higher
-- the data is routed the same way as the label, only if the label is '1',
-- if a label is '0', its corresponding data is set to zero as well. 
entity SelectSetBase is
port (
--  clk : in std_logic;
  a_label : in  std_logic;
  a_data  : in  std_logic;
  b_label : in  std_logic;
  b_data  : in  std_logic;
  l_label : out std_logic;
  l_data  : out std_logic;
  h_label : out std_logic;
  h_data  : out std_logic 
);
end entity SelectSetBase;

architecture Behavioral of SelectSetBase is

signal l_label_tmp : std_logic;
signal h_label_tmp : std_logic;
signal l_data_tmp : std_logic;
signal h_data_tmp : std_logic;

begin
  
l_label_tmp <= a_label and b_label;
h_label_tmp <= a_label or b_label;

-- equal assignments if data would not be set to zero when label is not set
--l_data_tmp  <= b_data when a_label = '1' and b_label = '0' else a_data;
--h_data_tmp  <= a_data when a_label = '1' and b_label = '0' else b_data;
--l_data_tmp <= (b_data and a_label and not b_label) or (a_data and (not a_label or b_label));
--h_data_tmp <= (a_data and a_label and not b_label) or (b_data and (not a_label or b_label));

l_data_tmp <= a_data and a_label and b_label; -- b_data can never be routed to lower since it must have a zero label then, where data will be set to zero anyway
h_data_tmp <= (a_data and a_label and not b_label) or (b_data and b_label); -- set to zero only if no label is set

--buf_p: process
--begin
--  wait until rising_edge(clk);
  
  l_label <= l_label_tmp;
  h_label <= h_label_tmp;
  l_data  <= l_data_tmp;  
  h_data  <= h_data_tmp;
--end process buf_p;  
  
end Behavioral;
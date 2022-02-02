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

entity CheckIncon is
generic (
  DATA_WIDTH : integer := 16
);
port ( 
  clk            : in  std_logic;
  
  -- reference and besthaps input
  reference_in   : in  std_logic_vector(DATA_WIDTH-1 downto 0);
  besthaps_in    : in  std_logic_vector(DATA_WIDTH-1 downto 0);
  valid_in       : in  std_logic; 
  last_fs_in     : in  std_logic;
  last_in        : in  std_logic;
  
  -- target input (always considered valid if reference input is valid)
  target_is0_in  : in  std_logic;
  target_is2_in  : in  std_logic;
  splitsite_in   : in  std_logic;
 
  -- ref/incon output 
  refinc_out     : out std_logic_vector(DATA_WIDTH-1 downto 0); 
  besthaps_out   : out std_logic_vector(DATA_WIDTH-1 downto 0);
  valid_out      : out std_logic;
  last_fs_out    : out std_logic;
  last_out       : out std_logic;
  splitsite_out  : out std_logic 
);
end CheckIncon;

architecture Behavioral of CheckIncon is

begin

check_p: process -- (reference_in, besthaps_in, valid_in, last_fs_in, last_in, target_is0_in, target_is2_in, splitsite_in)
  variable incon : std_logic_vector(DATA_WIDTH-1 downto 0);
begin
  wait until rising_edge(clk);
  
  -- check inconsistency against reference
  for I in 0 to DATA_WIDTH-1 loop
    incon(I) := (target_is0_in and not target_is2_in and reference_in(I)) or (not target_is0_in and target_is2_in and not reference_in(I));
  end loop;
    
  -- if the incoming site is a split site, we simply forward the reference;
  -- incons are only required between split sites
  if splitsite_in = '1' then
    refinc_out <= reference_in;
  else
    refinc_out <= incon;
  end if;
  
  -- many signals only need to be delayed by one cycle
  besthaps_out <= besthaps_in;
  valid_out <= valid_in;
  last_fs_out <= last_fs_in;
  last_out <= last_in;
  splitsite_out <= splitsite_in;
  
end process check_p;

end Behavioral;

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

use work.eagleimp_prep_pkg.all;

package eagleimp_prep_info is

subtype field_t is std_logic_vector(15 downto 0);

constant INFO_APP_ID : std_logic_vector(7 downto 0) := x"04"; -- application identifier for EagleImp
constant INFO_VERSION_MAJOR : std_logic_vector(7 downto 0) := x"01";
constant INFO_VERSION_MINOR : std_logic_vector(7 downto 0) := x"06";
constant INFO_VERSION_REVISION : std_logic_vector(7 downto 0) := x"00";
         
constant INFO_PBWT_FREQUENCY : std_logic_vector(31 downto 0) := x"0FDAD680"; -- 266 MHz
--constant INFO_BASE_FREQUENCY : std_logic_vector(31 downto 0) := x"0BEBC200"; -- 200 MHz
constant INFO_BASE_FREQUENCY : std_logic_vector(31 downto 0) := x"06CB8080"; -- 114 MHz

constant INFO_RAM_AVAILABLE : std_logic_vector(31 downto 0) := x"00002000"; -- 8 GiB (8192 MiB)

constant INFO_NUM_PIPELINES : std_logic_vector(7 downto 0) := std_logic_vector(to_unsigned(NUM_IMPL_PIPELINES, 8));

constant INFO_MAX_SITES : std_logic_vector(15 downto 0) := x"0040"; --  64k (value needs to be multiplied by 1k) 
constant INFO_MAX_HAPS  : std_logic_vector(15 downto 0) := x"0080"; -- 128k (value needs to be multiplied by 1k)
constant INFO_MAX_K     : std_logic_vector(15 downto 0) := (LOG_MAXK-10 => '1', others => '0'); -- (value needs to be multiplied by 1k)
         
constant INFO_RESERVED16 : field_t := x"0000";
constant INFO_RESERVED8 : std_logic_vector(7 downto 0) := x"00";

-- should be read as uint16_t, so reverse order in the fields...
constant INFO_DATA : std_logic_vector(223 downto 0) :=
    INFO_RESERVED16
  & INFO_RESERVED16
  & INFO_MAX_K  
  & INFO_MAX_HAPS  
  & INFO_MAX_SITES
  & INFO_RESERVED8 & INFO_NUM_PIPELINES  
  & INFO_RAM_AVAILABLE
  & INFO_PBWT_FREQUENCY
  & INFO_BASE_FREQUENCY
  & INFO_VERSION_MINOR & INFO_VERSION_REVISION
  & INFO_APP_ID & INFO_VERSION_MAJOR;

end eagleimp_prep_info;

package body eagleimp_prep_info is

end eagleimp_prep_info;

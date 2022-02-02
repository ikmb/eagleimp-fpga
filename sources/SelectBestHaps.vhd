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

entity SelectBestHaps is
generic (
  DATA_WIDTH : integer := 16 -- needs to be an even number!
);
port (
  clk : in std_logic;
  reset : in std_logic;
  
  refinc_in     : in  std_logic_vector(DATA_WIDTH-1 downto 0);
  besthaps_in   : in  std_logic_vector(DATA_WIDTH-1 downto 0);
  valid_in      : in  std_logic;
  last_fs_in    : in  std_logic; -- if last_fs_in = '1' we need to have at least one cycle of valid_in = '0' afterwards!
  last_in       : in  std_logic;
  splitsite_in  : in  std_logic;
  
  refinc_out    : out std_logic_vector(DATA_WIDTH-1 downto 0);
  valid_out     : out std_logic;
  last_fs_out   : out std_logic;
  last_out      : out std_logic;
  splitsite_out : out std_logic
);
end entity SelectBestHaps;

architecture Behavioral of SelectBestHaps is

signal selectset_h_data_in       : std_logic_vector(DATA_WIDTH-1 downto 0);
signal selectset_h_selection_in  : std_logic_vector(DATA_WIDTH-1 downto 0);
signal selectset_l_data_in       : std_logic_vector(DATA_WIDTH-1 downto 0);
signal selectset_l_selection_in  : std_logic_vector(DATA_WIDTH-1 downto 0);
signal selectset_h_data_out      : std_logic_vector(DATA_WIDTH-1 downto 0);
signal selectset_h_selection_out : std_logic_vector(DATA_WIDTH-1 downto 0);
signal selectset_l_data_out      : std_logic_vector(DATA_WIDTH-1 downto 0);
signal selectset_l_selection_out : std_logic_vector(DATA_WIDTH-1 downto 0);

signal low_full : std_logic;
signal high_empty : std_logic;
signal drain : std_logic := '0';

begin

-- also ensures that output data is set to zero where no selection bits are set
selectset_i: entity work.SelectSet
  generic map(
    DATA_WIDTH => 2*DATA_WIDTH
  )
  port map(
    data_in       (2*DATA_WIDTH-1 downto DATA_WIDTH) => selectset_h_data_in,
    data_in       (  DATA_WIDTH-1 downto          0) => selectset_l_data_in,
    selection_in  (2*DATA_WIDTH-1 downto DATA_WIDTH) => selectset_h_selection_in,
    selection_in  (  DATA_WIDTH-1 downto          0) => selectset_l_selection_in,
    data_out      (2*DATA_WIDTH-1 downto DATA_WIDTH) => selectset_h_data_out,
    data_out      (  DATA_WIDTH-1 downto          0) => selectset_l_data_out,
    selection_out (2*DATA_WIDTH-1 downto DATA_WIDTH) => selectset_h_selection_out,
    selection_out (  DATA_WIDTH-1 downto          0) => selectset_l_selection_out
  );

selectset_h_data_in      <= refinc_in;
selectset_h_selection_in <= besthaps_in and (DATA_WIDTH-1 downto 0 => not drain); -- needs to be zero when draining

low_full <= '1' when selectset_l_selection_out = (DATA_WIDTH-1 downto 0 => '1') else '0';
high_empty <= '1' when selectset_h_selection_out = (DATA_WIDTH-1 downto 0 => '0') else '0';

select_hl_p: process
  variable drain_last : std_logic := '0';
  variable drain_splitsite : std_logic := '0';
begin
  wait until rising_edge(clk);

  refinc_out <= selectset_l_data_out or not selectset_l_selection_out; -- this ensures a 1-padding on not selected sites

  -- only valid if the lower word is full and data is already in the higher word or if it's the last word
  valid_out <= (valid_in and ((low_full and not high_empty) or (last_fs_in and high_empty))) or drain;
  last_fs_out <= (last_fs_in and valid_in and high_empty) or drain;
  last_out <= (last_in and valid_in and high_empty) or drain_last;
  splitsite_out <= (splitsite_in and valid_in) or drain_splitsite;
  
  drain <= '0';
  drain_last := '0';
  drain_splitsite := '0';
  
  if reset = '1' then
    selectset_l_data_in      <= (others => '0'); 
    selectset_l_selection_in <= (others => '0');
  else
    if valid_in = '1' or drain = '1' then   
      if low_full = '1' and high_empty = '0' then
        selectset_l_data_in      <= selectset_h_data_out;
        selectset_l_selection_in <= selectset_h_selection_out;
      else
        selectset_l_data_in      <= selectset_l_data_out;
        selectset_l_selection_in <= selectset_l_selection_out;
      end if;
      if last_fs_in = '1' or drain = '1' then
        if high_empty = '0' then -- need draining (if drain was set, this will not be the case)
          drain <= '1';
          drain_last := last_in;
          drain_splitsite := splitsite_in;
        else -- reset lower input (data will be set to zero implicitly if selectors are zero)
          selectset_l_selection_in <= (others => '0');
        end if;
      end if;
        
    end if;
  end if;
  
end process select_hl_p; 

end Behavioral;
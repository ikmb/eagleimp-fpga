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

entity PipelineInit is
generic (
  ID : integer := 0;
  DATA_WIDTH  : integer := 128;
  ADDR_WIDTH  : integer := 7;
  COUNT_WIDTH : integer := 20; -- split sites counter width
  WORDCOUNT_WIDTH : integer := 30 -- word counter for cref width
);
port (
  clk   : in std_logic;
  reset : in std_logic;
  -- Bus input
  data_in           : in  std_logic_vector(DATA_WIDTH-1 downto 0);
  data_addr_in      : in  unsigned(ADDR_WIDTH-1 downto 0);
  data_tag_in       : in  pipelineinit_data_tag_t;
  data_valid_in     : in  std_logic;
  data_last_in      : in  std_logic;
  -- Bus output
  data_out          : out std_logic_vector(DATA_WIDTH-1 downto 0);
  data_addr_out     : out unsigned(ADDR_WIDTH-1 downto 0);
  data_tag_out      : out pipelineinit_data_tag_t;
  data_valid_out    : out std_logic;
  data_last_out     : out std_logic;
  -- Pipeline init output
  tgt_gts_out       : out std_logic_vector(DATA_WIDTH-1 downto 0);
  tgt_gts_valid     : out std_logic;
  tgt_gts_last      : out std_logic;
  split_sites_out   : out std_logic_vector(DATA_WIDTH-1 downto 0);
  split_sites_valid : out std_logic;
  split_sites_last  : out std_logic;
  besthaps_out      : out std_logic_vector(DATA_WIDTH-1 downto 0);
  besthaps_valid    : out std_logic;
  besthaps_last     : out std_logic;
  splitcount_out    : out unsigned(COUNT_WIDTH-1 downto 0); -- split sites per target
  pbwtwordcount_out : out unsigned(WORDCOUNT_WIDTH-1 downto 0) -- ref/incon segments in ram words (512bit) for this target
);
end PipelineInit;

architecture Behavioral of PipelineInit is

signal data        : std_logic_vector(DATA_WIDTH-1 downto 0);
signal data_addr   : unsigned(ADDR_WIDTH-1 downto 0);
signal data_tag    : pipelineinit_data_tag_t;
signal data_valid  : std_logic := '0';
signal data_last   : std_logic := '0';

begin

stream_p: process
  variable splitcount : unsigned(COUNT_WIDTH-1 downto 0) := (others => '0');
  variable pbwtwordcount : unsigned(WORDCOUNT_WIDTH-1 downto 0) := (others => '0');
begin
  wait until rising_edge(clk);
  
  tgt_gts_out <= data_in;
  tgt_gts_valid <= '0';
  tgt_gts_last <= '0';
  split_sites_out <= data_in;
  split_sites_valid <= '0';
  split_sites_last <= '0';
  besthaps_out <= data_in;
  besthaps_valid <= '0';
  besthaps_last <= '0';
  
  splitcount_out <= splitcount;
  pbwtwordcount_out <= pbwtwordcount;
--  totcrefwordcount_out <= totcrefwordcount;
  
  -- always send data over the complete bus, one cycle delay
  data <= data_in;
  data_addr <= data_addr_in;
  data_tag <= data_tag_in;
  data_valid <= data_valid_in;
  data_last <= data_last_in;
    
  if reset = '1' then
    splitcount := (others => '0');
    pbwtwordcount := (others => '0');
--    totcrefwordcount := (others => '0');
    data_valid <= '0'; -- block valid on reset
  else
          
    if data_valid_in = '1' then
      if data_addr_in = to_unsigned(ID, ADDR_WIDTH) then -- datum is for this unit
        if data_tag_in = TGT_CNT then -- counter
          splitcount := unsigned(data_in(COUNT_WIDTH-1 downto 0));
          pbwtwordcount := unsigned(data_in(31+WORDCOUNT_WIDTH downto 32));
        elsif data_tag_in = TGT_DATA then -- target
          tgt_gts_valid <= '1';
          if data_last_in = '1' then
            tgt_gts_last <= '1';
          end if;
        elsif data_tag_in = SPLIT_DATA then -- split sites
          split_sites_valid <= '1';
          if data_last_in = '1' then
            split_sites_last <= '1';
          end if;
        else -- best haps
          besthaps_valid <= '1';
          if data_last_in = '1' then
            besthaps_last <= '1';
          end if;
        end if;
      end if;
    end if;
  
  end if;
end process stream_p;

data_valid_out  <= data_valid;
data_last_out   <= data_last ;
data_out        <= data      ;
data_addr_out   <= data_addr ;
data_tag_out    <= data_tag  ;

end Behavioral;

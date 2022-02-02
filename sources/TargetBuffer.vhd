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

-- output behaviour similar to FWFT FIFO,
-- if "reverse" is asserted when loading target or split data, the data is provided the other way round at the output
entity TargetBuffer is
generic(
  LOG_DIN_WIDTH : integer := 128
);
port (
  clk   : in  std_logic;
  reset : in  std_logic;
  -- Input
  tgt_gts_in        : in  std_logic_vector(2*(2**LOG_DIN_WIDTH)-1 downto 0); -- each gt is encoded as a combination of "is0" and "is2"
  tgt_gts_valid     : in  std_logic;
  tgt_gts_ready     : out std_logic;
  split_sites_in    : in  std_logic_vector(2**LOG_DIN_WIDTH-1 downto 0);
  split_sites_valid : in  std_logic; 
  split_sites_ready : out std_logic;
  reverse           : in  std_logic; --  is sampled whenever data is loaded
  firstskip         : in  unsigned(LOG_DIN_WIDTH-1 downto 0); -- can skip up to all but one site _from_ _each_ loaded data
  -- Output
  curr_tgt_gt_is0   : out std_logic;
  curr_tgt_gt_is2   : out std_logic;
  curr_split_site   : out std_logic;
  get_next          : in  std_logic;
  empty             : out std_logic
);
end TargetBuffer;

architecture Behavioral of TargetBuffer is

signal tgt_buf_is0 : std_logic_vector(2**LOG_DIN_WIDTH-1 downto 0);
signal tgt_buf_is2 : std_logic_vector(2**LOG_DIN_WIDTH-1 downto 0);
signal split_buf : std_logic_vector(2**LOG_DIN_WIDTH-1 downto 0);
signal tgt_gts_ready_int : std_logic := '0';
signal split_sites_ready_int : std_logic := '0';
signal empty_int : std_logic := '1';
signal reverse_int : std_logic := '0';

begin

buf_p: process
  variable fill_cnt : integer range 0 to 2**LOG_DIN_WIDTH := 0;
  variable gt_filled : std_logic := '0';
  variable split_filled : std_logic := '0';
  variable skip_count : unsigned(LOG_DIN_WIDTH-1 downto 0) := (others => '0');
  variable reverse_del : std_logic := '0';
begin
  wait until rising_edge(clk);

  tgt_gts_ready_int <= '0';
  split_sites_ready_int <= '0';

  if reset = '1' then
    fill_cnt := 0;
    gt_filled := '0';
    split_filled := '0';
  else
    if empty_int = '1' then -- to be filled   
      
      if tgt_gts_ready_int = '1' and tgt_gts_valid = '1' then -- load target genotypes
        for i in 0 to (2**LOG_DIN_WIDTH)/GT_DATATYPE_WIDTH-1 loop
          tgt_buf_is0((i+1)*GT_DATATYPE_WIDTH-1 downto i*GT_DATATYPE_WIDTH) <= tgt_gts_in((2*i+1)*GT_DATATYPE_WIDTH-1 downto 2*i*GT_DATATYPE_WIDTH);
          tgt_buf_is2((i+1)*GT_DATATYPE_WIDTH-1 downto i*GT_DATATYPE_WIDTH) <= tgt_gts_in(2*(i+1)*GT_DATATYPE_WIDTH-1 downto (2*i+1)*GT_DATATYPE_WIDTH);
        end loop;
        gt_filled := '1';
      elsif gt_filled = '0' then
        tgt_gts_ready_int <= '1';
      end if;
      
      if split_sites_ready_int = '1' and split_sites_valid = '1' then -- load target split sites
        split_buf <= split_sites_in;
        split_filled := '1';
      elsif split_filled = '0' then
        split_sites_ready_int <= '1';
      end if;
      
      if gt_filled = '1' and split_filled = '1' then
        reverse_int <= reverse; -- save "reverse" requirement when last missing values are filled
        skip_count := firstskip; -- skip this number of sites from loaded data
        fill_cnt := 2**LOG_DIN_WIDTH;
        gt_filled := '0';
        split_filled := '0';
      end if;
      
    else -- data is ready to be fetched
      
      if get_next = '1' or skip_count /= (skip_count'range => '0') then -- fetched or skipped one gt
        if reverse_int = '0' then
          -- right shift buffers
          tgt_buf_is0(2**LOG_DIN_WIDTH-2 downto 0) <= tgt_buf_is0(2**LOG_DIN_WIDTH-1 downto 1);
          tgt_buf_is2(2**LOG_DIN_WIDTH-2 downto 0) <= tgt_buf_is2(2**LOG_DIN_WIDTH-1 downto 1);
          split_buf(2**LOG_DIN_WIDTH-2 downto 0)   <= split_buf(2**LOG_DIN_WIDTH-1 downto 1);
        else
          -- left shift buffers
          tgt_buf_is0(2**LOG_DIN_WIDTH-1 downto 1) <= tgt_buf_is0(2**LOG_DIN_WIDTH-2 downto 0);
          tgt_buf_is2(2**LOG_DIN_WIDTH-1 downto 1) <= tgt_buf_is2(2**LOG_DIN_WIDTH-2 downto 0);
          split_buf(2**LOG_DIN_WIDTH-1 downto 1)   <= split_buf(2**LOG_DIN_WIDTH-2 downto 0);
        end if;
        fill_cnt := fill_cnt - 1;
        if skip_count /= (skip_count'range => '0') then
          skip_count := skip_count - 1;
        end if;
      end if; 
      
    end if;
  end if;
  
  -- signalize "empty" either when all data was called or when the reverse signal changes
  if fill_cnt = 0 or reverse_del /= reverse then
    fill_cnt := 0; 
    empty_int <= '1';
    empty <= '1';
    if gt_filled = '0' then
      tgt_gts_ready_int <= '1';
    end if;
    if split_filled = '0' then
      split_sites_ready_int <= '1';
    end if;
  else
    empty_int <= '0';
    if skip_count /= (skip_count'range => '0') then
      empty <= '1';
    else
      empty <= '0';
    end if;
  end if;
  
  reverse_del := reverse;
end process buf_p;

tgt_gts_ready <= tgt_gts_ready_int;
split_sites_ready <= split_sites_ready_int;

curr_tgt_gt_is2 <= tgt_buf_is2(0) when reverse_int = '0' else tgt_buf_is2(2**LOG_DIN_WIDTH-1);
curr_tgt_gt_is0 <= tgt_buf_is0(0) when reverse_int = '0' else tgt_buf_is0(2**LOG_DIN_WIDTH-1);
curr_split_site <=   split_buf(0) when reverse_int = '0' else   split_buf(2**LOG_DIN_WIDTH-1);

end Behavioral;


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

entity CondensedRefPipeline is
generic (
  LOG_INIT_WIDTH : integer := 7; -- 128
  LOG_PROC_WIDTH : integer := 4; -- 16
  COUNT_WIDTH : integer := 20 
);
port ( 
  clk            : in  std_logic;
  reset          : in  std_logic;
  -- constants
  last_site_idx  : in  unsigned(COUNT_WIDTH-1 downto 0); -- used to calculate the number of sites to skip before reverse run
  -- target buffer initialization
  tgt_gts_in        : in  std_logic_vector(2**LOG_INIT_WIDTH-1 downto 0);
  tgt_gts_valid     : in  std_logic;
  tgt_gts_last      : in  std_logic;
  tgt_gts_ready     : out std_logic;
  split_sites_in    : in  std_logic_vector(2**LOG_INIT_WIDTH-1 downto 0);
  split_sites_valid : in  std_logic;
  split_sites_last  : in  std_logic; 
  split_sites_ready : out std_logic;  
  besthaps_in       : in  std_logic_vector(2**LOG_INIT_WIDTH-1 downto 0);
  besthaps_valid    : in  std_logic;
  besthaps_last     : in  std_logic; 
  besthaps_ready    : out std_logic;  
  -- reference stream input
  refdata_in        : in  std_logic_vector(2**LOG_PROC_WIDTH-1 downto 0);
  refdata_valid_in  : in  std_logic;
  refdata_last_from_site_in   : in  std_logic; -- marks the last word for this site
  refdata_last_in   : in  std_logic; -- marks the last word for the complete reference
  -- reference stream delayed output (for next pipeline)
  refdata_out       : out std_logic_vector(2**LOG_PROC_WIDTH-1 downto 0); -- in delayed
  refdata_valid_out : out std_logic; -- in delayed
  refdata_last_from_site_out  : out std_logic; -- in delayed
  refdata_last_out  : out std_logic; -- in delayed
  -- condensed output (not buffered)
  condensed_out       : out std_logic_vector(2**LOG_PROC_WIDTH-1 downto 0);
  condensed_ready     : in  std_logic;
  condensed_valid     : out std_logic;
  condensed_splitsite : out std_logic;
  condensed_last_fs   : out std_logic; -- signalizes the last word for this site
  condensed_last      : out std_logic; -- signalizes the last word for the complete reference
  stop_next_site      : out std_logic  -- signalizes that data is in the out FIFO, so starting with the next site might cause data loss when a switch is required 
  
  -- DEBUG
  ;checkincon_valid_cnt_out : out unsigned(DBG_COUNT_WIDTH-1 downto 0);
  selectbh_valid_cnt_out   : out unsigned(DBG_COUNT_WIDTH-1 downto 0);
  condensed_valid_cnt_out  : out unsigned(DBG_COUNT_WIDTH-1 downto 0);
  checkincon_lastfs_cnt_out : out unsigned(31 downto 0);
  checkincon_last_cnt_out : out unsigned(15 downto 0);
  checkincon_split_cnt_out : out unsigned(15 downto 0);
  selectbh_lastfs_cnt_out : out unsigned(31 downto 0);
  selectbh_last_cnt_out : out unsigned(15 downto 0);
  selectbh_split_cnt_out : out unsigned(15 downto 0);
  condensed_lastfs_cnt_out : out unsigned(31 downto 0);
  condensed_last_cnt_out : out unsigned(15 downto 0);
  condensed_split_cnt_out : out unsigned(15 downto 0);
  empty_getnext_cnt_out : out unsigned(15 downto 0);
  switchundrained_out : out std_logic := '0'
--  dbg_curr_block : in unsigned(7 downto 0);
--  dbg_curr_tgt   : in unsigned(7 downto 0);
--  splitcount      : in  unsigned(COUNT_WIDTH-1 downto 0) -- split sites per target from pipeline
--  wordcount       : in  unsigned(SITE_COUNT_WIDTH-1 downto 0); -- DOUT words for one(!) condensed reference from pipeline
--  wrong_scount_gt_flag : out std_logic;
--  wrong_scount_lt_flag : out std_logic;
--  wrong_scount_eq1_count : out unsigned(REF_COUNT_WIDTH-1 downto 0) := (others => '0');
--  wrong_scount_gt1_count : out unsigned(REF_COUNT_WIDTH-1 downto 0) := (others => '0')
--  refpopcnt_out : out unsigned(15 downto 0);
--  incpopcnt_out : out unsigned(15 downto 0)
);
end CondensedRefPipeline;

architecture Behavioral of CondensedRefPipeline is

signal target_collect_valid : std_logic := '0';
signal target_init_finished : std_logic := '0';
signal splits_init_finished : std_logic := '0';
signal bh_init_finished : std_logic := '0';

--signal targetfifo_wr_reset         : std_logic := '0'; 
signal targetfifo_wr_start_addr    : unsigned(9 downto 0) := (others => '0');
signal targetfifo_wr_stop_addr     : unsigned(9 downto 0) := (others => '0');
signal targetfifo_wr_fifo_din      : std_logic_vector(255 downto 0) := (others => '0'); 
signal targetfifo_wr_fifo_en       : std_logic := '0'; 
signal targetfifo_wr_fifo_full     : std_logic := '0'; 
signal targetfifo_wr_bram_data     : std_logic_vector(255 downto 0) := (others => '0'); 
signal targetfifo_wr_bram_addr     : std_logic_vector(8 downto 0) := (others => '0'); 
signal targetfifo_wr_bram_wr_en    : std_logic := '0'; 

signal targetfifo_rd_reset         : std_logic := '0';
signal targetfifo_rd_start_addr    : unsigned(9 downto 0) := (others => '0');
signal targetfifo_rd_stop_addr     : unsigned(9 downto 0) := (others => '0');
signal targetfifo_rd_reverse       : std_logic := '0';
signal targetfifo_rd_fifo_dout     : std_logic_vector(255 downto 0) := (others => '0');
signal targetfifo_rd_fifo_en       : std_logic := '0';
signal targetfifo_rd_fifo_am_empty : std_logic := '0';
signal targetfifo_rd_fifo_empty    : std_logic := '0';  
signal targetfifo_rd_bram_req_addr : std_logic_vector(8 downto 0) := (others => '0');
signal targetfifo_rd_bram_req_en   : std_logic := '0'; 
signal targetfifo_rd_bram_data     : std_logic_vector(255 downto 0) := (others => '0');
signal targetfifo_rd_bram_valid    : std_logic := '0';

--signal splitfifo_wr_reset         : std_logic := '0'; 
signal splitfifo_wr_start_addr    : unsigned(9 downto 0) := (others => '0');
signal splitfifo_wr_stop_addr     : unsigned(9 downto 0) := (others => '0');
signal splitfifo_wr_fifo_din      : std_logic_vector(127 downto 0) := (others => '0'); 
signal splitfifo_wr_fifo_en       : std_logic := '0'; 
signal splitfifo_wr_fifo_full     : std_logic := '0'; 
signal splitfifo_wr_bram_data     : std_logic_vector(127 downto 0) := (others => '0'); 
signal splitfifo_wr_bram_addr     : std_logic_vector(8 downto 0) := (others => '0'); 
signal splitfifo_wr_bram_wr_en    : std_logic := '0';
 
signal splitfifo_rd_reset         : std_logic := '0';
signal splitfifo_rd_start_addr    : unsigned(9 downto 0) := (others => '0');
signal splitfifo_rd_stop_addr     : unsigned(9 downto 0) := (others => '0');
signal splitfifo_rd_reverse       : std_logic := '0';
signal splitfifo_rd_fifo_dout     : std_logic_vector(127 downto 0) := (others => '0');
signal splitfifo_rd_fifo_en       : std_logic := '0';
signal splitfifo_rd_fifo_am_empty : std_logic := '0';
signal splitfifo_rd_fifo_empty    : std_logic := '0';  
signal splitfifo_rd_bram_req_addr : std_logic_vector(8 downto 0) := (others => '0');
signal splitfifo_rd_bram_req_en   : std_logic := '0'; 
signal splitfifo_rd_bram_data     : std_logic_vector(127 downto 0) := (others => '0');
signal splitfifo_rd_bram_valid    : std_logic := '0';

signal targetfifo_rd_skip : std_logic := '0';
signal splitfifo_rd_skip  : std_logic := '0';

signal bhrrfifo_din       : std_logic_vector(2**LOG_INIT_WIDTH-1 downto 0);
signal bhrrfifo_dout      : std_logic_vector(2**LOG_INIT_WIDTH-1 downto 0);
signal bhrrfifo_en        : std_logic;                              
signal bhrrfifo_rnw       : std_logic;    
signal bhrrfifo_empty     : std_logic;                              
signal bhrrfifo_full      : std_logic;                              
signal bhrrfifo_rst_busy  : std_logic;                              

signal bhfifo_din         : std_logic_vector(2**LOG_INIT_WIDTH-1 DOWNTO 0);
signal bhfifo_wr_en       : std_logic;
signal bhfifo_rd_en       : std_logic;
signal bhfifo_dout        : std_logic_vector(2**LOG_INIT_WIDTH-1 DOWNTO 0);
signal bhfifo_full        : std_logic;
signal bhfifo_almost_full : std_logic;
signal bhfifo_empty       : std_logic;
signal bhfifo_wr_rst_busy : std_logic;
signal bhfifo_rd_rst_busy : std_logic;

signal targetbuffer_tgt_gts_in        : std_logic_vector(2*(2**LOG_INIT_WIDTH)-1 downto 0);
signal targetbuffer_tgt_gts_valid     : std_logic;
signal targetbuffer_tgt_gts_ready     : std_logic;
signal targetbuffer_split_sites_in    : std_logic_vector(2**LOG_INIT_WIDTH-1 downto 0);
signal targetbuffer_split_sites_valid : std_logic; 
signal targetbuffer_split_sites_ready : std_logic;
signal targetbuffer_reverse           : std_logic;
signal targetbuffer_firstskip         : unsigned(LOG_INIT_WIDTH-1 downto 0);
signal targetbuffer_curr_tgt_gt_is0   : std_logic;
signal targetbuffer_curr_tgt_gt_is2   : std_logic;
signal targetbuffer_curr_split_site   : std_logic;
signal targetbuffer_get_next          : std_logic;
signal targetbuffer_empty             : std_logic;

signal bhbuffer_reset               : std_logic;
signal bhbuffer_besthaps_in         : std_logic_vector(2**LOG_INIT_WIDTH-1 downto 0);
signal bhbuffer_besthaps_valid      : std_logic;
signal bhbuffer_besthaps_ready      : std_logic;
signal bhbuffer_curr_besthaps       : std_logic_vector(2**LOG_PROC_WIDTH-1 downto 0);
signal bhbuffer_curr_besthaps_valid : std_logic;
signal bhbuffer_curr_besthaps_ready : std_logic; 

signal refdata : std_logic_vector(2**LOG_PROC_WIDTH-1 downto 0);
signal refdata_valid : std_logic := '0';
signal refdata_last_from_site : std_logic := '0';
signal refdata_last  : std_logic := '0';

signal checkincon_reference_in  : std_logic_vector(2**LOG_PROC_WIDTH-1 downto 0);
signal checkincon_besthaps_in   : std_logic_vector(2**LOG_PROC_WIDTH-1 downto 0);
signal checkincon_valid_in      : std_logic;
signal checkincon_last_fs_in    : std_logic;
signal checkincon_last_in       : std_logic;
signal checkincon_target_is0_in : std_logic;
signal checkincon_target_is2_in : std_logic;
signal checkincon_splitsite_in  : std_logic;
signal checkincon_refinc_out    : std_logic_vector(2**LOG_PROC_WIDTH-1 downto 0);
signal checkincon_besthaps_out  : std_logic_vector(2**LOG_PROC_WIDTH-1 downto 0);
signal checkincon_valid_out     : std_logic;
signal checkincon_last_fs_out   : std_logic;
signal checkincon_last_out      : std_logic;
signal checkincon_splitsite_out : std_logic;

signal selectbh_refinc_in     : std_logic_vector(2**LOG_PROC_WIDTH-1 downto 0);
signal selectbh_besthaps_in   : std_logic_vector(2**LOG_PROC_WIDTH-1 downto 0);
signal selectbh_valid_in      : std_logic;
signal selectbh_last_fs_in    : std_logic;
signal selectbh_last_in       : std_logic;
signal selectbh_splitsite_in  : std_logic;
signal selectbh_refinc_out    : std_logic_vector(2**LOG_PROC_WIDTH-1 downto 0);
signal selectbh_valid_out     : std_logic;
signal selectbh_last_fs_out   : std_logic;
signal selectbh_last_out      : std_logic;
signal selectbh_splitsite_out : std_logic;

signal krrfifo_din       : std_logic_vector(2**LOG_PROC_WIDTH+2 downto 0);
signal krrfifo_dout      : std_logic_vector(2**LOG_PROC_WIDTH+2 downto 0);
signal krrfifo_en        : std_logic;                              
signal krrfifo_rnw       : std_logic;    
signal krrfifo_merge     : std_logic;    
signal krrfifo_empty     : std_logic;                              
signal krrfifo_full      : std_logic;                              
signal krrfifo_rst_busy  : std_logic;                              

signal kfifo_din         : std_logic_vector(2**LOG_PROC_WIDTH+2 DOWNTO 0);
signal kfifo_wr_en       : std_logic;
signal kfifo_rd_en       : std_logic;
signal kfifo_dout        : std_logic_vector(2**LOG_PROC_WIDTH+2 DOWNTO 0);
signal kfifo_full        : std_logic;
signal kfifo_almost_full : std_logic;
signal kfifo_empty       : std_logic;
signal kfifo_wr_rst_busy : std_logic;
signal kfifo_rd_rst_busy : std_logic;

signal kfifo_select : std_logic := '0';
signal kfifo_drain_dout  : std_logic_vector(2**LOG_PROC_WIDTH+2 downto 0);
signal kfifo_drain       : std_logic := '0';
signal kfifo_drain_empty : std_logic;

signal kfifoA_din         : std_logic_vector(2**LOG_PROC_WIDTH+2 DOWNTO 0);
signal kfifoA_wr_en       : std_logic;
signal kfifoA_rd_en       : std_logic;
signal kfifoA_dout        : std_logic_vector(2**LOG_PROC_WIDTH+2 DOWNTO 0);
signal kfifoA_full        : std_logic;
signal kfifoA_almost_full : std_logic;
signal kfifoA_empty       : std_logic;
signal kfifoA_wr_rst_busy : std_logic;
signal kfifoA_rd_rst_busy : std_logic;

signal kfifoB_din         : std_logic_vector(2**LOG_PROC_WIDTH+2 DOWNTO 0);
signal kfifoB_wr_en       : std_logic;
signal kfifoB_rd_en       : std_logic;
signal kfifoB_dout        : std_logic_vector(2**LOG_PROC_WIDTH+2 DOWNTO 0);
signal kfifoB_full        : std_logic;
signal kfifoB_almost_full : std_logic;
signal kfifoB_empty       : std_logic;
signal kfifoB_wr_rst_busy : std_logic;
signal kfifoB_rd_rst_busy : std_logic;

signal condensed_out_int       : std_logic_vector(2**LOG_PROC_WIDTH-1 downto 0);       
signal condensed_valid_int     : std_logic;
signal condensed_splitsite_int : std_logic;
signal condensed_last_fs_int   : std_logic;
signal condensed_last_int      : std_logic;

-- DEBUG
signal checkincon_valid_cnt : unsigned(DBG_COUNT_WIDTH-1 downto 0) := (others => '0');
signal selectbh_valid_cnt   : unsigned(DBG_COUNT_WIDTH-1 downto 0) := (others => '0');
signal condensed_valid_cnt  : unsigned(DBG_COUNT_WIDTH-1 downto 0) := (others => '0');
signal checkincon_lastfs_cnt : unsigned(31 downto 0) := (others => '0');
signal checkincon_last_cnt   : unsigned(15 downto 0) := (others => '0');
signal checkincon_split_cnt  : unsigned(15 downto 0) := (others => '0');
signal selectbh_lastfs_cnt : unsigned(31 downto 0) := (others => '0');
signal selectbh_last_cnt   : unsigned(15 downto 0) := (others => '0');
signal selectbh_split_cnt  : unsigned(15 downto 0) := (others => '0');
signal condensed_lastfs_cnt : unsigned(31 downto 0) := (others => '0');
signal condensed_last_cnt   : unsigned(15 downto 0) := (others => '0');
signal condensed_split_cnt  : unsigned(15 downto 0) := (others => '0');
signal empty_getnext_cnt : unsigned(15 downto 0) := (others => '0');

begin

collect_p: process
  variable is2nd : std_logic := '0';
--  variable target_collect_valid_pending : std_logic := '0';
  variable target_init_finished_pre : std_logic_vector(1 downto 0) := "00";
  variable splits_init_finished_pre : std_logic_vector(1 downto 0) := "00";
begin
  wait until rising_edge(clk);
  
  target_collect_valid <= '0';
  
  -- delay the finished signals by two cycles
  target_init_finished <= target_init_finished_pre(0); 
  target_init_finished_pre(0) := target_init_finished_pre(1); 
  splits_init_finished <= splits_init_finished_pre(0); 
  splits_init_finished_pre(0) := splits_init_finished_pre(1);
  
  if reset = '1' then
    is2nd := '0';
--    target_collect_valid_pending := '0';
    target_init_finished_pre := "00";
    splits_init_finished_pre := "00";
    bh_init_finished <= '0';
  else
    if tgt_gts_valid = '1' then
      -- shift
      targetfifo_wr_fifo_din(2**LOG_INIT_WIDTH-1 downto 0) <= targetfifo_wr_fifo_din(2*(2**LOG_INIT_WIDTH)-1 downto 2**LOG_INIT_WIDTH);
      targetfifo_wr_fifo_din(2*(2**LOG_INIT_WIDTH)-1 downto 2**LOG_INIT_WIDTH) <= tgt_gts_in;
      target_collect_valid <= is2nd; -- always need to combine two data words for DIN_WIDTH target genotypes
      is2nd := not is2nd;
      -- last target data?
      if tgt_gts_last = '1' then 
        target_init_finished_pre(1) := '1';
      end if;
    end if;
--    if target_collect_valid_pending = '1' then
--      if targetfifo_full = '0' and targetfifo_wr_rst_busy = '0' then
--        -- submit to FIFO
--        target_collect_valid <= '1';
--        target_collect_valid_pending := '0';
--      else -- need to block and wait
--        tgt_gts_ready <= '0';
--      end if;
--    end if;
    
    -- last split data?
    if split_sites_valid = '1' and split_sites_last = '1' then
      splits_init_finished_pre(1) := '1';
    end if;
    
    -- last best haps data?
    if besthaps_valid = '1' and besthaps_last = '1' then
      bh_init_finished <= '1';
    end if;
    
  end if;
  
end process collect_p;

-- the BRAM has a depth of 512 entries
targetfifo_wr_start_addr <= (others => '0');
targetfifo_wr_stop_addr  <= (9 => '1', others => '0'); 
targetfifo_rd_start_addr <= (others => '0');
targetfifo_rd_stop_addr(9) <= '0';
targetfifo_rd_stop_addr(8 downto 0) <= unsigned(targetfifo_wr_bram_addr); -- reset is timed to occur when the _next_ write address is asserted to the BRAM

targetfifo_wr_i: entity work.BRAMFIFOWrInterface
  generic map(
    DATA_WIDTH => 2**(LOG_INIT_WIDTH+1),
    ADDR_WIDTH => 9
  )
  port map(
    clk             => clk,
    reset           => reset,
    fifo_start_addr => targetfifo_wr_start_addr,
    fifo_stop_addr  => targetfifo_wr_stop_addr,
    fifo_din        => targetfifo_wr_fifo_din,
    fifo_en         => targetfifo_wr_fifo_en,
    fifo_full       => targetfifo_wr_fifo_full,
    bram_data       => targetfifo_wr_bram_data,
    bram_addr       => targetfifo_wr_bram_addr,
    bram_wr_en      => targetfifo_wr_bram_wr_en
  );

targetfifo_rd_i: entity work.BRAMFIFORdInterface
  generic map(
    DATA_WIDTH   => 2**(LOG_INIT_WIDTH+1),
    ADDR_WIDTH   => 9,
    BRAM_LATENCY => 2
  )
  port map(
    clk             => clk,
    reset           => targetfifo_rd_reset or reset,
    fifo_start_addr => targetfifo_rd_start_addr,
    fifo_stop_addr  => targetfifo_rd_stop_addr,
    reverse         => targetfifo_rd_reverse,
    fifo_dout       => targetfifo_rd_fifo_dout,
    fifo_en         => targetfifo_rd_fifo_en,
    fifo_am_empty   => targetfifo_rd_fifo_am_empty,
    fifo_empty      => targetfifo_rd_fifo_empty,
    bram_req_addr   => targetfifo_rd_bram_req_addr,
    bram_req_en     => targetfifo_rd_bram_req_en,
    bram_data       => targetfifo_rd_bram_data,
    bram_valid      => targetfifo_rd_bram_valid
  );

targetbram_rd_valid_p: process
  variable req_buf : std_logic := '0'; -- for BRAM latency = 2
begin
  wait until rising_edge(clk);
  if reset = '1' or targetfifo_rd_reset = '1' then
    req_buf := '0';
  else
    targetfifo_rd_bram_valid <= req_buf;
    req_buf := targetfifo_rd_bram_req_en;
  end if;
end process targetbram_rd_valid_p;

targetbram_i: entity work.TargetDataBRAM_Wrapper
  port map(
    clk   => clk,
    wea(0)=> targetfifo_wr_bram_wr_en,
    addra => targetfifo_wr_bram_addr,
    dina  => targetfifo_wr_bram_data,
    addrb => targetfifo_rd_bram_req_addr,
    doutb => targetfifo_rd_bram_data
  );

-- the BRAM has a depth of 512 entries
splitfifo_wr_start_addr <= (others => '0');
splitfifo_wr_stop_addr  <= (9 => '1', others => '0'); 
splitfifo_rd_start_addr <= (others => '0');
splitfifo_rd_stop_addr(9) <= '0';
splitfifo_rd_stop_addr(8 downto 0) <= unsigned(splitfifo_wr_bram_addr); -- reset is timed to occur when the _next_ write address is asserted to the BRAM

splitfifo_wr_i: entity work.BRAMFIFOWrInterface
  generic map(
    DATA_WIDTH => 2**LOG_INIT_WIDTH,
    ADDR_WIDTH => 9
  )
  port map(
    clk             => clk,
    reset           => reset,
    fifo_start_addr => splitfifo_wr_start_addr,
    fifo_stop_addr  => splitfifo_wr_stop_addr,
    fifo_din        => splitfifo_wr_fifo_din,
    fifo_en         => splitfifo_wr_fifo_en,
    fifo_full       => splitfifo_wr_fifo_full,
    bram_data       => splitfifo_wr_bram_data,
    bram_addr       => splitfifo_wr_bram_addr,
    bram_wr_en      => splitfifo_wr_bram_wr_en
  );

splitfifo_rd_i: entity work.BRAMFIFORdInterface
  generic map(
    DATA_WIDTH   => 2**LOG_INIT_WIDTH,
    ADDR_WIDTH   => 9,
    BRAM_LATENCY => 2
  )
  port map(
    clk             => clk,
    reset           => splitfifo_rd_reset or reset,
    fifo_start_addr => splitfifo_rd_start_addr,
    fifo_stop_addr  => splitfifo_rd_stop_addr,
    reverse         => splitfifo_rd_reverse,
    fifo_dout       => splitfifo_rd_fifo_dout,
    fifo_en         => splitfifo_rd_fifo_en,
    fifo_am_empty   => splitfifo_rd_fifo_am_empty,
    fifo_empty      => splitfifo_rd_fifo_empty,
    bram_req_addr   => splitfifo_rd_bram_req_addr,
    bram_req_en     => splitfifo_rd_bram_req_en,
    bram_data       => splitfifo_rd_bram_data,
    bram_valid      => splitfifo_rd_bram_valid
  );

splitbram_rd_valid_p: process
  variable req_buf : std_logic := '0'; -- for BRAM latency = 2
begin
  wait until rising_edge(clk);
  if reset = '1' or splitfifo_rd_reset = '1' then
    req_buf := '0';
  else
    splitfifo_rd_bram_valid <= req_buf;
    req_buf := splitfifo_rd_bram_req_en;
  end if;
end process splitbram_rd_valid_p;

splitbram_i: entity work.SplitDataBRAM_Wrapper
  port map(
    clk   => clk,
    wea(0)=> splitfifo_wr_bram_wr_en,
    addra => splitfifo_wr_bram_addr,
    dina  => splitfifo_wr_bram_data,
    addrb => splitfifo_rd_bram_req_addr,
    doutb => splitfifo_rd_bram_data
  );

bh_rrfifo_i: entity work.ReReadableFIFO
generic map (
  DATA_WIDTH => 2**LOG_INIT_WIDTH
)
port map (
  -- User interface
  din      => bhrrfifo_din      ,
  dout     => bhrrfifo_dout     ,
  en       => bhrrfifo_en     ,
  rnw      => bhrrfifo_rnw    ,
  merge    => '0',
  empty    => bhrrfifo_empty ,
  full     => bhrrfifo_full  ,
  rst_busy => bhrrfifo_rst_busy, 
  -- directly connect to FIFO instance (using the same clk and reset signals as for this entity)
  fifo_din         => bhfifo_din,            
  fifo_wr_en       => bhfifo_wr_en,          
  fifo_rd_en       => bhfifo_rd_en,          
  fifo_dout        => bhfifo_dout,           
  fifo_full        => bhfifo_full,           
  fifo_almost_full => bhfifo_almost_full,    
  fifo_empty       => bhfifo_empty,          
  fifo_wr_rst_busy => bhfifo_wr_rst_busy,    
  fifo_rd_rst_busy => bhfifo_rd_rst_busy     
);

bhfifo_i: entity work.BestHapsDataFIFO_Wrapper
port map(
  clk         => clk,
  reset        => reset,
  din         => bhfifo_din,
  wr_en       => bhfifo_wr_en,
  rd_en       => bhfifo_rd_en,
  dout        => bhfifo_dout,
  full        => bhfifo_full,
  almost_full => bhfifo_almost_full,
  empty       => bhfifo_empty,
  wr_rst_busy => bhfifo_wr_rst_busy,
  rd_rst_busy => bhfifo_rd_rst_busy
);

targetfifo_wr_fifo_en <= target_collect_valid;
targetfifo_rd_fifo_en <= not targetfifo_rd_fifo_empty and targetbuffer_tgt_gts_ready and target_init_finished;
tgt_gts_ready <= '1';

targetbuffer_tgt_gts_in <= targetfifo_rd_fifo_dout;
targetbuffer_tgt_gts_valid <= targetfifo_rd_fifo_en and not targetfifo_rd_skip;

splitfifo_wr_fifo_din <= split_sites_in;
splitfifo_wr_fifo_en <= split_sites_valid;
splitfifo_rd_fifo_en <= not splitfifo_rd_fifo_empty and targetbuffer_split_sites_ready and splits_init_finished;
split_sites_ready <= '1';

targetbuffer_split_sites_in <= splitfifo_rd_fifo_dout;
targetbuffer_split_sites_valid <= splitfifo_rd_fifo_en and not splitfifo_rd_skip;

reverse_p: process
  variable target_init_finished_del : std_logic := '0';
  variable splits_init_finished_del : std_logic := '0';
  -- can skip up to three input words completely before entering the target buffer
  -- (required since target and split input comes in 512 sites units)
  variable targetfifo_rd_skip_words : unsigned(1 downto 0) := "00";
  variable splitfifo_rd_skip_words : unsigned(1 downto 0) := "00";
begin
  wait until rising_edge(clk);
  
  targetfifo_rd_reset <= '0';
  splitfifo_rd_reset <= '0';
  targetfifo_rd_skip <= '0';
  splitfifo_rd_skip <= '0';
  
  if reset = '1' then
    targetfifo_rd_reverse <= '0';
    splitfifo_rd_reverse <= '0';
    targetbuffer_reverse <= '0';
    targetbuffer_firstskip <= (others => '0');
    targetfifo_rd_skip_words := "00";
    splitfifo_rd_skip_words := "00";
  else
    
    -- initial reset of rd FIFOs
    if target_init_finished_del = '0' and target_init_finished = '1' then
      targetfifo_rd_reset <= '1';
    end if;
    if splits_init_finished_del = '0' and splits_init_finished = '1' then
      splitfifo_rd_reset <= '1';
    end if;
    
    if targetfifo_rd_skip_words /= (targetfifo_rd_skip_words'range => '0') then
      if targetfifo_rd_fifo_en = '1' then
        targetfifo_rd_skip_words := targetfifo_rd_skip_words - 1;
      end if;
    end if;
    if splitfifo_rd_skip_words /= (splitfifo_rd_skip_words'range => '0') then
      if splitfifo_rd_fifo_en = '1' then
        splitfifo_rd_skip_words := splitfifo_rd_skip_words - 1;
      end if;
    end if;
    
    if refdata_last_from_site = '1' then
      -- only skipping the first sites after switch to reverse, on other sites, we have to reset the skip value
      targetbuffer_firstskip <= (others => '0');
    end if;
    
    -- switch target buffer to reverse after last datum from reference buffer (next forward begins after reset)
    -- and reset targetfifo and splitfifo and set to reverse at the same time 
    if refdata_last_from_site = '1' and refdata_last = '1' then -- switch to reverse after last datum (next forward begins after reset)
      targetbuffer_reverse <= '1';
      
      -- in total 511 - lastidx%512 sites need to be skipped (since initialization is in 512 sites units), the target buffer support up to 127 sites
      targetbuffer_firstskip <= (2**LOG_INIT_WIDTH-1) - last_site_idx(LOG_INIT_WIDTH-1 downto 0); -- 127 - lastidx%128
      targetfifo_rd_skip_words := 3 - last_site_idx(LOG_INIT_WIDTH+1 downto LOG_INIT_WIDTH); -- complete words to skip
      splitfifo_rd_skip_words := targetfifo_rd_skip_words; -- the same
      
      targetfifo_rd_reset <= '1';
      targetfifo_rd_reverse <= '1';
      splitfifo_rd_reset <= '1';
      splitfifo_rd_reverse <= '1';
    end if;
  end if;
  
  target_init_finished_del := target_init_finished;
  splits_init_finished_del := splits_init_finished;
  
  if targetfifo_rd_skip_words /= (targetfifo_rd_skip_words'range => '0') then
    targetfifo_rd_skip <= '1';
  end if;
  if splitfifo_rd_skip_words /= (splitfifo_rd_skip_words'range => '0') then
    splitfifo_rd_skip <= '1';
  end if;
  
end process reverse_p;

targetbuf_i: entity work.TargetBuffer
generic map (
  LOG_DIN_WIDTH => LOG_INIT_WIDTH
)
port map (
  clk   => clk,
  reset => reset,
  -- Input
  tgt_gts_in        => targetbuffer_tgt_gts_in       ,
  tgt_gts_valid     => targetbuffer_tgt_gts_valid    ,
  tgt_gts_ready     => targetbuffer_tgt_gts_ready    ,
  split_sites_in    => targetbuffer_split_sites_in   ,
  split_sites_valid => targetbuffer_split_sites_valid,
  split_sites_ready => targetbuffer_split_sites_ready,
  reverse           => targetbuffer_reverse,
  firstskip         => targetbuffer_firstskip,
  -- Output
  curr_tgt_gt_is0 => targetbuffer_curr_tgt_gt_is0,
  curr_tgt_gt_is2 => targetbuffer_curr_tgt_gt_is2,
  curr_split_site => targetbuffer_curr_split_site,
  get_next        => targetbuffer_get_next       ,
  empty           => targetbuffer_empty     
);

bhrrfifo_din <= besthaps_in;
bhrrfifo_en <= besthaps_valid or bhbuffer_besthaps_valid;
bhrrfifo_rnw <= not besthaps_valid; -- always write when a valid datum arrives
besthaps_ready <= not bhrrfifo_full and not bhrrfifo_rst_busy;
bhbuffer_besthaps_in <= bhrrfifo_dout;
bhbuffer_besthaps_valid <= not bhrrfifo_empty and bhbuffer_besthaps_ready and not bhrrfifo_rst_busy and bh_init_finished;

bhbuffer_reset <= reset;

bhbuffer_i: entity work.BestHapsBuffer
generic map (
  DIN_WIDTH => 2**LOG_INIT_WIDTH,
  DOUT_WIDTH => 2**LOG_PROC_WIDTH
)
port map (
  clk   => clk,
  reset => bhbuffer_reset,
  -- Input
  besthaps_in         => bhbuffer_besthaps_in       ,
  besthaps_valid      => bhbuffer_besthaps_valid    ,
  besthaps_ready      => bhbuffer_besthaps_ready    ,
  -- Output
  curr_besthaps       => bhbuffer_curr_besthaps      ,
  curr_besthaps_valid => bhbuffer_curr_besthaps_valid, -- not used, has to be synchronous to refdata_valid
  curr_besthaps_ready => bhbuffer_curr_besthaps_ready
);

refdata_p: process
begin
  wait until rising_edge(clk);
  
  refdata <= refdata_in;
  
  if reset = '1' then
    refdata_valid <= '0';
    refdata_last_from_site <= '0';
    refdata_last <= '0';
  else
    refdata_valid <= refdata_valid_in;
    refdata_last_from_site <= refdata_last_from_site_in;
    refdata_last <= refdata_last_in;
  end if;
end process refdata_p;
targetbuffer_get_next <= refdata_last_from_site; -- request next target site

refdata_valid_out <= refdata_valid;
refdata_last_from_site_out <= refdata_last_from_site;
refdata_last_out <= refdata_last;
refdata_out <= refdata;

-- synchronize best haps with incoming reference stream
bhbuffer_curr_besthaps_ready <= refdata_valid;

checkincon_reference_in  <= refdata;
checkincon_besthaps_in   <= bhbuffer_curr_besthaps;
checkincon_valid_in      <= refdata_valid;
checkincon_last_fs_in    <= refdata_last_from_site;
checkincon_last_in       <= refdata_last;
checkincon_target_is0_in <= targetbuffer_curr_tgt_gt_is0; 
checkincon_target_is2_in <= targetbuffer_curr_tgt_gt_is2; 
checkincon_splitsite_in  <= targetbuffer_curr_split_site;

checkincon_i: entity work.CheckIncon
  generic map(
    DATA_WIDTH => 2**LOG_PROC_WIDTH
  )
  port map (
    clk           => clk          ,
    reference_in  => checkincon_reference_in ,
    besthaps_in   => checkincon_besthaps_in  ,
    valid_in      => checkincon_valid_in     ,
    last_fs_in    => checkincon_last_fs_in   ,
    last_in       => checkincon_last_in      ,
    target_is0_in => checkincon_target_is0_in,
    target_is2_in => checkincon_target_is2_in,
    splitsite_in  => checkincon_splitsite_in ,
    refinc_out    => checkincon_refinc_out,
    besthaps_out  => checkincon_besthaps_out ,
    valid_out     => checkincon_valid_out    ,
    last_fs_out   => checkincon_last_fs_out  ,
    last_out      => checkincon_last_out     ,
    splitsite_out => checkincon_splitsite_out 
  );

selectbh_refinc_in    <= checkincon_refinc_out   ;
selectbh_besthaps_in  <= checkincon_besthaps_out ;
selectbh_valid_in     <= checkincon_valid_out    ;   
selectbh_last_fs_in   <= checkincon_last_fs_out  ;
selectbh_last_in      <= checkincon_last_out     ;
selectbh_splitsite_in <= checkincon_splitsite_out;

selectbh_i: entity work.SelectBestHaps
  generic map(
    DATA_WIDTH => 2**LOG_PROC_WIDTH
  )
  port map(
    clk           => clk,
    reset         => reset,
    refinc_in     => selectbh_refinc_in,
    besthaps_in   => selectbh_besthaps_in,
    valid_in      => selectbh_valid_in,
    last_fs_in    => selectbh_last_fs_in,
    last_in       => selectbh_last_in,
    splitsite_in  => selectbh_splitsite_in,
    refinc_out    => selectbh_refinc_out,
    valid_out     => selectbh_valid_out,
    last_fs_out   => selectbh_last_fs_out,
    last_out      => selectbh_last_out,
    splitsite_out => selectbh_splitsite_out
  );

-- put and merge into KFIFO
kfifo_merge_p: process
  variable prevsite_was_split_or_last : std_logic := '1';
  variable prevsite_was_2nd_last  : std_logic := '0';
  variable prevsite_was_last_flag : std_logic := '0';
  variable prevword_was_lastfs : std_logic := '0';
begin
  wait until rising_edge(clk);
  
  krrfifo_en <= '0';
  
  krrfifo_din(2**LOG_PROC_WIDTH+2) <= selectbh_last_out;
  krrfifo_din(2**LOG_PROC_WIDTH+1) <= selectbh_last_fs_out;
  krrfifo_din(2**LOG_PROC_WIDTH)   <= selectbh_splitsite_out; -- probably this is a bit overkill to mark every word, but the space is there anyway
  krrfifo_din(2**LOG_PROC_WIDTH-1 downto 0) <= selectbh_refinc_out;
  
  if reset = '1' then
    kfifo_select <= '0';
    prevsite_was_split_or_last := '1';
    prevsite_was_2nd_last  := '0';
    prevsite_was_last_flag := '0';
    prevword_was_lastfs := '0';
    switchundrained_out <= '0';
  else
    
    -- switch fifos if previous cycle contained the last word for a site and this site is a split site or previous site was a split site
    -- OR the previous word was the very last word for this target and draining from the previous site has finished.
    if prevword_was_lastfs = '1' and (selectbh_valid_out = '1' or (prevsite_was_2nd_last = '1' and kfifo_drain_empty = '1')) then
      if prevsite_was_split_or_last = '1' or selectbh_splitsite_out = '1' then 
        kfifo_select <= not kfifo_select;
        -- DEBUG
        if kfifo_drain_empty = '0' then
          switchundrained_out <= '1';
        end if;
      end if;
      prevword_was_lastfs := '0';
    end if;
    
    -- last site was a split site (or the last site), so these are the first incons, or current site is a split site
    if prevsite_was_split_or_last = '1' or selectbh_splitsite_out = '1' then
      krrfifo_rnw <= '0'; -- write, not merge
    else
      krrfifo_rnw <= '1'; -- merge
    end if;
    
    -- write or merge if there's a valid datum
    krrfifo_en <= selectbh_valid_out;
      
    -- last word for this site
    if selectbh_valid_out = '1' and selectbh_last_fs_out = '1' then
      -- keep the status of the last site
      prevsite_was_split_or_last := selectbh_splitsite_out or selectbh_last_out;
      if selectbh_last_out = '1' then
        prevsite_was_2nd_last := prevsite_was_last_flag;
        prevsite_was_last_flag := not prevsite_was_last_flag;
      end if;
      -- switch FIFOs after this cycle
      prevword_was_lastfs := '1';
    end if;
    
  end if;
  
end process kfifo_merge_p;
krrfifo_merge <= '1'; -- we never read from this interface, so we can set this to a constant. ignored when writing anyway.


k_rrfifo_i: entity work.ReReadableFIFO
generic map (
  DATA_WIDTH => 2**LOG_PROC_WIDTH+3
)
port map (
--  clk   => clk,
--  reset => reset,
  -- User interface
  din      => krrfifo_din  ,
  dout     => krrfifo_dout , -- not required
  en       => krrfifo_en   ,
  rnw      => krrfifo_rnw  ,
  merge    => krrfifo_merge,
  empty    => krrfifo_empty, -- not required
  full     => krrfifo_full , -- never allowed to get full! if so, K is greater than allowed
  rst_busy => krrfifo_rst_busy, 
  -- directly connect to FIFO instance (using the same clk and reset signals as for this entity)
  fifo_din         => kfifo_din,            
  fifo_wr_en       => kfifo_wr_en,          
  fifo_rd_en       => kfifo_rd_en,          
  fifo_dout        => kfifo_dout,           
  fifo_full        => kfifo_full,           
  fifo_almost_full => kfifo_almost_full,    
  fifo_empty       => kfifo_empty,          
  fifo_wr_rst_busy => kfifo_wr_rst_busy,    
  fifo_rd_rst_busy => kfifo_rd_rst_busy     
);

-- switch underlying FIFOs according to kfifo_select
kfifoA_din <= kfifo_din; -- ok since only first FIFO will be written to
kfifoB_din <= kfifo_din; -- and second is always read
kfifoA_wr_en <= kfifo_wr_en and not kfifo_select; -- write possible only if A is selected 
kfifoB_wr_en <= kfifo_wr_en and kfifo_select; -- write possible only if B is selected
kfifoA_rd_en <= kfifo_rd_en when kfifo_select = '0' else kfifo_drain; 
kfifoB_rd_en <= kfifo_rd_en when kfifo_select = '1' else kfifo_drain; 

kfifo_dout <= kfifoA_dout when kfifo_select = '0' else kfifoB_dout;
kfifo_drain_dout <= kfifoA_dout when kfifo_select = '1' else kfifoB_dout;
kfifo_full <= kfifoA_full when kfifo_select = '0' else kfifoB_full;
kfifo_almost_full <= kfifoA_almost_full when kfifo_select = '0' else kfifoB_almost_full;
kfifo_empty <= kfifoA_empty when kfifo_select = '0' else kfifoB_empty;
kfifo_drain_empty <= kfifoA_empty when kfifo_select = '1' else kfifoB_empty;
kfifo_wr_rst_busy <= kfifoA_wr_rst_busy or kfifoB_wr_rst_busy;
kfifo_rd_rst_busy <= kfifoA_rd_rst_busy or kfifoB_rd_rst_busy;

kfifoA_i: entity work.KFIFO_Wrapper
port map(
  clk         => clk,
  reset        => reset,
  din         => kfifoA_din,
  wr_en       => kfifoA_wr_en,
  rd_en       => kfifoA_rd_en,
  dout        => kfifoA_dout,
  full        => kfifoA_full,
  almost_full => kfifoA_almost_full,
  empty       => kfifoA_empty,
  wr_rst_busy => kfifoA_wr_rst_busy,
  rd_rst_busy => kfifoA_rd_rst_busy
);

kfifoB_i: entity work.KFIFO_Wrapper
port map(
  clk         => clk,
  reset        => reset,
  din         => kfifoB_din,
  wr_en       => kfifoB_wr_en,
  rd_en       => kfifoB_rd_en,
  dout        => kfifoB_dout,
  full        => kfifoB_full,
  almost_full => kfifoB_almost_full,
  empty       => kfifoB_empty,
  wr_rst_busy => kfifoB_wr_rst_busy,
  rd_rst_busy => kfifoB_rd_rst_busy
);


-- drain KFIFO to output
condensed_out_int       <= kfifo_drain_dout(2**LOG_PROC_WIDTH-1 downto 0);
condensed_splitsite_int <= kfifo_drain_dout(2**LOG_PROC_WIDTH);
condensed_last_fs_int   <= kfifo_drain_dout(2**LOG_PROC_WIDTH+1); 
condensed_last_int      <= kfifo_drain_dout(2**LOG_PROC_WIDTH+2); 
condensed_valid_int     <= kfifo_drain;
kfifo_drain <= condensed_ready and not kfifo_drain_empty;

condensed_out       <= condensed_out_int      ;
condensed_splitsite <= condensed_splitsite_int;
condensed_last_fs   <= condensed_last_fs_int  ;
condensed_last      <= condensed_last_int     ;
condensed_valid     <= condensed_valid_int    ;

stop_next_site <= not kfifo_drain_empty; -- for now the simplest solution: don't start a new site if there's still data in the drain FIFO 

-- DEBUG
dbg_cnt_p: process
begin
  wait until rising_edge(clk);
  
  if checkincon_valid_out = '1' then
    checkincon_valid_cnt <= checkincon_valid_cnt + 1;
    if checkincon_last_fs_out = '1' then
      checkincon_lastfs_cnt <= checkincon_lastfs_cnt + 1;
      if checkincon_last_out = '1' then
        checkincon_last_cnt <= checkincon_last_cnt + 1;
      end if;
      if checkincon_splitsite_out = '1' then
        checkincon_split_cnt <= checkincon_split_cnt + 1;
      end if;
    end if;
  end if;
  if selectbh_valid_out = '1' then
    selectbh_valid_cnt <= selectbh_valid_cnt + 1;
    if selectbh_last_fs_out = '1' then
      selectbh_lastfs_cnt <= selectbh_lastfs_cnt + 1;
      if selectbh_last_out = '1' then
        selectbh_last_cnt <= selectbh_last_cnt + 1;
      end if;
      if selectbh_splitsite_out = '1' then
        selectbh_split_cnt <= selectbh_split_cnt + 1;
      end if;
    end if;
  end if;
  if condensed_valid_int = '1' then
    condensed_valid_cnt <= condensed_valid_cnt + 1;
    if condensed_last_fs_int = '1' then
      condensed_lastfs_cnt <= condensed_lastfs_cnt + 1;
      if condensed_last_int = '1' then
        condensed_last_cnt <= condensed_last_cnt + 1;
      end if;
      if condensed_splitsite_int = '1' then
        condensed_split_cnt <= condensed_split_cnt + 1;
      end if;
    end if;
  end if;
  
  if targetbuffer_get_next = '1' and targetbuffer_empty = '1' then
    empty_getnext_cnt <= empty_getnext_cnt + 1;
  end if;
  
end process dbg_cnt_p;

checkincon_valid_cnt_out <= checkincon_valid_cnt;
selectbh_valid_cnt_out   <= selectbh_valid_cnt  ;
condensed_valid_cnt_out  <= condensed_valid_cnt ;
checkincon_lastfs_cnt_out <= checkincon_lastfs_cnt;
checkincon_last_cnt_out   <= checkincon_last_cnt;
checkincon_split_cnt_out  <= checkincon_split_cnt;
selectbh_lastfs_cnt_out <= selectbh_lastfs_cnt;
selectbh_last_cnt_out   <= selectbh_last_cnt;
selectbh_split_cnt_out  <= selectbh_split_cnt;
condensed_lastfs_cnt_out <= condensed_lastfs_cnt;
condensed_last_cnt_out   <= condensed_last_cnt;
condensed_split_cnt_out  <= condensed_split_cnt;
empty_getnext_cnt_out  <= empty_getnext_cnt;
  
end Behavioral;

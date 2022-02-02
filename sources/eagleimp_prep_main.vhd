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

use work.eagleimp_prep_info.all;
use work.eagleimp_prep_pkg.all;

entity eagleimp_prep_main is
generic(
  NUM_PIPELINES : integer := 32
);
port(
  -- Clocks / Reset
  reset                : in  std_logic;
  pci_clk              : in  std_logic;
  ram_clk0             : in  std_logic;
  ram_clk0_reset       : in  std_logic;
  ram_clk1             : in  std_logic;
  ram_clk1_reset       : in  std_logic;
  user_clk0            : in  std_logic;
  user_clk1            : in  std_logic;

  -- PCIe DMA
  dma_din_data       : in  std_logic_vector(2**LOG_PCI_WIDTH-1 downto 0);
  dma_din_valid      : in  std_logic;
  dma_din_ready      : out std_logic;
  dma_dout_data      : out std_logic_vector(2**LOG_PCI_WIDTH-1 downto 0);
  dma_dout_valid     : out std_logic;
  dma_dout_ready     : in  std_logic;

  -- PCIe Register Interface
  reg_clk              : out std_logic;
  reg_we               : out std_logic;
  reg_addr             : out std_logic_vector(9 downto 0) := (others => '0');
  reg_din              : in  std_logic_vector(2**LOG_PCI_WIDTH-1 downto 0);
  reg_dout             : out std_logic_vector(2**LOG_PCI_WIDTH-1 downto 0);
  
  -- Status (clock independent, use false-path constraints for reading)
  status               : out std_logic_vector(7 downto 0);

  -- DRAM
  dr0_addr          : out std_logic_vector(RAM_ADDR_WIDTH-1 downto 0);
  dr0_cmd           : out std_logic_vector(2 downto 0);
  dr0_en            : out std_logic;
  dr0_wr_data       : out std_logic_vector(REAL_RAM_WIDTH-1 downto 0);
  dr0_wr_data_end   : out std_logic;
  dr0_wr_data_en    : out std_logic;
  dr0_rd_data       : in  std_logic_vector(REAL_RAM_WIDTH-1 downto 0);
  dr0_rd_data_end   : in  std_logic;
  dr0_rd_data_valid : in  std_logic;
  dr0_rdy           : in  std_logic;
  dr0_wr_rdy        : in  std_logic;
  dr1_addr          : out std_logic_vector(RAM_ADDR_WIDTH-1 downto 0);
  dr1_cmd           : out std_logic_vector(2 downto 0);
  dr1_en            : out std_logic;
  dr1_wr_data       : out std_logic_vector(REAL_RAM_WIDTH-1 downto 0);
  dr1_wr_data_end   : out std_logic;
  dr1_wr_data_en    : out std_logic;
  dr1_rd_data       : in  std_logic_vector(REAL_RAM_WIDTH-1 downto 0);
  dr1_rd_data_end   : in  std_logic;
  dr1_rd_data_valid : in  std_logic;
  dr1_rdy           : in  std_logic;
  dr1_wr_rdy        : in  std_logic
);
end eagleimp_prep_main;

architecture Behavioral of eagleimp_prep_main is 

signal pci_clk_sync_reset    : std_logic := '1';
signal ram_clk_sync_reset   : std_logic := '1';
signal user_clk0_sync_reset   : std_logic := '1';
signal user_clk1_sync_reset   : std_logic := '1';
signal ndcm_locked_or_host_reset : std_logic;
signal dcm_locked            : std_logic;

signal inbuffer_wr_en : std_logic := '0';
signal inbuffer_rd_en : std_logic := '0';
signal inbuffer_full  : std_logic;
signal inbuffer_empty : std_logic;
signal inbuffer_dout  : std_logic_vector(255 downto 0);
signal inbuffer_wr_rst_busy : std_logic;
signal inbuffer_rd_rst_busy : std_logic;
signal inbuffer_rd_req : std_logic := '0';

constant START_WORD : std_logic_vector(31 downto 0) := x"deadbeef";
type indata_state_t is (WAIT_FOR_START, CONSTANTS, TARGETCOUNTS, TARGETS, SPLITS, BESTHAPS, REFERENCES, REFERENCES_REV);
signal indata_state : indata_state_t := WAIT_FOR_START;
signal indata_state_tig : indata_state_t := WAIT_FOR_START;

-- constants
signal last_target_initword_tig    : unsigned(COUNT_WIDTH-1 downto 0); -- index of the last 256 bit target word of raw data from PCIe for one pipeline!
signal last_split_initword_tig     : unsigned(COUNT_WIDTH-1 downto 0); -- index of the last 256 bit split site word of raw data from PCIe for one pipeline!
signal last_hap_pcieword_tig       : unsigned(COUNT_WIDTH-1 downto 0); -- index of the last 256 bit best haps word of raw data from PCIe for one pipeline!
signal last_total_ref_pcieword_tig : unsigned(31 downto 0); -- index of the last 256 bit reference word of raw data from PCIe
signal last_site_idx_tig           : unsigned(COUNT_WIDTH-1 downto 0); -- number of sites per target/reference -1 !  
signal K_tig                       : unsigned(LOG_MAXK downto 0); -- number of conditioning haps (K)
signal K_ramwords_tig          : unsigned(COUNT_WIDTH-1 downto 0); -- number of conditioning haps (K) in 512 bit words
signal bufsize_pciwords        : unsigned(31 downto 0) := (0 => '1', others => '0'); -- buffer size in 256bit words
signal bufsize_pciwords_tig    : unsigned(31 downto 0) := (0 => '1', others => '0');
signal last_target_charge      : std_logic := '0'; -- signalizes, that this will be the last charge of targets, thus, padding of the outbuffer will be required afterwards
signal last_target_charge_tig  : std_logic := '0';
--signal total_cref_ramword_count : unsigned(RAM_ADDR_WIDTH-1 downto 0); -- number of ref/incon segments in ram words (512bit) for each target, accumulated

signal referencebuffer_reference_in     : std_logic_vector(2**LOG_PCI_WIDTH-1 downto 0);
signal referencebuffer_reference_valid  : std_logic := '0';
signal referencebuffer_reference_valid_int  : std_logic := '0';
signal referencebuffer_reference_ready  : std_logic := '0';
signal referencebuffer_rout_data           : std_logic_vector(2**LOG_REF_PROC_WIDTH-1 downto 0);
signal referencebuffer_rout_ready          : std_logic := '0';
signal referencebuffer_rout_valid          : std_logic := '0';
signal referencebuffer_rout_last_from_site : std_logic := '0';
signal referencebuffer_rout_last           : std_logic := '0';

signal pipelineinit_data_in0          : std_logic_vector(2**LOG_PIPELINE_INIT_WIDTH-1 downto 0);
signal pipelineinit_data_addr_in0     : unsigned(PIPELINEADDR_WIDTH-1 downto 0);
signal pipelineinit_data_tag_in0      : pipelineinit_data_tag_t := TGT_CNT;
signal pipelineinit_data_valid_in0    : std_logic := '0';
signal pipelineinit_data_last_in0     : std_logic := '0';
signal pipelineinit_data_in           : pipeline_init_word_array(0 to NUM_PIPELINES-1);
signal pipelineinit_data_addr_in      : pipelineaddr_array (0 to NUM_PIPELINES-1);
signal pipelineinit_data_tag_in       : pipelineinit_data_tag_array(0 to NUM_PIPELINES-1);
signal pipelineinit_data_valid_in     : std_logic_vector   (0 to NUM_PIPELINES-1) := (others => '0');
signal pipelineinit_data_last_in      : std_logic_vector   (0 to NUM_PIPELINES-1) := (others => '0');
signal pipelineinit_data_out          : pipeline_init_word_array(0 to NUM_PIPELINES-1);
signal pipelineinit_data_addr_out     : pipelineaddr_array (0 to NUM_PIPELINES-1);
signal pipelineinit_data_tag_out      : pipelineinit_data_tag_array(0 to NUM_PIPELINES-1);
signal pipelineinit_data_valid_out    : std_logic_vector   (0 to NUM_PIPELINES-1) := (others => '0');  
signal pipelineinit_data_last_out     : std_logic_vector   (0 to NUM_PIPELINES-1) := (others => '0');  

signal pipeline_tgt_gts_in        : pipeline_init_word_array(0 to NUM_PIPELINES-1);
signal pipeline_tgt_gts_valid     : std_logic_vector  (0 to NUM_PIPELINES-1) := (others => '0');
signal pipeline_tgt_gts_last      : std_logic_vector  (0 to NUM_PIPELINES-1) := (others => '0');
signal pipeline_split_sites_in    : pipeline_init_word_array(0 to NUM_PIPELINES-1);
signal pipeline_split_sites_valid : std_logic_vector  (0 to NUM_PIPELINES-1) := (others => '0');
signal pipeline_split_sites_last  : std_logic_vector  (0 to NUM_PIPELINES-1) := (others => '0');
signal pipeline_bh_in             : pipeline_init_word_array(0 to NUM_PIPELINES-1);
signal pipeline_bh_valid          : std_logic_vector  (0 to NUM_PIPELINES-1) := (others => '0');
signal pipeline_bh_last           : std_logic_vector  (0 to NUM_PIPELINES-1) := (others => '0');
signal pipeline_refdata_in        : refproc_array     (0 to NUM_PIPELINES-1);
signal pipeline_refdata_valid_in  : std_logic_vector  (0 to NUM_PIPELINES-1) := (others => '0');
signal pipeline_refdata_last_from_site_in  : std_logic_vector  (0 to NUM_PIPELINES-1);
signal pipeline_refdata_last_in   : std_logic_vector  (0 to NUM_PIPELINES-1);
signal pipeline_refdata_out       : refproc_array     (0 to NUM_PIPELINES-1);
signal pipeline_refdata_valid_out : std_logic_vector  (0 to NUM_PIPELINES-1) := (others => '0');
signal pipeline_refdata_last_from_site_out : std_logic_vector  (0 to NUM_PIPELINES-1);
signal pipeline_refdata_last_out  : std_logic_vector  (0 to NUM_PIPELINES-1);
signal pipeline_condensed_out     : refproc_array    (0 to NUM_PIPELINES-1);
signal pipeline_condensed_ready   : std_logic_vector  (0 to NUM_PIPELINES-1);
signal pipeline_condensed_valid   : std_logic_vector  (0 to NUM_PIPELINES-1) := (others => '0');
signal pipeline_condensed_last_fs : std_logic_vector  (0 to NUM_PIPELINES-1);
signal pipeline_condensed_last    : std_logic_vector  (0 to NUM_PIPELINES-1);
signal pipeline_condensed_splitsite : std_logic_vector  (0 to NUM_PIPELINES-1);
signal pipeline_stop_next_site    : std_logic_vector  (0 to NUM_PIPELINES-1) := (others => '0');
signal pipelineoutput_overflow_flag_out : std_logic_vector  (0 to NUM_PIPELINES-1) := (others => '0');
signal pipeline_pbwtwordcount     : ram_addr_array     (0 to NUM_PIPELINES-1);
signal pipeline_pbwtwordcount_tig : ram_addr_array     (0 to NUM_PIPELINES-1);
-- DEBUG
signal pipeline_checkincon_valid_cnt  : dbg_counter_array(0 to NUM_PIPELINES-1) := (others => (others => '0'));
signal pipeline_selectbh_valid_cnt    : dbg_counter_array(0 to NUM_PIPELINES-1) := (others => (others => '0'));
signal pipeline_condensed_valid_cnt   : dbg_counter_array(0 to NUM_PIPELINES-1) := (others => (others => '0')); 
signal pipeline_checkincon_lastfs_cnt : dbg_counter_array(0 to NUM_PIPELINES-1) := (others => (others => '0')); 
signal pipeline_checkincon_last_cnt   : dbg_counter_array(0 to NUM_PIPELINES-1) := (others => (others => '0')); 
signal pipeline_checkincon_split_cnt  : dbg_counter_array(0 to NUM_PIPELINES-1) := (others => (others => '0')); 
signal pipeline_selectbh_lastfs_cnt   : dbg_counter_array(0 to NUM_PIPELINES-1) := (others => (others => '0')); 
signal pipeline_selectbh_last_cnt     : dbg_counter_array(0 to NUM_PIPELINES-1) := (others => (others => '0')); 
signal pipeline_selectbh_split_cnt    : dbg_counter_array(0 to NUM_PIPELINES-1) := (others => (others => '0')); 
signal pipeline_condensed_lastfs_cnt  : dbg_counter_array(0 to NUM_PIPELINES-1) := (others => (others => '0')); 
signal pipeline_condensed_last_cnt    : dbg_counter_array(0 to NUM_PIPELINES-1) := (others => (others => '0')); 
signal pipeline_condensed_split_cnt   : dbg_counter_array(0 to NUM_PIPELINES-1) := (others => (others => '0')); 
signal pipeline_empty_getnext_cnt     : dbg_counter_array(0 to NUM_PIPELINES-1) := (others => (others => '0')); 
signal pipeline_switchundrained_out : std_logic_vector(0 to NUM_PIPELINES-1);
--signal pipeline_wrong_scount_gt_flag : std_logic_vector  (0 to NUM_PIPELINES-1) := (others => '0');
--signal pipeline_wrong_scount_lt_flag : std_logic_vector  (0 to NUM_PIPELINES-1) := (others => '0');
--signal pipeline_wrong_scount_eq1_count : ref_counter_array(0 to NUM_PIPELINES-1);
--signal pipeline_wrong_scount_gt1_count : ref_counter_array(0 to NUM_PIPELINES-1);
--signal pipeline_wrong_scount_gt_flag_tig : std_logic := '0';
--signal pipeline_wrong_scount_lt_flag_tig : std_logic := '0';
--signal pipeline_wrong_scount_eq1_acc_tig : unsigned(REF_COUNT_WIDTH-1 downto 0);
--signal pipeline_wrong_scount_gt1_acc_tig : unsigned(REF_COUNT_WIDTH-1 downto 0);
--type popcnt_array is array (natural range <>) of unsigned(15 downto 0);
--signal pipeline_refpopcnt : popcnt_array(0 to NUM_PIPELINES-1);
--signal pipeline_incpopcnt : popcnt_array(0 to NUM_PIPELINES-1);

signal block_stream : std_logic := '0';
signal pipeline_stall_tig : std_logic := '0'; -- need to stall the pipeline, i.e. don't stream further reference data!
signal pipeline_overflow_flag_tig : std_logic := '0';
signal pipeline_switchundrained_flag_tig : std_logic := '0';

signal pbwtpipeline_pbwt_data           : pbwtblock_array(0 to NUM_PIPELINES-1) := (others => (others => '0'));
signal pbwtpipeline_pbwt_ready          : std_logic_vector(0 to NUM_PIPELINES-1) := (others => '0');
signal pbwtpipeline_pbwt_valid          : std_logic_vector(0 to NUM_PIPELINES-1) := (others => '0');
signal pbwtpipeline_pbwt_splitsite      : std_logic_vector(0 to NUM_PIPELINES-1) := (others => '0');
signal pbwtpipeline_pbwt_last_fs        : std_logic_vector(0 to NUM_PIPELINES-1) := (others => '0');
signal pbwtpipeline_pbwt_last           : std_logic_vector(0 to NUM_PIPELINES-1) := (others => '0');
--signal pbwtpipeline_pbwt_cnt0           : count0_array(0 to NUM_PIPELINES-1) := (others => (others => '0'));

signal pipelineoutput_bus_data_in     : pipelinedata_array (0 to NUM_PIPELINES-1);
signal pipelineoutput_bus_addr_in     : ram_addr_array     (0 to NUM_PIPELINES-1);
signal pipelineoutput_bus_valid_in    : std_logic_vector   (0 to NUM_PIPELINES-1) := (others => '0');
signal pipelineoutput_bus_data_out    : pipelinedata_array (0 to NUM_PIPELINES-1);
signal pipelineoutput_bus_addr_out    : ram_addr_array     (0 to NUM_PIPELINES-1);
signal pipelineoutput_bus_valid_out   : std_logic_vector   (0 to NUM_PIPELINES-1) := (others => '0');
signal pipelineoutput_currsite_prev   : counter_array      (0 to NUM_PIPELINES-1);
signal pipelineoutput_currsite_next   : counter_array      (0 to NUM_PIPELINES-1);
signal pipelineoutput_currsite_toprev : counter_array      (0 to NUM_PIPELINES-1);
signal pipelineoutput_currsite_tonext : counter_array      (0 to NUM_PIPELINES-1);
--DEBUG
signal pipelineoutput_my_valid_cnt : dbg_counter_array(0 to NUM_PIPELINES-1);

signal pipelineoutput_stall_tig : std_logic := '0';
signal pipelineoutput_overflow_flag_tig : std_logic := '0';
signal pipeline_finished : std_logic := '0';
signal pipeline_finished_tig : std_logic := '0';
signal dram0_read_finished : std_logic := '1';
signal dram0_read_finished_tig : std_logic := '1';
signal dram0_reset_read_finished : std_logic := '0';
signal dram1_read_finished : std_logic := '1';
signal dram1_read_finished_tig : std_logic := '1';
signal dram1_reset_read_finished : std_logic := '0';

signal dram0_in_fifo_din         : std_logic_vector(2**LOG_RAM_WIDTH+2+RAM_ADDR_WIDTH-1 downto 0);
signal dram0_in_fifo_wr_en       : std_logic := '0';
signal dram0_in_fifo_rd_en       : std_logic := '0';
signal dram0_in_fifo_dout        : std_logic_vector(2**LOG_RAM_WIDTH+2+RAM_ADDR_WIDTH-1 downto 0);
signal dram0_in_fifo_full        : std_logic;
signal dram0_in_fifo_empty       : std_logic;
signal dram0_in_fifo_prog_full   : std_logic;
signal dram0_in_fifo_wr_rst_busy : std_logic;
signal dram0_in_fifo_rd_rst_busy : std_logic;
signal dram0_in_fifo_overflow : std_logic;

signal dram1_in_fifo_din         : std_logic_vector(2**LOG_RAM_WIDTH+2+RAM_ADDR_WIDTH-1 downto 0);
signal dram1_in_fifo_wr_en       : std_logic := '0';
signal dram1_in_fifo_rd_en       : std_logic := '0';
signal dram1_in_fifo_dout        : std_logic_vector(2**LOG_RAM_WIDTH+2+RAM_ADDR_WIDTH-1 downto 0);
signal dram1_in_fifo_full        : std_logic;
signal dram1_in_fifo_empty       : std_logic;
signal dram1_in_fifo_prog_full   : std_logic;
signal dram1_in_fifo_wr_rst_busy : std_logic;
signal dram1_in_fifo_rd_rst_busy : std_logic;
signal dram1_in_fifo_overflow : std_logic;

signal dram0_rd_addr : std_logic_vector(RAM_ADDR_WIDTH-1 downto 0);
signal dram0_wr_addr : std_logic_vector(RAM_ADDR_WIDTH-1 downto 0);
signal dram0_wd_addr        : std_logic_vector(RAM_ADDR_WIDTH-1 downto 0);
signal dram0_app_en         : std_logic := '0';
signal dram0_rd_data        : std_logic_vector(REAL_RAM_WIDTH-1 downto 0);
signal dram0_rd_data_end    : std_logic := '0';
signal dram0_rd_data_valid  : std_logic := '0';
signal dram0_app_rdy        : std_logic;
signal dram0_req            : std_logic := '0';

signal dram1_rd_addr : std_logic_vector(RAM_ADDR_WIDTH-1 downto 0);
signal dram1_wr_addr : std_logic_vector(RAM_ADDR_WIDTH-1 downto 0);
signal dram1_wd_addr        : std_logic_vector(RAM_ADDR_WIDTH-1 downto 0);
signal dram1_app_en         : std_logic := '0';
signal dram1_rd_data        : std_logic_vector(REAL_RAM_WIDTH-1 downto 0);
signal dram1_rd_data_end    : std_logic := '0';
signal dram1_rd_data_valid  : std_logic := '0';
signal dram1_app_rdy        : std_logic;
signal dram1_req            : std_logic := '0';

signal dram_wr_select : std_logic := '0'; -- selects the active RAM for pipeline output
signal dram_rd_select : std_logic := '0'; -- selects the active RAM for transmission to host

signal outbuffer0_ready : std_logic := '0';
signal outbuffer0_wr_en : std_logic := '0';
signal outbuffer0_rd_en : std_logic := '0';
signal outbuffer0_full  : std_logic;
signal outbuffer0_prog_full  : std_logic;
signal outbuffer0_empty : std_logic;
signal outbuffer0_din   : std_logic_vector(2**LOG_RAM_WIDTH-1 downto 0);
signal outbuffer0_dout  : std_logic_vector(2**LOG_PCI_WIDTH-1 downto 0);
signal outbuffer0_wr_rst_busy : std_logic := '0';
signal outbuffer0_rd_rst_busy : std_logic := '0';
signal outbuffer0_overflow_flag : std_logic := '0';

signal outbuffer1_ready : std_logic := '0';
signal outbuffer1_wr_en : std_logic := '0';
signal outbuffer1_rd_en : std_logic := '0';
signal outbuffer1_full  : std_logic;
signal outbuffer1_prog_full  : std_logic;
signal outbuffer1_empty : std_logic;
signal outbuffer1_din   : std_logic_vector(2**LOG_RAM_WIDTH-1 downto 0);
signal outbuffer1_dout  : std_logic_vector(2**LOG_PCI_WIDTH-1 downto 0);
signal outbuffer1_wr_rst_busy : std_logic := '0';
signal outbuffer1_rd_rst_busy : std_logic := '0';
signal outbuffer1_overflow_flag : std_logic := '0';

signal outbuffer_overflow_flag_tig : std_logic := '0';

signal dma_ins_pad : std_logic := '0';
signal dma_dout1_tvalid_int : std_logic;

signal host_reset_sig : std_logic := '0';
signal host_reset_userclk : std_logic := '0'; -- TIG

signal pipe_busy : std_logic := '0'; -- enabled when initialization data for block arrives and disabled when pipeline data has been written to RAM
signal pipe_busy_tig : std_logic := '0'; -- TIG version of pipe_busy (user_clk)
signal pipe_busy_tig2 : std_logic := '0'; -- TIG version of pipe_busy (ram_clk0) required for status
signal out0_busy : std_logic := '0'; -- enabled when reading data from RAM0 started and disabled when data has been written in the output queue for transmission to host
signal out1_busy : std_logic := '0'; -- enabled when reading data from RAM1 started and disabled when data has been written in the output queue for transmission to host
signal out_busy_tig : std_logic := '0'; -- TIG version of out0_busy AND out1_busy
signal busy_falling_edge : std_logic := '0'; -- indicates when busy is deasserted
signal unit_reset_userclk0 : std_logic := '0';
signal unit_reset_userclk1 : std_logic := '0';

-- for debugging:

signal reg_we_dbg       : std_logic := '0';
signal reg_addr_dbg     : std_logic_vector(9 downto 0) := (others => '0');
signal reg_dout_dbg     : std_logic_vector(2**LOG_PCI_WIDTH-1 downto 0);

signal dma_dout1_tready_tig : std_logic;
--signal inbuffer_full_tig : std_logic;
signal inbuffer_empty_tig : std_logic;

signal inbuffer_wr_cnt   : unsigned(63 downto 0) := (others => '0');  
signal inbuffer_rd_cnt   : unsigned(63 downto 0) := (others => '0');
signal outbuffer0_rd_cnt : unsigned(63 downto 0) := (others => '0');
signal outbuffer1_rd_cnt : unsigned(63 downto 0) := (others => '0');
signal pcie_wr_cnt       : unsigned(63 downto 0) := (others => '0');

signal dram0_rd_data_cnt : unsigned(63 downto 0) := (others => '0');
signal dram0_wr_data_cnt : unsigned(63 downto 0) := (others => '0');
signal dram0_app_en_cnt  : unsigned(63 downto 0) := (others => '0');
--signal dram0_rd_data_end_cnt : unsigned(63 downto 0) := (others => '0');
signal dram1_rd_data_cnt : unsigned(63 downto 0) := (others => '0');
signal dram1_wr_data_cnt : unsigned(63 downto 0) := (others => '0');
signal dram1_app_en_cnt  : unsigned(63 downto 0) := (others => '0');
--signal dram1_rd_data_end_cnt : unsigned(63 downto 0) := (others => '0');
--signal dram_rd_min_addr : unsigned(29 downto 0) := (others => '1');
--signal dram_wr_min_addr : unsigned(29 downto 0) := (others => '1');
--signal dram_rd_max_addr : unsigned(29 downto 0) := (others => '0');
--signal dram_wr_max_addr : unsigned(29 downto 0) := (others => '0');

signal pipe_busy_cnt : unsigned(63 downto 0) := (others => '0');
signal out_busy_cnt   : unsigned(63 downto 0) := (others => '0');
signal pipe_stall_cnt : unsigned(63 downto 0) := (others => '0');
signal out_stall_cnt  : unsigned(63 downto 0) := (others => '0');

signal block_cnt : unsigned(15 downto 0) := (others => '0');

signal dma_ins_pad_cnt : unsigned(31 downto 0) := (others => '0');
signal dma_next_word : unsigned(31 downto 0) := (others => '0');

begin

dcm_locked <= not reset;
ndcm_locked_or_host_reset <= not dcm_locked or host_reset_userclk;

pci_clk_sync_reset_p : process
begin
  wait until rising_edge(pci_clk);
  pci_clk_sync_reset <= ndcm_locked_or_host_reset;
end process pci_clk_sync_reset_p;

user_clk0_sync_reset_p : process
  constant UNIT_RESET_CYCLES : integer := 127;
  variable timer : integer range 0 to UNIT_RESET_CYCLES := 0;
begin
  wait until rising_edge(user_clk0);
  user_clk0_sync_reset <= ndcm_locked_or_host_reset;
  if timer = 0 then
    unit_reset_userclk0 <= ndcm_locked_or_host_reset;
    if busy_falling_edge = '1' then -- reset pipeline units whenever a bunch has finished
      unit_reset_userclk0 <= '1';
      timer := UNIT_RESET_CYCLES;
    end if;
  else
    timer := timer - 1;
  end if;
end process user_clk0_sync_reset_p;

user_clk1_sync_reset_p : process
begin
  wait until rising_edge(user_clk1);
  user_clk1_sync_reset <= ndcm_locked_or_host_reset;
  unit_reset_userclk1 <= unit_reset_userclk0; -- TIG
end process user_clk1_sync_reset_p;

ram_clk_sync_reset_p : process
begin
  wait until rising_edge(ram_clk0);
  ram_clk_sync_reset <= ndcm_locked_or_host_reset;
end process ram_clk_sync_reset_p;

-----------------------------------------------------------------------------------
-- WARNING! Ensure that the BRAM port this FPGA writes to is in "NO CHANGE" mode.
-- Otherwise, writing to the BRAM from the FPGA could cause a reset here! 
-----------------------------------------------------------------------------------
host_reset_sig <= reg_din(0);

host_reset_userclk_p: process
  variable timer : integer range 0 to HOST_RESET_CYCLES := 0;
begin
  wait until rising_edge(user_clk0); -- which has to be the same as reg_clk!
  
    if host_reset_sig = '1' and timer = 0 then -- reset flag is set by host
      timer := HOST_RESET_CYCLES;
      host_reset_userclk <= '1';
    elsif timer /= 0 then
      timer := timer - 1;
    else
      host_reset_userclk <= '0';
    end if;        
    
end process host_reset_userclk_p;

--host_reset_pciclk_p: process
--begin
--  wait until rising_edge(pci_clk);
--  host_reset_pciclk <= host_reset_ramclk; -- path to host reset pciclk is TIG
--end process host_reset_pciclk_p;

inbuffer_i : entity work.InbufferFIFO_Wrapper
port map(
  reset     => user_clk0_sync_reset,
  wr_clk  => pci_clk,  
  rd_clk  => user_clk0,  
  din     => dma_din_data,
  wr_en   => inbuffer_wr_en,
  rd_en   => inbuffer_rd_en,
  dout    => inbuffer_dout,
  full    => inbuffer_full,
  empty   => inbuffer_empty,
  wr_rst_busy => inbuffer_wr_rst_busy,
  rd_rst_busy => inbuffer_rd_rst_busy
);
inbuffer_wr_en  <= dma_din_valid and not inbuffer_full and not inbuffer_wr_rst_busy;
dma_din_ready <= not inbuffer_full and not inbuffer_wr_rst_busy;
inbuffer_rd_en <= inbuffer_rd_req and not inbuffer_empty and not inbuffer_rd_rst_busy;


io_p : process
  variable incounter : unsigned(31 downto 0) := (others => '0');
  variable last_target_initword : unsigned(COUNT_WIDTH-1 downto 0) := (others => '1');
  variable last_split_initword  : unsigned(COUNT_WIDTH-1 downto 0) := (others => '1');
  variable last_hap_pcieword    : unsigned(COUNT_WIDTH-1 downto 0) := (others => '1');
  variable last_total_ref_pcieword : unsigned(31 downto 0) := (others => '1');
  variable last_site_idx        : unsigned(COUNT_WIDTH-1 downto 0) := (others => '1');
  variable K                    : unsigned(LOG_MAXK downto 0) := (others => '1');
  variable K_ramwords           : unsigned(COUNT_WIDTH-1 downto 0) := (others => '1');
  variable pipeinit_addr        : unsigned(PIPELINEADDR_WIDTH-1 downto 0) := (others => '0');
  constant BUSY_DEL_CYCLES : integer := 31;
  variable busy_del_cnt : integer range 0 to BUSY_DEL_CYCLES := BUSY_DEL_CYCLES;
  variable init1st : std_logic := '0'; -- for the pipeline init the incoming words are split into two words, flag indicates if the first half is to be processed
begin
  wait until rising_edge(user_clk0);

  inbuffer_rd_req  <= '0';
  
  pipelineinit_data_valid_in0 <= '0';
  pipelineinit_data_last_in0 <= '0';
  referencebuffer_reference_valid_int <= '0';  
  
  last_target_initword_tig    <= last_target_initword;
  last_split_initword_tig     <= last_split_initword;
  last_hap_pcieword_tig       <= last_hap_pcieword;
  last_total_ref_pcieword_tig <= last_total_ref_pcieword;
  last_site_idx_tig           <= last_site_idx;
  K_tig                       <= K;
  K_ramwords_tig          <= K_ramwords;
  
  indata_state_tig <= indata_state;
  pipeline_finished_tig <= pipeline_finished;
  pipe_busy_tig <= pipe_busy;
  out_busy_tig <= out0_busy and out1_busy; -- only if both output FIFOs still contain data, we need to wait before further processing
  
  busy_falling_edge <= '0';
  
  dram0_reset_read_finished <= '0';
  dram1_reset_read_finished <= '0';
  
  if user_clk0_sync_reset = '1' then
    
    incounter := (others => '0');
    indata_state <= WAIT_FOR_START;
    pipeinit_addr := (others => '0');
    pipe_busy <= '0';
    busy_del_cnt := BUSY_DEL_CYCLES;
    init1st := '0';
    dram_wr_select <= '0';
    block_cnt <= (others => '0');
    
  else
    if pipe_busy = '0' then
      if indata_state /= WAIT_FOR_START then
        pipe_busy <= '1';
        dram0_reset_read_finished <= not dram_wr_select;
        dram1_reset_read_finished <= dram_wr_select;
      end if;
    else
      if pipeline_finished_tig = '1' then
        pipe_busy <= '0';
        busy_falling_edge <= '1';
        dram_wr_select <= not dram_wr_select; -- switch DRAM target
        block_cnt <= block_cnt + 1;
      end if;
    end if;
    
    if pipe_busy = '1' or out_busy_tig = '1' or unit_reset_userclk0 = '1' then -- gap between pipe_busy and potential out_busy must be smaller than BUSY_DEL_CYCLES!
      busy_del_cnt := BUSY_DEL_CYCLES;
    elsif busy_del_cnt /= 0 then
      busy_del_cnt := busy_del_cnt -1;
    end if;
    
    case indata_state is
    when WAIT_FOR_START =>
      if busy_del_cnt = 0 then
        inbuffer_rd_req <= '1'; -- only read if not busy
      end if;       
      if inbuffer_rd_en = '1' then
        if inbuffer_dout(31 downto 0) = START_WORD then
          indata_state <= CONSTANTS;
          inbuffer_rd_req <= '1'; -- can read as soon as data arrives
        end if;
      end if;

    when CONSTANTS =>
      inbuffer_rd_req <= '1'; -- can read as soon as data arrives
      if inbuffer_rd_en = '1' then

        -- load constants
        last_target_initword    := unsigned(inbuffer_dout(COUNT_WIDTH-4 downto 0)&'1'&'1'&'1'); -- last split word*2 +1
        last_split_initword     := unsigned(inbuffer_dout(COUNT_WIDTH-3 downto 0)&'1'&'1'); -- unit words to pipelinit init bus words
        K                       := unsigned(inbuffer_dout(32+LOG_MAXK downto 32)); -- number of conditioning haps (K)
        K_ramwords              := unsigned(inbuffer_dout(63+COUNT_WIDTH downto 64)); -- number of conditioning haps (K) in ram words (512 bit): ceil(K/512)
        last_total_ref_pcieword := unsigned(inbuffer_dout(126 downto 96)&'1'); -- unit words to PCIe words
        last_hap_pcieword       := unsigned(inbuffer_dout(126+COUNT_WIDTH downto 128)&'1'); -- unit words to pcie words
        last_site_idx           := unsigned(inbuffer_dout(159+COUNT_WIDTH downto 160)); -- number of common target and reference sites minus 1
        bufsize_pciwords        <= unsigned(inbuffer_dout(223 downto 192)); -- buffer size in PCIe words
        -- 254 downto 224 unused       
        last_target_charge      <= inbuffer_dout(255); -- indicates if this is the last charge of targets to process
        
        incounter := (others => '0');
        pipeinit_addr := (others => '0');
        init1st := '0';
        indata_state <= TARGETCOUNTS;
      end if;
            
    when TARGETCOUNTS =>
      inbuffer_rd_req <= '1'; -- can read as soon as data arrives
      pipelineinit_data_in0 <= inbuffer_dout(2**LOG_PIPELINE_INIT_WIDTH-1 downto 0); -- we only need the LSBs to initialize the counters
      pipelineinit_data_valid_in0 <= inbuffer_rd_en;-- or init2nd;
      pipelineinit_data_tag_in0 <= TGT_CNT; -- counter data
      pipelineinit_data_addr_in0 <= pipeinit_addr;
      
      if inbuffer_rd_en = '1' then --or init2nd = '1' then
        pipelineinit_data_last_in0 <= '1'; -- to be consistent, there is only one counter word for each target
        -- prepare for next target
        if pipeinit_addr = to_unsigned(NUM_PIPELINES-1, PIPELINEADDR_WIDTH) then
          incounter := (others => '0');
          pipeinit_addr := (others => '0');
          init1st := '1';
          inbuffer_rd_req <= '0'; -- wait before reading the next word, other half needs to be processed!
          indata_state <= TARGETS;
        else
          pipeinit_addr := pipeinit_addr + 1;
        end if;
      end if;
      
    when TARGETS =>      
      if init1st = '1' then
        pipelineinit_data_in0 <= inbuffer_dout(2**LOG_PIPELINE_INIT_WIDTH-1 downto 0);
      else
        pipelineinit_data_in0 <= inbuffer_dout(2**LOG_PCI_WIDTH-1 downto 2**LOG_PIPELINE_INIT_WIDTH);
      end if;
      pipelineinit_data_valid_in0 <= inbuffer_rd_en or (init1st and not inbuffer_empty);
      pipelineinit_data_tag_in0 <= TGT_DATA; -- target data
      pipelineinit_data_addr_in0 <= pipeinit_addr;
      
      if inbuffer_rd_en = '1' or (init1st = '1' and inbuffer_empty = '0') then
        if init1st = '1' then
          init1st := '0';
          inbuffer_rd_req <= '1'; -- can remove the data from the buffer in the next cycle
        else
          init1st := '1';
        end if;
        if incounter(COUNT_WIDTH-1 downto 0) = last_target_initword_tig then  
          -- signalize last
          pipelineinit_data_last_in0 <= '1';
          -- prepare for next target
          incounter := (others => '0');
          if pipeinit_addr = to_unsigned(NUM_PIPELINES-1, PIPELINEADDR_WIDTH) then
            pipeinit_addr := (others => '0');
            init1st := '1'; -- should already be the case...
            inbuffer_rd_req <= '0'; -- just in case...
            indata_state <= SPLITS;
          else
            pipeinit_addr := pipeinit_addr + 1;
          end if;
        else
          incounter := incounter + 1;
        end if;
      end if;
        
    when SPLITS =>        
      if init1st = '1' then
        pipelineinit_data_in0 <= inbuffer_dout(2**LOG_PIPELINE_INIT_WIDTH-1 downto 0);
      else
        pipelineinit_data_in0 <= inbuffer_dout(2**LOG_PCI_WIDTH-1 downto 2**LOG_PIPELINE_INIT_WIDTH);
      end if;
      pipelineinit_data_valid_in0 <= inbuffer_rd_en or (init1st and not inbuffer_empty);
      pipelineinit_data_tag_in0 <= SPLIT_DATA; -- split site information
      pipelineinit_data_addr_in0 <= pipeinit_addr;
      
      if inbuffer_rd_en = '1' or (init1st = '1' and inbuffer_empty = '0') then
        if init1st = '1' then
          init1st := '0';
          inbuffer_rd_req <= '1'; -- can remove the data from the buffer in the next cycle
        else
          init1st := '1';
        end if;
        if incounter(COUNT_WIDTH-1 downto 0) = last_split_initword_tig then  
          -- signalize last
          pipelineinit_data_last_in0 <= '1';
          -- prepare for next target
          incounter := (others => '0');
          if pipeinit_addr = to_unsigned(NUM_PIPELINES-1, PIPELINEADDR_WIDTH) then
            pipeinit_addr := (others => '0');
            init1st := '1'; -- should already be the case...
            inbuffer_rd_req <= '0'; -- just in case...
            indata_state <= BESTHAPS;
          else
            pipeinit_addr := pipeinit_addr + 1;
          end if;
        else
          incounter := incounter + 1;
        end if;
      end if;
      
    when BESTHAPS =>        
      if init1st = '1' then
        pipelineinit_data_in0 <= inbuffer_dout(2**LOG_PIPELINE_INIT_WIDTH-1 downto 0);
      else
        pipelineinit_data_in0 <= inbuffer_dout(2**LOG_PCI_WIDTH-1 downto 2**LOG_PIPELINE_INIT_WIDTH);
      end if;
      pipelineinit_data_valid_in0 <= inbuffer_rd_en or (init1st and not inbuffer_empty);
      pipelineinit_data_tag_in0 <= BH_DATA; -- best haps 
      pipelineinit_data_addr_in0 <= pipeinit_addr;
      
      if inbuffer_rd_en = '1' or (init1st = '1' and inbuffer_empty = '0') then
        if init1st = '1' then
          init1st := '0';
          inbuffer_rd_req <= '1'; -- can remove the data from the buffer in the next cycle
        else
          init1st := '1';
        end if;
        if incounter(COUNT_WIDTH downto 0) = (last_hap_pcieword_tig&'1') then -- convert last pcie word to last init word  
          -- signalize last
          pipelineinit_data_last_in0 <= '1';
          -- prepare for next target
          incounter := (others => '0');
          if pipeinit_addr = to_unsigned(NUM_PIPELINES-1, PIPELINEADDR_WIDTH) then
            pipeinit_addr := (others => '0');
            init1st := '1'; -- should already be the case... and is not required for the reference buffer anyway
            inbuffer_rd_req <= '0'; -- just in case...
            indata_state <= REFERENCES;
          else
            pipeinit_addr := pipeinit_addr + 1;
          end if;
        else
          incounter := incounter + 1;
        end if;
      end if;
        
    when REFERENCES | REFERENCES_REV => 
      inbuffer_rd_req <= '1'; -- can read as soon as data arrives
      if inbuffer_rd_en = '1' then
        -- provide data to reference buffer
        referencebuffer_reference_in <= inbuffer_dout;
        referencebuffer_reference_valid_int <= '1';
        inbuffer_rd_req <= '0'; -- block further requests until it is accepted
      end if;
        
      if referencebuffer_reference_valid_int = '1' then
        if referencebuffer_reference_ready = '0' then
          -- buffer did not accept the datum
          referencebuffer_reference_valid_int <= '1'; -- offer again
          inbuffer_rd_req <= '0'; -- block further
        else
          -- count
          if incounter = last_total_ref_pcieword_tig then
            -- references have finished
            incounter := (others => '0');
            if indata_state = REFERENCES then
              indata_state <= REFERENCES_REV; -- now reverse data arrives
            else -- REFERENCES_REV
              indata_state <= WAIT_FOR_START;
            end if;
            inbuffer_rd_req <= '0'; -- block further until next state accepts data
          else
            incounter := incounter + 1;
          end if;
        end if;
      end if; 

    end case;

  end if;
  
end process io_p;


refbuf_i: entity work.ReferenceBuffer
generic map (
  DIN_WIDTH => 2**LOG_PCI_WIDTH,
  DOUT_WIDTH => 2**LOG_REF_PROC_WIDTH,
  COUNT_WIDTH => COUNT_WIDTH
)
port map (
  clk   => user_clk0,
  reset => unit_reset_userclk0,
  -- Constants: set to TIG!
  last_site_idx       => last_site_idx_tig,
  last_word_per_site  => last_hap_pcieword_tig,
  -- in
  reference_in        => referencebuffer_reference_in    ,
  reference_valid     => referencebuffer_reference_valid ,
  reference_ready     => referencebuffer_reference_ready ,
  -- out
  rout_data           => referencebuffer_rout_data             ,
  rout_ready          => referencebuffer_rout_ready            ,
  rout_valid          => referencebuffer_rout_valid            ,
  rout_last_from_site => referencebuffer_rout_last_from_site   ,
  rout_last           => referencebuffer_rout_last             
);
referencebuffer_reference_valid <= referencebuffer_reference_valid_int and referencebuffer_reference_ready;

-- stream is stopped between sites, and between forward and backward run: see block_stream_p below 
referencebuffer_rout_ready <= not block_stream;

block_stream_p: process
  -- wait some cycles after one site before starting the next:
  -- - at least two cycles are required for target buffers to reload a data word
  -- - at least one cycle is required by SelectBestHaps
  -- - pipelines may stall the beginning of the next site if their output FIFOs are not drained yet
  -- - when switching from fwd to bck the target buffers in the pipeline need to skip some data when switching to reverse -> wait at least PIPELINE_INIT_WIDTH cycles
  constant BLOCKCYCLES : integer := 1; -- covers all needs (two cycles are added anyway due to structure of state machine)
  variable block_cnt : integer range 0 to 2**LOG_PIPELINE_INIT_WIDTH := 0;
  type block_stream_state_t is (NOBLOCK, BLOCKTIMEOUT, BLOCKCHECKSTALL);
  variable block_stream_state : block_stream_state_t := NOBLOCK;
begin
  wait until rising_edge(user_clk0);
  
  if unit_reset_userclk0 = '1' then
    block_stream_state := NOBLOCK;
    block_cnt := 0;
    block_stream <= '0';
  else
    
    case block_stream_state is 
    
    when NOBLOCK =>
      block_stream <= '0';
      if referencebuffer_rout_last_from_site = '1' then
        if referencebuffer_rout_last = '1' then 
          -- this was the last data for this target 
          -- -> switch to reverse 
          -- -> need to wait up to 128 cycles to let the target buffers in the pipelines switch to reverse reading
          block_cnt := 2**LOG_PIPELINE_INIT_WIDTH;
        else 
          block_cnt := BLOCKCYCLES;
        end if;
        block_stream_state := BLOCKTIMEOUT;
        block_stream <= '1';
      end if;
    
    when BLOCKTIMEOUT =>
      block_stream <= '1';
      if block_cnt /= 0 then
        block_cnt := block_cnt - 1;
      else
        block_stream_state := BLOCKCHECKSTALL;
      end if;
    
    when BLOCKCHECKSTALL =>
      block_stream <= '1';
      if pipeline_stall_tig = '0' then
        -- release
        block_stream <= '0';
        block_stream_state := NOBLOCK;
      end if;
    
    end case;
    
  end if;
end process block_stream_p; 
     

pipelines_g: for P in 0 to NUM_PIPELINES-1 generate
  
  pipelineinit_i: entity work.PipelineInit
  generic map (
    ID => P,
    DATA_WIDTH => 2**LOG_PIPELINE_INIT_WIDTH,
    ADDR_WIDTH => PIPELINEADDR_WIDTH,
    COUNT_WIDTH => COUNT_WIDTH,
    WORDCOUNT_WIDTH => RAM_ADDR_WIDTH
  )
  port map (
    clk => user_clk0,
    reset => unit_reset_userclk0,
    -- Bus input
    data_in         => pipelineinit_data_in        (P),  
    data_addr_in    => pipelineinit_data_addr_in   (P),  
    data_tag_in  => pipelineinit_data_tag_in    (P),  
    data_valid_in   => pipelineinit_data_valid_in  (P),  
    data_last_in    => pipelineinit_data_last_in   (P),  
    -- Bus output                                 
    data_out        => pipelineinit_data_out       (P),  
    data_addr_out   => pipelineinit_data_addr_out  (P),  
    data_tag_out => pipelineinit_data_tag_out   (P),  
    data_valid_out  => pipelineinit_data_valid_out (P),  
    data_last_out   => pipelineinit_data_last_out  (P),  
    -- Pipeline init output
    tgt_gts_out       => pipeline_tgt_gts_in       (P),
    tgt_gts_valid     => pipeline_tgt_gts_valid    (P),
    tgt_gts_last      => pipeline_tgt_gts_last     (P),
    split_sites_out   => pipeline_split_sites_in   (P), 
    split_sites_valid => pipeline_split_sites_valid(P),
    split_sites_last  => pipeline_split_sites_last (P),
    besthaps_out      => pipeline_bh_in   (P), 
    besthaps_valid    => pipeline_bh_valid(P),
    besthaps_last     => pipeline_bh_last (P),
    splitcount_out    => open,
    pbwtwordcount_out => pipeline_pbwtwordcount_tig   (P)
  );
  
  crefpipeline_i: entity work.CondensedRefPipeline
  generic map (
    LOG_INIT_WIDTH  => LOG_PIPELINE_INIT_WIDTH,
    LOG_PROC_WIDTH  => LOG_REF_PROC_WIDTH,
    COUNT_WIDTH => COUNT_WIDTH
  )
  port map (
    clk            => user_clk0,
    reset          => unit_reset_userclk0,
    -- constants
    last_site_idx       => last_site_idx_tig,
    -- target buffer initialization
    tgt_gts_in        => pipeline_tgt_gts_in(P),
    tgt_gts_valid     => pipeline_tgt_gts_valid(P),
    tgt_gts_last      => pipeline_tgt_gts_last(P),
    tgt_gts_ready     => open,
    split_sites_in    => pipeline_split_sites_in(P),
    split_sites_valid => pipeline_split_sites_valid(P),
    split_sites_last  => pipeline_split_sites_last(P),
    split_sites_ready => open,
    besthaps_in       => pipeline_bh_in(P),
    besthaps_valid    => pipeline_bh_valid(P),
    besthaps_last     => pipeline_bh_last(P),
    besthaps_ready    => open,
    -- reference stream input
    refdata_in         => pipeline_refdata_in(P),
    refdata_valid_in   => pipeline_refdata_valid_in(P),
    refdata_last_from_site_in => pipeline_refdata_last_from_site_in(P),
    refdata_last_in    => pipeline_refdata_last_in(P),
    -- reference stream delayed output (for next pipeline)
    refdata_out        => pipeline_refdata_out(P),
    refdata_valid_out  => pipeline_refdata_valid_out(P),
    refdata_last_from_site_out => pipeline_refdata_last_from_site_out(P),
    refdata_last_out   => pipeline_refdata_last_out(P),
    -- buffered condensed output
    condensed_out       => pipeline_condensed_out(P),
    condensed_ready     => pipeline_condensed_ready(P),
    condensed_valid     => pipeline_condensed_valid(P),
    condensed_splitsite => pipeline_condensed_splitsite(P),
    condensed_last_fs   => pipeline_condensed_last_fs(P),
    condensed_last      => pipeline_condensed_last(P),
    stop_next_site      => pipeline_stop_next_site(P)
    
    -- DEBUG
   ,checkincon_valid_cnt_out => pipeline_checkincon_valid_cnt(P),
    selectbh_valid_cnt_out   => pipeline_selectbh_valid_cnt  (P),
    condensed_valid_cnt_out  => pipeline_condensed_valid_cnt (P),
    checkincon_lastfs_cnt_out => pipeline_checkincon_lastfs_cnt(P)(31 downto 0),
    checkincon_last_cnt_out   => pipeline_checkincon_last_cnt  (P)(15 downto 0),
    checkincon_split_cnt_out  => pipeline_checkincon_split_cnt (P)(15 downto 0),
    selectbh_lastfs_cnt_out => pipeline_selectbh_lastfs_cnt(P)(31 downto 0),
    selectbh_last_cnt_out   => pipeline_selectbh_last_cnt  (P)(15 downto 0),
    selectbh_split_cnt_out  => pipeline_selectbh_split_cnt (P)(15 downto 0),
    condensed_lastfs_cnt_out => pipeline_condensed_lastfs_cnt(P)(31 downto 0),
    condensed_last_cnt_out   => pipeline_condensed_last_cnt  (P)(15 downto 0),
    condensed_split_cnt_out  => pipeline_condensed_split_cnt (P)(15 downto 0),
    empty_getnext_cnt_out => pipeline_empty_getnext_cnt(P)(15 downto 0),
    switchundrained_out => pipeline_switchundrained_out(P) 
    
--    -- current reference
--    curr_site          => pipeline_curr_site(P),
--    -- pipeline stall required (set to TIG!)
--    stall_out          => pipeline_stall_out(P),
--    -- flag is set if an overflow occurred
--    overflow_flag_out  => pipeline_overflow_flag_out(P),
    -- DEBUG
--    dbg_curr_block => dbg_curr_block,
--    dbg_curr_tgt   => to_unsigned(P, 8)
--    splitcount    => pipeline_splitcount(P) 
--    wrong_scount_gt_flag => pipeline_wrong_scount_gt_flag(P),
--    wrong_scount_lt_flag => pipeline_wrong_scount_lt_flag(P),
--    wrong_scount_eq1_count => pipeline_wrong_scount_eq1_count(P),
--    wrong_scount_gt1_count => pipeline_wrong_scount_gt1_count(P)
--    refpopcnt_out => pipeline_refpopcnt(P),    
--    incpopcnt_out => pipeline_incpopcnt(P)    
  );
  
  pbwtpipeline_i: entity work.PBWTPipeline
    generic map(
      LOG_PROC_WIDTH => LOG_REF_PROC_WIDTH,
      LOG_PBWT_BLOCK_WIDTH => LOG_PBWT_BLOCK_WIDTH,
      LOG_MAXK       => LOG_MAXK
    )
    port map(
      proc_clk            => user_clk0,
      pbwt_clk            => user_clk1,
      reset_proc          => unit_reset_userclk0,
      reset_pbwt          => unit_reset_userclk1,
      K                   => K_tig,
      condensed_data      => pipeline_condensed_out      (P),
      condensed_ready     => pipeline_condensed_ready    (P),
      condensed_valid     => pipeline_condensed_valid    (P),
      condensed_splitsite => pipeline_condensed_splitsite(P),
      condensed_last_fs   => pipeline_condensed_last_fs  (P),
      condensed_last      => pipeline_condensed_last     (P),
      pbwt_data           => pbwtpipeline_pbwt_data          (P),
      pbwt_ready          => pbwtpipeline_pbwt_ready         (P),
      pbwt_valid          => pbwtpipeline_pbwt_valid         (P),
      pbwt_splitsite      => pbwtpipeline_pbwt_splitsite     (P),
      pbwt_last_fs        => pbwtpipeline_pbwt_last_fs       (P),
      pbwt_last           => pbwtpipeline_pbwt_last          (P)--,
--      pbwt_cnt0           => pbwtpipeline_pbwt_cnt0          (P)
    );
  
  pipelineoutput_i: entity work.PipelineOutput
  generic map (
    BUS_WIDTH => 2**LOG_RAM_WIDTH+2,
    PBWTBLOCKS_PER_BUS => 16,
    BUS_ADDR_WIDTH => RAM_ADDR_WIDTH,
    LOG_PBWT_BLOCK_WIDTH => LOG_PBWT_BLOCK_WIDTH,
    LOG_MAXK => LOG_MAXK,
    COUNT_WIDTH => COUNT_WIDTH
    -- DEBUG
   ,DBG_COUNT_WIDTH => DBG_COUNT_WIDTH 
  )
  port map (
    clk   => user_clk0,
    reset => unit_reset_userclk0,
    -- pipeline output
    pbwt_data      => pbwtpipeline_pbwt_data   (P),
    pbwt_ready     => pbwtpipeline_pbwt_ready  (P),
    pbwt_valid     => pbwtpipeline_pbwt_valid  (P),
    pbwt_splitsite => pbwtpipeline_pbwt_splitsite(P),
    pbwt_last_fs   => pbwtpipeline_pbwt_last_fs(P),
    pbwt_last      => pbwtpipeline_pbwt_last   (P),
--    pbwt_count0    => pbwtpipeline_pbwt_cnt0   (P),
    -- considered constant (TIG?)
    K              => K_tig,
    Kpbwtwords     => K_ramwords_tig,
    pbwtwordcount  => pipeline_pbwtwordcount (P), 
    -- bus input     
    bus_data_in   => pipelineoutput_bus_data_in  (P), 
    bus_addr_in   => pipelineoutput_bus_addr_in  (P), 
    bus_valid_in  => pipelineoutput_bus_valid_in (P), 
    -- bus output    
    bus_data_out  => pipelineoutput_bus_data_out (P), 
    bus_addr_out  => pipelineoutput_bus_addr_out (P), 
    bus_valid_out => pipelineoutput_bus_valid_out(P),
    -- site counts (to introduce order for site data)
    currsite_prev   => pipelineoutput_currsite_prev  (P),
    currsite_next   => pipelineoutput_currsite_next  (P),
    currsite_toprev => pipelineoutput_currsite_toprev(P), 
    currsite_tonext => pipelineoutput_currsite_tonext(P),
    -- bus stall
    stall_in      => pipelineoutput_stall_tig,
    overflow_flag_out  => pipelineoutput_overflow_flag_out(P)
    -- DEBUG
   ,my_valid_cnt_out => pipelineoutput_my_valid_cnt(P)
  );

end generate pipelines_g;

pipeline_interconnect_g: for P in 1 to NUM_PIPELINES-1 generate
  
  -- pipeline init bus
  pipelineinit_data_in       (P) <= pipelineinit_data_out       (P-1);
  pipelineinit_data_addr_in  (P) <= pipelineinit_data_addr_out  (P-1);
  pipelineinit_data_tag_in   (P) <= pipelineinit_data_tag_out   (P-1);
  pipelineinit_data_valid_in (P) <= pipelineinit_data_valid_out (P-1);
  pipelineinit_data_last_in  (P) <= pipelineinit_data_last_out  (P-1);

  -- pipeline stream bus
  pipeline_refdata_in      (P) <= pipeline_refdata_out      (P-1);
  pipeline_refdata_valid_in(P) <= pipeline_refdata_valid_out(P-1);
  pipeline_refdata_last_from_site_in (P) <= pipeline_refdata_last_from_site_out (P-1);
  pipeline_refdata_last_in (P) <= pipeline_refdata_last_out (P-1);
  
  -- pipeline output bus (reverse)
  pipelineoutput_bus_data_in (P-1) <= pipelineoutput_bus_data_out (P);
  pipelineoutput_bus_addr_in (P-1) <= pipelineoutput_bus_addr_out (P);
  pipelineoutput_bus_valid_in(P-1) <= pipelineoutput_bus_valid_out(P);
  
  -- reference counts
  pipelineoutput_currsite_prev(P)   <= pipelineoutput_currsite_tonext(P-1);
  pipelineoutput_currsite_next(P-1) <= pipelineoutput_currsite_toprev(P);
  
end generate pipeline_interconnect_g;

-- TIG sampling of "constants"
cnt_tig_p: process
begin
  wait until rising_edge(user_clk0);
  
  for P in 0 to NUM_PIPELINES-1 loop
    pipeline_pbwtwordcount(P) <= pipeline_pbwtwordcount_tig(P);
  end loop;

end process cnt_tig_p;

-- init pipeline
pipelineinit_data_in       (0) <= pipelineinit_data_in0       ;
pipelineinit_data_addr_in  (0) <= pipelineinit_data_addr_in0  ;
pipelineinit_data_tag_in   (0) <= pipelineinit_data_tag_in0   ;
pipelineinit_data_valid_in (0) <= pipelineinit_data_valid_in0 ;
pipelineinit_data_last_in  (0) <= pipelineinit_data_last_in0  ;

-- reference stream into the pipeline
pipeline_refdata_in      (0) <= referencebuffer_rout_data;
pipeline_refdata_valid_in(0) <= referencebuffer_rout_valid;
pipeline_refdata_last_from_site_in(0) <= referencebuffer_rout_last_from_site;
pipeline_refdata_last_in (0) <= referencebuffer_rout_last;

-- empty output stream for last output entity
pipelineoutput_bus_data_in(NUM_PIPELINES-1) <= (others => '0');
pipelineoutput_bus_addr_in(NUM_PIPELINES-1) <= (others => '0');
pipelineoutput_bus_valid_in(NUM_PIPELINES-1) <= '0';

-- reference counts set to max at pipeline ends
pipelineoutput_currsite_prev(0)               <= (others => '1');
pipelineoutput_currsite_next(NUM_PIPELINES-1) <= (others => '1');

-- combine stalls and overflows
pipeline_combine_p: process
  variable tmp_stall : std_logic := '0'; -- TIG to
  variable tmp_ov : std_logic := '0';    -- TIG to
  variable tmp_swundr : std_logic := '0';    -- TIG to
--  variable tmp_wrong_scount_gt_flag : std_logic := '0';
--  variable tmp_wrong_scount_lt_flag : std_logic := '0';
--  variable tmp_acc_eq1 : ref_counter_array(0 to NUM_PIPELINES-1) := (others => (others => '0'));
--  variable tmp_acc_gt1 : ref_counter_array(0 to NUM_PIPELINES-1) := (others => (others => '0'));
begin
  wait until rising_edge(user_clk0);
  
  tmp_stall := '0';
  tmp_ov := '0';
  tmp_swundr := '0';
--  -- DEBUG
--  tmp_wrong_scount_gt_flag := '0';
--  tmp_wrong_scount_lt_flag := '0';
  for P in 0 to NUM_PIPELINES-1 loop
    tmp_stall := tmp_stall or pipeline_stop_next_site(P);
    tmp_ov := tmp_ov or pipelineoutput_overflow_flag_out(P);
    tmp_swundr := tmp_swundr or pipeline_switchundrained_out(P);
--    -- DEBUG
--    tmp_wrong_scount_gt_flag := tmp_wrong_scount_gt_flag or pipeline_wrong_scount_gt_flag(P);
--    tmp_wrong_scount_lt_flag := tmp_wrong_scount_lt_flag or pipeline_wrong_scount_lt_flag(P);
  end loop;
  
  pipeline_stall_tig <= tmp_stall;
  pipeline_overflow_flag_tig <= pipeline_overflow_flag_tig or tmp_ov;
  pipeline_switchundrained_flag_tig <= pipeline_switchundrained_flag_tig or tmp_swundr;
  
  pipelineoutput_stall_tig <= (dram0_in_fifo_prog_full and not dram0_in_fifo_wr_rst_busy) or (dram1_in_fifo_prog_full and not dram1_in_fifo_wr_rst_busy);
  pipelineoutput_overflow_flag_tig <= pipelineoutput_overflow_flag_tig or dram0_in_fifo_overflow or dram1_in_fifo_overflow;
  
--  -- DEBUG
--  pipeline_wrong_scount_gt_flag_tig <= pipeline_wrong_scount_gt_flag_tig or tmp_wrong_scount_gt_flag;
--  pipeline_wrong_scount_lt_flag_tig <= pipeline_wrong_scount_lt_flag_tig or tmp_wrong_scount_lt_flag;
--  
--  pipeline_wrong_scount_eq1_acc_tig <= tmp_acc_eq1(0);
--  pipeline_wrong_scount_gt1_acc_tig <= tmp_acc_gt1(0);
--  for P in 0 to NUM_PIPELINES-2 loop
--    tmp_acc_eq1(P) := tmp_acc_eq1(P+1) + pipeline_wrong_scount_eq1_count(P);
--    tmp_acc_gt1(P) := tmp_acc_gt1(P+1) + pipeline_wrong_scount_gt1_count(P);
--  end loop;
--  tmp_acc_eq1(NUM_PIPELINES-1) := pipeline_wrong_scount_eq1_count(NUM_PIPELINES-1);
--  tmp_acc_gt1(NUM_PIPELINES-1) := pipeline_wrong_scount_gt1_count(NUM_PIPELINES-1);
   
  if user_clk0_sync_reset = '1' then
    pipeline_overflow_flag_tig <= '0';
    pipelineoutput_overflow_flag_tig <= '0';
    pipeline_switchundrained_flag_tig <= '0';
--    pipeline_wrong_scount_gt_flag_tig <= '0';
--    pipeline_wrong_scount_lt_flag_tig <= '0';
  end if;
   
end process pipeline_combine_p;

dram0_in_fifo_din(2**LOG_RAM_WIDTH+1 downto 0) <= pipelineoutput_bus_data_out(0);
dram0_in_fifo_din(2**LOG_RAM_WIDTH+RAM_ADDR_WIDTH+1 downto 2**LOG_RAM_WIDTH+2) <= std_logic_vector(pipelineoutput_bus_addr_out(0));
dram0_in_fifo_wr_en <= pipelineoutput_bus_valid_out(0) and not dram_wr_select;

dram0_in_fifo_i: entity work.DRAM0_inFIFO_Wrapper
port map(
  reset         => user_clk0_sync_reset,
  wr_clk      => user_clk0,
  rd_clk      => ram_clk0,
  din         => dram0_in_fifo_din,
  wr_en       => dram0_in_fifo_wr_en,
  rd_en       => dram0_in_fifo_rd_en,
  dout        => dram0_in_fifo_dout,
  full        => dram0_in_fifo_full,
  empty       => dram0_in_fifo_empty,
  prog_full   => dram0_in_fifo_prog_full, -- 96 words before full
  wr_rst_busy => dram0_in_fifo_wr_rst_busy,
  rd_rst_busy => dram0_in_fifo_rd_rst_busy
);
dram0_in_fifo_overflow <= dram0_in_fifo_full and dram0_in_fifo_wr_en;

dram1_in_fifo_din(2**LOG_RAM_WIDTH+1 downto 0) <= pipelineoutput_bus_data_out(0);
dram1_in_fifo_din(2**LOG_RAM_WIDTH+RAM_ADDR_WIDTH+1 downto 2**LOG_RAM_WIDTH+2) <= std_logic_vector(pipelineoutput_bus_addr_out(0));
dram1_in_fifo_wr_en <= pipelineoutput_bus_valid_out(0) and dram_wr_select;

dram1_in_fifo_i: entity work.DRAM0_inFIFO_Wrapper
port map(
  reset         => user_clk0_sync_reset,
  wr_clk      => user_clk0,
  rd_clk      => ram_clk1,
  din         => dram1_in_fifo_din,
  wr_en       => dram1_in_fifo_wr_en,
  rd_en       => dram1_in_fifo_rd_en,
  dout        => dram1_in_fifo_dout,
  full        => dram1_in_fifo_full,
  empty       => dram1_in_fifo_empty,
  prog_full   => dram1_in_fifo_prog_full, -- 96 words before full
  wr_rst_busy => dram1_in_fifo_wr_rst_busy,
  rd_rst_busy => dram1_in_fifo_rd_rst_busy
);
dram1_in_fifo_overflow <= dram1_in_fifo_full and dram1_in_fifo_wr_en;


pipeline_finished_p: process
  variable wrcnt : integer range 0 to 2*NUM_PIPELINES; 
begin
  wait until rising_edge(user_clk0);
  
  if unit_reset_userclk0 = '1' then
    pipeline_finished <= '0';
    wrcnt := 0;
  else
    if wrcnt = 2*NUM_PIPELINES then -- each target generates two PBWTs (fwd + bck)
      pipeline_finished <= '1'; -- signalizes that we can switch RAM for the next pipeline run, but NOT that we can start reading from RAM!!!
    end if; 
    -- check the input of the RAM FIFO and count each datum flagged "last" and "last_fs"
    if pipelineoutput_bus_valid_out(0) = '1' and pipelineoutput_bus_data_out(0)(2**LOG_RAM_WIDTH) = '1' and pipelineoutput_bus_data_out(0)(2**LOG_RAM_WIDTH+1) = '1' then
      wrcnt := wrcnt + 1;
    end if;
  end if;
  
end process pipeline_finished_p;


-- DRAM address mapping (from 512bit words to 64bit words)
dram0_wr_addr(RAM_ADDR_WIDTH-1 downto 3) <= dram0_in_fifo_dout(2**LOG_RAM_WIDTH+RAM_ADDR_WIDTH+1-3 downto 2**LOG_RAM_WIDTH+2);
dram0_wr_addr(2 downto 0) <= "000"; 
dram0_rd_addr(RAM_ADDR_WIDTH-1 downto 3) <= dram0_wd_addr(RAM_ADDR_WIDTH-4 downto 0);
dram0_rd_addr(2 downto 0) <= "000"; 
dram1_wr_addr(RAM_ADDR_WIDTH-1 downto 3) <= dram1_in_fifo_dout(2**LOG_RAM_WIDTH+RAM_ADDR_WIDTH+1-3 downto 2**LOG_RAM_WIDTH+2);
dram1_wr_addr(2 downto 0) <= "000"; 
dram1_rd_addr(RAM_ADDR_WIDTH-1 downto 3) <= dram1_wd_addr(RAM_ADDR_WIDTH-4 downto 0);
dram1_rd_addr(2 downto 0) <= "000"; 

-- memory access (c0)
dr0_addr        <= dram0_wr_addr when dram0_in_fifo_rd_en = '1' else dram0_rd_addr;
dr0_cmd         <= MEM_CMD_WR when dram0_in_fifo_rd_en = '1' else MEM_CMD_RD;
dr0_en          <= dram0_in_fifo_rd_en or dram0_app_en;
dram0_in_fifo_rd_en <= dr0_rdy and dr0_wr_rdy and not dram0_in_fifo_empty and not dram0_in_fifo_rd_rst_busy;
dr0_wr_data(REAL_RAM_WIDTH-1 downto 2**LOG_RAM_WIDTH+2) <= (others => '0'); -- yet unused
dr0_wr_data(2**LOG_RAM_WIDTH+1 downto 2**LOG_RAM_WIDTH) <= dram0_in_fifo_dout(2**LOG_RAM_WIDTH+1 downto 2**LOG_RAM_WIDTH); -- status bits
--c0_splitdata_g: for I in 0 to 7 generate
--  -- fill MSBs of count0 offsets with zeros
--  dr0_wr_data(2**(LOG_PBWT_BLOCK_WIDTH+1)-1+I*2*(2**LOG_PBWT_BLOCK_WIDTH) downto 2**LOG_PBWT_BLOCK_WIDTH+LOG_MAXK+1+I*2*(2**LOG_PBWT_BLOCK_WIDTH)) <= (others => '0');
--  -- copy count0 offsets and data
--  dr0_wr_data(2**LOG_PBWT_BLOCK_WIDTH+LOG_MAXK+I*2*(2**LOG_PBWT_BLOCK_WIDTH) downto I*2*(2**LOG_PBWT_BLOCK_WIDTH)) <= dram0_in_fifo_dout((I+1)*(2**LOG_PBWT_BLOCK_WIDTH+LOG_MAXK+1)-1 downto I*(2**LOG_PBWT_BLOCK_WIDTH+LOG_MAXK+1));  
--end generate;
dr0_wr_data(2**LOG_RAM_WIDTH-1 downto 0) <= dram0_in_fifo_dout(2**LOG_RAM_WIDTH-1 downto 0);
dr0_wr_data_end     <= dram0_in_fifo_rd_en;
dr0_wr_data_en    <= dram0_in_fifo_rd_en;
dram0_rd_data <= dr0_rd_data;
dram0_rd_data_end <= dr0_rd_data_end;
dram0_rd_data_valid <= dr0_rd_data_valid;
dram0_app_rdy <= dr0_rdy and not (dr0_wr_rdy and not dram0_in_fifo_empty and not dram0_in_fifo_rd_rst_busy); -- dr0_rdy and not dram_in_fifo_rd_en;
dram0_app_en <= dram0_app_rdy and dram0_req and outbuffer0_ready;

-- memory access (c1)
dr1_addr        <= dram1_wr_addr when dram1_in_fifo_rd_en = '1' else dram1_rd_addr;
dr1_cmd         <= MEM_CMD_WR when dram1_in_fifo_rd_en = '1' else MEM_CMD_RD;
dr1_en          <= dram1_in_fifo_rd_en or dram1_app_en;
dram1_in_fifo_rd_en <= dr1_rdy and dr1_wr_rdy and not dram1_in_fifo_empty and not dram1_in_fifo_rd_rst_busy;
dr1_wr_data(REAL_RAM_WIDTH-1 downto 2**LOG_RAM_WIDTH+2) <= (others => '0'); -- yet unused
dr1_wr_data(2**LOG_RAM_WIDTH+1 downto 2**LOG_RAM_WIDTH) <= dram1_in_fifo_dout(2**LOG_RAM_WIDTH+1 downto 2**LOG_RAM_WIDTH); -- status bits
--c1_splitdata_g: for I in 0 to 7 generate
--  -- fill MSBs of count0 offsets with zeros
--  dr1_wr_data(2**(LOG_PBWT_BLOCK_WIDTH+1)-1+I*2*(2**LOG_PBWT_BLOCK_WIDTH) downto 2**LOG_PBWT_BLOCK_WIDTH+LOG_MAXK+1+I*2*(2**LOG_PBWT_BLOCK_WIDTH)) <= (others => '0');
--  -- copy count0 offsets and data
--  dr1_wr_data(2**LOG_PBWT_BLOCK_WIDTH+LOG_MAXK+I*2*(2**LOG_PBWT_BLOCK_WIDTH) downto I*2*(2**LOG_PBWT_BLOCK_WIDTH)) <= dram1_in_fifo_dout((I+1)*(2**LOG_PBWT_BLOCK_WIDTH+LOG_MAXK+1)-1 downto I*(2**LOG_PBWT_BLOCK_WIDTH+LOG_MAXK+1));  
--end generate;
dr1_wr_data(2**LOG_RAM_WIDTH-1 downto 0) <= dram1_in_fifo_dout(2**LOG_RAM_WIDTH-1 downto 0);
dr1_wr_data_end     <= dram1_in_fifo_rd_en;
dr1_wr_data_en    <= dram1_in_fifo_rd_en;
dram1_rd_data <= dr1_rd_data;
dram1_rd_data_end <= dr1_rd_data_end;
dram1_rd_data_valid <= dr1_rd_data_valid;
dram1_app_rdy <= dr1_rdy and not (dr1_wr_rdy and not dram1_in_fifo_empty and not dram1_in_fifo_rd_rst_busy); -- dr1_rdy and not dram_in_fifo_rd_en;
dram1_app_en <= dram1_app_rdy and dram1_req and outbuffer1_ready;

dram0_rd_ctrl_p: process
  type mem_state_t is (WAIT_FOR_PIPEFINISH, REQ);
  variable mem_state : mem_state_t := WAIT_FOR_PIPEFINISH;
  variable curr_addr : unsigned(RAM_ADDR_WIDTH-1 downto 0) := (others => '0');
  variable last_wr_addr : std_logic_vector(RAM_ADDR_WIDTH-1 downto 0) := (others => '0');
  variable requests_finished : std_logic := '0';
  variable wrcnt : integer range 0 to 2*NUM_PIPELINES;
  variable rdcnt : integer range 0 to 2*NUM_PIPELINES;
  variable dram0_reset_read_finished_tig : std_logic := '0';
begin
  wait until rising_edge(ram_clk0);

  dram0_req <= '0';
  pipe_busy_tig2 <= pipe_busy;
  
  if ram_clk_sync_reset = '1' then
    
    mem_state := WAIT_FOR_PIPEFINISH;
    curr_addr := (others => '0');
    dram0_wd_addr <= (others => '0');
    last_wr_addr := (others => '0');
    requests_finished := '0';
    dram0_read_finished <= '1';
    out0_busy <= '0';
    wrcnt := 0;
    rdcnt := 0;

  else    
        
    case mem_state is 
    when WAIT_FOR_PIPEFINISH =>
      if dram0_in_fifo_rd_en = '1' and dram0_in_fifo_dout(2**LOG_RAM_WIDTH) = '1' and dram0_in_fifo_dout(2**LOG_RAM_WIDTH+1) = '1' then -- check the 'last' signal together with 'last_fs'
        wrcnt := wrcnt + 1;
        if wrcnt = 2*NUM_PIPELINES then -- each target generates two PBWTs (fwd + bck)
          wrcnt := 0;
          mem_state := REQ;
        end if;
      end if;
      
    when REQ => -- apply memory request
      if requests_finished = '0' then
        dram0_req <= '1'; -- request has already been prepared before        
        if dram0_app_en = '1' then -- request accepted
          curr_addr := curr_addr + 1;
          dram0_wd_addr <= std_logic_vector(curr_addr); -- word address
          -- check if this was the last request
          if dram0_rd_addr = last_wr_addr then
            dram0_req <= '0'; -- disable further requests
            requests_finished := '1';
          end if;
        end if;
      end if;
      if dr0_rd_data_valid = '1' and dr0_rd_data(2**LOG_RAM_WIDTH) = '1' and dr0_rd_data(2**LOG_RAM_WIDTH+1) = '1' then -- check the 'last' signal together with 'last_fs'
        rdcnt := rdcnt + 1;
        if rdcnt = 2*NUM_PIPELINES then -- each target generates two PBWTs (fwd + bck)
          curr_addr := (others => '0');
          dram0_wd_addr <= (others => '0');
          last_wr_addr := (others => '0');
          requests_finished := '0';
          wrcnt := 0;
          rdcnt := 0;
          dram0_read_finished <= '1';
          out0_busy <= '0';
          mem_state := WAIT_FOR_PIPEFINISH;
        end if; 
      end if;
        
    end case;
    
    -- watch write addresses
    if dram0_in_fifo_rd_en = '1' then
      if unsigned(dram0_wr_addr) > unsigned(last_wr_addr) then
        last_wr_addr := dram0_wr_addr;
      end if;
    end if;
    
    -- reset read finished signal whenever we start a new pipeline process that targets this DRAM
--    if dram_wr_select_tig = '0' and pipe_busy_tig2 = '1' then -- removed due to possible TIG issue
    if dram0_reset_read_finished_tig = '1' then
      dram0_read_finished <= '0';
    end if;
    
    -- set busy signal whenever the DRAM input FIFO is not empty
    if dram0_in_fifo_empty = '0' then
      out0_busy <= '1';
    end if;
    
  end if;
  
  dram0_reset_read_finished_tig := dram0_reset_read_finished;
  
end process dram0_rd_ctrl_p;

dram1_rd_ctrl_p: process
  type mem_state_t is (WAIT_FOR_PIPEFINISH, REQ);
  variable mem_state : mem_state_t := WAIT_FOR_PIPEFINISH;
  variable curr_addr : unsigned(RAM_ADDR_WIDTH-1 downto 0) := (others => '0');
  variable last_wr_addr : std_logic_vector(RAM_ADDR_WIDTH-1 downto 0) := (others => '0');
  variable requests_finished : std_logic := '0';
  variable wrcnt : integer range 0 to 2*NUM_PIPELINES;
  variable rdcnt : integer range 0 to 2*NUM_PIPELINES;
  variable dram1_reset_read_finished_tig : std_logic := '0';
begin
  wait until rising_edge(ram_clk1);

  dram1_req <= '0';
  
  if ram_clk_sync_reset = '1' then
    
    mem_state := WAIT_FOR_PIPEFINISH;
    curr_addr := (others => '0');
    dram1_wd_addr <= (others => '0');
    last_wr_addr := (others => '0');
    requests_finished := '0';
    dram1_read_finished <= '1';
    out1_busy <= '0';
    wrcnt := 0;
    rdcnt := 0;

  else    
        
    case mem_state is 
    when WAIT_FOR_PIPEFINISH =>
      if dram1_in_fifo_rd_en = '1' and dram1_in_fifo_dout(2**LOG_RAM_WIDTH) = '1' and dram1_in_fifo_dout(2**LOG_RAM_WIDTH+1) = '1' then -- check the 'last' signal together with 'last_fs'
        wrcnt := wrcnt + 1;
        if wrcnt = 2*NUM_PIPELINES then -- each target generates two PBWTs (fwd + bck)
          wrcnt := 0;
          mem_state := REQ;
        end if;
      end if;
      
    when REQ => -- apply memory request
      if requests_finished = '0' then
        dram1_req <= '1'; -- request has already been prepared before        
        if dram1_app_en = '1' then -- request accepted
          curr_addr := curr_addr + 1;
          dram1_wd_addr <= std_logic_vector(curr_addr); -- word address
          -- check if this was the last request
          if dram1_rd_addr = last_wr_addr then
            dram1_req <= '0'; -- disable further requests
            requests_finished := '1';
          end if;
        end if;
      end if;
      if dr1_rd_data_valid = '1' and dr1_rd_data(2**LOG_RAM_WIDTH) = '1' and dr1_rd_data(2**LOG_RAM_WIDTH+1) = '1' then -- check the 'last' signal together with 'last_fs'
        rdcnt := rdcnt + 1;
        if rdcnt = 2*NUM_PIPELINES then -- each target generates two PBWTs (fwd + bck)
          curr_addr := (others => '0');
          dram1_wd_addr <= (others => '0');
          last_wr_addr := (others => '0');
          requests_finished := '0';
          wrcnt := 0;
          rdcnt := 0;
          dram1_read_finished <= '1';
          out1_busy <= '0';
          mem_state := WAIT_FOR_PIPEFINISH;
        end if; 
      end if;
        
    end case;
    
    -- watch write addresses
    if dram1_in_fifo_rd_en = '1' then
      if unsigned(dram1_wr_addr) > unsigned(last_wr_addr) then
        last_wr_addr := dram1_wr_addr;
      end if;
    end if;
    
    -- reset read finished signal whenever we start a new pipeline process that targets this DRAM
--    if dram_wr_select_tig = '1' and pipe_busy_tig2 = '1' then -- removed due to possible TIG issue
    if dram1_reset_read_finished_tig = '1' then
      dram1_read_finished <= '0';
    end if;
    
    -- set busy signal whenever the DRAM input FIFO is not empty
    if dram1_in_fifo_empty = '0' then
      out1_busy <= '1';
    end if;
    
  end if;
  
  dram1_reset_read_finished_tig := dram1_reset_read_finished;
  
end process dram1_rd_ctrl_p;

-- output of cref data (RAM0)
outbuffer0_i : entity work.OutbufferFIFO_Wrapper  
port map(
  reset    => ram_clk_sync_reset,
  wr_clk => ram_clk0,
  rd_clk => pci_clk,
  din    => outbuffer0_din,
  wr_en  => outbuffer0_wr_en,
  rd_en  => outbuffer0_rd_en,
  dout   => outbuffer0_dout,
  full   => outbuffer0_full,
  prog_full => outbuffer0_prog_full, -- 64 words before full
  empty  => outbuffer0_empty,
  wr_rst_busy => outbuffer0_wr_rst_busy,
  rd_rst_busy => outbuffer0_rd_rst_busy
);

-- output of cref data (RAM1)
outbuffer1_i : entity work.OutbufferFIFO_Wrapper 
port map(
  reset    => ram_clk_sync_reset,
  wr_clk => ram_clk1,
  rd_clk => pci_clk,
  din    => outbuffer1_din,
  wr_en  => outbuffer1_wr_en,
  rd_en  => outbuffer1_rd_en,
  dout   => outbuffer1_dout,
  full   => outbuffer1_full,
  prog_full => outbuffer1_prog_full, -- 64 words before full
  empty  => outbuffer1_empty,
  wr_rst_busy => outbuffer1_wr_rst_busy,
  rd_rst_busy => outbuffer1_rd_rst_busy
);

-- host expects all data from LSB word to MSB word, hence switch MSB and LSB word for outbuffer input since behavior of async FIFO is the other way round!
outbuffer0_din(255 downto   0) <= dram0_rd_data(511 downto 256); --when dma_ins_pad0 = '0' else x"ff00deadbeef00ffff00c001cafe00ffff00deadbeef00ffff00c001cafe00ff";
outbuffer0_din(511 downto 256) <= dram0_rd_data(255 downto   0); --when dma_ins_pad0 = '0' else x"8800deadbeef00888800c001cafe00888800deadbeef00888800c001cafe0088";
outbuffer0_wr_en <= dram0_rd_data_valid;
-- we use prog_full (64 words before full) to ensure RAM requests already in the pipeline can still be stored
outbuffer0_ready <= not outbuffer0_prog_full and not outbuffer0_wr_rst_busy;

-- host expects all data from LSB word to MSB word, hence switch MSB and LSB word for outbuffer input since behavior of async FIFO is the other way round!
outbuffer1_din(255 downto   0) <= dram1_rd_data(511 downto 256); --when dma_ins_pad0 = '0' else x"ff00deadbeef00ffff00c001cafe00ffff00deadbeef00ffff00c001cafe00ff";
outbuffer1_din(511 downto 256) <= dram1_rd_data(255 downto   0); --when dma_ins_pad0 = '0' else x"8800deadbeef00888800c001cafe00888800deadbeef00888800c001cafe0088";
outbuffer1_wr_en <= dram1_rd_data_valid;
-- we use prog_full (64 words before full) to ensure RAM requests already in the pipeline can still be stored
outbuffer1_ready <= not outbuffer1_prog_full and not outbuffer1_wr_rst_busy;

outbuffer0_ov_p: process
begin
  wait until rising_edge(ram_clk0);
  if ram_clk_sync_reset = '1' then
    outbuffer0_overflow_flag <= '0';
  else
    outbuffer0_overflow_flag <= outbuffer0_overflow_flag or (outbuffer0_full and outbuffer0_wr_en);
  end if;
  
  -- since we setup a tig signal here, we don't care that the outbuffer1 flag was set by another clock domain
  outbuffer_overflow_flag_tig <= outbuffer0_overflow_flag or outbuffer1_overflow_flag;
end process outbuffer0_ov_p;

outbuffer1_ov_p: process
begin
  wait until rising_edge(ram_clk1);
  if ram_clk_sync_reset = '1' then
    outbuffer1_overflow_flag <= '0';
  else
    outbuffer1_overflow_flag <= outbuffer1_overflow_flag or (outbuffer1_full and outbuffer1_wr_en);
  end if;
end process outbuffer1_ov_p;

dma_dout_data <= outbuffer0_dout when dram_rd_select = '0' else outbuffer1_dout;
dma_dout1_tvalid_int <= outbuffer0_rd_en or outbuffer1_rd_en or (dma_ins_pad and dma_dout_ready); -- correct selection is done below
dma_dout_valid <= dma_dout1_tvalid_int;
outbuffer0_rd_en <= dma_dout_ready and not outbuffer0_empty and not outbuffer0_rd_rst_busy and not dram_rd_select;
outbuffer1_rd_en <= dma_dout_ready and not outbuffer1_empty and not outbuffer1_rd_rst_busy and dram_rd_select;

dram_rd_select_p: process
  variable dram_rd_select_int : std_logic := '0';
  constant WAIT_CYCLES : integer := 63;
  variable wait_cnt : integer range 0 to WAIT_CYCLES := WAIT_CYCLES;
  variable rd0_busy : std_logic := '0';
  variable rd1_busy : std_logic := '0';
  type dram_rd_select_state_t is (WAIT_RD_FINISHED, WAIT_EMPTY);
  variable dram_rd_select_state : dram_rd_select_state_t := WAIT_RD_FINISHED;
begin
  wait until rising_edge(pci_clk);
  
  -- due to timing problems we delay the select signal by one cycle
  dram_rd_select <= dram_rd_select_int;
  
  if pci_clk_sync_reset = '1' then
    dram_rd_select_int := '0';
    wait_cnt := WAIT_CYCLES;
    rd0_busy := '0';
    rd1_busy := '0';
    dram_rd_select_state := WAIT_RD_FINISHED;
  else
    -- whatever RAM is currently selected, we first need to wait for the rd process being started,
    -- then we wait until it is finished before we select the other RAM
    
    case dram_rd_select_state is 
    when WAIT_RD_FINISHED =>
      if dram_rd_select_int = '0' then
        if rd0_busy = '1' and dram0_read_finished_tig = '1' then
          wait_cnt := WAIT_CYCLES;
          dram_rd_select_state := WAIT_EMPTY;
        end if;
      else -- dram_rd_select_int = '1'
        if rd1_busy = '1' and dram1_read_finished_tig = '1' then
          wait_cnt := WAIT_CYCLES;
          dram_rd_select_state := WAIT_EMPTY;
        end if;
      end if;
        
    when WAIT_EMPTY =>
      if dram_rd_select_int = '0' then
        if outbuffer0_empty = '1' then
          if wait_cnt = 0 then
            wait_cnt := WAIT_CYCLES;
            rd0_busy := '0';
            dram_rd_select_int := '1'; -- switch!
            dram_rd_select_state := WAIT_RD_FINISHED;
          else
            wait_cnt := wait_cnt - 1;
          end if;
        else
          wait_cnt := WAIT_CYCLES;
        end if;
      else -- dram_rd_select_int = '1' 
        if outbuffer1_empty = '1' then
          if wait_cnt = 0 then
            wait_cnt := WAIT_CYCLES;
            rd1_busy := '0';
            dram_rd_select_int := '0'; -- switch!
            dram_rd_select_state := WAIT_RD_FINISHED;
          else
            wait_cnt := wait_cnt - 1;
          end if;
        else
          wait_cnt := WAIT_CYCLES;
        end if;
      end if;
        
    end case;
      
    -- watch for the read process being started
    if dram0_read_finished_tig = '0' then
      rd0_busy := '1';
    end if;
    if dram1_read_finished_tig = '0' then
      rd1_busy := '1';
    end if;    
    
  end if;
  
end process dram_rd_select_p;

out_padding_p: process
  variable next_word_count : unsigned(31 downto 0) := x"00000001";
  constant PAD_WAIT_CYCLES : integer := 63;
  variable pad_wait_cnt : integer range 0 to PAD_WAIT_CYCLES := PAD_WAIT_CYCLES;
  variable pad_wait : std_logic := '1';
begin
  wait until rising_edge(pci_clk);
  
  dma_ins_pad <= '0';
  
  dram0_read_finished_tig <= dram0_read_finished;
  dram1_read_finished_tig <= dram1_read_finished;
  bufsize_pciwords_tig <= bufsize_pciwords;
  last_target_charge_tig <= last_target_charge;
  
  if pci_clk_sync_reset = '1' then
    next_word_count := x"00000001";
    pad_wait_cnt := PAD_WAIT_CYCLES;
    pad_wait := '1';
  else
    
    if dma_dout1_tvalid_int = '1' then
      if next_word_count = bufsize_pciwords_tig then
        next_word_count := x"00000001";
      else
        next_word_count := next_word_count + 1;
      end if;
    end if;

    if next_word_count /= x"00000001" and pad_wait = '0' then
      dma_ins_pad <= '1';
    end if;
    
    -- it needs several cycles at the beginning to set all busy signals.
    -- waiting a bit before actually starting the padding ensures padding is not inserted too early before it is really required    
    if dram0_read_finished_tig = '1' and dram1_read_finished_tig = '1' and last_target_charge_tig = '1' then
      -- potential padding situation
      if pad_wait_cnt = 0 then
        pad_wait := '0';
      else
        pad_wait_cnt := pad_wait_cnt -1;
        pad_wait := '1';
      end if;
    else
      pad_wait_cnt := PAD_WAIT_CYCLES;
      pad_wait := '1';
    end if;
    
  end if;
  
  -- DEBUG
  dma_next_word <= next_word_count;
  
end process out_padding_p;


-- some status flags (can be adapted)  (ram_clk0 domain!)
-- but keep in mind, that a "clean" status has to be x"00"!
status(0) <= pipe_busy_tig2; 
status(1) <= out_busy_tig;
status(2) <= pipeline_overflow_flag_tig or pipelineoutput_overflow_flag_tig;
status(3) <= pipeline_switchundrained_flag_tig;
status(4) <= not inbuffer_empty_tig;
status(5) <= not dma_dout1_tready_tig;
status(6) <= '0' when indata_state_tig = WAIT_FOR_START else '1';
status(7) <= outbuffer_overflow_flag_tig;

reg_clk <= user_clk0; -- clock for PCIe register BRAM

-----------
-- DEBUG --
-----------

--reg_we <= '0';
--reg_addr <= (others => '0');
--reg_dout <= (others => '-');

reg_we <= reg_we_dbg;
reg_addr <= reg_addr_dbg when reg_we_dbg = '1' else (others => '0'); -- need to read addr0 for a host reset whenever I'm not writing
reg_dout <= reg_dout_dbg;

watch_dram0_p: process
begin
  wait until rising_edge(ram_clk0);
  
  if ram_clk_sync_reset = '1' then
    dram0_rd_data_cnt <= (others => '0');
    dram0_wr_data_cnt <= (others => '0');
    dram0_app_en_cnt  <= (others => '0');
--    dram0_rd_data_end_cnt <= (others => '0');
--    dram0_rd_min_addr <= (others => '1');
--    dram0_wr_min_addr <= (others => '1');
--    dram0_rd_max_addr <= (others => '0');
--    dram0_wr_max_addr <= (others => '0');
  else
    if dram0_in_fifo_rd_en = '1' or dram0_app_en = '1' then
      dram0_app_en_cnt <= dram0_app_en_cnt + 1;
      -- minmax
--      if dram0_in_fifo_rd_en = '1' then
--        if dram0_wr_min_addr > unsigned(dram0_wr_addr) then
--          dram0_wr_min_addr <= unsigned(dram0_wr_addr);
--        end if;
--        if dram0_wr_max_addr < unsigned(dram0_wr_addr) then
--          dram0_wr_max_addr <= unsigned(dram0_wr_addr);
--        end if;        
--      else
--        if dram0_rd_min_addr > unsigned(dram0_rd_addr) then
--          dram0_rd_min_addr <= unsigned(dram0_rd_addr);
--        end if;                           
--        if dram0_rd_max_addr < unsigned(dram0_rd_addr) then
--          dram0_rd_max_addr <= unsigned(dram0_rd_addr);
--        end if;
--      end if;
    end if;
    if dram0_in_fifo_rd_en = '1' then
      dram0_wr_data_cnt <= dram0_wr_data_cnt + 1;
    end if;
    if dr0_rd_data_valid = '1' then
      dram0_rd_data_cnt <= dram0_rd_data_cnt + 1;
--      if dr0_rd_data_end = '1' then
--        dram0_rd_data_end_cnt <= dram0_rd_data_end_cnt + 1;
--      end if;
    end if;
    
  end if;
end process watch_dram0_p;

watch_dram1_p: process
begin
  wait until rising_edge(ram_clk1);
  
  if ram_clk_sync_reset = '1' then
    dram1_rd_data_cnt <= (others => '0');
    dram1_wr_data_cnt <= (others => '0');
    dram1_app_en_cnt  <= (others => '0');
--    dram1_rd_data_end_cnt <= (others => '0');
--    dram1_rd_min_addr <= (others => '1');
--    dram1_wr_min_addr <= (others => '1');
--    dram1_rd_max_addr <= (others => '0');
--    dram1_wr_max_addr <= (others => '0');
  else
    if dram1_in_fifo_rd_en = '1' or dram1_app_en = '1' then
      dram1_app_en_cnt <= dram1_app_en_cnt + 1;
      -- minmax
--      if dram1_in_fifo_rd_en = '1' then
--        if dram1_wr_min_addr > unsigned(dram1_wr_addr) then
--          dram1_wr_min_addr <= unsigned(dram1_wr_addr);
--        end if;
--        if dram1_wr_max_addr < unsigned(dram1_wr_addr) then
--          dram1_wr_max_addr <= unsigned(dram1_wr_addr);
--        end if;        
--      else
--        if dram1_rd_min_addr > unsigned(dram1_rd_addr) then
--          dram1_rd_min_addr <= unsigned(dram1_rd_addr);
--        end if;                           
--        if dram1_rd_max_addr < unsigned(dram1_rd_addr) then
--          dram1_rd_max_addr <= unsigned(dram1_rd_addr);
--        end if;
--      end if;
    end if;
    if dram1_in_fifo_rd_en = '1' then
      dram1_wr_data_cnt <= dram1_wr_data_cnt + 1;
    end if;
    if dr1_rd_data_valid = '1' then
      dram1_rd_data_cnt <= dram1_rd_data_cnt + 1;
--      if dr1_rd_data_end = '1' then
--        dram1_rd_data_end_cnt <= dram1_rd_data_end_cnt + 1;
--      end if;
    end if;
    
  end if;
end process watch_dram1_p;

buf_cnt_pciclk_p: process
begin
  wait until rising_edge(pci_clk);
  
  if pci_clk_sync_reset = '1' then
    inbuffer_wr_cnt  <= (others => '0');
    outbuffer0_rd_cnt <= (others => '0');
    outbuffer1_rd_cnt <= (others => '0');
    pcie_wr_cnt <= (others => '0');
    dma_ins_pad_cnt <= (others => '0');
  else
    if inbuffer_wr_en = '1' then
      inbuffer_wr_cnt <= inbuffer_wr_cnt + 1;
    end if; 
    if outbuffer0_rd_en = '1' then
      outbuffer0_rd_cnt <= outbuffer0_rd_cnt + 1;
    end if;  
    if outbuffer1_rd_en = '1' then
      outbuffer1_rd_cnt <= outbuffer1_rd_cnt + 1;
    end if;  
    if dma_dout1_tvalid_int = '1' then
      pcie_wr_cnt <= pcie_wr_cnt + 1;
    end if;
    if dma_dout_ready = '1' and dma_ins_pad = '1' then
      dma_ins_pad_cnt <= dma_ins_pad_cnt + 1;
    end if;
  end if;
  
end process buf_cnt_pciclk_p;

buf_cnt_userclk_p: process
begin
  wait until rising_edge(user_clk0);
  
  if user_clk0_sync_reset = '1' then
    inbuffer_rd_cnt  <= (others => '0');
  else
    if inbuffer_rd_en = '1' then
      inbuffer_rd_cnt <= inbuffer_rd_cnt + 1;
    end if;  
  end if;
  
end process buf_cnt_userclk_p;

watch_busy_stall_p: process
begin
  wait until rising_edge(user_clk0);
  
  if user_clk0_sync_reset = '1' then
    pipe_busy_cnt <= (others => '0');
    out_busy_cnt <= (others => '0');
    pipe_stall_cnt <= (others => '0');
    out_stall_cnt <= (others => '0');
  else
    if pipe_busy_tig = '1' then
      pipe_busy_cnt <= pipe_busy_cnt + 1;
    end if;
    if out_busy_tig = '1' then
      out_busy_cnt <= out_busy_cnt + 1;
    end if;
    if pipeline_stall_tig = '1' and block_stream = '1' then
      pipe_stall_cnt <= pipe_stall_cnt + 1;
    end if;
    if pipelineoutput_stall_tig = '1' then
      out_stall_cnt <= out_stall_cnt + 1;
    end if;
  end if;
end process watch_busy_stall_p;

reg_dbg_p : process
  
  variable timer      : unsigned(31 downto 0) := (others => '0');
  variable dbg_wr_cnt : unsigned(63 downto 0) := (others => '0');
  variable dbg_state  : integer range 0 to 8 := 0;

  variable inbuffer_wr_cnt_tig   : unsigned(63 downto 0); 
  variable inbuffer_rd_cnt_tig   : unsigned(63 downto 0); 
  variable outbuffer0_rd_cnt_tig : unsigned(63 downto 0); 
  variable outbuffer1_rd_cnt_tig : unsigned(63 downto 0);
  variable pcie_wr_cnt_tig       : unsigned(63 downto 0);
  variable bufsize_pciwords_tig2 : unsigned(31 downto 0);
  variable block_cnt_tig        : unsigned(15 downto 0);
  
  variable dram0_rd_data_cnt_tig : unsigned(63 downto 0);
  variable dram0_wr_data_cnt_tig : unsigned(63 downto 0);
  variable dram0_app_en_cnt_tig  : unsigned(63 downto 0);
--  variable dram0_rd_data_end_cnt_tig  : unsigned(63 downto 0);
  variable dram1_rd_data_cnt_tig : unsigned(63 downto 0);
  variable dram1_wr_data_cnt_tig : unsigned(63 downto 0);
  variable dram1_app_en_cnt_tig  : unsigned(63 downto 0);
--  variable dram1_rd_data_end_cnt_tig  : unsigned(63 downto 0);
--  variable dram_rd_min_addr_tig : unsigned(29 downto 0);
--  variable dram_wr_min_addr_tig : unsigned(29 downto 0);
--  variable dram_rd_max_addr_tig : unsigned(29 downto 0);
--  variable dram_wr_max_addr_tig : unsigned(29 downto 0);
  variable pipe_busy_cnt_tig : unsigned(63 downto 0) := (others => '0');
  variable out_busy_cnt_tig  : unsigned(63 downto 0) := (others => '0');
  variable indata_state_dbg_tig : indata_state_t := WAIT_FOR_START;
  variable pipe_stall_cnt_tig : unsigned(63 downto 0) := (others => '0');
  variable out_stall_cnt_tig : unsigned(63 downto 0) := (others => '0');
  variable pipeline0_checkincon_valid_cnt_tig : unsigned(DBG_COUNT_WIDTH-1 downto 0) := (others => '0');
  variable pipeline0_selectbh_valid_cnt_tig   : unsigned(DBG_COUNT_WIDTH-1 downto 0) := (others => '0');
  variable pipeline0_condensed_valid_cnt_tig  : unsigned(DBG_COUNT_WIDTH-1 downto 0) := (others => '0');
  variable pipelineoutput0_my_valid_cnt_tig   : unsigned(DBG_COUNT_WIDTH-1 downto 0) := (others => '0');
  variable pipeline0_checkincon_dbg_tig : unsigned(63 downto 0) := (others => '0');
  variable pipeline0_selectbh_dbg_tig   : unsigned(63 downto 0) := (others => '0');
  variable pipeline0_condensed_dbg_tig  : unsigned(63 downto 0) := (others => '0');
  variable pipeline0_empty_getnext_cnt_tig : unsigned(15 downto 0) := (others => '0');
--  variable pipeline1_checkincon_valid_cnt_tig : unsigned(DBG_COUNT_WIDTH-1 downto 0) := (others => '0');
--  variable pipeline1_selectbh_valid_cnt_tig   : unsigned(DBG_COUNT_WIDTH-1 downto 0) := (others => '0');
--  variable pipeline1_condensed_valid_cnt_tig  : unsigned(DBG_COUNT_WIDTH-1 downto 0) := (others => '0');
  variable pipelineoutput1_my_valid_cnt_tig   : unsigned(DBG_COUNT_WIDTH-1 downto 0) := (others => '0');
--  variable pipeline0_refpopcnt_tig : unsigned(15 downto 0);
--  variable pipeline0_incpopcnt_tig : unsigned(15 downto 0);
--  variable pipeline1_refpopcnt_tig : unsigned(15 downto 0);
--  variable pipeline1_incpopcnt_tig : unsigned(15 downto 0);

--  variable last_target_charge_tig2 : std_logic;
--  variable out0_busy_tig : std_logic;
--  variable out1_busy_tig : std_logic;
--  variable dram0_rd_finished_tig : std_logic;
--  variable dram1_rd_finished_tig : std_logic;
--  variable dma_next_word_tig : unsigned(31 downto 0);
--  variable dma_ins_pad_cnt_tig : unsigned(31 downto 0);
   
begin
  wait until rising_edge(user_clk0);

  reg_we_dbg   <= '0';
  reg_dout_dbg <= (others => '0'); -- default
  
  -- will overflow every second
  if timer = 0 then
    dbg_state := 0;
    timer     := unsigned(INFO_BASE_FREQUENCY);
  else
    timer := timer - 1;
  end if;
  
  case dbg_state is

  when 0 =>
    -- to identify endianess
    reg_dout_dbg                 <= x"fffefdfcfbfaf9f8f7f6f5f4f3f2f1f00f0e0d0c0b0a09080706050403020100";
    reg_dout_dbg(63 downto 0)    <= std_logic_vector(dbg_wr_cnt); -- number of debug writes (overwrites lower 8 bytes)
    reg_we_dbg                   <= '1';
    reg_addr_dbg             <= "0000000001"; -- ADDR 1
    dbg_wr_cnt               := dbg_wr_cnt + 1;
    dbg_state                := dbg_state + 1;

  when 1 => 
    reg_we_dbg               <= '1';
    reg_addr_dbg             <= "0000000010"; -- ADDR 2
    reg_dout_dbg(239 downto 224) <= std_logic_vector(block_cnt_tig);
    reg_dout_dbg(223 downto 192) <= std_logic_vector(bufsize_pciwords_tig2);
    reg_dout_dbg(191 downto 128) <= std_logic_vector(pcie_wr_cnt_tig);  
    reg_dout_dbg(127 downto  64) <= std_logic_vector(inbuffer_wr_cnt_tig);  
    reg_dout_dbg( 63 downto   0) <= std_logic_vector(inbuffer_rd_cnt_tig);  
    dbg_state := dbg_state + 1;

  when 2 => 
    reg_we_dbg               <= '1';
    reg_addr_dbg             <= "0000000011"; -- ADDR 3
    case indata_state_dbg_tig is
    when WAIT_FOR_START =>
      reg_dout_dbg(135 downto 128) <= "00000001";
    when CONSTANTS =>
      reg_dout_dbg(135 downto 128) <= "00000010";
    when TARGETCOUNTS =>
      reg_dout_dbg(135 downto 128) <= "00000100";
    when TARGETS =>
      reg_dout_dbg(135 downto 128) <= "00001000";
    when SPLITS =>
      reg_dout_dbg(135 downto 128) <= "00010000";
    when BESTHAPS =>
      reg_dout_dbg(135 downto 128) <= "00100000";
    when REFERENCES =>
      reg_dout_dbg(135 downto 128) <= "01000000";
    when REFERENCES_REV =>
      reg_dout_dbg(135 downto 128) <= "10000000";
    end case;
--    reg_dout_dbg(255 downto 240) <= std_logic_vector(pipeline1_incpopcnt_tig);
--    reg_dout_dbg(239 downto 224) <= std_logic_vector(pipeline1_refpopcnt_tig);
--    reg_dout_dbg(223 downto 208) <= std_logic_vector(pipeline0_incpopcnt_tig);
--    reg_dout_dbg(207 downto 192) <= std_logic_vector(pipeline0_refpopcnt_tig);
    reg_dout_dbg(127 downto 64) <= std_logic_vector(out_stall_cnt_tig);  
--    reg_dout_dbg(REF_COUNT_WIDTH+95 downto 96) <= std_logic_vector(pipeline_wrong_scount_gt1_acc_tig);  
--    reg_dout_dbg(REF_COUNT_WIDTH+63 downto 64) <= std_logic_vector(pipeline_wrong_scount_eq1_acc_tig);  
    reg_dout_dbg( 63 downto   0) <= std_logic_vector(pipe_stall_cnt_tig);
    dbg_state := dbg_state + 1;

  when 3 => 
    reg_we_dbg               <= '1';
    reg_addr_dbg             <= "0000000100"; -- ADDR 4
    reg_dout_dbg(255 downto 192) <= std_logic_vector(dram0_rd_data_cnt_tig);  
    reg_dout_dbg(191 downto 128) <= std_logic_vector(dram0_app_en_cnt_tig);  
    reg_dout_dbg(127 downto  64) <= std_logic_vector(dram0_wr_data_cnt_tig);  
    reg_dout_dbg( 63 downto   0) <= std_logic_vector(outbuffer0_rd_cnt_tig);  
    dbg_state := dbg_state + 1;
    
  when 4 => 
    reg_we_dbg               <= '1';
    reg_addr_dbg             <= "0000000101"; -- ADDR 5
    reg_dout_dbg(255 downto 192) <= std_logic_vector(dram1_rd_data_cnt_tig);  
    reg_dout_dbg(191 downto 128) <= std_logic_vector(dram1_app_en_cnt_tig);  
    reg_dout_dbg(127 downto  64) <= std_logic_vector(dram1_wr_data_cnt_tig);  
    reg_dout_dbg( 63 downto   0) <= std_logic_vector(outbuffer1_rd_cnt_tig);  
    dbg_state := dbg_state + 1;
    
  when 5 => 
    reg_we_dbg               <= '1';
    reg_addr_dbg             <= "0000000110"; -- ADDR 6
    reg_dout_dbg(255 downto 192) <= std_logic_vector(pipe_busy_cnt_tig);
    reg_dout_dbg(191 downto 128) <= std_logic_vector(out_busy_cnt_tig);
--    reg_dout_dbg(127 downto 96) <= std_logic_vector(dma_ins_pad_cnt_tig);
--    reg_dout_dbg(95 downto 64) <= std_logic_vector(dma_next_word_tig);
--    reg_dout_dbg(4) <= last_target_charge_tig2;
--    reg_dout_dbg(3) <= out1_busy_tig;
--    reg_dout_dbg(2) <= dram1_rd_finished_tig;
--    reg_dout_dbg(1) <= out0_busy_tig;
--    reg_dout_dbg(0) <= dram0_rd_finished_tig;
    dbg_state := dbg_state + 1;
    
  when 6 => 
    reg_we_dbg               <= '1';
    reg_addr_dbg             <= "0000000111"; -- ADDR 7
    reg_dout_dbg(  0+DBG_COUNT_WIDTH-1 downto   0) <= std_logic_vector(pipeline0_checkincon_valid_cnt_tig);  
    reg_dout_dbg( 64+DBG_COUNT_WIDTH-1 downto  64) <= std_logic_vector(pipeline0_selectbh_valid_cnt_tig);  
    reg_dout_dbg(128+DBG_COUNT_WIDTH-1 downto 128) <= std_logic_vector(pipeline0_condensed_valid_cnt_tig);  
    reg_dout_dbg(192+DBG_COUNT_WIDTH-1 downto 192) <= std_logic_vector(pipelineoutput0_my_valid_cnt_tig);  
    dbg_state := dbg_state + 1;
    
  when 7 => 
    reg_we_dbg               <= '1';
    reg_addr_dbg             <= "0000001000"; -- ADDR 8
    reg_dout_dbg( 63 downto   0) <= std_logic_vector(pipeline0_checkincon_dbg_tig);  
    reg_dout_dbg(127 downto  64) <= std_logic_vector(pipeline0_selectbh_dbg_tig);  
    reg_dout_dbg(191 downto 128) <= std_logic_vector(pipeline0_condensed_dbg_tig);
    reg_dout_dbg(255 downto 240) <= std_logic_vector(pipeline0_empty_getnext_cnt_tig);  
--    reg_dout_dbg(  0+DBG_COUNT_WIDTH-1 downto   0) <= std_logic_vector(pipeline1_checkincon_valid_cnt_tig);  
--    reg_dout_dbg( 64+DBG_COUNT_WIDTH-1 downto  64) <= std_logic_vector(pipeline1_selectbh_valid_cnt_tig);  
--    reg_dout_dbg(128+DBG_COUNT_WIDTH-1 downto 128) <= std_logic_vector(pipeline1_condensed_valid_cnt_tig);  
    reg_dout_dbg(192+DBG_COUNT_WIDTH-1 downto 192) <= std_logic_vector(pipelineoutput1_my_valid_cnt_tig);  
    dbg_state := dbg_state + 1;

  when others => -- it is important to have at least one null state!
    null;
  end case;
  
  -- TIGs
  
  dma_dout1_tready_tig                     <= dma_dout_ready;
--  inbuffer_full_tig                        <= inbuffer_full;
  inbuffer_empty_tig                       <= inbuffer_empty;
  
  inbuffer_wr_cnt_tig   := inbuffer_wr_cnt ;
  inbuffer_rd_cnt_tig   := inbuffer_rd_cnt ;
  outbuffer0_rd_cnt_tig := outbuffer0_rd_cnt;
  outbuffer1_rd_cnt_tig := outbuffer1_rd_cnt;
  pcie_wr_cnt_tig       := pcie_wr_cnt;
  bufsize_pciwords_tig2 := bufsize_pciwords; 
  block_cnt_tig         := block_cnt;
  
  dram0_rd_data_cnt_tig := dram0_rd_data_cnt;
  dram0_wr_data_cnt_tig := dram0_wr_data_cnt;
  dram0_app_en_cnt_tig  := dram0_app_en_cnt ;
--  dram0_rd_data_end_cnt_tig  := dram0_rd_data_end_cnt;
  dram1_rd_data_cnt_tig := dram1_rd_data_cnt;
  dram1_wr_data_cnt_tig := dram1_wr_data_cnt;
  dram1_app_en_cnt_tig  := dram1_app_en_cnt ;
--  dram0_rd_data_end_cnt_tig  := dram1_rd_data_end_cnt;
--  dram_rd_min_addr_tig := dram_rd_min_addr;
--  dram_wr_min_addr_tig := dram_wr_min_addr;
--  dram_rd_max_addr_tig := dram_rd_max_addr;
--  dram_wr_max_addr_tig := dram_wr_max_addr;
  pipe_busy_cnt_tig := pipe_busy_cnt;
  out_busy_cnt_tig  := out_busy_cnt;
  indata_state_dbg_tig := indata_state;
  pipe_stall_cnt_tig := pipe_stall_cnt;
  out_stall_cnt_tig := out_stall_cnt;
  
  pipeline0_checkincon_valid_cnt_tig := pipeline_checkincon_valid_cnt(0);
  pipeline0_selectbh_valid_cnt_tig   := pipeline_selectbh_valid_cnt  (0);
  pipeline0_condensed_valid_cnt_tig  := pipeline_condensed_valid_cnt (0);
  pipelineoutput0_my_valid_cnt_tig   := pipelineoutput_my_valid_cnt  (0);
  
  pipeline0_checkincon_dbg_tig(63 downto 48) := pipeline_checkincon_last_cnt(0)(15 downto 0);
  pipeline0_checkincon_dbg_tig(47 downto 32) := pipeline_checkincon_split_cnt(0)(15 downto 0);
  pipeline0_checkincon_dbg_tig(31 downto  0) := pipeline_checkincon_lastfs_cnt(0)(31 downto 0);
  pipeline0_selectbh_dbg_tig(63 downto 48) := pipeline_selectbh_last_cnt(0)(15 downto 0);
  pipeline0_selectbh_dbg_tig(47 downto 32) := pipeline_selectbh_split_cnt(0)(15 downto 0);
  pipeline0_selectbh_dbg_tig(31 downto  0) := pipeline_selectbh_lastfs_cnt(0)(31 downto 0);
  pipeline0_condensed_dbg_tig(63 downto 48) := pipeline_condensed_last_cnt(0)(15 downto 0);
  pipeline0_condensed_dbg_tig(47 downto 32) := pipeline_condensed_split_cnt(0)(15 downto 0);
  pipeline0_condensed_dbg_tig(31 downto  0) := pipeline_condensed_lastfs_cnt(0)(31 downto 0);
  pipeline0_empty_getnext_cnt_tig := pipeline_empty_getnext_cnt(0)(15 downto 0);
  
  
--  pipeline1_checkincon_valid_cnt_tig := pipeline_checkincon_valid_cnt(1);
--  pipeline1_selectbh_valid_cnt_tig   := pipeline_selectbh_valid_cnt  (1);
--  pipeline1_condensed_valid_cnt_tig  := pipeline_condensed_valid_cnt (1);
  pipelineoutput1_my_valid_cnt_tig   := pipelineoutput_my_valid_cnt  (1);
--  pipeline0_refpopcnt_tig := pipeline_refpopcnt(0);
--  pipeline0_incpopcnt_tig := pipeline_incpopcnt(0);
--  pipeline1_refpopcnt_tig := pipeline_refpopcnt(1);
--  pipeline1_incpopcnt_tig := pipeline_incpopcnt(1);

--  last_target_charge_tig2 := last_target_charge;
--  out0_busy_tig := out0_busy;
--  out1_busy_tig := out1_busy;
--  dram0_rd_finished_tig := dram0_read_finished;
--  dram1_rd_finished_tig := dram1_read_finished;
--  dma_next_word_tig := dma_next_word;
--  dma_ins_pad_cnt_tig := dma_ins_pad_cnt;
  
  
end process reg_dbg_p;

end Behavioral;

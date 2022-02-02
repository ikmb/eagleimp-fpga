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



entity eagleimp_prep_tb is
end entity eagleimp_prep_tb;

architecture Behavioral of eagleimp_prep_tb is
  
  constant PCI_CLK_PERIOD : time := 5 ns;
  constant RAM_CLK_PERIOD : time := 4 ns; --100 ns;
  constant ADDN_RAM_CLK_PERIOD : time := 10 ns;
  
  constant RAM_SIZE : natural := 32768;

  signal clk, resetn        : std_logic;
  signal dma0_m_axis_tdata  : std_logic_vector(255 downto 0);
  signal dma0_m_axis_tvalid : std_logic;
  signal dma0_m_axis_tvalid_int : std_logic;
  signal dma0_m_axis_tready : std_logic;
  signal dma1_s_axis_tdata  : std_logic_vector(255 downto 0);
  signal dma1_s_axis_tvalid : std_logic;
  signal dma1_s_axis_tready : std_logic;
  
  signal bram_clk_b  : std_logic;
  signal bram_we_b   : std_logic;
  signal bram_din_b  : std_logic_vector(255 downto 0);
  signal bram_dout_b : std_logic_vector(255 downto 0);

  signal main_reg_addr : std_logic_vector(9 downto 0) := "0000000000";
  
  signal main_status : std_logic_vector(7 downto 0);
  
  signal dr0_addr          : std_logic_vector(29 downto 0);
  signal dr0_cmd           : std_logic_vector(2 downto 0);
  signal dr0_en            : std_logic;
  signal dr0_wr_data      : std_logic_vector(575 downto 0);
  signal dr0_wr_data_end       : std_logic;
  signal dr0_wr_data_en      : std_logic;
  signal dr0_rd_data       : std_logic_vector(575 downto 0);
  signal dr0_rd_data_end   : std_logic;
  signal dr0_rd_data_valid : std_logic;
  signal dr0_rdy           : std_logic;
  signal dr0_wr_rdy       : std_logic;

  signal c0_ui_clk              : std_logic;
  signal c0_addn_ui_clk         : std_logic;
  signal c0_ui_clk_sync_rst     : std_logic;
  signal c0_calib_done        : std_logic;    
  
  signal dr1_addr          : std_logic_vector(29 downto 0);
  signal dr1_cmd           : std_logic_vector(2 downto 0);
  signal dr1_en            : std_logic;
  signal dr1_wr_data      : std_logic_vector(575 downto 0);
  signal dr1_wr_data_end       : std_logic;
  signal dr1_wr_data_en      : std_logic;
  signal dr1_rd_data       : std_logic_vector(575 downto 0);
  signal dr1_rd_data_end   : std_logic;
  signal dr1_rd_data_valid : std_logic;
  signal dr1_rdy           : std_logic;
  signal dr1_wr_rdy       : std_logic;

  signal c1_ui_clk              : std_logic;
  signal c1_addn_ui_clk         : std_logic;
  signal c1_ui_clk_sync_rst     : std_logic;
  
  --signal data : std_logic_vector(255 downto 0);
  
  
  procedure senddata(
    signal clk : in std_logic; 
    variable data_in : in std_logic_vector(255 downto 0);
    signal ready : in std_logic;
    signal data_out : out std_logic_vector(255 downto 0);
    signal valid_out : out std_logic
  ) is
  begin
    data_out <= data_in;
    valid_out <= '1';
    wait until rising_edge(clk);
    if ready = '0' then 
      wait until ready = '1';
      wait until rising_edge(clk);
    end if;
    return;
  end procedure senddata;
  
begin

  -- clk processes
  clk_p: process
  begin
    clk <= '0';
    wait for PCI_CLK_PERIOD / 2;
    clk <= '1';
    wait for PCI_CLK_PERIOD / 2;
  end process clk_p;
  
  resetn <= '0', '1' after 10 * PCI_CLK_PERIOD;
  
  -- main instance
  main_i : entity work.eagleimp_prep_main
    generic map (
      NUM_PIPELINES => NUM_SIM_PIPELINES
    )
    port map (
      -- Clock / Reset
      reset => not resetn,
      pci_clk => clk,
      ram_clk0 => c0_ui_clk,
      ram_clk0_reset => c0_ui_clk_sync_rst,
      ram_clk1 => c1_ui_clk,
      ram_clk1_reset => c1_ui_clk_sync_rst,
      user_clk0 => c0_addn_ui_clk,
      user_clk1 => c1_addn_ui_clk,
      -- PCIe DMA
      dma_din_data => dma0_m_axis_tdata,
      dma_din_valid => dma0_m_axis_tvalid,
      dma_din_ready => dma0_m_axis_tready,
      dma_dout_data => dma1_s_axis_tdata,
      dma_dout_valid => dma1_s_axis_tvalid,
      dma_dout_ready => dma1_s_axis_tready,
      -- PCIe Register Interface
      reg_clk => bram_clk_b,
      reg_we => bram_we_b,
      reg_addr => main_reg_addr,
      reg_din => bram_dout_b,
      reg_dout => bram_din_b,
      -- Status
      status => main_status,
      -- DRAM
      dr0_addr => dr0_addr,
      dr0_cmd => dr0_cmd,
      dr0_en => dr0_en,
      dr0_wr_data => dr0_wr_data,
      dr0_wr_data_end => dr0_wr_data_end,
      dr0_wr_data_en => dr0_wr_data_en,
      dr0_rd_data => dr0_rd_data,
      dr0_rd_data_end => dr0_rd_data_end,
      dr0_rd_data_valid => dr0_rd_data_valid,
      dr0_rdy => dr0_rdy,
      dr0_wr_rdy => dr0_wr_rdy,
      dr1_addr => dr1_addr,
      dr1_cmd => dr1_cmd,
      dr1_en => dr1_en,
      dr1_wr_data => dr1_wr_data,
      dr1_wr_data_end => dr1_wr_data_end,
      dr1_wr_data_en => dr1_wr_data_en,
      dr1_rd_data => dr1_rd_data,
      dr1_rd_data_end => dr1_rd_data_end,
      dr1_rd_data_valid => dr1_rd_data_valid,
      dr1_rdy => dr1_rdy,
      dr1_wr_rdy => dr1_wr_rdy
      );
 
 -- RAM simulation
 ram0_sim_i: entity work.ad_RAM_sim
   generic map(
     RAM_SIZE => RAM_SIZE,
     RAM_CLK_PERIOD => RAM_CLK_PERIOD,
     ADDN_RAM_CLK_PERIOD => ADDN_RAM_CLK_PERIOD
   )
   port map(
     dram_clk           => c0_ui_clk,
     dram_addn_clk      => c0_addn_ui_clk,
     dram_calib_done    => c0_calib_done,
     dram_reset         => c0_ui_clk_sync_rst,
     dram_addr          => dr0_addr,
     dram_cmd           => dr0_cmd,
     dram_en            => dr0_en,
     dram_rdy           => dr0_rdy,
     dram_wr_data       => dr0_wr_data,
     dram_wr_data_end   => dr0_wr_data_end,
     dram_wr_rdy        => dr0_wr_rdy,
     dram_wr_data_en    => dr0_wr_data_en,
     dram_rd_data       => dr0_rd_data,
     dram_rd_data_end   => dr0_rd_data_end,
     dram_rd_data_valid => dr0_rd_data_valid
   );
   
 ram1_sim_i: entity work.ad_RAM_sim
   generic map(
     RAM_SIZE => RAM_SIZE,
     RAM_CLK_PERIOD => RAM_CLK_PERIOD,
     ADDN_RAM_CLK_PERIOD => PCI_CLK_PERIOD -- used as PBWT clk here
   )
   port map(
     dram_clk           => c1_ui_clk,
     dram_addn_clk      => c1_addn_ui_clk,
     dram_calib_done    => open,
     dram_reset         => c1_ui_clk_sync_rst,
     dram_addr          => dr1_addr,
     dram_cmd           => dr1_cmd,
     dram_en            => dr1_en,
     dram_rdy           => dr1_rdy,
     dram_wr_data       => dr1_wr_data,
     dram_wr_data_end   => dr1_wr_data_end,
     dram_wr_rdy        => dr1_wr_rdy,
     dram_wr_data_en    => dr1_wr_data_en,
     dram_rd_data       => dr1_rd_data,
     dram_rd_data_end   => dr1_rd_data_end,
     dram_rd_data_valid => dr1_rd_data_valid
   );
  
  
 -- constants:

 -- DMA input: only send data if main is ready
 dma0_m_axis_tvalid <= dma0_m_axis_tvalid_int and dma0_m_axis_tready;
 
 -- DMA out always ready
 dma1_s_axis_tready <= '1';
 
 bram_dout_b <= (0 => '1', others => '0'), (others => '0') after 500 ns; -- simulate host reset
 
 
 
 -- Testbench process
 tb_p: process
   variable data : std_logic_vector(255 downto 0);
 begin
   dma0_m_axis_tvalid_int <= '0';
   dma0_m_axis_tdata <= (others => '0');
   wait for 100 ns;
   wait until dma0_m_axis_tready = '1';
   wait for 50 ns;
   wait until rising_edge(clk);
   
   -- provide all required data, ignore ready signal
   dma0_m_axis_tvalid_int <= '1';
   data := (others => '1'); -- garbage
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- first bunch
   data := x"deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef"; -- init word
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- wait a bit
   dma0_m_axis_tvalid_int <= '0';
   wait until rising_edge(clk);
   wait for 100 ns;
   wait until rising_edge(clk);
   
   -- constants:
   -- 1 split word (512 bit) -> 2 target words: last split index = 0
   -- K = 768 (x300) (needs to be conform to the popcnt of the besthaps data per target)
   -- 2 K PBWT RAM words (each 16*32 bit data) 
   -- 34 reference words in total (2 for each site) (512 bit): last word index = 33 (x21) 
   -- 2 reference words per site (512 bit): last word index = 1
   -- 17 sites: last site index = 16 (x10)
   -- buffer size is 1024 words (256 bit) (x400)
   -- NOT marked last target charge (set MSB to 0, rest unused)
   data := x"00000000" & x"00000400" & x"00000010" & x"00000001" & x"00000021" & x"00000002" & x"00000300" & x"00000000";
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- wait a bit
   dma0_m_axis_tvalid_int <= '0';
   wait until rising_edge(clk);
   wait for 100 ns;
   wait until rising_edge(clk);
      
   -- target counts: 1 word (for real target) + 3 words (for empty targets)
   -- target #0: 7 splits -> 15*2*2 RAM words (2*7 + 1 refincon sites x2 due to fwd/bck) (x3c)
   -- targets #1-#3: 0 splits -> 1*2*2 RAM words (2*0 + 1 refincon sites x2 due to fwd/bck)
   data := x"000000000000000000000000000000000000000000000000" & x"0000003c" & x"00000007";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);   
   data := x"000000000000000000000000000000000000000000000000" & x"00000004" & x"00000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"000000000000000000000000000000000000000000000000" & x"00000004" & x"00000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"000000000000000000000000000000000000000000000000" & x"00000004" & x"00000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- wait a bit
   dma0_m_axis_tvalid_int <= '0';
   wait until rising_edge(clk);
   wait for 100 ns;
   wait until rising_edge(clk);
   
   -- target data: 2 words (for real target) + 6 words (empty targets) (512bit)
   data := x"00000000000000000000000000000000" & x"0000000000005500" & x"0000000000010055";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"00000000000000000000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- wait a bit
   dma0_m_axis_tvalid_int <= '0';
   wait until rising_edge(clk);
   wait for 100 ns;
   wait until rising_edge(clk);
   
   -- split data: 1 word (for real target) + 3 words zero (dummy targets) (512bit)
   -- split #0:
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000008aaa";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- wait a bit
   dma0_m_axis_tvalid_int <= '0';
   wait until rising_edge(clk);
   wait for 100 ns;
   wait until rising_edge(clk);
   
   -- best haps data: 2 words (for real target) + 6 words zero (dummy targets) (512bit)
   data := x"ffffffffffffffff" & x"ffffffffffffffff" & x"00000000ffffffff" & x"aaaaaaaaaaaaaaaa"; -- popcnt: 64 + 64 + 32 + 32 = 192 
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"ffffffffffffffff" & x"ffffffffffffffff" & x"f0f0f0f0f0f0f0f0" & x"5555555555555555"; -- popcnt: 64 + 64 + 32 + 32 = 192
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"00000000ffffffff" & x"aaaaaaaaaaaaaaaa" & x"ffffffffffffffff" & x"ffffffffffffffff"; -- popcnt: 32 + 32 + 64 + 64 = 192
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);                                  
   data := x"f0f0f0f0f0f0f0f0" & x"5555555555555555" & x"ffffffffffffffff" & x"ffffffffffffffff"; -- popcnt: 32 + 32 + 64 + 64 = 192 (total = 768)
--   data := x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff";
--   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
--   data := x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff";
--   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
--   data := x"7fff0000ffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff";
--   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
--   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- wait a bit
   dma0_m_axis_tvalid_int <= '0';
   wait until rising_edge(clk);
   wait for 300 ns;
   wait until rising_edge(clk);
   
   -- reference data: 34 words (2 per site)
   -- reference site #0:
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000005500";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000005500";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000005500";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"f000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000005500";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #1:
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000000ffaa";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000000ffaa";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000000ffaa";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000000ffaa";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #2:
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000001aaff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000001aaff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000001aaff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000001aaff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #3:
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000010055";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000010055";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000010055";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000010055";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #4:
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #5:
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #6:
   data := x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);

   -- reference site #7:
   data := x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #8:
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #9:
   data := x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #10:
   data := x"5555555555555555" & x"5555555555555555" & x"5555555555555555" & x"5555555555555555";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"5555555555555555" & x"5555555555555555" & x"5555555555555555" & x"5555555555555555";  
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"5555555555555555" & x"5555555555555555" & x"5555555555555555" & x"5555555555555555";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"5555555555555555" & x"5555555555555555" & x"5555555555555555" & x"5555555555555555";  
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #11:
   data := x"aaaaaaaaaaaaaaaa" & x"aaaaaaaaaaaaaaaa" & x"aaaaaaaaaaaaaaaa" & x"aaaaaaaaaaaaaaaa";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"aaaaaaaaaaaaaaaa" & x"aaaaaaaaaaaaaaaa" & x"aaaaaaaaaaaaaaaa" & x"aaaaaaaaaaaaaaaa";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"aaaaaaaaaaaaaaaa" & x"aaaaaaaaaaaaaaaa" & x"aaaaaaaaaaaaaaaa" & x"aaaaaaaaaaaaaaaa";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"aaaaaaaaaaaaaaaa" & x"aaaaaaaaaaaaaaaa" & x"aaaaaaaaaaaaaaaa" & x"aaaaaaaaaaaaaaaa";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #12:
   data := x"0f0f0f0f0f0f0f0f" & x"0f0f0f0f0f0f0f0f" & x"0f0f0f0f0f0f0f0f" & x"0f0f0f0f0f0f0f0f";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"f0f0f0f0f0f0f0f0" & x"f0f0f0f0f0f0f0f0" & x"f0f0f0f0f0f0f0f0" & x"f0f0f0f0f0f0f0f0";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0f0f0f0f0f0f0f0f" & x"0f0f0f0f0f0f0f0f" & x"0f0f0f0f0f0f0f0f" & x"0f0f0f0f0f0f0f0f";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"f0f0f0f0f0f0f0f0" & x"f0f0f0f0f0f0f0f0" & x"f0f0f0f0f0f0f0f0" & x"f0f0f0f0f0f0f0f0";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #13:
   data := x"1111111111111111" & x"2222222222222222" & x"3333333333333333" & x"4444444444444444";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"8888888888888888" & x"6666666666666666" & x"cccccccccccccccc" & x"eeeeeeeeeeeeeeee";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"1111111111111111" & x"2222222222222222" & x"3333333333333333" & x"4444444444444444";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"8888888888888888" & x"6666666666666666" & x"cccccccccccccccc" & x"eeeeeeeeeeeeeeee";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #14:
   data := x"3333333333333333" & x"2222222222222222" & x"1111111111111111" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"7777777777777777" & x"6666666666666666" & x"5555555555555555" & x"4444444444444444";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"bbbbbbbbbbbbbbbb" & x"aaaaaaaaaaaaaaaa" & x"9999999999999999" & x"8888888888888888";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"ffffffffffffffff" & x"eeeeeeeeeeeeeeee" & x"dddddddddddddddd" & x"cccccccccccccccc";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #15:
   data := x"0123456789abcdef" & x"fedcba9876543210" & x"0123456789abcdef" & x"fedcba9876543210";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0123456789abcdef" & x"fedcba9876543210" & x"0123456789abcdef" & x"fedcba9876543210";
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0123456789abcdef" & x"fedcba9876543210" & x"0123456789abcdef" & x"fedcba9876543210";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0123456789abcdef" & x"fedcba9876543210" & x"0123456789abcdef" & x"fedcba9876543210";
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #16:
   data := x"32278157281fd1c1" & x"0798767bfe13a14c" & x"0e335549989972d0" & x"056464646001f00f";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0056798767888347" & x"7477445bcd834deb" & x"d385763bcde299f7" & x"cdf321724386cce4";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"c001cafec001cafec001cafec001cafec001cafec001cafec001cafec001cafe"; -- init word, to test that it is recognized as data here
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"c001cafec001cafec001cafec001cafec001cafec001cafec001cafec001cafe"; -- init word, to test that it is recognized as data here
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
--   data := x"deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef"; -- init word, to test that it is recognized as data here
--   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
--   data := x"deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef"; -- init word, to test that it is recognized as data here
--   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- wait a bit
   dma0_m_axis_tvalid_int <= '0';
   wait until rising_edge(clk);
   wait for 100 ns;
   wait until rising_edge(clk);
   
   -- reference data (reverse): 34 words (2 per site)
   -- reference site #16:
   data := x"32278157281fd1c1" & x"0798767bfe13a14c" & x"0e335549989972d0" & x"056464646001f00f";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0056798767888347" & x"7477445bcd834deb" & x"d385763bcde299f7" & x"cdf321724386cce4";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef"; -- init word, to test that it is recognized as data here
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef"; -- init word, to test that it is recognized as data here
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #15:
   data := x"0123456789abcdef" & x"fedcba9876543210" & x"0123456789abcdef" & x"fedcba9876543210";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0123456789abcdef" & x"fedcba9876543210" & x"0123456789abcdef" & x"fedcba9876543210";
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0123456789abcdef" & x"fedcba9876543210" & x"0123456789abcdef" & x"fedcba9876543210";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0123456789abcdef" & x"fedcba9876543210" & x"0123456789abcdef" & x"fedcba9876543210";
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #14:
   data := x"3333333333333333" & x"2222222222222222" & x"1111111111111111" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"7777777777777777" & x"6666666666666666" & x"5555555555555555" & x"4444444444444444";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"bbbbbbbbbbbbbbbb" & x"aaaaaaaaaaaaaaaa" & x"9999999999999999" & x"8888888888888888";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"ffffffffffffffff" & x"eeeeeeeeeeeeeeee" & x"dddddddddddddddd" & x"cccccccccccccccc";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #13:
   data := x"1111111111111111" & x"2222222222222222" & x"3333333333333333" & x"4444444444444444";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"8888888888888888" & x"6666666666666666" & x"cccccccccccccccc" & x"eeeeeeeeeeeeeeee";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"1111111111111111" & x"2222222222222222" & x"3333333333333333" & x"4444444444444444";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"8888888888888888" & x"6666666666666666" & x"cccccccccccccccc" & x"eeeeeeeeeeeeeeee";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #12:
   data := x"0f0f0f0f0f0f0f0f" & x"0f0f0f0f0f0f0f0f" & x"0f0f0f0f0f0f0f0f" & x"0f0f0f0f0f0f0f0f";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"f0f0f0f0f0f0f0f0" & x"f0f0f0f0f0f0f0f0" & x"f0f0f0f0f0f0f0f0" & x"f0f0f0f0f0f0f0f0";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0f0f0f0f0f0f0f0f" & x"0f0f0f0f0f0f0f0f" & x"0f0f0f0f0f0f0f0f" & x"0f0f0f0f0f0f0f0f";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"f0f0f0f0f0f0f0f0" & x"f0f0f0f0f0f0f0f0" & x"f0f0f0f0f0f0f0f0" & x"f0f0f0f0f0f0f0f0";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #11:
   data := x"aaaaaaaaaaaaaaaa" & x"aaaaaaaaaaaaaaaa" & x"aaaaaaaaaaaaaaaa" & x"aaaaaaaaaaaaaaaa";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"aaaaaaaaaaaaaaaa" & x"aaaaaaaaaaaaaaaa" & x"aaaaaaaaaaaaaaaa" & x"aaaaaaaaaaaaaaaa";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"aaaaaaaaaaaaaaaa" & x"aaaaaaaaaaaaaaaa" & x"aaaaaaaaaaaaaaaa" & x"aaaaaaaaaaaaaaaa";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"aaaaaaaaaaaaaaaa" & x"aaaaaaaaaaaaaaaa" & x"aaaaaaaaaaaaaaaa" & x"aaaaaaaaaaaaaaaa";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #10:
   data := x"5555555555555555" & x"5555555555555555" & x"5555555555555555" & x"5555555555555555";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"5555555555555555" & x"5555555555555555" & x"5555555555555555" & x"5555555555555555";  
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"5555555555555555" & x"5555555555555555" & x"5555555555555555" & x"5555555555555555";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"5555555555555555" & x"5555555555555555" & x"5555555555555555" & x"5555555555555555";  
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #9:
   data := x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #8:
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #7:
   data := x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #6:
   data := x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);

   -- reference site #5:
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #4:
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #3:
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000010055";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000010055";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000010055";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000010055";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #2:
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000001aaff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000001aaff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000001aaff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000001aaff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #1:
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000000ffaa";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000000ffaa";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000000ffaa";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000000ffaa";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #0:
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000005500";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000005500";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000005500";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000005500";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- wait a bit
   dma0_m_axis_tvalid_int <= '0';
   wait until rising_edge(clk);
   wait for 100 ns;
   wait until rising_edge(clk);

--  wait;   
   
   -- repeat the first bunch for a couple of times, but without waiting time between bunches
   -- (bunch is slightly modified now)
   for I in 1 to 4 loop
   
   data := x"deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef"; -- init word
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- constants:
   -- 1 split word (512 bit) -> 2 target words: last split index = 0
   -- K = 705 (x2c1) (see popcnt of besthaps per target) -> forces 31 bits and one complete word to be padded
   -- 2 K PBWT RAM words (each 16*32 bit data)  
   -- 34 reference words in total (2 for each site) (512 bit): last word index = 33 (x21) 
   -- 2 reference words per site (512 bit): last word index = 1
   -- 17 sites: last site index = 16 (x10)
   -- buffer size is 1024 words (256 bit) (x400)
   -- NOT marked last target charge (set MSB to 0, rest unused)
   data := x"00000000" & x"00000400" & x"00000010" & x"00000001" & x"00000021" & x"00000002" & x"000002c1" & x"00000000";
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- target counts: 1 word (for real target) + 3 words (for empty targets)
   -- target #0: 8 splits -> 17*2*2 RAM words (2*8 + 1 refincon sites x2 due to fwd/bck) (x44)
   -- targets #1-#3: 0 splits -> 1*2*2 RAM words (2*0 + 1 refincon sites x2 due to fwd/bck)
   data := x"000000000000000000000000000000000000000000000000" & x"00000044" & x"00000008";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);   
   data := x"000000000000000000000000000000000000000000000000" & x"00000004" & x"00000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"000000000000000000000000000000000000000000000000" & x"00000004" & x"00000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"000000000000000000000000000000000000000000000000" & x"00000004" & x"00000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- target data: 2 words (for real target) + 6 words (empty targets) (512bit)
   data := x"00000000000000000000000000000000" & x"0000000000005500" & x"0000000000010055";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"00000000000000000000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- split data: 1 word (for real target) + 3 words zero (dummy targets) (512bit)
   -- split #0:
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000017035"; -- first and last site are split sites; includes two and three consecutive sites
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- best haps data: 2 words (for real target) + 6 words zero (dummy targets) (512bit)
   data := x"ffffffffffffffff" & x"ffffffffffffffff" & x"00000000ffffffff" & x"aaaaaaaaaaaaaaaa"; -- popcnt: 64 + 64 + 32 + 32 = 192 
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"ffffffffffffffff" & x"ffffffffffffffff" & x"f0f0f0f0f0f0f0f0" & x"5555555555555555"; -- popcnt: 64 + 64 + 32 + 32 = 192
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"00000000ffffffff" & x"aaaaaaaaaaaaaaaa" & x"ffffffffffffffff" & x"ffffffffffffffff"; -- popcnt: 32 + 32 + 64 + 64 = 192
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);                                  
   data := x"f0f0f0f0f0f0f0f0" & x"5555555555555555" & x"0000000100000000" & x"ffffffffffffffff"; -- popcnt: 32 + 32 + 01 + 64 = 129 (total = 705)
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference data: 34 words (2 per site)
   -- reference site #0:
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000005500";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000005500";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000005500";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000005500";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #1:
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000000ffaa";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000000ffaa";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000000ffaa";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000000ffaa";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #2:
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000001aaff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000001aaff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000001aaff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000001aaff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #3:
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000010055";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000010055";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000010055";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000010055";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #4:
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #5:
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #6:
   data := x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);

   -- reference site #7:
   data := x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #8:
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #9:
   data := x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #10:
   data := x"5555555555555555" & x"5555555555555555" & x"5555555555555555" & x"5555555555555555";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"5555555555555555" & x"5555555555555555" & x"5555555555555555" & x"5555555555555555";  
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"5555555555555555" & x"5555555555555555" & x"5555555555555555" & x"5555555555555555";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"5555555555555555" & x"5555555555555555" & x"5555555555555555" & x"5555555555555555";  
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #11:
   data := x"aaaaaaaaaaaaaaaa" & x"aaaaaaaaaaaaaaaa" & x"aaaaaaaaaaaaaaaa" & x"aaaaaaaaaaaaaaaa";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"aaaaaaaaaaaaaaaa" & x"aaaaaaaaaaaaaaaa" & x"aaaaaaaaaaaaaaaa" & x"aaaaaaaaaaaaaaaa";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"aaaaaaaaaaaaaaaa" & x"aaaaaaaaaaaaaaaa" & x"aaaaaaaaaaaaaaaa" & x"aaaaaaaaaaaaaaaa";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"aaaaaaaaaaaaaaaa" & x"aaaaaaaaaaaaaaaa" & x"aaaaaaaaaaaaaaaa" & x"aaaaaaaaaaaaaaaa";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #12:
   data := x"0f0f0f0f0f0f0f0f" & x"0f0f0f0f0f0f0f0f" & x"0f0f0f0f0f0f0f0f" & x"0f0f0f0f0f0f0f0f";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"f0f0f0f0f0f0f0f0" & x"f0f0f0f0f0f0f0f0" & x"f0f0f0f0f0f0f0f0" & x"f0f0f0f0f0f0f0f0";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0f0f0f0f0f0f0f0f" & x"0f0f0f0f0f0f0f0f" & x"0f0f0f0f0f0f0f0f" & x"0f0f0f0f0f0f0f0f";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"f0f0f0f0f0f0f0f0" & x"f0f0f0f0f0f0f0f0" & x"f0f0f0f0f0f0f0f0" & x"f0f0f0f0f0f0f0f0";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #13:
   data := x"1111111111111111" & x"2222222222222222" & x"3333333333333333" & x"4444444444444444";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"8888888888888888" & x"6666666666666666" & x"cccccccccccccccc" & x"eeeeeeeeeeeeeeee";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"1111111111111111" & x"2222222222222222" & x"3333333333333333" & x"4444444444444444";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"8888888888888888" & x"6666666666666666" & x"cccccccccccccccc" & x"eeeeeeeeeeeeeeee";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #14:
   data := x"3333333333333333" & x"2222222222222222" & x"1111111111111111" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"7777777777777777" & x"6666666666666666" & x"5555555555555555" & x"4444444444444444";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"bbbbbbbbbbbbbbbb" & x"aaaaaaaaaaaaaaaa" & x"9999999999999999" & x"8888888888888888";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"ffffffffffffffff" & x"eeeeeeeeeeeeeeee" & x"dddddddddddddddd" & x"cccccccccccccccc";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #15:
   data := x"0123456789abcdef" & x"fedcba9876543210" & x"0123456789abcdef" & x"fedcba9876543210";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0123456789abcdef" & x"fedcba9876543210" & x"0123456789abcdef" & x"fedcba9876543210";
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0123456789abcdef" & x"fedcba9876543210" & x"0123456789abcdef" & x"fedcba9876543210";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0123456789abcdef" & x"fedcba9876543210" & x"0123456789abcdef" & x"fedcba9876543210";
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #16:
   data := x"32278157281fd1c1" & x"0798767bfe13a14c" & x"0e335549989972d0" & x"056464646001f00f";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0056798767888347" & x"7477445bcd834deb" & x"d385763bcde299f7" & x"cdf321724386cce4";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef"; -- init word, to test that it is recognized as data here
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef"; -- init word, to test that it is recognized as data here
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference data (reverse): 34 words (2 per site)
   -- reference site #16:
   data := x"32278157281fd1c1" & x"0798767bfe13a14c" & x"0e335549989972d0" & x"056464646001f00f";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0056798767888347" & x"7477445bcd834deb" & x"d385763bcde299f7" & x"cdf321724386cce4";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef"; -- init word, to test that it is recognized as data here
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef"; -- init word, to test that it is recognized as data here
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #15:
   data := x"0123456789abcdef" & x"fedcba9876543210" & x"0123456789abcdef" & x"fedcba9876543210";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0123456789abcdef" & x"fedcba9876543210" & x"0123456789abcdef" & x"fedcba9876543210";
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0123456789abcdef" & x"fedcba9876543210" & x"0123456789abcdef" & x"fedcba9876543210";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0123456789abcdef" & x"fedcba9876543210" & x"0123456789abcdef" & x"fedcba9876543210";
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #14:
   data := x"3333333333333333" & x"2222222222222222" & x"1111111111111111" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"7777777777777777" & x"6666666666666666" & x"5555555555555555" & x"4444444444444444";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"bbbbbbbbbbbbbbbb" & x"aaaaaaaaaaaaaaaa" & x"9999999999999999" & x"8888888888888888";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"ffffffffffffffff" & x"eeeeeeeeeeeeeeee" & x"dddddddddddddddd" & x"cccccccccccccccc";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #13:
   data := x"1111111111111111" & x"2222222222222222" & x"3333333333333333" & x"4444444444444444";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"8888888888888888" & x"6666666666666666" & x"cccccccccccccccc" & x"eeeeeeeeeeeeeeee";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"1111111111111111" & x"2222222222222222" & x"3333333333333333" & x"4444444444444444";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"8888888888888888" & x"6666666666666666" & x"cccccccccccccccc" & x"eeeeeeeeeeeeeeee";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #12:
   data := x"0f0f0f0f0f0f0f0f" & x"0f0f0f0f0f0f0f0f" & x"0f0f0f0f0f0f0f0f" & x"0f0f0f0f0f0f0f0f";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"f0f0f0f0f0f0f0f0" & x"f0f0f0f0f0f0f0f0" & x"f0f0f0f0f0f0f0f0" & x"f0f0f0f0f0f0f0f0";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0f0f0f0f0f0f0f0f" & x"0f0f0f0f0f0f0f0f" & x"0f0f0f0f0f0f0f0f" & x"0f0f0f0f0f0f0f0f";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"f0f0f0f0f0f0f0f0" & x"f0f0f0f0f0f0f0f0" & x"f0f0f0f0f0f0f0f0" & x"f0f0f0f0f0f0f0f0";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #11:
   data := x"aaaaaaaaaaaaaaaa" & x"aaaaaaaaaaaaaaaa" & x"aaaaaaaaaaaaaaaa" & x"aaaaaaaaaaaaaaaa";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"aaaaaaaaaaaaaaaa" & x"aaaaaaaaaaaaaaaa" & x"aaaaaaaaaaaaaaaa" & x"aaaaaaaaaaaaaaaa";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"aaaaaaaaaaaaaaaa" & x"aaaaaaaaaaaaaaaa" & x"aaaaaaaaaaaaaaaa" & x"aaaaaaaaaaaaaaaa";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"aaaaaaaaaaaaaaaa" & x"aaaaaaaaaaaaaaaa" & x"aaaaaaaaaaaaaaaa" & x"aaaaaaaaaaaaaaaa";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #10:
   data := x"5555555555555555" & x"5555555555555555" & x"5555555555555555" & x"5555555555555555";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"5555555555555555" & x"5555555555555555" & x"5555555555555555" & x"5555555555555555";  
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"5555555555555555" & x"5555555555555555" & x"5555555555555555" & x"5555555555555555";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"5555555555555555" & x"5555555555555555" & x"5555555555555555" & x"5555555555555555";  
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #9:
   data := x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #8:
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #7:
   data := x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #6:
   data := x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff" & x"ffffffffffffffff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);

   -- reference site #5:
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #4:
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #3:
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000010055";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000010055";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000010055";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000010055";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #2:
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000001aaff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000001aaff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000001aaff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000001aaff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #1:
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000000ffaa";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000000ffaa";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000000ffaa";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000000ffaa";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #0:
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000005500";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000005500";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000005500";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000005500";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   end loop;
--   wait;
   
   -- wait a bit
   dma0_m_axis_tvalid_int <= '0';
   wait until rising_edge(clk);
   wait for 300 ns;
   wait until rising_edge(clk);
   
   -- second bunch
   data := x"deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef"; -- init word
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);

   -- constants:
   -- 1 split word (512 bit) -> 2 target words: last split index = 0
   -- K = 8 (see popcnt of target)
   -- 1 K PBWT RAM word (16*32) bit) 
   -- 17 reference words in total (1 for each site) (512 bit): last word index = 16 
   -- 1 references word per site (512 bit): last word index = 0
   -- 17 sites: last site index = 16
   -- buffer size is 1024 words (256 bit) (x400)
   -- marked last target charge (set MSB to 1, rest unused)
   data := x"80000000" & x"00000400" & x"00000010" & x"00000000" & x"00000010" & x"00000001" & x"00000008" & x"00000000";
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);

   -- target counts: 1 word (for real target) + 3 words (for empty targets)
   -- target #0: 8 splits -> 17*1*2 RAM words (2*8 + 1 refincon sites x2 due to fwd/bck) (x22)
   -- targets #1-#3: 0 splits -> 1*1*2 RAM words (2*0 + 1 refincon sites x2 due to fwd/bck)
   data := x"000000000000000000000000000000000000000000000000" & x"00000022" & x"00000008";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);   
   data := x"000000000000000000000000000000000000000000000000" & x"00000002" & x"00000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"000000000000000000000000000000000000000000000000" & x"00000002" & x"00000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"000000000000000000000000000000000000000000000000" & x"00000002" & x"00000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- target data: 2 words (for real target) + 6 words (empty targets) (512bit)
   data := x"00000000000000000000000000000000" & x"0000000000005500" & x"0000000000010055";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"00000000000000000000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- split data: 1 word (for real target) + 3 words zero (dummy targets) (512bit)
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000017035"; -- first and last site are split sites; includes two and three consecutive sites  
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- best haps data: 1 word (for real target) + 3 words zero (dummy targets) (512bit)
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000000a5a5";  -- popcnt = 8
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";  -- popcnt = 0 (total = 8)
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- wait a bit
   dma0_m_axis_tvalid_int <= '0';
   wait until rising_edge(clk);
   wait for 300 ns;
   wait until rising_edge(clk);   
   
   -- reference data: 17 words
   -- reference site #0:
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000005500";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000005500";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #1:
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000000ffaa";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000000ffaa";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #2:
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000001aaff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000001aaff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #3:
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000010055";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000010055";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #4:
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #5:
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #6:
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000001ffff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000001ffff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #7:
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000001ffff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000001ffff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #8:
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #9:
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000001ffff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000001ffff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #10:
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000005500";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000005500";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #11:
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000000ffaa";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000000ffaa";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #12:
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000001aaff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000001aaff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #13:
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000010055";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000010055";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #14:
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #15:
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #16:
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000001ffff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000001ffff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference data (reverse): 17 words
   -- reference site #16:
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000001ffff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000001ffff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);

   -- reference site #15:
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #14:
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #13:
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000010055";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000010055";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #12:
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000001aaff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000001aaff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #11:
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000000ffaa";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000000ffaa";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #10:
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000005500";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000005500";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #9:
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000001ffff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000001ffff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #8:
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #7:
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000001ffff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000001ffff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #6:
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000001ffff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000001ffff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #5:
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #4:
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000000000";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #3:
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000010055";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000010055";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #2:
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000001aaff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000001aaff";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #1:
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000000ffaa";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"000000000000ffaa";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- reference site #0:
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000005500";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   data := x"0000000000000000" & x"0000000000000000" & x"0000000000000000" & x"0000000000005500";   
   senddata(clk => clk, data_in => data, ready => dma0_m_axis_tready, data_out => dma0_m_axis_tdata, valid_out => dma0_m_axis_tvalid_int);
   
   -- that's it for now
   dma0_m_axis_tvalid_int <= '0';

   wait;
   
 end process tb_p;
   
end Behavioral;

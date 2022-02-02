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

entity PBWTPipeline is
generic (
  LOG_PROC_WIDTH : integer := 4; -- -> 16
  LOG_PBWT_BLOCK_WIDTH : integer := 5; -- -> 32
  LOG_MAXK : integer := 15 
);
port ( 
  proc_clk       : in  std_logic;
  pbwt_clk       : in  std_logic;
  reset_proc     : in  std_logic; -- proc_clk domain
  reset_pbwt     : in  std_logic; -- pbwt_clk domain
  -- constants
  K              : in  unsigned(LOG_MAXK downto 0); -- number of conditioning haps (K), sampled in pbwt_clk domain, but set to TIG internally
  -- cref pipeline output (proc_clk domain)
  condensed_data      : in  std_logic_vector(2**LOG_PROC_WIDTH-1 downto 0);
  condensed_ready     : out std_logic;
  condensed_valid     : in  std_logic;
  condensed_splitsite : in  std_logic;
  condensed_last_fs   : in  std_logic;
  condensed_last      : in  std_logic;
  -- PBWT pipeline output (proc_clk domain!)
  -- to be conform to the host expectations, we increase the data width to 2*PROC_WIDTH here
  pbwt_data      : out std_logic_vector(2**LOG_PBWT_BLOCK_WIDTH-1 downto 0);
  pbwt_ready     : in  std_logic;
  pbwt_valid     : out std_logic;
  pbwt_splitsite : out std_logic;
  pbwt_last_fs   : out std_logic;
  pbwt_last      : out std_logic--;
  -- the number of zero bits in the current data word
--  pbwt_cnt0      : out unsigned(LOG_PBWT_BLOCK_WIDTH downto 0) -- need to count up to 2*(2**LOG_PROC_WIDTH) (inclusive!)
);
end PBWTPipeline;

architecture Behavioral of PBWTPipeline is

constant BRAM_LATENCY : integer := 2;

signal clksw_in_din   : std_logic_vector(2**LOG_PROC_WIDTH+2 downto 0);
signal clksw_in_wr_en : std_logic;  
signal clksw_in_rd_en : std_logic;  
signal clksw_in_dout  : std_logic_vector(2**LOG_PROC_WIDTH+2 downto 0);
signal clksw_in_full  : std_logic; 
signal clksw_in_empty : std_logic;  

signal ser_din        : std_logic_vector(2**LOG_PROC_WIDTH-1 downto 0);
signal ser_meta_in    : std_logic_vector(2 downto 0);
signal ser_din_ready  : std_logic;
signal ser_din_valid  : std_logic;
signal ser_dout       : std_logic_vector(1 downto 0);
signal ser_meta_out   : std_logic_vector(2 downto 0);
signal ser_dout_ready : std_logic;
signal ser_dout_valid : std_logic;
signal ser_dout_last  : std_logic;

signal ser_meta_splitsite : std_logic;
signal ser_meta_last_fs   : std_logic;
signal ser_meta_last      : std_logic;

signal got_last_fs_flag : std_logic := '0';

signal infifo_reset : std_logic;

signal infifo_start_addr : unsigned(LOG_MAXK-1 downto 0);
signal infifo_stop_addr  : unsigned(LOG_MAXK-1 downto 0);
signal infifo_din        : std_logic_vector(1 downto 0);
signal infifo_en         : std_logic;
signal infifo_full       : std_logic;

signal inbuf_toggle : std_logic := '0';
signal inbuf_wr_din      : std_logic_vector(1 downto 0);
signal inbuf_wr_addr     : std_logic_vector(LOG_MAXK-2 downto 0);
signal inbuf_wr_en       : std_logic;

signal inbuf_wrA_din      : std_logic_vector(0 downto 0);
signal inbuf_wrA_addr     : std_logic_vector(LOG_MAXK-1 downto 0);
signal inbuf_wrA_sel      : std_logic;
signal inbuf_wrA_en       : std_logic;
signal inbuf_rdA_req_addr : std_logic_vector(LOG_MAXK-1 downto 0);
signal inbuf_rdA_req_sel  : std_logic;
signal inbuf_rdA_req_meta : std_logic_vector(LOG_MAXK-1 downto 0);
signal inbuf_rdA_req_en   : std_logic;
signal inbuf_rdA_dout     : std_logic_vector(0 downto 0);
signal inbuf_rdA_meta     : std_logic_vector(LOG_MAXK-1 downto 0);
signal inbuf_rdA_valid    : std_logic;
signal inbuf_wrB_din      : std_logic_vector(0 downto 0);
signal inbuf_wrB_addr     : std_logic_vector(LOG_MAXK-1 downto 0);
signal inbuf_wrB_sel      : std_logic;
signal inbuf_wrB_en       : std_logic;
signal inbuf_rdB_req_addr : std_logic_vector(LOG_MAXK-1 downto 0);
signal inbuf_rdB_req_sel  : std_logic;
signal inbuf_rdB_req_meta : std_logic_vector(LOG_MAXK-1 downto 0);
signal inbuf_rdB_req_en   : std_logic;
signal inbuf_rdB_dout     : std_logic_vector(0 downto 0);
signal inbuf_rdB_meta     : std_logic_vector(LOG_MAXK-1 downto 0);
signal inbuf_rdB_valid    : std_logic;

signal inbuf_bramA_wea   : std_logic_vector(0 downto 0);
signal inbuf_bramA_addra : std_logic_vector(LOG_MAXK-1 downto 0);
signal inbuf_bramA_dina  : std_logic_vector(0 downto 0);
signal inbuf_bramA_douta : std_logic_vector(0 downto 0);
signal inbuf_bramA_web   : std_logic_vector(0 downto 0);
signal inbuf_bramA_addrb : std_logic_vector(LOG_MAXK-1 downto 0);
signal inbuf_bramA_dinb  : std_logic_vector(0 downto 0);
signal inbuf_bramA_doutb : std_logic_vector(0 downto 0);
signal inbuf_bramB_wea   : std_logic_vector(0 downto 0);
signal inbuf_bramB_addra : std_logic_vector(LOG_MAXK-1 downto 0);
signal inbuf_bramB_dina  : std_logic_vector(0 downto 0);
signal inbuf_bramB_douta : std_logic_vector(0 downto 0);
signal inbuf_bramB_web   : std_logic_vector(0 downto 0);
signal inbuf_bramB_addrb : std_logic_vector(LOG_MAXK-1 downto 0);
signal inbuf_bramB_dinb  : std_logic_vector(0 downto 0);
signal inbuf_bramB_doutb : std_logic_vector(0 downto 0);

signal Kint           : unsigned(LOG_MAXK downto 0) := (others => '0');
signal count0         : unsigned(LOG_MAXK downto 0) := (others => '0');
signal splitsite      : std_logic := '0';
signal lastsite       : std_logic := '0';
signal newsite        : std_logic := '0';  
signal newsite_ready  : std_logic := '0';

signal pbwt_pbwt_splitsite : std_logic := '0'; 
signal pbwt_pbwt_lastsite  : std_logic := '0'; 
signal pbwt_pbwt_data      : std_logic_vector(1 downto 0);  
signal pbwt_pbwt_valid     : std_logic;  
signal pbwt_pbwt_newsite_ready : std_logic := '0';   
signal pbwt_pbwt_sitefinished : std_logic := '0';   

signal outfifo_in_reset : std_logic := '0';

signal outfifo_in_din        : std_logic_vector(1 downto 0);
signal outfifo_in_en         : std_logic;
signal outfifo_in_full       : std_logic;

signal outbuf_toggle : std_logic := '0';
signal outbuf_wr_din      : std_logic_vector(1 downto 0);
signal outbuf_wr_addr     : std_logic_vector(LOG_MAXK-2 downto 0);
signal outbuf_wr_sel      : std_logic_vector(0 downto 0);
signal outbuf_wr_en       : std_logic;
signal outbuf_rd_req_addr : std_logic_vector(LOG_MAXK-2 downto 0);
signal outbuf_rd_req_sel  : std_logic_vector(0 downto 0);
--signal outbuf_rd_req_meta : std_logic_vector(META_WIDTH-1 downto 0);
signal outbuf_rd_req_en   : std_logic;
signal outbuf_rd_dout     : std_logic_vector(1 downto 0);
--signal outbuf_rd_meta     : std_logic_vector(META_WIDTH-1 downto 0);
signal outbuf_rd_valid    : std_logic;
       
signal outbuf_bram_wea   : std_logic_vector(0 downto 0);
signal outbuf_bram_addra : std_logic_vector(LOG_MAXK-1 downto 0);
signal outbuf_bram_dina  : std_logic_vector(1 downto 0);
signal outbuf_bram_addrb : std_logic_vector(LOG_MAXK-1 downto 0);
signal outbuf_bram_doutb : std_logic_vector(1 downto 0);

signal outfifo_reset : std_logic := '0';
signal outfifo_waitfirst : std_logic := '1';

signal outfifo_dout       : std_logic_vector(1 downto 0);
signal outfifo_en         : std_logic;
signal outfifo_am_empty      : std_logic;
signal outfifo_empty      : std_logic;

signal deser_meta_splitsite : std_logic;
signal deser_meta_last      : std_logic;

signal deser_din        : std_logic_vector(1 downto 0);
signal deser_meta_in    : std_logic_vector(2 downto 0);
signal deser_din_ready  : std_logic;
signal deser_din_valid  : std_logic;
signal deser_skip_rem   : std_logic;
signal deser_dout       : std_logic_vector(2**LOG_PBWT_BLOCK_WIDTH-1 downto 0);
--signal deser_cnt0       : std_logic_vector(LOG_PBWT_BLOCK_WIDTH downto 0);
signal deser_meta_out   : std_logic_vector(2 downto 0);
signal deser_dout_ready : std_logic;
signal deser_dout_valid : std_logic;

signal clksw_out_din  : std_logic_vector(2**LOG_PBWT_BLOCK_WIDTH+2 downto 0);
signal clksw_out_wr_en : std_logic;  
signal clksw_out_rd_en : std_logic; 
signal clksw_out_dout : std_logic_vector(2**LOG_PBWT_BLOCK_WIDTH+2 downto 0);
signal clksw_out_full  : std_logic; 
signal clksw_out_empty : std_logic;  

begin

--
-- INPUT
--

-- switch clock domains
input_clksw: entity work.ClkSwitchFIFO_Wrapper
  generic map(
    DATA_WIDTH => 2**LOG_PROC_WIDTH+3
  )
  port map(
    wr_clk => proc_clk,
    wr_rst => reset_proc,
    rd_clk => pbwt_clk,
    rd_rst => reset_pbwt,
    din    => clksw_in_din,
    wr_en  => clksw_in_wr_en,
    rd_en  => clksw_in_rd_en,
    dout   => clksw_in_dout,
    full   => clksw_in_full,
    empty  => clksw_in_empty
  );
clksw_in_din(2**LOG_PROC_WIDTH+2) <= condensed_last;
clksw_in_din(2**LOG_PROC_WIDTH+1) <= condensed_last_fs;
clksw_in_din(2**LOG_PROC_WIDTH) <= condensed_splitsite;
clksw_in_din(2**LOG_PROC_WIDTH-1 downto 0) <= condensed_data;
clksw_in_wr_en <= condensed_valid;
condensed_ready <= not clksw_in_full;

ser: entity work.Serializer
  generic map(
    DIN_WIDTH  => 2**LOG_PROC_WIDTH,
    DOUT_WIDTH => 2,
    META_WIDTH => 3
  )
  port map(
    clk        => pbwt_clk,
    reset      => reset_pbwt,
    din        => ser_din,
    meta_in    => ser_meta_in,
    din_ready  => ser_din_ready,
    din_valid  => ser_din_valid,
    dout       => ser_dout,       
    meta_out   => ser_meta_out,   
    dout_ready => ser_dout_ready, 
    dout_valid => ser_dout_valid, 
    dout_last  => ser_dout_last,
    skip_rem   => '0'
  );
ser_din <= clksw_in_dout(2**LOG_PROC_WIDTH-1 downto 0);
ser_meta_in <= clksw_in_dout(2**LOG_PROC_WIDTH+2 downto 2**LOG_PROC_WIDTH);
clksw_in_rd_en <= ser_din_ready and not clksw_in_empty;
ser_din_valid <= clksw_in_rd_en;

-- FIFO interface of the inbuffer BRAM:
-- loads incoming condensed data into current 
-- selected part of the inbuffer
-- (selection is done via the control process)
infifo: entity work.BRAMFIFOWrInterface
  generic map(
    DATA_WIDTH => 2,
    ADDR_WIDTH => LOG_MAXK-1
  )
  port map(
    clk             => pbwt_clk,
    reset           => infifo_reset,
    fifo_start_addr => infifo_start_addr,
    fifo_stop_addr  => infifo_stop_addr, 
    fifo_din        => infifo_din,
    fifo_en         => infifo_en,
    fifo_full       => infifo_full,
    bram_data       => inbuf_wr_din,
    bram_addr       => inbuf_wr_addr,
    bram_wr_en      => inbuf_wr_en
  );
infifo_start_addr <= (others => '0');
infifo_stop_addr  <= (others => '1'); -- not required
infifo_din <= ser_dout;
ser_dout_ready <= not infifo_full and not got_last_fs_flag and not infifo_reset;
infifo_en <= ser_dout_valid;
-- split wr data (2bit) to both ports of tdp BRAM
inbuf_wrA_din(0) <= inbuf_wr_din(0);
inbuf_wrB_din(0) <= inbuf_wr_din(1);
inbuf_wrA_addr <= inbuf_wr_addr&'0';
inbuf_wrB_addr <= inbuf_wr_addr&'1';
inbuf_wrA_en <= inbuf_wr_en;
inbuf_wrB_en <= inbuf_wr_en;

ser_meta_splitsite <= ser_meta_out(0);
ser_meta_last_fs   <= ser_meta_out(1);
ser_meta_last      <= ser_meta_out(2);

input_control_p: process
  variable curr_cnt0 : unsigned(LOG_MAXK downto 0) := (others => '0');
  variable got_last_fs : std_logic := '0'; 
begin
  wait until rising_edge(pbwt_clk);
  
  -- constant sampling, set to TIG
  Kint <= K;
  
  infifo_reset <= '0';
  
  if reset_pbwt = '1' then
    infifo_reset <= '1';
    inbuf_toggle <= '0';
    curr_cnt0 := (others => '0');
    count0 <= (others => '0');
    splitsite <= '0';
    lastsite <= '0';
    got_last_fs := '0';
  else
    
    -- count zeros from serializer
    if ser_dout_valid = '1' then
      if ser_dout(0) = '0' then
        curr_cnt0 := curr_cnt0 + 1;
      end if;
      if ser_dout(1) = '0' then
        curr_cnt0 := curr_cnt0 + 1;
      end if;
    end if;
    
    -- last data from current site from serializer
    if ser_dout_valid = '1' and ser_dout_last = '1' and ser_meta_last_fs = '1' then
      count0 <= curr_cnt0;
      curr_cnt0 := (others => '0');
      splitsite <= ser_meta_splitsite;
      lastsite  <= ser_meta_last;
      got_last_fs := '1';
    end if;
    
    -- input FIFO just got initialized with the first site OR site was filled and PBWT is ready for next site
    if newsite = '1' then
      infifo_reset <= '1';
      inbuf_toggle <= not inbuf_toggle;
      got_last_fs := '0';
    end if;
    
  end if;
  
  -- to block further load of inbuffer until site is released for processing
  got_last_fs_flag <= got_last_fs;
    
end process input_control_p;

newsite <= got_last_fs_flag and newsite_ready; -- will be set only for one cycle! 

-- buffer that contains current input data:
-- keeps 2x up to MAXK bit for the current and previous sites,
-- while one site is loaded (via the FIFO interface),
-- the other site is processed by the PBWT unit
inbuf: entity work.BRAMToggleDual
  generic map(
    DATA_WIDTH   => 1,
    ADDR_WIDTH   => LOG_MAXK,
    META_WIDTH   => LOG_MAXK,
    BRAM_LATENCY => BRAM_LATENCY
  )
  port map(
    clk         => pbwt_clk,
    reset       => reset_pbwt,
    wrA_din      => inbuf_wrA_din,
    wrA_addr     => inbuf_wrA_addr,
    wrA_sel      => inbuf_wrA_sel,
    wrA_en       => inbuf_wrA_en,
    rdA_req_addr => inbuf_rdA_req_addr,
    rdA_req_sel  => inbuf_rdA_req_sel,
    rdA_req_meta => inbuf_rdA_req_meta,
    rdA_req_en   => inbuf_rdA_req_en,
    rdA_dout     => inbuf_rdA_dout,
    rdA_meta     => inbuf_rdA_meta,
    rdA_valid    => inbuf_rdA_valid,
    wrB_din      => inbuf_wrB_din,
    wrB_addr     => inbuf_wrB_addr,
    wrB_sel      => inbuf_wrB_sel,
    wrB_en       => inbuf_wrB_en,
    rdB_req_addr => inbuf_rdB_req_addr,
    rdB_req_sel  => inbuf_rdB_req_sel,
    rdB_req_meta => inbuf_rdB_req_meta,
    rdB_req_en   => inbuf_rdB_req_en,
    rdB_dout     => inbuf_rdB_dout,
    rdB_meta     => inbuf_rdB_meta,
    rdB_valid    => inbuf_rdB_valid,
    bramA_wea    => inbuf_bramA_wea,
    bramA_addra  => inbuf_bramA_addra,
    bramA_dina   => inbuf_bramA_dina,
    bramA_douta  => inbuf_bramA_douta,
    bramA_web    => inbuf_bramA_web,
    bramA_addrb  => inbuf_bramA_addrb,
    bramA_dinb   => inbuf_bramA_dinb,
    bramA_doutb  => inbuf_bramA_doutb,
    bramB_wea    => inbuf_bramB_wea,
    bramB_addra  => inbuf_bramB_addra,
    bramB_dina   => inbuf_bramB_dina,
    bramB_douta  => inbuf_bramB_douta,
    bramB_web    => inbuf_bramB_web,
    bramB_addrb  => inbuf_bramB_addrb,
    bramB_dinb   => inbuf_bramB_dinb,
    bramB_doutb  => inbuf_bramB_doutb
  );
-- select parts according to current toggle switch
inbuf_wrA_sel <= inbuf_toggle;
inbuf_wrB_sel <= inbuf_toggle;
inbuf_rdA_req_sel <= not inbuf_toggle;
inbuf_rdB_req_sel <= not inbuf_toggle;

inbuf_bramA: entity work.BitRAM_tdp_Wrapper
  generic map(
    ADDR_WIDTH => LOG_MAXK
  )
  port map(
    clk   => pbwt_clk,
    wea   => inbuf_bramA_wea,
    addra => inbuf_bramA_addra,
    dina  => inbuf_bramA_dina,
    douta => inbuf_bramA_douta,
    web   => inbuf_bramA_web,
    addrb => inbuf_bramA_addrb,
    dinb  => inbuf_bramA_dinb,
    doutb => inbuf_bramA_doutb
  );

inbuf_bramB: entity work.BitRAM_tdp_Wrapper
  generic map(
    ADDR_WIDTH => LOG_MAXK
  )
  port map(
    clk   => pbwt_clk,
    wea   => inbuf_bramB_wea,
    addra => inbuf_bramB_addra,
    dina  => inbuf_bramB_dina,
    douta => inbuf_bramB_douta,
    web   => inbuf_bramB_web,
    addrb => inbuf_bramB_addrb,
    dinb  => inbuf_bramB_dinb,
    doutb => inbuf_bramB_doutb
  );


--
-- PBWT processing
--
  
pbwt_i: entity work.PBWT
  generic map(
    LOG_MAXK   => LOG_MAXK,
    BRAM_LATENCY => BRAM_LATENCY
  ) 
  port map(
    clk                => pbwt_clk,
    reset              => reset_pbwt,
    K                  => Kint,
    count0             => count0,
    splitsite          => splitsite,
    lastsite           => lastsite,
    newsite            => newsite,
    newsite_ready      => newsite_ready,
    cdata_rdA_req_addr => inbuf_rdA_req_addr,
    cdata_rdA_req_meta => inbuf_rdA_req_meta,
    cdata_rdA_req_en   => inbuf_rdA_req_en,
    cdata_rdA_data     => inbuf_rdA_dout,
    cdata_rdA_meta     => inbuf_rdA_meta,
    cdata_rdA_valid    => inbuf_rdA_valid,
    cdata_rdB_req_addr => inbuf_rdB_req_addr,
    cdata_rdB_req_meta => inbuf_rdB_req_meta,
    cdata_rdB_req_en   => inbuf_rdB_req_en,
    cdata_rdB_data     => inbuf_rdB_dout,
    cdata_rdB_meta     => inbuf_rdB_meta,
    cdata_rdB_valid    => inbuf_rdB_valid,
    pbwt_splitsite     => pbwt_pbwt_splitsite,
    pbwt_lastsite      => pbwt_pbwt_lastsite,
    pbwt_data          => pbwt_pbwt_data,
    pbwt_valid         => pbwt_pbwt_valid,
    pbwt_newsite_ready => pbwt_pbwt_newsite_ready,
    pbwt_sitefinished  => pbwt_pbwt_sitefinished
  );


--
-- OUTPUT
--

output_control_p: process
  variable next_meta_splitsite : std_logic := '0';
  variable next_meta_last      : std_logic := '0';
  variable got_sitefinished : std_logic := '0';
begin
  wait until rising_edge(pbwt_clk);
  
  outfifo_reset <= '0';
  deser_skip_rem <= '0';
  
  if reset_pbwt = '1' then
    outfifo_reset <= '1';
    outfifo_waitfirst <= '1';
    outbuf_toggle <= '0';
    pbwt_pbwt_newsite_ready <= '1';
    next_meta_splitsite := '0';
    next_meta_last      := '0';
    deser_meta_splitsite <= '0';
    deser_meta_last      <= '0';
    got_sitefinished := '0';
  else
    
    -- skip remainder of Deserializer if outfifo just got empty
    if outfifo_am_empty = '1' and outfifo_en = '1' then
      deser_skip_rem <= '1';
    end if;
    
    -- previous site was fetched and new site is already processed -> toggle
    if  got_sitefinished = '1' and (outfifo_empty = '1' or outfifo_waitfirst = '1') then
      outfifo_reset <= '1';
      outfifo_waitfirst <= '0';
      outbuf_toggle <= not outbuf_toggle;
      pbwt_pbwt_newsite_ready <= '1'; -- may process the next site now
      deser_meta_splitsite <= next_meta_splitsite;
      deser_meta_last      <= next_meta_last     ;
      got_sitefinished := '0';
    elsif outfifo_in_en = '1' then
      -- disable ready signal after first data arrived
      pbwt_pbwt_newsite_ready <= '0';
    end if;
    
    -- flag that PBWT has finished this site, but reaction (reset+toggle) has to follow one cycle later since writing the output will still take place in this cycle 
    if pbwt_pbwt_sitefinished = '1' then
      next_meta_splitsite := pbwt_pbwt_splitsite;
      next_meta_last      := pbwt_pbwt_lastsite;
      got_sitefinished := '1';
    end if;
  end if;
  
end process output_control_p;

-- FIFO interfaces of the outbuffer BRAM:
-- incoming FIFO for PBWT output
-- outgoing FIFO
-- (selection is done via the control process)
outfifo_in: entity work.BRAMFIFOWrInterface
  generic map(
    DATA_WIDTH => 2,
    ADDR_WIDTH => LOG_MAXK-1
  )
  port map(
    clk             => pbwt_clk,
    reset           => outfifo_in_reset,
    fifo_start_addr => (others => '0'),
    fifo_stop_addr  => (others => '1'), -- not required 
    fifo_din        => outfifo_in_din,  
    fifo_en         => outfifo_in_en,   
    fifo_full       => outfifo_in_full,
    bram_data       => outbuf_wr_din,  
    bram_addr       => outbuf_wr_addr, 
    bram_wr_en      => outbuf_wr_en    
  );
-- using the same reset for in and out part
outfifo_in_reset <= outfifo_reset;
-- fill in-part of outfifo with PBWT data
outfifo_in_din <= pbwt_pbwt_data; 
outfifo_in_en  <= pbwt_pbwt_valid;

-- buffer that contains current output data:
-- keeps 2x up to 64k bit for the current and previous sites
outbuf: entity work.BRAMToggle
  generic map(
    DATA_WIDTH   => 2,
    ADDR_WIDTH   => LOG_MAXK-1,
    SELECT_WIDTH => 1, -- divide RAM into two parts
    META_WIDTH   => 1, -- not required here
    BRAM_LATENCY => BRAM_LATENCY
  )
  port map(
    clk         => pbwt_clk,
    reset       => reset_pbwt,
    wr_din      => outbuf_wr_din,
    wr_addr     => outbuf_wr_addr,
    wr_sel      => outbuf_wr_sel,
    wr_en       => outbuf_wr_en,
    rd_req_addr => outbuf_rd_req_addr,
    rd_req_sel  => outbuf_rd_req_sel,
    rd_req_meta => (others => '-'), --outbuf_rd_req_meta,
    rd_req_en   => outbuf_rd_req_en,
    rd_dout     => outbuf_rd_dout,
    rd_meta     => open, --outbuf_rd_meta,
    rd_valid    => outbuf_rd_valid,
    bram_wea    => outbuf_bram_wea,
    bram_addra  => outbuf_bram_addra,
    bram_dina   => outbuf_bram_dina,
    bram_addrb  => outbuf_bram_addrb,
    bram_doutb  => outbuf_bram_doutb
  );
-- select parts according to current toggle switch
outbuf_wr_sel(0) <= outbuf_toggle;
outbuf_rd_req_sel(0) <= not outbuf_toggle;

outbuf_bram: entity work.BitRAM_Wrapper
  generic map(
    ADDR_WIDTH => LOG_MAXK
  )
  port map(
    clk   => pbwt_clk,
    wea   => outbuf_bram_wea,
    addra => outbuf_bram_addra,
    dina  => outbuf_bram_dina,
    addrb => outbuf_bram_addrb,
    doutb => outbuf_bram_doutb
  );

outfifo_out: entity work.BRAMFIFORdInterface
  generic map(
    DATA_WIDTH   => 2,
    ADDR_WIDTH   => LOG_MAXK-1,
    BRAM_LATENCY => BRAM_LATENCY
  )
  port map(
    clk             => pbwt_clk,
    reset           => outfifo_reset,      
    fifo_start_addr => (others => '0'),
    fifo_stop_addr  => Kint(LOG_MAXK downto 1), -- does not work with odd K! (software disables odd K)
    reverse         => '0',
    fifo_dout       => outfifo_dout,
    fifo_en         => outfifo_en,
    fifo_am_empty   => outfifo_am_empty,
    fifo_empty      => outfifo_empty,
    bram_req_addr   => outbuf_rd_req_addr,
    bram_req_en     => outbuf_rd_req_en,
    bram_data       => outbuf_rd_dout,
    bram_valid      => outbuf_rd_valid
  ); 
 
-- deserialize bitwise data
deser: entity work.Deserializer
  generic map(
    DIN_WIDTH  => 2,
    DOUT_WIDTH => 2**LOG_PBWT_BLOCK_WIDTH,
    CNT0_WIDTH => 1,--LOG_PBWT_BLOCK_WIDTH+1,
    META_WIDTH => 3, -- splitsite + last_fs + last
    SKIP_PAD_VALUE => "11" -- one-padding (required for PBWT)
  )
  port map(
    clk        => pbwt_clk,
    reset      => reset_pbwt,
    din        => deser_din,
    meta_in    => deser_meta_in,
    din_ready  => deser_din_ready,
    din_valid  => deser_din_valid,
    skip_rem   => deser_skip_rem,
    dout       => deser_dout,
    cnt0       => open,--deser_cnt0,
    meta_out   => deser_meta_out,
    dout_ready => deser_dout_ready,
    dout_valid => deser_dout_valid
  );
deser_din <= outfifo_dout;
deser_meta_in(0) <= deser_meta_splitsite;
deser_meta_in(1) <= outfifo_am_empty; -- the last data bit from this site is marked by the almost empty signal from the FIFO Rd Interface
deser_meta_in(2) <= deser_meta_last;
outfifo_en <= deser_din_ready and not outfifo_empty and not outfifo_waitfirst and not outfifo_reset;
deser_din_valid <= outfifo_en;
clksw_out_din(2**LOG_PBWT_BLOCK_WIDTH-1 downto 0) <= deser_dout;
--clksw_out_din(2**LOG_PBWT_BLOCK_WIDTH+LOG_PBWT_BLOCK_WIDTH downto 2**LOG_PBWT_BLOCK_WIDTH) <= deser_cnt0;
--clksw_out_din(2**LOG_PBWT_BLOCK_WIDTH+LOG_PBWT_BLOCK_WIDTH+3 downto 2**LOG_PBWT_BLOCK_WIDTH+LOG_PBWT_BLOCK_WIDTH+1) <= deser_meta_out;
clksw_out_din(2**LOG_PBWT_BLOCK_WIDTH+2 downto 2**LOG_PBWT_BLOCK_WIDTH) <= deser_meta_out;
clksw_out_wr_en <= deser_dout_valid;
deser_dout_ready <= not clksw_out_full;

-- switch clock domains
output_clksw: entity work.ClkSwitchFIFO_Wrapper
  generic map(
    DATA_WIDTH => 2**LOG_PBWT_BLOCK_WIDTH+3
  )
  port map(
    wr_clk => pbwt_clk,
    wr_rst => reset_pbwt,
    rd_clk => proc_clk,
    rd_rst => reset_proc,
    din    => clksw_out_din,
    wr_en  => clksw_out_wr_en,
    rd_en  => clksw_out_rd_en,
    dout   => clksw_out_dout,
    full   => clksw_out_full,
    empty  => clksw_out_empty
  );
pbwt_data <= clksw_out_dout(2**LOG_PBWT_BLOCK_WIDTH-1 downto 0);
--pbwt_cnt0 <= unsigned(clksw_out_dout(2**LOG_PBWT_BLOCK_WIDTH+LOG_PBWT_BLOCK_WIDTH downto 2**LOG_PBWT_BLOCK_WIDTH));
pbwt_splitsite <= clksw_out_dout(2**LOG_PBWT_BLOCK_WIDTH);
pbwt_last_fs   <= clksw_out_dout(2**LOG_PBWT_BLOCK_WIDTH+1);
pbwt_last      <= clksw_out_dout(2**LOG_PBWT_BLOCK_WIDTH+2);
clksw_out_rd_en <= pbwt_ready and not clksw_out_empty;
pbwt_valid <= clksw_out_rd_en;

end Behavioral;

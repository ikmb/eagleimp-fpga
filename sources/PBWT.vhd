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

entity PBWT is
generic(
  LOG_MAXK : integer := 15;
  BRAM_LATENCY : integer := 2
);
port(
  clk : in std_logic;
  -- use the reset whenever a new target will be started
  reset : in std_logic;
  -- control signals:
  K : in unsigned(LOG_MAXK downto 0);
  -- meta data for current site 
  count0 : in unsigned(LOG_MAXK downto 0);
  splitsite : in std_logic;
  lastsite : in std_logic;
  -- set for one cycle whenever new site data is available,
  -- this effectively starts processing the site,
  -- meta data is also sampled in this cycle 
  newsite : in std_logic; 
  newsite_ready : out std_logic; -- indicates, if processing of a new site can start
  -- condensed data input access of previous site (two ports for processing 2 bits in one cycle):
  cdata_rdA_req_addr : out std_logic_vector(LOG_MAXK-1 downto 0);
  cdata_rdA_req_meta : out std_logic_vector(LOG_MAXK-1 downto 0);
  cdata_rdA_req_en   : out std_logic;
  cdata_rdA_data     : in  std_logic_vector(0 downto 0);
  cdata_rdA_meta     : in  std_logic_vector(LOG_MAXK-1 downto 0);
  cdata_rdA_valid    : in  std_logic;
  cdata_rdB_req_addr : out std_logic_vector(LOG_MAXK-1 downto 0);
  cdata_rdB_req_meta : out std_logic_vector(LOG_MAXK-1 downto 0);
  cdata_rdB_req_en   : out std_logic;
  cdata_rdB_data     : in  std_logic_vector(0 downto 0);
  cdata_rdB_meta     : in  std_logic_vector(LOG_MAXK-1 downto 0);
  cdata_rdB_valid    : in  std_logic;
  -- PBWT output of current site
  -- NOTE: ensure that output can handle a complete site since once the processing of one site has started it cannot be stopped before the next site!
  pbwt_splitsite : out std_logic;
  pbwt_lastsite  : out std_logic;
  pbwt_data      : out std_logic_vector(1 downto 0);
  pbwt_valid     : out std_logic;
  pbwt_newsite_ready : in std_logic;
  pbwt_sitefinished : out std_logic
);
end entity PBWT;

architecture Behaviour of PBWT is

signal waitfirst : std_logic := '1';
signal firstsite : std_logic := '0';
signal newsite_ready_int : std_logic := '0';
signal pbwt_newsite_ready_int : std_logic := '0';
signal K_int : unsigned(LOG_MAXK downto 0) := (others => '0');
signal count0_int : unsigned(LOG_MAXK downto 0) := (others => '0');
signal splitsite_int : std_logic := '0';
signal lastsite_int : std_logic := '0';
signal firstsite_perm : unsigned(LOG_MAXK-2 downto 0); -- since we're processing two bits at once, this represents the permutation div 2. 

signal perm_fifo_reset : std_logic;

signal perm_curr_fifo_start_addr : unsigned(LOG_MAXK-1 downto 0);
signal perm_curr_fifo_stop_addr  : unsigned(LOG_MAXK-1 downto 0);
signal perm_curr_fifo_dout       : std_logic_vector(2*LOG_MAXK-1 downto 0);
signal perm_curr_fifo_en         : std_logic;
signal perm_curr_fifo_empty      : std_logic;

signal perm_new0_fifo_start_addr : unsigned(LOG_MAXK downto 0);
signal perm_new0_fifo_stop_addr  : unsigned(LOG_MAXK downto 0);
signal perm_new0_fifo_dinA       : std_logic_vector(LOG_MAXK-1 downto 0);
signal perm_new0_fifo_enA        : std_logic;
signal perm_new0_fifo_dinB       : std_logic_vector(LOG_MAXK-1 downto 0);
signal perm_new0_fifo_enB        : std_logic;
signal perm_new0_fifo_full       : std_logic;
signal perm_new0_fifo_bram_dataA : std_logic_vector(LOG_MAXK-1 downto 0);
signal perm_new0_fifo_bram_addrA : std_logic_vector(LOG_MAXK-1 downto 0);
signal perm_new0_fifo_bram_wr_enA: std_logic;
signal perm_new0_fifo_bram_dataB : std_logic_vector(LOG_MAXK-1 downto 0);
signal perm_new0_fifo_bram_addrB : std_logic_vector(LOG_MAXK-1 downto 0);
signal perm_new0_fifo_bram_wr_enB: std_logic;

signal perm_new1_fifo_start_addr : unsigned(LOG_MAXK downto 0);
signal perm_new1_fifo_stop_addr  : unsigned(LOG_MAXK downto 0);
signal perm_new1_fifo_dinA       : std_logic_vector(LOG_MAXK-1 downto 0);
signal perm_new1_fifo_enA        : std_logic;
signal perm_new1_fifo_dinB       : std_logic_vector(LOG_MAXK-1 downto 0);
signal perm_new1_fifo_enB        : std_logic;
signal perm_new1_fifo_full       : std_logic;
signal perm_new1_fifo_bram_dataA : std_logic_vector(LOG_MAXK-1 downto 0);
signal perm_new1_fifo_bram_addrA : std_logic_vector(LOG_MAXK-1 downto 0);
signal perm_new1_fifo_bram_wr_enA: std_logic;
signal perm_new1_fifo_bram_dataB : std_logic_vector(LOG_MAXK-1 downto 0);
signal perm_new1_fifo_bram_addrB : std_logic_vector(LOG_MAXK-1 downto 0);
signal perm_new1_fifo_bram_wr_enB: std_logic;

signal perm_buf_toggle : std_logic := '0';
signal perm_buf_rd_req_addr : std_logic_vector(LOG_MAXK-2 downto 0);
signal perm_buf_rd_req_en   : std_logic;
signal perm_buf_rd_dout     : std_logic_vector(2*LOG_MAXK-1 downto 0);
signal perm_buf_rd_valid    : std_logic;
signal perm_buf_wrA_din      : std_logic_vector(LOG_MAXK-1 downto 0);
signal perm_buf_wrA_addr     : std_logic_vector(LOG_MAXK-1 downto 0);
signal perm_buf_wrA_sel      : std_logic;
signal perm_buf_wrA_en       : std_logic;
signal perm_buf_rdA_req_addr : std_logic_vector(LOG_MAXK-1 downto 0);
signal perm_buf_rdA_req_sel  : std_logic;
signal perm_buf_rdA_req_en   : std_logic;
signal perm_buf_rdA_dout     : std_logic_vector(LOG_MAXK-1 downto 0);
signal perm_buf_rdA_valid    : std_logic;
signal perm_buf_wrB_din      : std_logic_vector(LOG_MAXK-1 downto 0);
signal perm_buf_wrB_addr     : std_logic_vector(LOG_MAXK-1 downto 0);
signal perm_buf_wrB_sel      : std_logic;
signal perm_buf_wrB_en       : std_logic;
signal perm_buf_rdB_req_addr : std_logic_vector(LOG_MAXK-1 downto 0);
signal perm_buf_rdB_req_sel  : std_logic;
signal perm_buf_rdB_req_en   : std_logic;
signal perm_buf_rdB_dout     : std_logic_vector(LOG_MAXK-1 downto 0);
signal perm_buf_rdB_valid    : std_logic;

signal perm_bramA_wea   : std_logic_vector(0 downto 0);
signal perm_bramA_addra : std_logic_vector(LOG_MAXK-1 downto 0);
signal perm_bramA_dina  : std_logic_vector(LOG_MAXK-1 downto 0);
signal perm_bramA_douta : std_logic_vector(LOG_MAXK-1 downto 0);
signal perm_bramA_web   : std_logic_vector(0 downto 0);
signal perm_bramA_addrb : std_logic_vector(LOG_MAXK-1 downto 0);
signal perm_bramA_dinb  : std_logic_vector(LOG_MAXK-1 downto 0);
signal perm_bramA_doutb : std_logic_vector(LOG_MAXK-1 downto 0);
signal perm_bramB_wea   : std_logic_vector(0 downto 0);
signal perm_bramB_addra : std_logic_vector(LOG_MAXK-1 downto 0);
signal perm_bramB_dina  : std_logic_vector(LOG_MAXK-1 downto 0);
signal perm_bramB_douta : std_logic_vector(LOG_MAXK-1 downto 0);
signal perm_bramB_web   : std_logic_vector(0 downto 0);
signal perm_bramB_addrb : std_logic_vector(LOG_MAXK-1 downto 0);
signal perm_bramB_dinb  : std_logic_vector(LOG_MAXK-1 downto 0);
signal perm_bramB_doutb : std_logic_vector(LOG_MAXK-1 downto 0);

begin

newsite_ready <= newsite_ready_int and pbwt_newsite_ready_int;

-- Control the processing of the current site:
-- Read current permutation from permutation RdFIFO.
-- According to the current permutation, request the data at the corresponding position.
cdata_req_p: process
  variable sitefinished : std_logic_vector(BRAM_LATENCY-1 downto 0) := (others => '0');
  variable perm_fifo_reset_del : std_logic := '0';
  variable perm_curr_fifo_empty_del : std_logic := '1';
begin
  wait until rising_edge(clk);
  
  perm_fifo_reset <= '0';
  pbwt_newsite_ready_int <= pbwt_newsite_ready; -- to relax timing
  pbwt_sitefinished <= sitefinished(0);
  sitefinished(BRAM_LATENCY-2 downto 0) := sitefinished(BRAM_LATENCY-1 downto 1);
  sitefinished(BRAM_LATENCY-1) := '0';
  
  if reset = '1' then
    perm_fifo_reset <= '1';
    newsite_ready_int <= '0';
    waitfirst <= '1';
    firstsite <= '0';
    perm_buf_toggle <= '1'; -- we'll toggle at the first site already, so first new permutation data goes to part 0, actually data is read from 1 (but discarded at the first site anyway)
    K_int <= (others => '0');
    count0_int <= (others => '0');
    splitsite_int <= '0';
    lastsite_int <= '0';
    firstsite_perm <= (others => '0');
  else
    -- only during waiting for first data we set the ready signal unconditionally to 1.
    if waitfirst = '1' then
      newsite_ready_int <= '1';
    end if;
    
    if newsite = '1' then
      -- got the first site of the target ready to be processed
      perm_fifo_reset <= '1';
      newsite_ready_int <= '0';
      waitfirst <= '0'; -- got the first site (either now or already before)
      firstsite <= waitfirst; -- if we were waiting for the first site, this is the first site now, else it will be zero
      perm_buf_toggle <= not perm_buf_toggle;
      K_int <= K;
      count0_int <= count0;
      splitsite_int <= splitsite;
      lastsite_int <= lastsite;
    end if;
    
    -- requesting sample data is done in logic, we wait until the requests are finished here
    if perm_curr_fifo_empty = '1' and perm_curr_fifo_empty_del = '0' and perm_fifo_reset_del = '0' and newsite_ready_int = '0' then
      newsite_ready_int <= '1';
      -- if this was the last site, we expect either the reverse data now or a reset. For the reverse data, we reset the firstsite_perm and set waitfirst.
      if lastsite_int = '1' then
        waitfirst <= '1';
        firstsite_perm <= (others => '0');
      end if;
      -- signalize that we finished processing this site for one cycle
      -- -> due to reply-delay of three cycles, we make this signal available in the next cycle
      --    (which makes this signal be set together with the last pbwt_valid)
      sitefinished(BRAM_LATENCY-1) := '1'; 
    end if;
    
    -- Simply count every reply from the FIFO: 
    -- (which can be seen as the corresponding requested address in the permutation buffer,
    --  but only at the first site!)
    -- This is required for generating the identity permutation for the first site.
    if perm_curr_fifo_en = '1' then
      firstsite_perm <= firstsite_perm + 1;
    end if; 
  
  end if;

  -- delay the replies of the permutation requests by one cycle due to timing issues
  perm_new0_fifo_dinA <= cdata_rdA_meta;     
  perm_new1_fifo_dinA <= cdata_rdA_meta;     
  perm_new0_fifo_enA <= not cdata_rdA_data(0) and cdata_rdA_valid;     
  perm_new1_fifo_enA <= cdata_rdA_data(0) and cdata_rdA_valid;
  perm_new0_fifo_dinB <= cdata_rdB_meta;     
  perm_new1_fifo_dinB <= cdata_rdB_meta;     
  perm_new0_fifo_enB <= not cdata_rdB_data(0) and cdata_rdB_valid;     
  perm_new1_fifo_enB <= cdata_rdB_data(0) and cdata_rdB_valid;
  pbwt_splitsite <= splitsite_int;
  pbwt_lastsite <= lastsite_int;     
  pbwt_data  <= cdata_rdB_data & cdata_rdA_data;
  pbwt_valid <= cdata_rdA_valid; -- which is the same as cdata_rdB_valid
  
  perm_fifo_reset_del := perm_fifo_reset;
  perm_curr_fifo_empty_del := perm_curr_fifo_empty;
  
end process cdata_req_p;

-- start/stop addresses for FIFOs
perm_curr_fifo_start_addr <= (others => '0');
perm_curr_fifo_stop_addr  <= K_int(LOG_MAXK downto 1); -- does not work with odd K! (software disables odd K)
perm_new0_fifo_start_addr <= (others => '0');
perm_new0_fifo_stop_addr  <= count0_int;
perm_new1_fifo_start_addr <= count0_int;
perm_new1_fifo_stop_addr  <= K_int;

-- read whenever permutation FIFO is ready and request corresponding sample data,
-- but if we are the first site, we manually set the permutation data to the
-- identity no matter what is stored in the FIFO
perm_curr_fifo_en <= not perm_curr_fifo_empty and not waitfirst and not perm_fifo_reset;
cdata_rdA_req_addr <= perm_curr_fifo_dout(LOG_MAXK-1 downto 0) when firstsite = '0' else std_logic_vector(firstsite_perm & '0');
cdata_rdA_req_meta <= perm_curr_fifo_dout(LOG_MAXK-1 downto 0) when firstsite = '0' else std_logic_vector(firstsite_perm & '0'); -- metadata is the current permutation, will be stored in new order for next permutation 
cdata_rdA_req_en <= perm_curr_fifo_en;
cdata_rdB_req_addr <= perm_curr_fifo_dout(2*LOG_MAXK-1 downto LOG_MAXK) when firstsite = '0' else std_logic_vector(firstsite_perm & '1');
cdata_rdB_req_meta <= perm_curr_fifo_dout(2*LOG_MAXK-1 downto LOG_MAXK) when firstsite = '0' else std_logic_vector(firstsite_perm & '1'); -- metadata is the current permutation, will be stored in new order for next permutation 
cdata_rdB_req_en <= perm_curr_fifo_en;

-- FIFO interface of the PBWT permutations from last site,
-- selection of BRAM part is controlled via the toggle switch (controlled in the req process)
perm_curr_fifo: entity work.BRAMFIFORdInterface
  generic map(
    DATA_WIDTH   => 2*LOG_MAXK,
    ADDR_WIDTH   => LOG_MAXK-1,
    BRAM_LATENCY => BRAM_LATENCY+1 -- because we use BRAMToggleDual, we need to add 1 here!
  )
  port map(
    clk             => clk,
    reset           => perm_fifo_reset,
    fifo_start_addr => perm_curr_fifo_start_addr,
    fifo_stop_addr  => perm_curr_fifo_stop_addr,
    reverse         => '0',
    fifo_dout       => perm_curr_fifo_dout,
    fifo_en         => perm_curr_fifo_en,
    fifo_am_empty   => open,
    fifo_empty      => perm_curr_fifo_empty,
    bram_req_addr   => perm_buf_rd_req_addr,
    bram_req_en     => perm_buf_rd_req_en,
    bram_data       => perm_buf_rd_dout,
    bram_valid      => perm_buf_rd_valid
  );
-- split the RAM requests from the FIFO to both ports of the dual port RAM to read two consecutive permutations at once
perm_buf_rdA_req_addr <= perm_buf_rd_req_addr & '0';
perm_buf_rdB_req_addr <= perm_buf_rd_req_addr & '1';
perm_buf_rdA_req_en <= perm_buf_rd_req_en;
perm_buf_rdB_req_en <= perm_buf_rd_req_en;
perm_buf_rd_dout <= perm_buf_rdB_dout & perm_buf_rdA_dout;
perm_buf_rd_valid <= perm_buf_rdA_valid; -- which is actually the same as perm_buf_rdB_valid

-- FIFO interface for the new PBWT permutations of the currently processed site,
-- selection of BRAM part is controlled via the PBWT control process.
-- Two FIFOs are required here:
-- - one to store the origin of a 0-bit
-- - the other to store the origin of a 1-bit
-- The FIFOs are used mutually exclusive, so they can share
-- the data and RAM ports.
perm_new0_fifo: entity work.BRAMFIFOWrInterfaceDual
  generic map(
    DATA_WIDTH => LOG_MAXK,
    ADDR_WIDTH => LOG_MAXK
  )
  port map(
    clk             => clk,
    reset           => perm_fifo_reset,
    fifo_start_addr => perm_new0_fifo_start_addr,
    fifo_stop_addr  => perm_new0_fifo_stop_addr,
    fifo_dinA       => perm_new0_fifo_dinA,
    fifo_enA        => perm_new0_fifo_enA,
    fifo_dinB       => perm_new0_fifo_dinB,
    fifo_enB        => perm_new0_fifo_enB,
    fifo_full       => perm_new0_fifo_full,
    bram_dataA      => perm_new0_fifo_bram_dataA,    
    bram_addrA      => perm_new0_fifo_bram_addrA,    
    bram_wr_enA     => perm_new0_fifo_bram_wr_enA,
    bram_dataB      => perm_new0_fifo_bram_dataB,    
    bram_addrB      => perm_new0_fifo_bram_addrB,    
    bram_wr_enB     => perm_new0_fifo_bram_wr_enB 
  );                        
perm_new1_fifo: entity work.BRAMFIFOWrInterfaceDual
  generic map(
    DATA_WIDTH => LOG_MAXK,
    ADDR_WIDTH => LOG_MAXK
  )
  port map(
    clk             => clk,
    reset           => perm_fifo_reset,
    fifo_start_addr => perm_new1_fifo_start_addr,
    fifo_stop_addr  => perm_new1_fifo_stop_addr,
    fifo_dinA       => perm_new1_fifo_dinA,
    fifo_enA        => perm_new1_fifo_enA,
    fifo_dinB       => perm_new1_fifo_dinB,
    fifo_enB        => perm_new1_fifo_enB,
    fifo_full       => perm_new1_fifo_full,
    bram_dataA      => perm_new1_fifo_bram_dataA,    
    bram_addrA      => perm_new1_fifo_bram_addrA,    
    bram_wr_enA     => perm_new1_fifo_bram_wr_enA,
    bram_dataB      => perm_new1_fifo_bram_dataB,    
    bram_addrB      => perm_new1_fifo_bram_addrB,    
    bram_wr_enB     => perm_new1_fifo_bram_wr_enB 
  );
-- selective routing of currently active FIFO to BRAM
perm_buf_wrA_en   <= perm_new0_fifo_bram_wr_enA or perm_new1_fifo_bram_wr_enA;
perm_buf_wrA_addr <= perm_new0_fifo_bram_addrA when perm_new0_fifo_bram_wr_enA = '1' else perm_new1_fifo_bram_addrA;
perm_buf_wrA_din  <= perm_new0_fifo_bram_dataA when perm_new0_fifo_bram_wr_enA = '1' else perm_new1_fifo_bram_dataA;                          
perm_buf_wrB_en   <= perm_new0_fifo_bram_wr_enB or perm_new1_fifo_bram_wr_enB;
perm_buf_wrB_addr <= perm_new0_fifo_bram_addrB when perm_new0_fifo_bram_wr_enB = '1' else perm_new1_fifo_bram_addrB;
perm_buf_wrB_din  <= perm_new0_fifo_bram_dataB when perm_new0_fifo_bram_wr_enB = '1' else perm_new1_fifo_bram_dataB;                          

-- permutation buffer contains all global PBWT permutations of the last site
-- and space for the new partial PBWT permutations of the current site 
perm_buf: entity work.BRAMToggleDual
  generic map(
    DATA_WIDTH   => LOG_MAXK,
    ADDR_WIDTH   => LOG_MAXK,
    META_WIDTH   => 1, -- no need for metadata here, need a minimum of 1 though
    BRAM_LATENCY => BRAM_LATENCY
  )
  port map(
    clk         => clk,
    reset       => reset,
    wrA_din      => perm_buf_wrA_din,
    wrA_addr     => perm_buf_wrA_addr,
    wrA_sel      => perm_buf_wrA_sel,
    wrA_en       => perm_buf_wrA_en,
    rdA_req_addr => perm_buf_rdA_req_addr,
    rdA_req_sel  => perm_buf_rdA_req_sel,
    rdA_req_meta => (others => '-'),
    rdA_req_en   => perm_buf_rdA_req_en,
    rdA_dout     => perm_buf_rdA_dout,
    rdA_meta     => open,      
    rdA_valid    => perm_buf_rdA_valid,
    wrB_din      => perm_buf_wrB_din,
    wrB_addr     => perm_buf_wrB_addr,
    wrB_sel      => perm_buf_wrB_sel,
    wrB_en       => perm_buf_wrB_en,
    rdB_req_addr => perm_buf_rdB_req_addr,
    rdB_req_sel  => perm_buf_rdB_req_sel,
    rdB_req_meta => (others => '-'),
    rdB_req_en   => perm_buf_rdB_req_en,
    rdB_dout     => perm_buf_rdB_dout,
    rdB_meta     => open,      
    rdB_valid    => perm_buf_rdB_valid,
    bramA_wea    => perm_bramA_wea,
    bramA_addra  => perm_bramA_addra,
    bramA_dina   => perm_bramA_dina,
    bramA_douta  => perm_bramA_douta,
    bramA_web    => perm_bramA_web,
    bramA_addrb  => perm_bramA_addrb,
    bramA_dinb   => perm_bramA_dinb,
    bramA_doutb  => perm_bramA_doutb,
    bramB_wea    => perm_bramB_wea,
    bramB_addra  => perm_bramB_addra,
    bramB_dina   => perm_bramB_dina,
    bramB_douta  => perm_bramB_douta,
    bramB_web    => perm_bramB_web,
    bramB_addrb  => perm_bramB_addrb,
    bramB_dinb   => perm_bramB_dinb,
    bramB_doutb  => perm_bramB_doutb
  );
-- select parts according to current toggle switch
perm_buf_wrA_sel <= perm_buf_toggle;
perm_buf_wrB_sel <= perm_buf_toggle;
perm_buf_rdA_req_sel <= not perm_buf_toggle;
perm_buf_rdB_req_sel <= not perm_buf_toggle;
  
perm_bramA: entity work.PermutationRAM_Wrapper
  port map(
    clk   => clk,
    wea   => perm_bramA_wea,
    addra => perm_bramA_addra, -- @suppress "Incorrect array size in assignment: expected (<15>) but was (<LOG_MAXK>)"
    dina  => perm_bramA_dina,  -- @suppress "Incorrect array size in assignment: expected (<15>) but was (<LOG_MAXK>)"
    douta => perm_bramA_douta, -- @suppress "Incorrect array size in assignment: expected (<15>) but was (<LOG_MAXK>)"
    web   => perm_bramA_web,
    addrb => perm_bramA_addrb, -- @suppress "Incorrect array size in assignment: expected (<15>) but was (<LOG_MAXK>)"
    dinb  => perm_bramA_dinb,  -- @suppress "Incorrect array size in assignment: expected (<15>) but was (<LOG_MAXK>)"
    doutb => perm_bramA_doutb  -- @suppress "Incorrect array size in assignment: expected (<15>) but was (<LOG_MAXK>)"
  );
  
perm_bramB: entity work.PermutationRAM_Wrapper
  port map(
    clk   => clk,
    wea   => perm_bramB_wea,
    addra => perm_bramB_addra, -- @suppress "Incorrect array size in assignment: expected (<15>) but was (<LOG_MAXK>)"
    dina  => perm_bramB_dina,  -- @suppress "Incorrect array size in assignment: expected (<15>) but was (<LOG_MAXK>)"
    douta => perm_bramB_douta, -- @suppress "Incorrect array size in assignment: expected (<15>) but was (<LOG_MAXK>)"
    web   => perm_bramB_web,                                                                                          
    addrb => perm_bramB_addrb, -- @suppress "Incorrect array size in assignment: expected (<15>) but was (<LOG_MAXK>)"
    dinb  => perm_bramB_dinb,  -- @suppress "Incorrect array size in assignment: expected (<15>) but was (<LOG_MAXK>)"
    doutb => perm_bramB_doutb  -- @suppress "Incorrect array size in assignment: expected (<15>) but was (<LOG_MAXK>)"
  );

end Behaviour;

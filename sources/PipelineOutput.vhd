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

entity PipelineOutput is
generic (
  BUS_WIDTH       : integer := 514; -- complete data width of the pipeline output bus
  PBWTBLOCKS_PER_BUS : integer := 16; -- PBWT blocks per bus word
  BUS_ADDR_WIDTH  : integer := 30;  -- width of the address taken alongside to the output bus
  LOG_PBWT_BLOCK_WIDTH : integer := 5; -- log of PBWT data block size (only the data part, two blocks are required for data + cnt0 offset)
  LOG_MAXK : integer := 15;
  COUNT_WIDTH     : integer := 20   -- split sites and out words counter width
  -- DEBUG
 ;DBG_COUNT_WIDTH : integer := 48
);
port (
  clk : in std_logic;
  reset : in std_logic;
  -- pipeline output
  pbwt_data      : in  std_logic_vector(2**LOG_PBWT_BLOCK_WIDTH-1 downto 0);
  pbwt_ready     : out std_logic;
  pbwt_valid     : in  std_logic;
  pbwt_splitsite : in  std_logic;
  pbwt_last_fs   : in  std_logic;
  pbwt_last      : in  std_logic;
--  pbwt_count0    : in  unsigned(LOG_PBWT_BLOCK_WIDTH downto 0);
  -- considered constant (TIG)
  K               : in unsigned(LOG_MAXK downto 0);
  Kpbwtwords      : in unsigned(COUNT_WIDTH-1 downto 0);    -- 512bit PBWT words required for one site
  pbwtwordcount   : in unsigned(BUS_ADDR_WIDTH-1 downto 0); -- 512bit words required for this target's complete PBWT (including fwd and bck PBWT!)
  -- bus input
  bus_data_in     : in  std_logic_vector(BUS_WIDTH-1 downto 0);
  bus_addr_in     : in  unsigned(BUS_ADDR_WIDTH-1 downto 0);
  bus_valid_in    : in  std_logic;
  -- bus output
  bus_data_out    : out std_logic_vector(BUS_WIDTH-1 downto 0);
  bus_addr_out    : out unsigned(BUS_ADDR_WIDTH-1 downto 0);
  bus_valid_out   : out std_logic;
  -- reference counts (to introduce order for references)
  currsite_prev   : in  unsigned(COUNT_WIDTH-1 downto 0);
  currsite_next   : in  unsigned(COUNT_WIDTH-1 downto 0);
  currsite_toprev : out unsigned(COUNT_WIDTH-1 downto 0); 
  currsite_tonext : out unsigned(COUNT_WIDTH-1 downto 0); 
  -- control
  stall_in        : in  std_logic;   -- don't put more data on the bus
  overflow_flag_out : out std_logic;  -- flag is set if an overflow has occurred in the output buffer
  -- DEBUG
  my_valid_cnt_out : out unsigned(DBG_COUNT_WIDTH-1 downto 0)  
);
end PipelineOutput;

architecture Behavioral of PipelineOutput is

signal pbfifo_din         : std_logic_vector(2**LOG_PBWT_BLOCK_WIDTH+2 DOWNTO 0);
signal pbfifo_wr_en       : std_logic;
signal pbfifo_rd_en       : std_logic;
signal pbfifo_dout        : std_logic_vector(2**LOG_PBWT_BLOCK_WIDTH+2 DOWNTO 0);
signal pbfifo_full        : std_logic;
signal pbfifo_overflow    : std_logic;
signal pbfifo_empty       : std_logic;
signal pbfifo_wr_rst_busy : std_logic;
signal pbfifo_rd_rst_busy : std_logic;

signal overflow_flag : std_logic := '0';

signal bus_data_buf : std_logic_vector(PBWTBLOCKS_PER_BUS*(2**LOG_PBWT_BLOCK_WIDTH)-1 downto 0);
signal bus_data_splitsite : std_logic;
signal bus_data_last_fs : std_logic;
signal bus_data_last : std_logic;
signal bus_data_ready : std_logic := '0';
signal bus_data_ack : std_logic := '0';

signal currsite : unsigned(COUNT_WIDTH-1 downto 0);
signal pad_ramword : std_logic := '0'; -- set if the RAM word is padded after the last cref data word for a site

signal curraddr      : unsigned(BUS_ADDR_WIDTH-1 downto 0) := (others => '0');

signal block_int : std_logic := '0';
signal stall : std_logic := '0';

-- DEBUG
signal my_valid_cnt : unsigned(DBG_COUNT_WIDTH-1 downto 0) := (others => '0');

begin

pbfifo_i: entity work.PipelineBufferFIFO_Wrapper
  port map(
    clk         => clk,      
    reset        => reset,      
    din         => pbfifo_din        ,
    wr_en       => pbfifo_wr_en      ,
    rd_en       => pbfifo_rd_en      ,
    dout        => pbfifo_dout       ,
    full        => pbfifo_full       ,
    overflow    => pbfifo_overflow   ,
    empty       => pbfifo_empty      ,
    wr_rst_busy => pbfifo_wr_rst_busy,
    rd_rst_busy => pbfifo_rd_rst_busy
  );

-- incoming data is routed directly into the FIFO
pbfifo_din(2**LOG_PBWT_BLOCK_WIDTH-1 downto 0) <= pbwt_data;
--pbfifo_din(2**LOG_PBWT_BLOCK_WIDTH+LOG_PBWT_BLOCK_WIDTH downto 2**LOG_PBWT_BLOCK_WIDTH) <= std_logic_vector(pbwt_count0);
pbfifo_din(2**LOG_PBWT_BLOCK_WIDTH)   <= pbwt_splitsite;
pbfifo_din(2**LOG_PBWT_BLOCK_WIDTH+1) <= pbwt_last_fs;
pbfifo_din(2**LOG_PBWT_BLOCK_WIDTH+2) <= pbwt_last;
pbfifo_wr_en <= pbwt_valid and not pbfifo_wr_rst_busy;
pbwt_ready   <= not pbfifo_full and not pbfifo_wr_rst_busy; 

-- watch overfow
ov_p: process
begin
  wait until rising_edge(clk);
  if reset = '1' then
    overflow_flag <= '0';
  else
    if pbfifo_overflow = '1' then
      overflow_flag <= '1';
    end if;
  end if;
end process ov_p;
overflow_flag_out <= overflow_flag;

-- prepare bus data word and keep track of current site 
bus_prepare_p: process
  variable pbfifo_data : std_logic_vector(2**LOG_PBWT_BLOCK_WIDTH-1 downto 0);
  variable pbfifo_splitsite : std_logic;
  variable pbfifo_last_fs : std_logic;
  variable pbfifo_last : std_logic;
--  variable pbfifo_count0 : unsigned(LOG_PBWT_BLOCK_WIDTH downto 0);
  variable curr_fill : integer range 0 to PBWTBLOCKS_PER_BUS := 0;
--  variable count0_acc : unsigned(LOG_MAXK downto 0) := (others => '0');
begin
  wait until rising_edge(clk);
  
  pbfifo_data      := pbfifo_dout(2**LOG_PBWT_BLOCK_WIDTH-1 downto 0);
--  pbfifo_count0    := unsigned(pbfifo_dout(2**LOG_PBWT_BLOCK_WIDTH+LOG_PBWT_BLOCK_WIDTH downto 2**LOG_PBWT_BLOCK_WIDTH));
  pbfifo_splitsite := pbfifo_dout(2**LOG_PBWT_BLOCK_WIDTH);
  pbfifo_last_fs   := pbfifo_dout(2**LOG_PBWT_BLOCK_WIDTH+1);
  pbfifo_last      := pbfifo_dout(2**LOG_PBWT_BLOCK_WIDTH+2);
  
  if reset = '1' then
    curr_fill := 0;
--    count0_acc := (others => '0');
    pad_ramword <= '0';
  else
    
    -- prepare data
    if pbfifo_rd_en = '1' or pad_ramword = '1' then -- fetched a word from FIFO or padding
--      -- accumulate count0
--      count0_acc := count0_acc + pbfifo_count0;
      
      -- right shift buffer
      bus_data_buf((PBWTBLOCKS_PER_BUS-1)*(2**LOG_PBWT_BLOCK_WIDTH)-1 downto 0) <= bus_data_buf(PBWTBLOCKS_PER_BUS*(2**LOG_PBWT_BLOCK_WIDTH)-1 downto 2**LOG_PBWT_BLOCK_WIDTH);
--      -- right shift buffer
--      bus_data_buf((PBWTBLOCKS_PER_BUS-1)*(2**LOG_PBWT_BLOCK_WIDTH+LOG_MAXK+1)-1 downto 0) <= bus_data_buf(PBWTBLOCKS_PER_BUS*(2**LOG_PBWT_BLOCK_WIDTH+LOG_MAXK+1)-1 downto 2**LOG_PBWT_BLOCK_WIDTH+LOG_MAXK+1);
--      -- store current count0
--      bus_data_buf(PBWTBLOCKS_PER_BUS*(2**LOG_PBWT_BLOCK_WIDTH+LOG_MAXK+1)-1 downto (PBWTBLOCKS_PER_BUS-1)*(2**LOG_PBWT_BLOCK_WIDTH+LOG_MAXK+1)+2**LOG_PBWT_BLOCK_WIDTH) <= std_logic_vector(count0_acc);
      -- store data or padding
      if pad_ramword = '1' then
        bus_data_buf(PBWTBLOCKS_PER_BUS*(2**LOG_PBWT_BLOCK_WIDTH)-1 downto (PBWTBLOCKS_PER_BUS-1)*(2**LOG_PBWT_BLOCK_WIDTH)) <= (others => '0'); -- padding with zeros is ok, this part will never be touched by the host
      else                                                    
        bus_data_buf(PBWTBLOCKS_PER_BUS*(2**LOG_PBWT_BLOCK_WIDTH)-1 downto (PBWTBLOCKS_PER_BUS-1)*(2**LOG_PBWT_BLOCK_WIDTH)) <= pbfifo_data;
      end if;
--      -- store data or padding
--      if pad_ramword = '1' then
--        bus_data_buf((PBWTBLOCKS_PER_BUS-1)*(2**LOG_PBWT_BLOCK_WIDTH+LOG_MAXK+1)+2**LOG_PBWT_BLOCK_WIDTH-1 downto (PBWTBLOCKS_PER_BUS-1)*(2**LOG_PBWT_BLOCK_WIDTH+LOG_MAXK+1)) <= (others => '0'); -- padding with zeros is ok, this part will never be touched by the host
--      else                                                    
--        bus_data_buf((PBWTBLOCKS_PER_BUS-1)*(2**LOG_PBWT_BLOCK_WIDTH+LOG_MAXK+1)+2**LOG_PBWT_BLOCK_WIDTH-1 downto (PBWTBLOCKS_PER_BUS-1)*(2**LOG_PBWT_BLOCK_WIDTH+LOG_MAXK+1)) <= pbfifo_data;
--      end if;
      -- store flags (only if actually read from FIFO)
      if pbfifo_rd_en = '1' then
        bus_data_splitsite <= pbfifo_splitsite;
        bus_data_last_fs   <= pbfifo_last_fs;
        bus_data_last      <= pbfifo_last;
        if pbfifo_last_fs = '1' then
          -- activate padding
          pad_ramword <= '1';
        end if;
      end if;
      
      curr_fill := curr_fill + 1;
      if curr_fill = PBWTBLOCKS_PER_BUS then -- ready!
        -- mark ready
        bus_data_ready <= '1';
        pad_ramword <= '0';
      end if;
        
    elsif bus_data_ack = '1' then -- data was fetched and put on bus -> prepare next
      bus_data_ready <= '0';
      curr_fill := 0;
      -- reset flag
      bus_data_last_fs <= '0';
--      if bus_data_last_fs = '1' then -- was last RAM word for this site
--        -- reset count0
--        count0_acc := (others => '0');
--      end if;
    end if;
    
  end if;
end process bus_prepare_p;

pbfifo_rd_en <= not pbfifo_empty and not bus_data_ready and not pad_ramword; 


bus_submit_p: process
  variable padK: std_logic := '0'; -- set if a complete site is padded, i.e. when two split sites follow each other, an empty incon site has to be inserted
  variable padK_cnt : unsigned(COUNT_WIDTH-1 downto 0) := (others => '0');
--  variable padK_curr_cnt0 : unsigned(LOG_MAXK downto 0) := (others => '0');
  variable prev_was_split : std_logic := '1'; -- set if the previous site was a split site. '1' required at the beginning to create an incon segment if the first site is a split
  variable prev_was_last  : std_logic := '0'; -- set if the previous site was the last site of the target.
  variable pad_selector : integer range 0 to PBWTBLOCKS_PER_BUS-1 := 0;
  variable pad_num_bits : integer range 0 to 2**LOG_PBWT_BLOCK_WIDTH-1 := 0;
  variable pad_block : std_logic_vector(2**LOG_PBWT_BLOCK_WIDTH-1 downto 0) := (others => '0');
begin
  wait until rising_edge(clk);
  
  bus_valid_out <= '0';
  bus_data_ack <= '0';
  
  -- stall_in is TIG. We sample this signal to a synchronous register such that it does not do any harm to other combined logic
  stall <= stall_in;
  
  if reset = '1' then
    curraddr      <= (others => '0');
    currsite <= (others => '0');
    padK := '0';
    padK_cnt := (others => '0');
--    padK_curr_cnt0 := (others => '0');
    prev_was_split := '1'; -- '1' required at the beginning to create an incon segment if the first site is a split
    prev_was_last  := '0';
  else
    
    -- forward incoming data
    if bus_valid_in = '1' then
      bus_data_out <= bus_data_in;
      bus_addr_out <= bus_addr_in + pbwtwordcount; -- address mapping: each stage adds its wordcount to the address
      bus_valid_out <= '1';
    elsif (bus_data_ready = '1' and bus_data_ack = '0') or (prev_was_last = '1' and prev_was_split = '1') then -- accept the prepared data and put it on the bus (or insert a padding word at the end)
      -- check if we need a padding segment between two split sites first (or at the end if the last site was a split site, or at the beginning if the first site was a split site)
      if (bus_data_splitsite = '1' or prev_was_last = '1') and prev_was_split = '1' and padK = '0' then -- two subsequent split sites! (or last site was split site)
        padK := '1'; -- activate padding
        padK_cnt := Kpbwtwords; -- number of required padding words
        --padK_curr_cnt0 := (others => '0'); already reset, better timing!
      end if;
      
      bus_data_out <= (others => '0'); -- default
        
      if padK = '0' then -- normal copying
        bus_data_out(BUS_WIDTH-3 downto 0) <= bus_data_buf;      
        bus_data_out(BUS_WIDTH-2) <= bus_data_last_fs; -- MSB-1 contains finished information for this site      
        bus_data_out(BUS_WIDTH-1) <= bus_data_last and not bus_data_splitsite; -- MSB contains finished information, but we have to delay this information to the padding word, if this is a split site!
      elsif stall = '0' and block_int = '0' then -- padK = '1' and output is not stalled or blocked (required for the padding counter)
        -- need to insert an incon site with all zeros in PBWT format --(i.e. the count0 offset fields have to be set correctly).
        -- NOTE: inserting a zero site in a PBWT will luckily not affect the corresponding permutation arrays since the relative permutation passing this site is the identity.
--        for I in 0 to PBWTBLOCKS_PER_BUS-1 loop -- for all PBWT blocks in RAM data word including count0 offset
--          -- insert count0 offset
--          padK_curr_cnt0 := padK_curr_cnt0 + 2**LOG_PBWT_BLOCK_WIDTH;
--          bus_data_out((I+1)*(2**LOG_PBWT_BLOCK_WIDTH+LOG_MAXK+1)-1 downto I*(2**LOG_PBWT_BLOCK_WIDTH+LOG_MAXK+1)+2**LOG_PBWT_BLOCK_WIDTH) <= std_logic_vector(padK_curr_cnt0); 
--        end loop;
        if padK_cnt = 1 then -- this is the last padding word for this site
          bus_data_out(BUS_WIDTH-2) <= '1';
          bus_data_out(BUS_WIDTH-1) <= prev_was_last; -- if the previous site was the last site but also a split site, this padding word is the very last word now.
          -- if K does not match Kpbwtwords we need to insert a one padding --and the correct count0 value (which is K) here
          pad_num_bits := to_integer(K(LOG_PBWT_BLOCK_WIDTH-1 downto 0)); -- K%32 -> indicates the number of bits set to zero in this block (exception: if K%32 = 0 no modifications are required)
          if pad_num_bits /= 0 then
            pad_selector := to_integer(K(LOG_PBWT_BLOCK_WIDTH+3 downto LOG_PBWT_BLOCK_WIDTH)); -- K%512 -> upper 4 bits to indicate block that needs modification
            -- generate padding block with 1-padding
            for I in 0 to 2**LOG_PBWT_BLOCK_WIDTH-1 loop
              if I >= pad_num_bits then 
                pad_block(I) := '1';
              else
                pad_block(I) := '0';
              end if;
            end loop;
--            bus_data_out((pad_selector+1)*(2**LOG_PBWT_BLOCK_WIDTH+LOG_MAXK+1)-1 downto pad_selector*(2**LOG_PBWT_BLOCK_WIDTH+LOG_MAXK+1)+2**LOG_PBWT_BLOCK_WIDTH) <= std_logic_vector(K); -- count0 of block is K
--            bus_data_out(pad_selector*(2**LOG_PBWT_BLOCK_WIDTH+LOG_MAXK+1)+2**LOG_PBWT_BLOCK_WIDTH-1 downto pad_selector*(2**LOG_PBWT_BLOCK_WIDTH+LOG_MAXK+1)) <= pad_block;
            bus_data_out(pad_selector*(2**LOG_PBWT_BLOCK_WIDTH)+2**LOG_PBWT_BLOCK_WIDTH-1 downto pad_selector*(2**LOG_PBWT_BLOCK_WIDTH)) <= pad_block;
          end if;
        end if;
      end if;

      bus_addr_out <= curraddr; 
      
      if stall = '0' and block_int = '0' then 
        bus_valid_out <= '1';
        bus_data_ack <= not padK;
        curraddr <= curraddr + 1; -- counts 512bit words, mapped later correctly by the memory unit
        if padK = '1' then
          padK_cnt := padK_cnt - 1;
          if padK_cnt = 0 then
            padK := '0';
--            padK_curr_cnt0 := (others => '0');
            prev_was_split := prev_was_last; -- if this was the last site, and we needed padding, we will require padding again, if we turn to reverse
            prev_was_last  := '0';
            currsite <= currsite + 1; -- also count padded sites
          end if;
        elsif bus_data_last_fs = '1' then
          currsite <= currsite + 1;
          prev_was_split := bus_data_splitsite;
          prev_was_last  := bus_data_last;
        end if;
        -- DEBUG
        my_valid_cnt <= my_valid_cnt + 1;
      end if;     
    end if;
    
  end if;
end process bus_submit_p;

-- DEBUG
my_valid_cnt_out <= my_valid_cnt;


-- TODO this is not a good scheme since targets have a strongly varying number of sites 
-- and it takes each PE several cycles to prepare a word for the bus (currently 16!),
-- hence it doesn't make sense to block entities this lot.
-- Probably introduce something where each datum has to be ack'ed? Prioritized by how many words are left for the target? 
block_p: process
begin
  wait until rising_edge(clk);
  
  block_int <= '0'; -- default: don't block
  
  if reset = '1' then
    currsite_toprev <= (others => '0');
    currsite_tonext <= (others => '0');
  else
  
  -- for now, don't block!
--    if currsite_prev >= currsite then
--      currsite_tonext <= currsite;
--    else
--      currsite_tonext <= currsite_prev;
--      block_int <= '1';
--    end if;
--    
--    if currsite_next >= currsite then
--      currsite_toprev <= currsite;
--    else
--      currsite_toprev <= currsite_next;
--      block_int <= '1';
--    end if;
    
  end if;
    
end process block_p;

end Behavioral;

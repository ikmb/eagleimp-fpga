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

entity Serializer_tb is
end Serializer_tb;
  
architecture Behavioral of Serializer_tb is
  
constant DATA_WIDTH_LARGE : integer := 16;
constant DATA_WIDTH_SMALL : integer := 1;
constant CNT0_WIDTH : integer := 5;
constant META_WIDTH : integer := 4;
constant SKIP_PAD_VALUE : std_logic_vector(0 downto 0) := "1";

constant CLK_PERIOD : time := 5 ns;

signal clk : std_logic;
signal reset : std_logic;

signal ser_din        : std_logic_vector(DATA_WIDTH_LARGE-1 downto 0);
signal ser_meta_in    : std_logic_vector(META_WIDTH-1 downto 0);
signal ser_din_ready  : std_logic;
signal ser_din_valid  : std_logic;
signal ser_dout       : std_logic_vector(DATA_WIDTH_SMALL-1 downto 0);
signal ser_meta_out   : std_logic_vector(META_WIDTH-1 downto 0);
signal ser_dout_ready : std_logic;
signal ser_dout_valid : std_logic;
signal ser_dout_last  : std_logic;  
signal ser_skip_rem   : std_logic;  

signal deser_din        : std_logic_vector(DATA_WIDTH_SMALL-1 downto 0);
signal deser_meta_in    : std_logic_vector(META_WIDTH-1 downto 0);
signal deser_din_ready  : std_logic;
signal deser_din_valid  : std_logic;
signal deser_skip_rem   : std_logic;
signal deser_dout       : std_logic_vector(DATA_WIDTH_LARGE-1 downto 0);
signal deser_cnt0       : std_logic_vector(CNT0_WIDTH-1 downto 0);
signal deser_meta_out   : std_logic_vector(META_WIDTH-1 downto 0);
signal deser_dout_ready : std_logic;
signal deser_dout_valid : std_logic;

signal block_ser_dout_ready : std_logic := '0';

begin
  
  -- clk processes
  clk_p: process
  begin
    clk <= '0';
    wait for CLK_PERIOD / 2;
    clk <= '1';
    wait for CLK_PERIOD / 2;
  end process clk_p;
  
  reset <= '1', '0' after 10 * CLK_PERIOD;
  
  ser: entity work.Serializer
  generic map(
    DIN_WIDTH  => DATA_WIDTH_LARGE,
    DOUT_WIDTH => DATA_WIDTH_SMALL,
    META_WIDTH => META_WIDTH
  )
  port map(
    clk        => clk,
    reset      => reset,
    din        => ser_din,
    meta_in    => ser_meta_in,
    din_ready  => ser_din_ready,
    din_valid  => ser_din_valid,
    dout       => ser_dout,
    meta_out   => ser_meta_out,
    dout_ready => ser_dout_ready,
    dout_valid => ser_dout_valid,
    dout_last  => ser_dout_last,
    skip_rem   => ser_skip_rem
  );
  
  deser: entity work.Deserializer
  generic map(
    DIN_WIDTH  => DATA_WIDTH_SMALL,
    DOUT_WIDTH => DATA_WIDTH_LARGE,
    CNT0_WIDTH => CNT0_WIDTH,
    META_WIDTH => META_WIDTH,
    SKIP_PAD_VALUE => SKIP_PAD_VALUE
  )
  port map(
    clk        => clk,
    reset      => reset,
    din        => deser_din,
    meta_in    => deser_meta_in,
    din_ready  => deser_din_ready,
    din_valid  => deser_din_valid,
    skip_rem   => deser_skip_rem,
    dout       => deser_dout,
    cnt0       => deser_cnt0,
    meta_out   => deser_meta_out,
    dout_ready => deser_dout_ready,
    dout_valid => deser_dout_valid
  );
  
  deser_din <= ser_dout;
--  deser_meta_in <= ser_meta_out;
  deser_din_valid <= ser_dout_valid;
  ser_dout_ready <= deser_din_ready and not block_ser_dout_ready;
  
  input_p : process
  begin
    wait until rising_edge(clk);

    if reset = '1' then
      ser_din_valid <= '0';
      ser_skip_rem <= '0';
      deser_skip_rem <= '0';
    else
      
      if ser_din_ready = '0' then
        wait until ser_din_ready = '1';
      end if;
      wait until rising_edge(clk);
      
      ser_din_valid <= '1';
      ser_din <= x"1234";
      ser_meta_in <= x"f";
      
      wait until rising_edge(clk);
      if ser_din_ready = '0' then
        wait until ser_din_ready = '1';
      end if;
--      wait until rising_edge(clk);
      
      ser_din_valid <= '1';
      ser_din <= x"cdef";
      ser_meta_in <= x"6";
      
      wait until rising_edge(clk);
      ser_din_valid <= '0';
      
      if ser_din_ready = '0' then
        wait until ser_din_ready = '1';
      end if;
--      wait until rising_edge(clk);
      
      ser_din_valid <= '1';
      ser_din <= x"3a5c";
      ser_meta_in <= x"9";
      
      wait until rising_edge(clk);
      ser_din_valid <= '0';
      
      wait until rising_edge(clk);
      if ser_din_ready = '0' then
        wait until ser_din_ready = '1';
      end if;
--      wait until rising_edge(clk);

      ser_din_valid <= '1';
      ser_din <= x"3579";
      ser_meta_in <= x"1";
      
      wait until rising_edge(clk);
      ser_din_valid <= '0';
      
      wait until rising_edge(clk);
      wait until rising_edge(clk);
      wait until rising_edge(clk);
      wait until rising_edge(clk);
      
      ser_skip_rem <= '1';
      
      wait until rising_edge(clk);
      
      ser_skip_rem <= '0';
      
      wait until rising_edge(clk);
      wait until rising_edge(clk);
      wait until rising_edge(clk);

      deser_skip_rem <= '1';
      
      wait until rising_edge(clk);
      
      deser_skip_rem <= '0';
      wait;
      
    end if;

  end process input_p;
  
  block_ready_p: process
  begin
    
    block_ser_dout_ready <= '0';
    
    deser_dout_ready <= '0';
    
    wait for 150 ns;
    
    deser_dout_ready <= '1';
    
    wait for 20 ns;
    
    block_ser_dout_ready <= '1';
      
    wait for 10 ns;
    
    block_ser_dout_ready <= '0';
      
    wait for 20 ns;
    
    block_ser_dout_ready <= '1';
    
    wait for 100 ns;
    
    block_ser_dout_ready <= '0';

    wait;
  end process block_ready_p;
  
  deser_meta_p: process
  begin
    wait until rising_edge(clk);
    if reset = '1' then
      deser_meta_in <= (others => '0');
    else
      deser_meta_in <= std_logic_vector(unsigned(deser_meta_in)+1);
    end if;
  end process deser_meta_p;
  
end Behavioral;
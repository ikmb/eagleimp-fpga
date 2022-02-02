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

-- supports writes only if dram_en is set together with dram_wr_en!!!
-- masking is not supported!!!
ENTITY ad_RAM_sim IS
   generic(
      RAM_SIZE : integer := 4096;        -- in 576bit words
      RAM_CLK_PERIOD : time := 5 ns;
      ADDN_RAM_CLK_PERIOD : time := 5 ns
   );
   port(
      dram_clk           : out std_logic; -- DRAM clock
      dram_addn_clk      : out std_logic; -- additional DRAM clock
      dram_calib_done    : out std_logic; -- DRAM calibration done
      dram_reset         : out std_logic; -- DRAM clock-synchronous reset
      dram_addr          : in  std_logic_vector(29 downto 0)  := (others => '0'); -- DRAM address (BANK:ROW:COLUMN)
      dram_cmd           : in  std_logic_vector(2 downto 0)   := (others => '0'); -- DRAM command
      dram_en            : in  std_logic                      := '0'; -- DRAM start command
      dram_rdy           : out std_logic; -- DRAM ready to start command
      dram_wr_data       : in  std_logic_vector(575 downto 0) := (others => '0'); -- DRAM write data
      dram_wr_data_end   : in  std_logic                      := '0'; -- DRAM write data end
      dram_wr_rdy        : out std_logic; -- DRAM write fifo ready to accept
      dram_wr_data_en    : in  std_logic                      := '0'; -- DRAM write data enable
      dram_rd_data       : out std_logic_vector(575 downto 0); -- DRAM read data
      dram_rd_data_end   : out std_logic; -- DRAM read data end
      dram_rd_data_valid : out std_logic  -- DRAM read data valid
   );
END ad_RAM_sim;

ARCHITECTURE behavior OF ad_RAM_sim IS

   type ram_contents_t is array (natural range <>) of std_logic_vector(575 downto 0);
   signal ram_contents : ram_contents_t(0 to RAM_SIZE - 1) := (others => (others => '0'));

   signal clk   : std_logic;
   signal reset : std_logic;

   signal ready : std_logic := '0';

   signal rd_data_valid : std_logic := '0';
   signal rd_data       : std_logic_vector(575 downto 0);

BEGIN
   ramclk_process : process
   begin
      clk <= '1';
      wait for RAM_CLK_PERIOD / 2;
      clk <= '0';
      wait for RAM_CLK_PERIOD / 2;
   end process;
   
   addn_ramclk_process : process
   begin
      dram_addn_clk <= '1';
      wait for ADDN_RAM_CLK_PERIOD / 2;
      dram_addn_clk <= '0';
      wait for ADDN_RAM_CLK_PERIOD / 2;
   end process;

   reset <= '1', '0' after 10 * RAM_CLK_PERIOD;

   dram_clk        <= clk;
   dram_calib_done <= not reset;
   dram_reset      <= reset;

   ram_acc_p : process
   begin
      wait until rising_edge(clk);

      ready <= '0';
      rd_data_valid <= '0';

      if reset = '0' then
         ready <= '1';

         if ready = '1' and dram_en = '1' then
            ready <= '0';
            -- write
            if dram_cmd = "000" then
               if dram_wr_data_en = '1' and dram_wr_data_end = '1' then
                  ram_contents(to_integer(unsigned(dram_addr))) <= dram_wr_data;
               end if;
                  
            -- read
            elsif dram_cmd = "001" then
               rd_data_valid <= '1';
               rd_data <= ram_contents(to_integer(unsigned(dram_addr)));
            end if;

         end if;

      end if;

      -- this is delayed by one cycle
      dram_rd_data_valid <= rd_data_valid;
      dram_rd_data_end   <= rd_data_valid;
      dram_rd_data       <= rd_data;

   end process ram_acc_p;

   dram_rdy     <= ready;
   dram_wr_rdy <= ready;

--   ref_zq_p : process
--   begin
--      wait until rising_edge(clk);
--
--      dram_ref_ack <= '0';
--      dram_zq_ack  <= '0';
--
--      if reset = '0' then
--         if dram_ref_req = '1' then
--            dram_ref_ack <= '1';
--         end if;
--
--         if dram_zq_req = '1' then
--            dram_zq_ack <= '1';
--         end if;
--      end if;
--   end process ref_zq_p;

END behavior;
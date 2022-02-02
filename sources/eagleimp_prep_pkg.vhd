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

package eagleimp_prep_pkg is

constant MEM_CMD_WR : std_logic_vector(2 downto 0) := "000"; -- without auto-precharge (auto-precharge may be enabled at a separate pin if ddr entity was compiled with auto-precharge pin)
constant MEM_CMD_RD : std_logic_vector(2 downto 0) := "001"; -- without auto-precharge (auto-precharge may be enabled at a separate pin if ddr entity was compiled with auto-precharge pin)

constant NOT_BUSY_DELAY : integer := 65535;
constant HOST_RESET_CYCLES : integer := 1023; -- host_reset will at least last this many ramclk cycles
--constant HOST_RESET_CYCLES : integer := 268435455; -- host_reset will at least last this many ramclk cycles

constant PIPELINEADDR_WIDTH : integer := 5;
constant NUM_IMPL_PIPELINES : integer := 32; -- has to fit in PIPELINEADDR_WIDTH!!!!
constant NUM_SIM_PIPELINES : integer := 4; -- used for simulation only

constant LOG_PCI_WIDTH : integer := 8; -- 256
constant LOG_RAM_WIDTH : integer := 9; -- 512
constant LOG_PIPELINE_INIT_WIDTH : integer := 7; -- 128
constant LOG_REF_PROC_WIDTH : integer := 4; -- 16
constant LOG_PBWT_BLOCK_WIDTH : integer := 5; -- 32
constant LOG_MAXK : integer := 15; -- 32k
constant REAL_RAM_WIDTH : integer := 576; -- the real width of a RAM data word (including ECC reserved bits)
constant RAM_ADDR_WIDTH : integer := 30;

constant COUNT_WIDTH : integer := 20; -- max. 31 !!! (Restriction in PipelineOutput) -- TODO check, and provide in INFO!
-- DEBUG
constant DBG_COUNT_WIDTH : integer := 48;
type dbg_counter_array is array (natural range <>) of unsigned(DBG_COUNT_WIDTH-1 downto 0);

constant GT_DATATYPE_WIDTH : integer := 64; -- on the host, the genotypes are encoded as two words of this size, namely "is0" and "is2"

type pci_word_array is array (natural range <>) of std_logic_vector(2**LOG_PCI_WIDTH-1 downto 0);
type ram_word_array is array (natural range <>) of std_logic_vector(2**LOG_RAM_WIDTH-1 downto 0);
type pipeline_init_word_array is array (natural range <>) of std_logic_vector(2**LOG_PIPELINE_INIT_WIDTH-1 downto 0);
type refproc_array is array (natural range <>) of std_logic_vector(2**LOG_REF_PROC_WIDTH-1 downto 0);
type pbwtblock_array is array (natural range <>) of std_logic_vector(2**LOG_PBWT_BLOCK_WIDTH-1 downto 0);
type real_ram_word_array is array (natural range <>) of std_logic_vector(REAL_RAM_WIDTH-1 downto 0);
type ram_addr_array is array (natural range <>) of unsigned(RAM_ADDR_WIDTH-1 downto 0);
type counter_array is array (natural range <>) of unsigned(COUNT_WIDTH-1 downto 0);
type count0_array is array (natural range <>) of unsigned(LOG_PBWT_BLOCK_WIDTH downto 0);
type pipelinedata_array is array (natural range <>) of std_logic_vector(2**LOG_RAM_WIDTH+1 downto 0);
type pipelineaddr_array is array (natural range <>) of unsigned(PIPELINEADDR_WIDTH-1 downto 0);
type pipelineinit_data_tag_t is (TGT_CNT, TGT_DATA, SPLIT_DATA, BH_DATA);
type pipelineinit_data_tag_array is array (natural range <>) of pipelineinit_data_tag_t;

type integer_vector is array (natural range <>) of integer;
type boolean_vector is array (natural range <>) of boolean;

end eagleimp_prep_pkg;

package body eagleimp_prep_pkg is

 
end eagleimp_prep_pkg;

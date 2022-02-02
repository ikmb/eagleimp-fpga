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

-- This re-readable FIFO requires a regular FWFT FIFO with an almost full flag to be attached,
-- and the data width to be set accordingly via the generic.
-- Reading a word from the FIFO simply inserts it again, such that all data in the FIFO 
-- will be read again and again. The FIFO has to be reset explicitly
-- to remove the data. The same applies to reset the full flag.
-- The empty flag is asserted only after a reset if there are no words in the FIFO. The
-- output is undefined in that case.
-- The FIFO has an enable/rnw interface which makes it impossible to read and write data
-- at the same time.
-- However, if 'merge' is set together with 'rnw', the output as well as the re-inserted word is combined by
-- bitwise-OR with the data at 'din'. 
entity ReReadableFIFO is
generic (
  DATA_WIDTH : integer := 256
);
port (
--  clk   : in  std_logic;
--  reset : in  std_logic;
  -- User interface
  din      : in  std_logic_vector(DATA_WIDTH-1 downto 0);
  dout     : out std_logic_vector(DATA_WIDTH-1 downto 0);
  en       : in  std_logic;
  rnw      : in  std_logic; -- 1 read, 0 write
  merge    : in  std_logic; -- merges the output and the re-inserted word by bitwise-OR with the input 'din' when reading
  empty    : out std_logic;
  full     : out std_logic;
  rst_busy : out std_logic;     
  -- directly connect to FIFO instance (using the same clk and reset signals as for this entity)
  fifo_din         : out std_logic_vector(DATA_WIDTH-1 downto 0);
  fifo_wr_en       : out std_logic;
  fifo_rd_en       : out std_logic;
  fifo_dout        : in  std_logic_vector(DATA_WIDTH-1 downto 0);
  fifo_full        : in  std_logic;
  fifo_almost_full : in  std_logic;
  fifo_empty       : in  std_logic;
  fifo_wr_rst_busy : in  std_logic;
  fifo_rd_rst_busy : in  std_logic
);
end ReReadableFIFO;

architecture Behavioral of ReReadableFIFO is

signal dout_tmp : std_logic_vector(DATA_WIDTH-1 downto 0);

begin
   
   -- "almost_full" is used instead of "full" to ensure a read datum can be written into the FIFO again

   -- merging inserted data, when merge is set
   dout_tmp <= fifo_dout when merge = '0' else (din or fifo_dout);
   
   -- put data back into FIFO when reading
   fifo_din   <= din when rnw = '0' else dout_tmp;
   -- write if written from outside on non-full FIFO, or if read from non-empty FIFO
   fifo_wr_en <= en and ((not rnw and not fifo_almost_full) or (rnw and not fifo_empty));
   -- read if read from non-empty FIFO
   fifo_rd_en <= en and rnw and not fifo_empty;

   dout  <= dout_tmp;
   empty <= fifo_empty;
   full  <= fifo_almost_full;
   
   rst_busy <= fifo_wr_rst_busy or fifo_rd_rst_busy;

end Behavioral;
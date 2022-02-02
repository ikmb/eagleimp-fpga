# EagleImp-FPGA

This repository contains the VHDL source code to set up an FPGA design project for accelerating the haplotype phasing step in the genotype imputation software [_EagleImp_](https://github.com/ikmb/eagleimp). The sources are optimized for a _Xilinx Kintex UltraScale KU115_ FPGA attached to an _Alpha Data ADM-PCIE-8K5_ accelerator card.

**Note:** Due to licensing reasons, the top-level entity, that connects the main instance to the PCIe interface (for communication with the host) and to the two attached DRAM modules, is omitted. Also, whenever Xilinx IPs, such as BRAM or FIFO instances, are required, we instantiated a simple wrapper here, whose sources are not placed here for the same licensing reason.

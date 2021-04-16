create_clock -period 2.00 -name FPGA_CLK_P -wavefore {0.00 1.00} [get_ports FPGA_CLK_P]
create_clock -period 0.50 -name ADC_CLK_P -wavefore {0.00 0.25} [get_ports ADC_CLK_P]
create_clock -period 0.50 -name DAC_CLK_P -wavefore {0.00 0.25} [get_ports DAC_CLK_P]

set property LOC AA33 [get_ports FPGA_CLK_P]

# Asynchronous Clock Pahts between FPGA_CLK_P ADC_CLK_P AND DAC_CLK_P Clocks

set_clocks_groups -asynchronous - group [get_clocks -include_generated_clocks FPGA_CLK_P] \
                                - group [get_clocks -include_generated_clocks ADC_CLK_P] \
                                - group [get_clocks -include_generated_clocks DAC_CLK_P]
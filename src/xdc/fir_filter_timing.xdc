#create_clock -period 2.00 -name FPGA_CLK_P -waveform {0.00 1.00} [get_ports FPGA_CLK_P]
create_clock -period 0.50 -name ADC_CLK_P -waveform {0.00 0.25} [get_ports ADC_CLK_P]
#create_clock -period 0.50 -name DAC_CLK_P -waveform {0.00 0.25} [get_ports DAC_CLK_P]

#set_property PACKAGE_PIN P4 [get_ports FPGA_CLK_P]
#set_property PACKAGE_PIN R4 [get_ports FPGA_CLK_N]
#set_property IOSTANDARD LVDS [get_ports FPGA_CLK_P]
#set_property IOSTANDARD LVDS [get_ports FPGA_CLK_N]


#set_property PACKAGE_PIN R3 [get_ports ADC_CLK_P]
#set_property PACKAGE_PIN T3 [get_ports ADC_CLK_N]
#set_property IOSTANDARD LVDS [get_ports ADC_CLK_P]
#set_property IOSTANDARD LVDS [get_ports ADC_CLK_N]

#set_property PACKAGE_PIN T4 [get_ports DAC_CLK_P]
#set_property PACKAGE_PIN U3 [get_ports DAC_CLK_N]
#set_property IOSTANDARD LVDS [get_ports DAC_CLK_P]
#set_property IOSTANDARD LVDS [get_ports DAC_CLK_N]



## Asynchronous PATH between FPGA_CLK_P ADC_CLK_P AND DAC_CLK_P CPACKAGE_PINks

#set_clock_groups -asynchronous  -group [get_clocks -include_generated_clocks FPGA_CLK_P] \
#                                        -group [get_clocks -include_generated_clocks ADC_CLK_P]
                                
                                
#set_clock_groups -asynchronous -group [get_clocks -include_generated_clocks FPGA_CLK_P] \                             
#                                -group [get_clocks -include_generated_clocks DAC_CLK_P]
                                
                                
#set_clock_groups -asynchronous -group [get_clocks -include_generated_clocks DAC_CLK_P] \
#                                -group [get_clocks -include_generated_clocks ADC_CLK_P]                                


set_property VCCAUX_IO DONTCARE [get_ports {ADC_CLK_P}]
set_property IOSTANDARD DIFF_HSTL_II [get_ports {ADC_CLK_P}]
set_property LOC R3 [get_ports {ADC_CLK_P}]


set_property VCCAUX_IO DONTCARE [get_ports {ADC_CLK_N}]
set_property IOSTANDARD DIFF_HSTL_II [get_ports {ADC_CLK_N}]
set_property LOC T3 [get_ports {ADC_CLK_N}]


set_property PACKAGE_PIN P4 [get_ports fir_data_out_IO_P]
set_property PACKAGE_PIN R4 [get_ports fir_data_out_IO_N]
set_property IOSTANDARD LVDS [get_ports fir_data_out_IO_P]
set_property IOSTANDARD LVDS [get_ports fir_data_out_IO_N]
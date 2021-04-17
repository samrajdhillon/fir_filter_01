
set ip_name "FIR_FIFO1"
set ip_dir  "../modules/ip"

create_ip -name fifo_generator -vendor xilinx.com -library ip -version 13.2 -module_name $ip_name -dir $ip_dir


set_property -dict [list CONFIG.Input_Data_Width {16} \
                         CONFIG.Input_Depth {131072} \
                         CONFIG.Output_Data_Width {16} \
                         CONFIG.Output_Depth {131072} \
                         CONFIG.Almost_Full_Flag {true} \
                         CONFIG.Data_Count {true} \
                         CONFIG.Data_Count_Width {17} \
                         CONFIG.Write_Data_Count_Width {17} \
                         CONFIG.Read_Data_Count_Width {17} \
                         CONFIG.Full_Threshold_Assert_Value {131070} \
                         CONFIG.Full_Threshold_Negate_Value {131069}] [get_ips $ip_name]


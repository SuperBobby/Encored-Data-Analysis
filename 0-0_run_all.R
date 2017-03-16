#-----------------------#
# 15min data analysis

PLOTTING = T

source("0-1_pre-processing(15min).R")
source("0-2_pre-processing(RealSense).R")
source("0-3_functions_for_table.R")
source("0-4_funtions_for_plot.R")
all_expDate = get.expDate.all()

source("1_peak_avg_base.R")

# source("2_light_on_duration.R")
source('3_partial_light_on_ratio.R')
source("50-3_status2event(light).R")

source("4_light_off_afterwork_ratio.R")

source("5_lunch_saving.R")



# source("6_hvac_on_duration.R")
source("11_com_base_ratio.R")
source("12_max_consecutive_hvac.R")

# source("100_representation_table.R")

#-----------------------#
# sec data analysis 

## light 
## 

# source("50-0_raw2sec(light).R")
source("50-1_sec2status(light).R")
source("50-2_status_aggregation(light).R")
source("50-3_status2event(light).R")

## computer
## 
## 
source("50-0_raw2sec(com).R")
source("50-1_sec2status(com).R")

source("50-3_status2event(com).R")


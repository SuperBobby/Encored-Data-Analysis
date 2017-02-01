
PLOTTING = T
# preprocess all
source("0-1_pre-processing(15min).R")
source("0-2_pre-processing(RealSense).R")
# utility
source("0-3_functions_for_table.R")
source("0-4_funtions_for_plot.R")

#initialization
# data repository
all_expDate = get.expDate.all()

## summary list 
summary_list = list()

# power consumption analysis
source("1_peak_avg_base.R") 

# conservation behavior analysis
# source("2_light_on_duration.R")
source('3_partial_light_on_ratio.R')
source("4_whole_day_ligit_on_count.R") # FIXME: typo - ligit
source("5_lunch_saving.R")
# source("6_hvac_on_duration.R")
# source("11_com_base_ratio.R")
# source("12_max_consecutive_hvac.R")

source("100_representation_table.R")

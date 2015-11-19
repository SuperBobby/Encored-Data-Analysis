#install.packages("chron")
library(chron)
source("Encored-Data-Analysis/getSNUdata.R")

dashboard_data_raw = read.csv("data/lab_display_state_log.csv", header = F)
names(dashboard_data_raw) = c("timestamp", "location", "status")

dashboard_data_raw$timestamp = as.POSIXct(dashboard_data_raw$timestamp/1000, format="%Y-%m-%d", origin='1970-01-01', tz="ROK")

dashboard_data_loaded = subset(dashboard_data_raw, subset=dashboard_data_raw$status == "loaded")

IP_Lab = unlist(strsplit(as.character(dashboard_data_loaded$location), "/"))
IP = IP_Lab[seq(1,length(IP_Lab),2)]
Lab = IP_Lab[seq(2,length(IP_Lab),2)]

dashboard_data_loaded = cbind(dashboard_data_loaded, IP, Lab, Date = as.Date(dashboard_data_loaded$timestamp), 
                              Hours = hours(dashboard_data_loaded$timestamp)+9, Mins = minutes(dashboard_data_loaded$timestamp))

dashboard_data_loaded_MARG = subset(dashboard_data_loaded, subset=dashboard_data_loaded$Lab == "MARG")
dashboard_data_loaded_HCC  = subset(dashboard_data_loaded, subset=dashboard_data_loaded$Lab == "HCC")
dashboard_data_loaded_UX   = subset(dashboard_data_loaded, subset=dashboard_data_loaded$Lab == "UX")

valid_marg_index = ("147.47.123.97" == dashboard_data_loaded_MARG$IP | "147.47.123.230" == dashboard_data_loaded_MARG$IP)
valid_hcc_index = ("147.47.123.184" == dashboard_data_loaded_HCC$IP)
valid_ux_index = ("147.47.123.97" == dashboard_data_loaded_UX$IP)

dashboard_data_loaded_MARG = dashboard_data_loaded_MARG[valid_marg_index,]
dashboard_data_loaded_HCC  = dashboard_data_loaded_HCC[valid_hcc_index,]
dashboard_data_loaded_UX   = dashboard_data_loaded_UX[valid_ux_index,]

qplot(dashboard_data_loaded_MARG$timestamp, dashboard_data_loaded_MARG$Mins)
qplot(dashboard_data_loaded_HCC$timestamp, dashboard_data_loaded_HCC$Mins)
qplot(dashboard_data_loaded_UX$timestamp, dashboard_data_loaded_UX$Mins)





test_func <- function(Hours, Mins){
        print(paste(Hours, Mins))
}

test_func("c", "c")

tmp = aggregate(dashboard_data_loaded_MARG, nfrequency = 1, FUN = mean)



qplot(dashboard_data_loaded_MARG$timestamp)


tail(dashboard_data_loaded)

tail(dashboard_data_loaded_MARG,10)


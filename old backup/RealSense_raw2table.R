# install.packages("lubridate")
library(lubridate)
library(plyr)
library(ggplot2)
library(scales)
library(reshape2)
library(gridExtra)

getRealSenseTable = function(data){
        # convert data format
        joined = as.POSIXct(data$joined/1000, format="%Y-%m-%d", origin='1970-01-01', tz="ROK")
        leaved = as.POSIXct(data$leaved/1000, format="%Y-%m-%d", origin='1970-01-01', tz="ROK")
        duration = as.numeric(data$leaved - data$joined)
        
        basic_table = data.frame(joined, duration)
        basic_table = basic_table[order(joined, leaved),]
        
        # duration pre-processing ###############################
        duration[duration > 600000] = 0
        
        # set every second '0'
        second(basic_table$joined) = 0
        
        # minite
        minute(basic_table$joined)[minute(basic_table$joined) < 15] = 0
        minute(basic_table$joined)[minute(basic_table$joined) >= 15 & minute(basic_table$joined) < 30] = 15
        minute(basic_table$joined)[minute(basic_table$joined) >= 30 & minute(basic_table$joined) < 45] = 30
        minute(basic_table$joined)[minute(basic_table$joined) >= 45] = 45
        
        # aggregation
        basic_table$joined = as.factor(basic_table$joined)
        counting = count(basic_table, "joined")
        avg_duration = aggregate(duration ~ joined, data = basic_table, mean)
        
        return_table = merge(counting, avg_duration)
        return_table$joined = as.POSIXct(return_table$joined, origin='1970-01-01', tz="ROK")
        return_table$duration = return_table$duration/1000
        
        return(return_table)
}

#####################
# raw data loading
RS_raw_marg = read.csv("realsense/marg.csv")
RS_raw_hcc = read.csv("realsense/hcc.csv")
RS_raw_ux = read.csv("realsense/ux.csv")

RS_marg = getRealSenseTable(RS_raw_marg)
RS_hcc  = getRealSenseTable(RS_raw_hcc)
RS_ux   = getRealSenseTable(RS_raw_ux)

RS_start = "2015-10-07"
RS_marg = RS_marg[RS_marg$joined > RS_start,]
RS_hcc  = RS_hcc[RS_hcc$joined > RS_start,]
RS_ux   = RS_ux[RS_ux$joined > RS_start,]

save(RS_marg, file ="data/RS_marg.RData")
save( RS_hcc, file ="data/RS_hcc.RData")
save(  RS_ux, file ="data/RS_ux.RData")


library(data.table)

dt_marg = data.table(RS_marg)
dt_hcc = data.table(RS_hcc)
dt_ux = data.table(RS_ux)

###$$$$$$$$$$$$$$$$$
threshold = 2

dt_marg[, ':='(date = as.Date(joined))]

dt_marg[duration > threshold]

tmp = dt_marg[, .(du_sum = sum(duration)), by=date]
plot(tmp)
ggplot(tmp, aes(x=date, y=du_sum, label=du_sum)) +
        geom_line() +
        geom_point() +
        geom_text(vjust = 0, nudge_y = 1) +
        scale_x_date(date_labels = "%y-%b-%d", breaks = date_breaks("days")) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        ggtitle("MARG RealSense daily duration sum")






marg <- ggplot(tmp, aes(x=date, y=day_freq_sum, label=day_freq_sum)) +
        geom_line() +
        geom_point() +
        geom_text(vjust = 0, nudge_y = 1) +
        scale_x_date(date_labels = "%y-%b-%d", breaks = date_breaks("days")) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        ggtitle("MARG RealSense daily freq")


dt_hcc[, ':='(date = as.Date(joined))]
tmp = dt_hcc[, .(day_freq_sum = sum(freq)), by=date]
hcc <- ggplot(tmp, aes(x=date, y=day_freq_sum, label=day_freq_sum)) +
        geom_line() +
        geom_point() +
        geom_text(vjust = 0, nudge_y = 1) +
        scale_x_date(date_labels = "%y-%b-%d", breaks = date_breaks("days")) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        ggtitle("HCC RealSense daily freq")


dt_ux[, ':='(date = as.Date(joined))]
tmp = dt_ux[, .(day_freq_sum = sum(freq)), by=date]
ux <- ggplot(tmp, aes(x=date, y=day_freq_sum, label=day_freq_sum)) +
        geom_line() +
        geom_point() +
        geom_text(vjust = 0, nudge_y = 1) +
        scale_x_date(date_labels = "%y-%b-%d", breaks = date_breaks("days")) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        ggtitle("UX RealSense daily freq")

grid.arrange(marg, hcc, ux)

###$$$$$$$$$$$$$$$$$$


dt_marg[, ':='(date = as.Date(joined))]
tmp = dt_marg[, .(day_duration_mean = round(mean(duration))), by=date]
marg <- ggplot(tmp, aes(x=date, y=day_duration_mean, label=day_duration_mean)) +
        geom_line() +
        geom_point() +
        geom_text(vjust = 0, nudge_y = 1) +
        scale_x_date(date_labels = "%y-%b-%d", breaks = date_breaks("days")) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        ggtitle("MARG RealSense daily duration")


dt_hcc[, ':='(date = as.Date(joined))]
tmp = dt_hcc[, .(day_duration_mean = round(mean(duration))), by=date]
hcc <- ggplot(tmp, aes(x=date, y=day_duration_mean, label=day_duration_mean)) +
        geom_line() +
        geom_point() +
        geom_text(vjust = 0, nudge_y = 1) +
        scale_x_date(date_labels = "%y-%b-%d", breaks = date_breaks("days")) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        ggtitle("HCC RealSense daily duration")


dt_ux[, ':='(date = as.Date(joined))]
tmp = dt_ux[, .(day_duration_mean = round(mean(duration))), by=date]
ux <- ggplot(tmp, aes(x=date, y=day_duration_mean, label=day_duration_mean)) +
        geom_line() +
        geom_point() +
        geom_text(vjust = 0, nudge_y = 1) +
        scale_x_date(date_labels = "%y-%b-%d", breaks = date_breaks("days")) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        ggtitle("UX RealSense daily duration")

grid.arrange(marg, hcc, ux)


##$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


        

##################


### plotting ### Freq 
RS_total = merge(RS_marg, RS_hcc, by="joined", all = T)
RS_total = merge(RS_total, RS_ux, by="joined", all = T)

names(RS_total) <- c("timestamp", "marg_freq", "marg_duration", "hcc_freq", "hcc_duration", "ux_freq", "ux_duration")
str(RS_total)

marg_plot <- ggplot(data=RS_total, aes(x=timestamp)) +
        geom_point(aes(y=marg_freq), col="red", alpha=0.5) +
        scale_x_datetime(labels = date_format("%m/%d"), breaks = date_breaks("day")) + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1))

hcc_plot <- ggplot(data=RS_total, aes(x=timestamp)) +
        geom_point(aes(y=hcc_freq), col="green", alpha=0.5) +
        scale_x_datetime(labels = date_format("%m/%d"), breaks = date_breaks("day")) + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1))

ux_plot <- ggplot(data=RS_total, aes(x=timestamp)) +
        geom_point(aes(y=ux_freq), col="blue", alpha=0.5) + 
        scale_x_datetime(labels = date_format("%m/%d"), breaks = date_breaks("day")) + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1))


grid.arrange(marg_plot, hcc_plot, ux_plot)

########################


### plotting ### duration

marg_plot <- ggplot(data=RS_total, aes(x=timestamp)) +
        geom_point(aes(y=marg_duration ), col="red", alpha=0.5) +
        scale_x_datetime(labels = date_format("%m/%d"), breaks = date_breaks("day")) + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        scale_y_continuous(limits = c(1, 10))

hcc_plot <- ggplot(data=RS_total, aes(x=timestamp)) +
        geom_point(aes(y=hcc_duration ), col="green", alpha=0.5) +
        scale_x_datetime(labels = date_format("%m/%d"), breaks = date_breaks("day")) + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        scale_y_continuous(limits = c(1, 10))

ux_plot <- ggplot(data=RS_total, aes(x=timestamp)) +
        geom_point(aes(y=ux_duration ), col="blue", alpha=0.5) + 
        scale_x_datetime(labels = date_format("%m/%d"), breaks = date_breaks("day")) + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        scale_y_continuous(limits = c(1, 10))


grid.arrange(marg_plot, hcc_plot, ux_plot)

########################

realSense_marg_raw = read.csv("data/marg.csv")
realSense_hcc_raw = read.csv("data/hcc.csv")
realSense_ux_raw = read.csv("data/ux.csv")

set_KOR_timpstamp <- function(data){
        data[,2] = as.POSIXct(data[,2]/1000, format="%Y-%m-%d", origin='1970-01-01', tz="ROK")
        data[,3] = as.POSIXct(data[,3]/1000, format="%Y-%m-%d", origin='1970-01-01', tz="ROK")
        return(data)
}

realSense_marg_raw = set_KOR_timpstamp(realSense_marg_raw)
realSense_hcc_raw = set_KOR_timpstamp(realSense_hcc_raw)
realSense_ux_raw = set_KOR_timpstamp(realSense_ux_raw)


source("Encored-Data-Analysis/getSNUdata.R")
update_start = "2014-09-01"
update_end = "2016-01-06"
tmp <- loadSNUData(marg_defalut_table_15min, "marg", update_start, update_end, verbose = T)


############################################################


tmp_marg = merge(marg_defalut_table_15min, RS_marg, by.x="timestamp", by.y="joined", all=T)
tmp_hcc = merge(hcc_defalut_table_15min, RS_hcc, by.x="timestamp", by.y="joined", all=T)
tmp_ux = merge(ux_defalut_table_15min, RS_ux, by.x="timestamp", by.y="joined", all=T)

tmp_marg = na.omit(tmp_marg)
tmp_hcc = na.omit(tmp_hcc)
tmp_ux = na.omit(tmp_ux)


ggplot(x = tmp_ux$timestamp, y = tmp_ux$freq)





str(tmp_marg)
cor(tmp_marg[,c(2,3,4,5,6,9,10)])

cor(marg_defalut_table_15min[,2:6])
cor(hcc_defalut_table_15min[,2:6])
cor(ux_defalut_table_15min[,2:6])


plot(RS_marg$freq)
plot(RS_hcc$freq)
plot(RS_ux$freq)

plot(RS_marg$duration)
plot(RS_hcc$duration)
plot(RS_ux$duration)




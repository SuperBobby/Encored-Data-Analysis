library(lubridate)
library(plyr)
library(ggplot2)
library(scales)
library(reshape2)
library(gridExtra)
library(data.table)
library(timeDate)
require(bit64)
library(data.table)

table.time2string <- function(RS_raw_data){
        RS_raw_data$joined = as.POSIXct(RS_raw_data$joined/1000, origin = "1970-01-01", tz="ROK")
        RS_raw_data$leaved = as.POSIXct(RS_raw_data$leaved/1000, origin = "1970-01-01", tz="ROK")
        
        duration = as.numeric(difftime(RS_raw_data$leaved, RS_raw_data$joined, units = "secs"))
        
        return(cbind(RS_raw_data, duration))
}

make.quarter.label = function(input){ 
        # 15min data label maker 
        h = floor(((input-1) * 15)/60)
        m = (input-1)*15 - h*60
        l = paste0(h,":",m)
        return(l)
}

### ----------------------- ###
# raw data loading
# RS_adsl_raw = fread("realsense/adsl.csv")
RS_marg_raw = fread("realsense/marg.csv")
RS_hcc_raw = fread("realsense/hcc.csv")
RS_ux_raw = fread("realsense/ux.csv")

# adsl_RS = table.time2string(RS_adsl_raw)
marg_RS = table.time2string(RS_marg_raw)
hcc_RS = table.time2string(RS_hcc_raw)
ux_RS = table.time2string(RS_ux_raw)

date_adjust_parameter = 7 * 60 * 60 # 7 hours 

marg_RS[, ':='(date=as.Date(joined-date_adjust_parameter, tz="rok"), weekday = isWeekday(joined-date_adjust_parameter))]
hcc_RS[, ':='(date=as.Date(joined-date_adjust_parameter, tz="rok"), weekday = isWeekday(joined-date_adjust_parameter))]
ux_RS[, ':='(date=as.Date(joined-date_adjust_parameter, tz="rok"), weekday = isWeekday(joined-date_adjust_parameter))]

### update : day, weekday depending on the aggDay
marg_RS[, ':='(weekday = isWeekday(date))]
hcc_RS[, ':='(weekday = isWeekday(date))]
ux_RS[, ':='(weekday = isWeekday(date))]

write.csv(marg_RS, file ="data/marg_RS.csv")
write.csv(hcc_RS, file ="data/hcc_RS.csv")
write.csv(ux_RS, file ="data/ux_RS.csv")


### ----------------------- ###
## pre-processing 

{
# duration.filter <- function(dt, threshold_min, threshold_max){
#         filtered_min = nrow(dt[duration <= threshold_min])
#         filtered_max = nrow(dt[duration >= threshold_max])
#         
#         print(paste("filtered", filtered_min, "and", filtered_max, "rows (by min & max, respectively)"))
#         
#         return(dt[duration > threshold_min & duration < threshold_max])
# }
}


## vaild date 
marg_RS = marg_RS[date > "2015-10-14"]
hcc_RS = hcc_RS[date > "2015-10-14"]
ux_RS = ux_RS[date > "2015-10-14"]

## Validate the "duration"
marg_RS[duration > 5*60] # 4 --> Over 1000 case should be removed (single case : duration 1494.048)
marg_RS = marg_RS[duration < 1000]
hcc_RS[duration > 5*60]  # 48 --> All are in the abnormal period (will be removed) 
ux_RS[duration > 5*60]   # 3 --> All cases make sense 


# Find HCC abnormal period --> 15-12-11 ~ 15-12-30 
ggplot(hcc_RS[duration < 1 & date > "2015-12-01" & date < "2016-01-05" ], aes(factor(date), as.numeric(duration))) + 
        geom_boxplot(aes(fill = factor(weekday))) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
        ggtitle(paste("Distribution of individual sample's duration -", toupper("hcc")))

tmp <- hcc_RS[date < "2015-12-11" | date > "2015-12-30"]

ggplot(tmp[duration < 1 & date > "2015-12-01" & date < "2016-01-05" ], aes(factor(date), as.numeric(duration))) + 
        geom_boxplot(aes(fill = factor(weekday))) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
        ggtitle(paste("Distribution of individual sample's duration -", toupper("hcc")))

hcc_RS <- hcc_RS[date < "2015-12-11" | date > "2015-12-30"]


## Sum of duration 
marg_SumOfDuration = marg_RS[, .(sum_of_duration = sum(duration)), by=date]
hcc_SumOfDuration = hcc_RS[, .(sum_of_duration = sum(duration)), by=date]
ux_SumOfDuration = ux_RS[, .(sum_of_duration = sum(duration)), by=date]

marg_Freq = marg_RS[, .(freq = nrow(.SD)), by=date]
hcc_Freq = hcc_RS[, .(freq = nrow(.SD)), by=date]
ux_Freq = ux_RS[, .(freq = nrow(.SD)), by=date]


# set max sum_of_duration as 600
max_sum_of_duration = 600
marg_SumOfDuration[sum_of_duration > max_sum_of_duration]
hcc_SumOfDuration[sum_of_duration > max_sum_of_duration]
ux_SumOfDuration[sum_of_duration > max_sum_of_duration]

marg_SumOfDuration[sum_of_duration > max_sum_of_duration, ':='(sum_of_duration = max_sum_of_duration)]
hcc_SumOfDuration[sum_of_duration > max_sum_of_duration, ':='(sum_of_duration = max_sum_of_duration)]
ux_SumOfDuration[sum_of_duration > max_sum_of_duration, ':='(sum_of_duration = max_sum_of_duration)]

# hist(marg_Freq$freq, 100)
# hist(hcc_Freq$freq, 100)
# hist(ux_Freq$freq, 100)
# 
marg_Freq[freq > 100]
hcc_Freq[freq > 100]
ux_Freq[freq > 100]

# set max freq as 300 
marg_Freq[freq > 300, ':='(freq = 300)]
hcc_Freq[freq > 300, ':='(freq = 300)]
ux_Freq[freq > 300, ':='(freq = 300)]


X.all <- data.frame(unique(marg_dt$aggDay))
colnames(X.all) <- c("date")
marg_SumOfDuration = merge(X.all, marg_SumOfDuration, all.x=TRUE, by.x="date", by.y="date")
hcc_SumOfDuration = merge(X.all, hcc_SumOfDuration, all.x=TRUE, by.x="date", by.y="date")
ux_SumOfDuration = merge(X.all, ux_SumOfDuration, all.x=TRUE, by.x="date", by.y="date")

marg_Freq = merge(X.all, marg_Freq, all.x=TRUE, by.x="date", by.y="date")
hcc_Freq = merge(X.all, hcc_Freq, all.x=TRUE, by.x="date", by.y="date")
ux_Freq = merge(X.all, ux_Freq, all.x=TRUE, by.x="date", by.y="date")

# marg_Freq[is.na(marg_Freq)] <- 0

## Plotting 
## 1. sum of duration 
lab_names = c("MARG", "HCC", "UX")

RS_sum_of_duration_list = list(marg_SumOfDuration, hcc_SumOfDuration, ux_SumOfDuration)
RS_freq_list = list(marg_Freq, hcc_Freq, ux_Freq)

for(i in 1:3){
        plot1_dt = RS_sum_of_duration_list[[i]]
        plot2_dt = RS_freq_list[[i]]
        
        lab_name = lab_names[i]
        
        plot1_name = paste0("RealSense Sum of duration - ", lab_name)
        plot2_name = paste0("RealSense Freq - ", lab_name)
        
        p1 <- ggplot(plot1_dt, aes(x=date, y=sum_of_duration))+
                geom_bar(aes(fill=isWeekday(date)), stat = "identity") + 
                geom_text(aes(label=round(sum_of_duration, 0)), position=position_dodge(width=0.9), vjust=-0.25) + 
                theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                scale_x_date("Timestamp", labels = date_format("%y-%m-%d"), breaks = date_breaks("week")) +
                
                geom_vline(aes(xintercept = as.numeric(as.Date("2015-10-08"))),color="red") +
                geom_vline(aes(xintercept = as.numeric(as.Date("2015-12-01"))),color="red") +
                geom_vline(aes(xintercept = as.numeric(as.Date("2016-01-11"))),color="red") +
                geom_vline(aes(xintercept = as.numeric(as.Date("2016-02-01"))),color="red") +
                geom_vline(aes(xintercept = as.numeric(as.Date("2016-05-16"))),color="red") +
                geom_vline(aes(xintercept = as.numeric(as.Date("2016-06-13"))),color="red") +    
                ggtitle(plot1_name)
        
        p2 <- ggplot(plot2_dt, aes(x=date, y=freq))+
                geom_bar(aes(fill=isWeekday(date)), stat = "identity") + 
                geom_text(aes(label=round(freq, 0)), position=position_dodge(width=0.9), vjust=-0.25) + 
                theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                scale_x_date("Timestamp", labels = date_format("%y-%m-%d"), breaks = date_breaks("week")) +
                
                geom_vline(aes(xintercept = as.numeric(as.Date("2015-10-08"))),color="red") +
                geom_vline(aes(xintercept = as.numeric(as.Date("2015-12-01"))),color="red") +
                geom_vline(aes(xintercept = as.numeric(as.Date("2016-01-11"))),color="red") +
                geom_vline(aes(xintercept = as.numeric(as.Date("2016-02-01"))),color="red") +
                geom_vline(aes(xintercept = as.numeric(as.Date("2016-05-16"))),color="red") +
                geom_vline(aes(xintercept = as.numeric(as.Date("2016-06-13"))),color="red") +    
                ylim(0,300) + 
                ggtitle(plot2_name)

        print(p1)
        print(p2)
        
        plots <- arrangeGrob(p1, p2)
        ggsave(file = paste0("plots/RealSense-",lab_name, ".png"), width = 25, height = 20, dpi = 300, plots)
}
        
# 
# ## 2. Freq 
# RS_freq_list = list(marg_Freq, hcc_Freq, ux_Freq)
# 
# for(i in 1:3){
#         plot_dt = RS_freq_list[[i]]
#         lab_name = lab_names[i]
#         
#         plot_name = paste0("RealSense Freq - ", lab_name)
#         
#         p2 <- ggplot(plot_dt, aes(x=date, y=freq))+
#                 geom_bar(aes(fill=isWeekday(date)), stat = "identity") + 
#                 geom_text(aes(label=round(freq, 0)), position=position_dodge(width=0.9), vjust=-0.25) + 
#                 theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#                 scale_x_date("Timestamp", labels = date_format("%y-%m-%d"), breaks = date_breaks("week")) +
#                 
#                 geom_vline(aes(xintercept = as.numeric(as.Date("2015-10-08"))),color="red") +
#                 geom_vline(aes(xintercept = as.numeric(as.Date("2015-12-01"))),color="red") +
#                 geom_vline(aes(xintercept = as.numeric(as.Date("2016-01-11"))),color="red") +
#                 geom_vline(aes(xintercept = as.numeric(as.Date("2016-02-01"))),color="red") +
#                 geom_vline(aes(xintercept = as.numeric(as.Date("2016-05-16"))),color="red") +
#                 geom_vline(aes(xintercept = as.numeric(as.Date("2016-06-13"))),color="red") +    
#                 ggtitle(plot_name)
#         
#         print(p)
#         ggsave(file = paste0(plot_name, ".png"), width = 25, height = 10, dpi = 300, p)
# }







#@@@@@@@@@@@@@@@@@@# ----------------------------------------------------- #@@@@@@@@@@@@@@@@@@@@@@@@@#
# 
# ppp <- ggplot(marg_SumOfDuration, aes(x=date, y=sum_of_duration))+
#         geom_bar(aes(fill=isWeekday(date)), stat = "identity") + 
#         geom_text(aes(label=round(sum_of_duration, 0)), position=position_dodge(width=0.9), vjust=-0.25) + 
#         theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#         scale_x_date("Timestamp", labels = date_format("%y-%m-%d"), breaks = date_breaks("week")) +
# 
#         geom_vline(aes(xintercept = as.numeric(as.Date("2015-10-08"))),color="red") +
#         geom_vline(aes(xintercept = as.numeric(as.Date("2015-12-01"))),color="red") +
#         geom_vline(aes(xintercept = as.numeric(as.Date("2016-01-11"))),color="red") +
#         geom_vline(aes(xintercept = as.numeric(as.Date("2016-02-01"))),color="red") +
#         geom_vline(aes(xintercept = as.numeric(as.Date("2016-05-16"))),color="red") +
#         geom_vline(aes(xintercept = as.numeric(as.Date("2016-06-13"))),color="red") +    
#         ggtitle("RealSense Sum of duration - MARG")
# 
# ggsave(file = "plots/RealSense Sum of duration - MARG.png", width = 25, height = 10, dpi = 300, ppp)
# # 
# # 
# ggplot(hcc_SumOfDuration, aes(x=date, y=sum_of_duration))+
#         geom_bar(aes(fill=isWeekday(date)), stat = "identity") + 
#         geom_text(aes(label=round(sum_of_duration, 0)), position=position_dodge(width=0.9), vjust=-0.25) + 
#         
#         theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#         
#         scale_x_date("Timestamp", labels = date_format("%y-%m-%d"), breaks = date_breaks("week")) +
#         
#         geom_vline(aes(xintercept = as.numeric(as.Date("2015-10-08"))),color="red") +
#         geom_vline(aes(xintercept = as.numeric(as.Date("2015-12-01"))),color="red") +
#         geom_vline(aes(xintercept = as.numeric(as.Date("2016-01-11"))),color="red") +
#         geom_vline(aes(xintercept = as.numeric(as.Date("2016-02-01"))),color="red") +
#         geom_vline(aes(xintercept = as.numeric(as.Date("2016-05-16"))),color="red") +
#         geom_vline(aes(xintercept = as.numeric(as.Date("2016-06-13"))),color="red") +    
#         ggtitle("RealSense Sum of duration - HCC")
# 
# ggsave(file = "plots/RealSense Sum of duration - HCC.png", width = 25, height = 10, dpi = 300)
# 
# 
# ggplot(ux_SumOfDuration, aes(x=date, y=sum_of_duration))+
#         geom_bar(aes(fill=isWeekday(date)), stat = "identity") + 
#         geom_text(aes(label=round(sum_of_duration, 0)), position=position_dodge(width=0.9), vjust=-0.25) + 
#         
#         theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#         
#         scale_x_date("Timestamp", labels = date_format("%y-%m-%d"), breaks = date_breaks("week")) +
#         
#         geom_vline(aes(xintercept = as.numeric(as.Date("2015-10-08"))),color="red") +
#         geom_vline(aes(xintercept = as.numeric(as.Date("2015-12-01"))),color="red") +
#         geom_vline(aes(xintercept = as.numeric(as.Date("2016-01-11"))),color="red") +
#         geom_vline(aes(xintercept = as.numeric(as.Date("2016-02-01"))),color="red") +
#         geom_vline(aes(xintercept = as.numeric(as.Date("2016-05-16"))),color="red") +
#         geom_vline(aes(xintercept = as.numeric(as.Date("2016-06-13"))),color="red") +    
#         ggtitle("RealSense Sum of duration - UX")
# 
# ggsave(file = "plots/RealSense Sum of duration - UX.png", width = 25, height = 10, dpi = 300)
# 
# 
# 
# 
# 
# 

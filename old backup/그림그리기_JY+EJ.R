library(data.table)
library(ggplot2)
library(gridExtra)
library(timeDate)
library(zoo)
library(lubridate)
library(plyr)
library(scales)
library(reshape2)
library(data.table)
require(bit64)


## -------------------- ##
### data loading 
# load("Encored-Data-Analysis/marg_15min.RData")
# load("Encored-Data-Analysis/hcc_15min.RData")
# load("Encored-Data-Analysis/ux_15min.RData")

source("Encored-Data-Analysis/getSNUdata.R")
update_start = "2014-10-01"
update_end = "2016-7-11"

marg_defalut_table_15min = reviseSNUData(marg_defalut_table_15min, "marg", update_start, update_end, verbose = T)
hcc_defalut_table_15min = reviseSNUData( hcc_defalut_table_15min, "hcc",  update_start, update_end, verbose = T)
ux_defalut_table_15min = reviseSNUData(  ux_defalut_table_15min, "ux",   update_start, update_end, verbose = T)

# marg_defalut_table_hours = reviseSNUData(marg_defalut_table_hours, "marg", update_start, update_end, verbose = T)
# hcc_defalut_table_hours = reviseSNUData( hcc_defalut_table_hours, "hcc",  update_start, update_end, verbose = T)
# ux_defalut_table_hours = reviseSNUData(  ux_defalut_table_hours, "ux",   update_start, update_end, verbose = T)


## -------------------- ##
#### 기본 data.table 빌드 
marg_dt = data.table(marg_defalut_table_15min, index=rep(1:96, (nrow(marg_defalut_table_15min)/96)))
hcc_dt = data.table(hcc_defalut_table_15min, index=rep(1:96, (nrow(marg_defalut_table_15min)/96)))
ux_dt = data.table(ux_defalut_table_15min, index=rep(1:96, (nrow(marg_defalut_table_15min)/96)))

#### Seeking new date timming ==> 7am 
## 오전 7시를 기점으로 하루를 자르면 될 듯 (3개 랩 패턴 매우 유사함)
p1 = ggplot(marg_dt[, .(light_sum = sum(light)), by=index], aes(x=index, y=light_sum)) +
        geom_point() +
        scale_x_continuous(breaks = 1:96)
p2 = ggplot(hcc_dt[, .(light_sum = sum(light)), by=index], aes(x=index, y=light_sum)) +
        geom_point() +
        scale_x_continuous(breaks = 1:96)
p3 = ggplot(ux_dt[, .(light_sum = sum(light)), by=index], aes(x=index, y=light_sum)) +
        geom_point() +
        scale_x_continuous(breaks = 1:96)

grid.arrange(p1, p2, p3)

aggDay = c(rep((as.Date(marg_defalut_table_15min$timestamp[1], tz="rok")-1),28), 
           as.Date(marg_defalut_table_15min$timestamp, tz='rok')[29:(nrow(marg_defalut_table_15min))-28])

marg_dt = data.table(marg_dt, aggDay)
hcc_dt = data.table(hcc_dt, aggDay)
ux_dt = data.table(ux_dt, aggDay)

# cut the date depending on addDay & index update 
marg_dt = marg_dt[aggDay >= "2014-10-01" & aggDay <= "2016-6-30"]
hcc_dt = hcc_dt[aggDay >= "2014-10-01" & aggDay <= "2016-6-30"]
ux_dt = ux_dt[aggDay >= "2014-10-01" & aggDay <= "2016-6-30"]

marg_dt[, ':='(index=rep(1:96, nrow(marg_dt)/96))]
hcc_dt[, ':='(index=rep(1:96, nrow(marg_dt)/96))]
ux_dt[, ':='(index=rep(1:96, nrow(marg_dt)/96))]


### update : day, weekday depending on the aggDay
marg_dt[, ':='(day = weekdays(aggDay, abbreviate = T), weekday = isWeekday(aggDay))]
hcc_dt[, ':='(day = weekdays(aggDay, abbreviate = T), weekday = isWeekday(aggDay))]
ux_dt[, ':='(day = weekdays(aggDay, abbreviate = T), weekday = isWeekday(aggDay))]

## Add aggWeek column
marg_dt[, ':='(aggWeek=as.Date(cut(aggDay, breaks = "week", start.on.monday = T)))]
hcc_dt[, ':='(aggWeek=as.Date(cut(aggDay, breaks = "week", start.on.monday = T)))]
ux_dt[, ':='(aggWeek=as.Date(cut(aggDay, breaks = "week", start.on.monday = T)))]

# View(marg_dt)

## -------------------- ##
#### pre-processing  
## missing data : 컴퓨터기준, 15분 사용량 marg:0.1, hcc:0.05, ux:0.02 미만 시점은 NA처리 

na.missing.data <- function(dt, threshold_min){
        df = data.frame(dt)
        na_indexes = which(df$computer < threshold_min)
        print(df[na_indexes, c("timestamp", "computer", "light", "hvac", "etc", "total")])
        print(paste("missing lengths :", length(na_indexes)))
        df[na_indexes, c("computer", "light", "hvac", "etc", "total")] = NA
        return(data.table(df))        
}

{# 
# hist(marg_dt$computer, 100)
# hist( hcc_dt$computer, 100)
# hist(  ux_dt$computer, 100)
# 
# dt = na.omit(ux_dt)
# tmp = numeric(0)
# for(thre in seq(0, 1, by=0.1)){
#         print(paste(thre, sum(dt$computer < thre)))
#         tmp = c(tmp, sum(dt$computer < thre))
# }
# plot(tmp)
# diff(tmp)
}
marg_dt = na.missing.data(marg_dt, 0.1)
hcc_dt = na.missing.data(hcc_dt, 0.05)
ux_dt = na.missing.data(ux_dt, 0.02)

### Computer
## abnormal computer usage : 
## hcc : computer usage over 0.5 = NA
##  ux : computer usage over 0.35 = NA

hcc_dt[computer > 0.5, ':='(computer = NA)]
ux_dt[computer > 0.35, ':='(computer = NA)]

# check the distributions
par(mfrow=c(2,3))
hist(marg_dt$computer, 100)
hist(hcc_dt$computer, 100)
hist(ux_dt$computer, 100)

hist(marg_dt$total, 100)
hist(hcc_dt$total, 100)
hist(ux_dt$total, 100)
par(mfrow=c(1,1))


### Light 
## 
hist(marg_dt[light>0.01]$light, 100) # no problem
hist(hcc_dt[light>0.01]$light, 100)  # abnormal : over 0.5
hist(ux_dt[light>0.01]$light, 100)   # abnormal : over 0.5

hcc_dt[light > 0.5, ':='(light = NA)]
ux_dt[light > 0.5, ':='(light = NA)]

hist(marg_dt[light>0.01]$light, 100) # no problem
hist(hcc_dt[light>0.01]$light, 100)  # abnormal : over 0.5
hist(ux_dt[light>0.01]$light, 100)   # abnormal : over 0.5


### --------------------------- ###
#### RealSense
# source("Encored-Data-Analysis/그림그리기+리얼센스_EJ.R")

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

marg_RS[, ':='(aggDay=as.Date(joined-date_adjust_parameter, tz="rok"), weekday = isWeekday(joined-date_adjust_parameter))]
hcc_RS[, ':='(aggDay=as.Date(joined-date_adjust_parameter, tz="rok"), weekday = isWeekday(joined-date_adjust_parameter))]
ux_RS[, ':='(aggDay=as.Date(joined-date_adjust_parameter, tz="rok"), weekday = isWeekday(joined-date_adjust_parameter))]

# write.csv(marg_RS, file ="data/marg_RS.csv")
# write.csv(hcc_RS, file ="data/hcc_RS.csv")
# write.csv(ux_RS, file ="data/ux_RS.csv")


## vaild date 
marg_RS = marg_RS[aggDay > "2015-10-14" & aggDay <= "2016-6-30"]
hcc_RS = hcc_RS[aggDay > "2015-10-14" & aggDay <= "2016-6-30"]
ux_RS = ux_RS[aggDay > "2015-10-14" & aggDay <= "2016-6-30"]


## Validate the "duration"
marg_RS[duration > 5*60] # 4 --> Over 1000 case should be removed (single case : duration 1494.048)
marg_RS = marg_RS[duration < 1000]
hcc_RS[duration > 5*60]  # 48 --> All are in the abnormal period (will be removed) 
ux_RS[duration > 5*60]   # 3 --> All cases make sense 

hcc_RS <- hcc_RS[aggDay < "2015-12-11" | aggDay > "2015-12-30"]


# Sum of duration 
marg_SumOfDuration_day = marg_RS[, .(sum_of_duration = sum(duration)), by=aggDay]
hcc_SumOfDuration_day = hcc_RS[, .(sum_of_duration = sum(duration)), by=aggDay]
ux_SumOfDuration_day = ux_RS[, .(sum_of_duration = sum(duration)), by=aggDay]

marg_Freq_day = marg_RS[, .(freq = nrow(.SD)), by=aggDay]
hcc_Freq_day = hcc_RS[, .(freq = nrow(.SD)), by=aggDay]
ux_Freq_day = ux_RS[, .(freq = nrow(.SD)), by=aggDay]


# set max sum_of_duration as 600
max_sum_of_duration = 600
marg_SumOfDuration_day[sum_of_duration > max_sum_of_duration]
hcc_SumOfDuration_day[sum_of_duration > max_sum_of_duration]
ux_SumOfDuration_day[sum_of_duration > max_sum_of_duration]

marg_SumOfDuration_day[sum_of_duration > max_sum_of_duration, ':='(sum_of_duration = max_sum_of_duration)]
hcc_SumOfDuration_day[sum_of_duration > max_sum_of_duration, ':='(sum_of_duration = max_sum_of_duration)]
ux_SumOfDuration_day[sum_of_duration > max_sum_of_duration, ':='(sum_of_duration = max_sum_of_duration)]

# hist(marg_Freq$freq, 100)
# hist(hcc_Freq$freq, 100)
# hist(ux_Freq$freq, 100)
# 

marg_Freq_day[freq > 100]
hcc_Freq_day[freq > 100]
ux_Freq_day[freq > 100]

# set max freq as 300 
marg_Freq_day[freq > 300, ':='(freq = 300)]
hcc_Freq_day[freq > 300, ':='(freq = 300)]
ux_Freq_day[freq > 300, ':='(freq = 300)]


datetime.all <- data.table(marg_dt$aggDay)
colnames(datetime.all) <- c("aggDay")
datetime.all[, ':='(sum_of_duration = NA)][, ':='(freq = NA)]
datetime.all[, sum_of_duration := as.numeric(sum_of_duration)]
datetime.all[, freq := as.numeric(freq)]

margRS_dt <- datetime.all
margRS_dt[which(aggDay >= as.Date(marg_RS$joined[1],format="%y-%m-%d") & aggDay <= "2016-6-30"), ]$sum_of_duration = 0
margRS_dt[which(aggDay >= as.Date(marg_RS$joined[1],format="%y-%m-%d") & aggDay <= "2016-6-30"), ]$freq = 0
uniqueDate = unique(marg_SumOfDuration_day$aggDay)

for(i in 1:length(uniqueDate)){
        margRS_dt[min(which(margRS_dt$aggDay == uniqueDate[i]))]$sum_of_duration = marg_SumOfDuration_day[marg_SumOfDuration_day[,marg_SumOfDuration_day$aggDay==uniqueDate[i]],]$sum_of_duration
        margRS_dt[min(which(margRS_dt$aggDay == uniqueDate[i]))]$freq = marg_Freq_day[marg_Freq_day[,marg_Freq_day$aggDay==uniqueDate[i]],]$freq
}

hccRS_dt <- datetime.all
hccRS_dt[which(aggDay >= as.Date(hcc_RS$joined[1],format="%y-%m-%d") & aggDay <= "2016-6-30"), ]$sum_of_duration = 0
hccRS_dt[which(aggDay >= as.Date(hcc_RS$joined[1],format="%y-%m-%d") & aggDay <= "2016-6-30"), ]$freq = 0
uniqueDate = unique(hcc_SumOfDuration_day$aggDay)
for(i in 1:length(uniqueDate)){
        hccRS_dt[min(which(hccRS_dt$aggDay == uniqueDate[i]))]$sum_of_duration = hcc_SumOfDuration_day[hcc_SumOfDuration_day[,hcc_SumOfDuration_day$aggDay==uniqueDate[i]],]$sum_of_duration
        hccRS_dt[min(which(hccRS_dt$aggDay == uniqueDate[i]))]$freq = hcc_Freq_day[hcc_Freq_day[,hcc_Freq_day$aggDay==uniqueDate[i]],]$freq
}

uxRS_dt <- datetime.all
uxRS_dt[which(aggDay >= as.Date(ux_RS$joined[1],format="%y-%m-%d") & aggDay <= "2016-6-30"), ]$sum_of_duration = 0
uxRS_dt[which(aggDay >= as.Date(ux_RS$joined[1],format="%y-%m-%d") & aggDay <= "2016-6-30"), ]$freq = 0
uniqueDate = unique(ux_SumOfDuration_day$aggDay)
for(i in 1:length(uniqueDate)){
        uxRS_dt[min(which(uxRS_dt$aggDay == uniqueDate[i]))]$sum_of_duration = ux_SumOfDuration_day[ux_SumOfDuration_day[,ux_SumOfDuration_day$aggDay==uniqueDate[i]],]$sum_of_duration
        uxRS_dt[min(which(uxRS_dt$aggDay == uniqueDate[i]))]$freq = ux_Freq_day[ux_Freq_day[,ux_Freq_day$aggDay==uniqueDate[i]],]$freq
}




marg_dt[,sum_of_duration := margRS_dt$sum_of_duration][,freq := margRS_dt$freq]
hcc_dt[,sum_of_duration := hccRS_dt$sum_of_duration][,freq := hccRS_dt$freq]
ux_dt[,sum_of_duration := uxRS_dt$sum_of_duration][,freq := uxRS_dt$freq]


### Build AllLabs data.table
##
AllLabs_dt = marg_dt
AllLabs_dt$computer = AllLabs_dt$computer + hcc_dt$computer + ux_dt$computer
AllLabs_dt$light = AllLabs_dt$light + hcc_dt$light + ux_dt$light
AllLabs_dt$etc = AllLabs_dt$etc + hcc_dt$etc + ux_dt$etc
AllLabs_dt$total = AllLabs_dt$total + hcc_dt$total + ux_dt$total
AllLabs_dt$sum_of_duration = AllLabs_dt$sum_of_duration + hcc_dt$sum_of_duration + ux_dt$sum_of_duration
AllLabs_dt$freq = AllLabs_dt$freq + hcc_dt$freq + ux_dt$freq

### --------------------------- ###
### Build tables and functions for extraction & aggregation

return_dts = list(0)

## four stat : peak, base, avg, mid --> c(1,2,3,4)
get.four.stats <- function(usage, type){
        peak = quantile(usage, .95, na.rm = T)
        base = quantile(usage, .05, na.rm = T)
        avg  = mean(usage, na.rm = T)
        med  = median(usage, na.rm = T)
        
        result=c(peak, base, avg, med)
        return(result[type])
}

filter.fault.partial.lightOn <- function(input){
        light = na.locf(input)
        
        for(i in 2:(length(light)-1)){
                
                if(sum(light[(i-1):(i+1)]) == 1){
                        light[i] = 0
                }
        }
        return(light)
}

# 1. lab
lab_names = c("MARG", "HCC", "UX", "All Labs")
dt_list = list(marg_dt, hcc_dt, ux_dt, AllLabs_dt)

# 2. aggregation unit 
agg_Units = c("aggWeek", "aggDay")

# 3. day selection
day_selections = c("allDay", "weekDay", "weekEnd")

# 4. feeders
feeders = c("total", "computer", "light", "hvac")

for(lab in 1:4){ 
        # lab_dt & name selection 
        lab_dt = dt_list[[lab]]
        lab_name = lab_names[lab]
        
        for(agg_Unit in agg_Units){
                # aggregation unit selection
                # agg_Units = c("aggWeek", "aggDay")
                # get(agg_Unit)
                
                for(day_selection in day_selections){
                        
                        # day selection
                        # print(day_selection)
                        
                        for(feeder in feeders){
                                # feeder selection
                                # get(feeder)
                                
                                dt_name = paste(lab_name, agg_Unit, day_selection, feeder, sep="_")
                                print(dt_name)
                                
                                if(day_selection == "allDay"){
                                        
                                        return_dt = lab_dt[, .(peak = get.four.stats(get(feeder), 1),
                                                               base = get.four.stats(get(feeder), 2),
                                                               avg  = get.four.stats(get(feeder), 3),
                                                               med  = get.four.stats(get(feeder), 4),
                                                               sum_of_duration = sum(sum_of_duration), freq = sum(freq)), by=get(agg_Unit)]
                                        
                                } else if(day_selection == "weekDay") {
                                        
                                        return_dt = lab_dt[weekday == T, .(peak = get.four.stats(get(feeder), 1),
                                                                           base = get.four.stats(get(feeder), 2),
                                                                           avg  = get.four.stats(get(feeder), 3),
                                                                           med  = get.four.stats(get(feeder), 4),
                                                                           sum_of_duration = sum(sum_of_duration), freq = sum(freq)), by=get(agg_Unit)]
                                        
                                } else if(day_selection == "weekEnd") {
                                        
                                        return_dt = lab_dt[weekday == F, .(peak = get.four.stats(get(feeder), 1),
                                                                           base = get.four.stats(get(feeder), 2),
                                                                           avg  = get.four.stats(get(feeder), 3),
                                                                           med  = get.four.stats(get(feeder), 4),
                                                                           sum_of_duration = sum(sum_of_duration), freq = sum(freq)), by=get(agg_Unit)]
                                }
                                
                                light_min = 0.01
                                light_peak = quantile(lab_dt$light, .95, na.rm = T)
                                light_dt = lab_dt[, .(timestamp = timestamp,
                                                      aggDay = aggDay,
                                                      aggWeek = aggWeek,
                                                      light = na.locf(light),
                                                      lightON = ifelse(na.locf(light) > light_min, 1, 0),
                                                      peak_50 = filter.fault.partial.lightOn(ifelse((na.locf(light) < light_peak * 0.5) & (na.locf(light) > light_min), 1, 0)),
                                                      peak_60 = filter.fault.partial.lightOn(ifelse((na.locf(light) < light_peak * 0.6) & (na.locf(light) > light_min), 1, 0)),
                                                      peak_70 = filter.fault.partial.lightOn(ifelse((na.locf(light) < light_peak * 0.7) & (na.locf(light) > light_min), 1, 0)),
                                                      peak_80 = filter.fault.partial.lightOn(ifelse((na.locf(light) < light_peak * 0.8) & (na.locf(light) > light_min), 1, 0)),
                                                      peak_90 = filter.fault.partial.lightOn(ifelse((na.locf(light) < light_peak * 0.9) & (na.locf(light) > light_min), 1, 0)))]
                                
                                partial_light = light_dt[, .(lightON = sum(lightON),
                                                             threshold50 = sum(peak_50)/sum(lightON),
                                                             threshold60 = sum(peak_60)/sum(lightON),
                                                             threshold70 = sum(peak_70)/sum(lightON),
                                                             threshold80 = sum(peak_80)/sum(lightON),
                                                             threshold90 = sum(peak_90)/sum(lightON)), by=get(agg_Unit)]
                                
                                return_dt = merge(return_dt, partial_light, by="get")
                                
                                assign(dt_name, return_dt)
                                return_dts = append(return_dts, setNames(list(return_dt),dt_name))
                        }
                }
        }
}


## check the returns 
# return_dts[15]
# marg_dt[, .(peak = get.four.stats(computer, 1),
#             base = get.four.stats(computer, 2),
#             avg  = get.four.stats(computer, 3),
#             med  = get.four.stats(computer, 4)), by=aggDay]


### ------------------------------------------- ###
### Plotting

add.event.vline <- function(plot_body){
        result = plot_body + 
                scale_x_date("Timestamp", labels = date_format("%y-%m"), breaks = date_breaks("month")) +
                
                geom_vline(aes(xintercept = as.numeric(as.Date("2015-10-08"))),color="green4") +
                geom_vline(aes(xintercept = as.numeric(as.Date("2015-12-01"))),color="green4") +
                geom_vline(aes(xintercept = as.numeric(as.Date("2016-01-11"))),color="green4") +
                geom_vline(aes(xintercept = as.numeric(as.Date("2016-02-01"))),color="green4") +
                geom_vline(aes(xintercept = as.numeric(as.Date("2016-05-16"))),color="green4") +
                geom_vline(aes(xintercept = as.numeric(as.Date("2016-06-13"))),color="green4") +         
                theme(legend.position = "bottom")
        
        return(result)
}


## 1. Four stats plots  +  partial_lightON & lightON duration plots 

for(i in 2:(length(return_dts))){
        # (length(return_dts)) 
        plot_dt   = return_dts[[i]]
        # plot_name = names(return_dts[i])
        # plot_name = paste0(names(return_dts[i]), "_partial_lightON")
        
        for(s in seq(0.1, 0.5, 0.1)) {
                      
                plot_name = paste(names(return_dts[i]), "- span", s)  
                print(plot_name)
                
                stats <- ggplot(plot_dt, aes(x=get)) +
                        geom_point(aes(y=peak, color='peak')) +
                        geom_smooth(aes(y=peak, color='peak'), span = s) +
                        
                        geom_point(aes(y=base, color='base')) +
                        geom_smooth(aes(y=base, color='base'), span = s) +
                        
                        geom_point(aes(y=avg, color='avg')) +
                        geom_smooth(aes(y=avg, color='avg'), span = s) +
                        
                        geom_point(aes(y=med, color='med')) +
                        geom_smooth(aes(y=med, color='med'), span = s) +
                        
                        ggtitle(plot_name)
                
                if(strsplit(plot_name,"_")[[1]][2] == "aggWeek"){
                  ylim_RS_duration <- 2000
                  ylim_RS_freq <- 1000
                } else{
                  ylim_RS_duration <- 600
                  ylim_RS_freq <- 300
                }    
                  
                RS_duration <- ggplot(plot_dt, aes(x=get, y=sum_of_duration, ymax=ylim_RS_duration))+
                              geom_point(aes(y=sum_of_duration), colour="darkcyan") +
                              geom_smooth(method = "auto", color="darkcyan", span = s) +
                              geom_text(aes(label=round(sum_of_duration, 0)), position=position_dodge(width=0.9), vjust=-0.25, colour="grey50") + 
                              # scale_x_date("Timestamp", labels = date_format("%y-%m"), breaks = date_breaks("month")) +
                              scale_y_continuous(limits=c(0,ylim_RS_duration), oob=rescale_none) +
                              ggtitle("RealSense Sum of duration")
                      
                RS_freq <- ggplot(plot_dt, aes(x=get, y=freq, ymax=ylim_RS_freq))+
                          geom_point(aes(y=freq), colour="darkcyan") +
                          geom_smooth(method = "auto", colour="darkcyan", span = s) +
                          geom_text(aes(label=round(freq, 0)), position=position_dodge(width=0.9), vjust=-0.25, colour="grey50") + 
#                           scale_x_date("Timestamp", labels = date_format("%y-%m"), breaks = date_breaks("month")) +
                          scale_y_continuous(limits=c(0,ylim_RS_freq), oob=rescale_none) +
                          ggtitle("RealSense Freq")
                
                if(grepl("light", plot_name) | grepl("total", plot_name)){
        
                        partial_lightON <- ggplot(plot_dt, aes(x=get)) +
                                
                                geom_point(aes(y=threshold50, color='threshold50')) +
                                geom_smooth(aes(y=threshold50, color='threshold50'), span = s) +    
                                
                                geom_point(aes(y=threshold60, color='threshold60')) +
                                geom_smooth(aes(y=threshold60, color='threshold60'), span = s) +             
                                
                                geom_point(aes(y=threshold70, color='threshold70')) +
                                geom_smooth(aes(y=threshold70, color='threshold70'), span = s) +             
                                
                                geom_point(aes(y=threshold80, color='threshold80')) +
                                geom_smooth(aes(y=threshold80, color='threshold80'), span = s) +  
                                
                                geom_point(aes(y=threshold90, color='threshold90')) +
                                geom_smooth(aes(y=threshold90, color='threshold90'), span = s) +  
                                
                                ggtitle("partial_lightON")
                        
                        lightON_duration <- ggplot(plot_dt, aes(x=get, y=lightON)) +
                                
                                geom_point(aes(y=lightON, color='lightON')) +
                                geom_smooth(aes(y=lightON, color='lightON'), span = s) +          
                                
                                ggtitle("lightON duration")
                        
                        
                        stats            = add.event.vline(stats)
                        partial_lightON  = add.event.vline(partial_lightON)
                        lightON_duration = add.event.vline(lightON_duration)
                        RS_duration      = add.event.vline(RS_duration)
                        RS_freq          = add.event.vline(RS_freq)
                        
                        plots <- arrangeGrob(stats, partial_lightON, lightON_duration, RS_duration, RS_freq, ncol=1)
                        ggsave(file = paste0("plots/",plot_name, ".png"), width = 20, height = 50, dpi = 300, plots, limitsize=FALSE)
                        
                } else {
                        
                        stats            = add.event.vline(stats)
                        RS_duration      = add.event.vline(RS_duration)
                        RS_freq          = add.event.vline(RS_freq)
                        
                        plots <- arrangeGrob(stats, RS_duration, RS_freq, ncol=1)
                        ggsave(file = paste0("plots/",plot_name, ".png"), width = 20, height = 30, dpi = 300, plots)
                }
        }
}


###
## 2. lunch light OFF plots --> 상단 1번 과정 안에 통합되어야 함 

get.lunch.lightOFF <- function(dt, threshold){
        # print(dt)
        
        before_lunch = max(dt$light[17:20], na.rm = T) # 11:00 ~ 12:00
        during_lunch = min(dt$light[19:26], na.rm = T) # 11:30 ~ 13:30 
        after_luhch = max(dt$light[25:28], na.rm = T) # 13:00 ~ 14:00 
        
        # print(paste(before_lunch, during_lunch, after_luhch))
        
        if(during_lunch < before_lunch * threshold & 
           during_lunch <  after_luhch * threshold){
                return(1)
        } else {
                return(0)
        }
}

for(lab in 1:4){ 
        # lab_dt & name selection 
        lab_dt = dt_list[[lab]]
        lab_name = lab_names[lab]
        
        lunch_dt_aggDay <- lab_dt[, .(aggWeek=as.Date(cut(aggDay, breaks = "week", start.on.monday = T)),
                                      lunch_lightOFF_50 = get.lunch.lightOFF(.SD, 0.5),
                                      lunch_lightOFF_60 = get.lunch.lightOFF(.SD, 0.6),
                                      lunch_lightOFF_70 = get.lunch.lightOFF(.SD, 0.7),
                                      lunch_lightOFF_80 = get.lunch.lightOFF(.SD, 0.8),
                                      lunch_lightOFF_90 = get.lunch.lightOFF(.SD, 0.9)), by=aggDay]
        
        lunch_dt_aggWeek <- lunch_dt_aggDay[, .(lunch_lightOFF_50 = sum(lunch_lightOFF_50),
                                                lunch_lightOFF_60 = sum(lunch_lightOFF_60),
                                                lunch_lightOFF_70 = sum(lunch_lightOFF_70),
                                                lunch_lightOFF_80 = sum(lunch_lightOFF_80),
                                                lunch_lightOFF_90 = sum(lunch_lightOFF_90)), by=aggWeek]
        
        for(s in seq(0.1, 0.5, 0.1)) {
                
                plot_name = paste(lab_name, "lunch light OFF count - span", s)  
                print(plot_name)
                
                p1 <- ggplot(lunch_dt_aggDay, aes(x=aggDay)) +
                        
                        geom_point(aes(y=lunch_lightOFF_50, color='lunch_lightOFF_50')) +
                        geom_smooth(aes(y=lunch_lightOFF_50, color='lunch_lightOFF_50'), span = s) +    
                        
                        geom_point(aes(y=lunch_lightOFF_60, color='lunch_lightOFF_60')) +
                        geom_smooth(aes(y=lunch_lightOFF_60, color='lunch_lightOFF_60'), span = s) +             
                        
                        geom_point(aes(y=lunch_lightOFF_70, color='lunch_lightOFF_70')) +
                        geom_smooth(aes(y=lunch_lightOFF_70, color='lunch_lightOFF_70'), span = s) +             
                        
                        geom_point(aes(y=lunch_lightOFF_80, color='lunch_lightOFF_80')) +
                        geom_smooth(aes(y=lunch_lightOFF_80, color='lunch_lightOFF_80'), span = s) +   
                        
                        geom_point(aes(y=lunch_lightOFF_90, color='lunch_lightOFF_90')) +
                        geom_smooth(aes(y=lunch_lightOFF_90, color='lunch_lightOFF_90'), span = s) +   
                        
                        ggtitle(paste(plot_name, "aggDay"))
                
                p2 <- ggplot(lunch_dt_aggWeek, aes(x=aggWeek)) +
                        
                        geom_point(aes(y=lunch_lightOFF_50, color='lunch_lightOFF_50')) +
                        geom_smooth(aes(y=lunch_lightOFF_50, color='lunch_lightOFF_50'), span = s) +    
                        
                        geom_point(aes(y=lunch_lightOFF_60, color='lunch_lightOFF_60')) +
                        geom_smooth(aes(y=lunch_lightOFF_60, color='lunch_lightOFF_60'), span = s) +             
                        
                        geom_point(aes(y=lunch_lightOFF_70, color='lunch_lightOFF_70')) +
                        geom_smooth(aes(y=lunch_lightOFF_70, color='lunch_lightOFF_70'), span = s) +             
                        
                        geom_point(aes(y=lunch_lightOFF_80, color='lunch_lightOFF_80')) +
                        geom_smooth(aes(y=lunch_lightOFF_80, color='lunch_lightOFF_80'), span = s) +   
                        
                        geom_point(aes(y=lunch_lightOFF_90, color='lunch_lightOFF_90')) +
                        geom_smooth(aes(y=lunch_lightOFF_90, color='lunch_lightOFF_90'), span = s) +   
                        
                        ggtitle(paste(plot_name, "aggWeek"))

                
                plots <- arrangeGrob(p1, p2)
                
                ggsave(file = paste0("plots/", plot_name, ".png"), width = 20, height = 20, dpi = 600, plots)
        }
}

### 
## 3. strong_light counting light 
## ==> # of over "peak * (0.5~0.8)" in 15mins 

for(lab in 1:4){ 
        # lab_dt & name selection 
        lab_dt = dt_list[[lab]]
        lab_name = lab_names[lab]
        
        print(lab_name)
        
        light_peak = quantile(lab_dt$light, .95, na.rm = T)
        print(light_peak)
        
        strong_light_aggDay_dt = lab_dt[, .(strong_light_50 = sum(light > (light_peak * 0.5)),
                                            strong_light_60 = sum(light > (light_peak * 0.6)),
                                            strong_light_70 = sum(light > (light_peak * 0.7)),
                                            strong_light_80 = sum(light > (light_peak * 0.8)),
                                            strong_light_90 = sum(light > (light_peak * 0.9))), by=aggDay]
        
        strong_light_aggWeek_dt = lab_dt[, .(strong_light_50 = sum(light > (light_peak * 0.5)),
                                             strong_light_60 = sum(light > (light_peak * 0.6)),
                                             strong_light_70 = sum(light > (light_peak * 0.7)),
                                             strong_light_80 = sum(light > (light_peak * 0.8)),
                                             strong_light_90 = sum(light > (light_peak * 0.9))), by=aggWeek]
        
        for(s in seq(0.1, 0.5, 0.1)) {
                
                plot_name = paste(lab_name, "strong_light count - span", s)  
                print(plot_name)
                
                p1 <- ggplot(strong_light_aggDay_dt, aes(x=aggDay)) +
                        
                        geom_point(aes(y=strong_light_50, color='strong_light_50')) +
                        geom_smooth(aes(y=strong_light_50, color='strong_light_50'), span = s) +    
                        
                        geom_point(aes(y=strong_light_60, color='strong_light_60')) +
                        geom_smooth(aes(y=strong_light_60, color='strong_light_60'), span = s) +             
                        
                        geom_point(aes(y=strong_light_70, color='strong_light_70')) +
                        geom_smooth(aes(y=strong_light_70, color='strong_light_70'), span = s) +             
                        
                        geom_point(aes(y=strong_light_80, color='strong_light_80')) +
                        geom_smooth(aes(y=strong_light_80, color='strong_light_80'), span = s) +    
                        
                        geom_point(aes(y=strong_light_90, color='strong_light_90')) +
                        geom_smooth(aes(y=strong_light_90, color='strong_light_90'), span = s) +    

                        ggtitle(paste(plot_name, "aggDay"))
                
                p2 <- ggplot(strong_light_aggWeek_dt, aes(x=aggWeek)) +
                        
                        geom_point(aes(y=strong_light_50, color='strong_light_50')) +
                        geom_smooth(aes(y=strong_light_50, color='strong_light_50'), span = s) +    
                        
                        geom_point(aes(y=strong_light_60, color='strong_light_60')) +
                        geom_smooth(aes(y=strong_light_60, color='strong_light_60'), span = s) +             
                        
                        geom_point(aes(y=strong_light_70, color='strong_light_70')) +
                        geom_smooth(aes(y=strong_light_70, color='strong_light_70'), span = s) +             
                        
                        geom_point(aes(y=strong_light_80, color='strong_light_80')) +
                        geom_smooth(aes(y=strong_light_80, color='strong_light_80'), span = s) +    
                        
                        geom_point(aes(y=strong_light_90, color='strong_light_90')) +
                        geom_smooth(aes(y=strong_light_90, color='strong_light_90'), span = s) +    

                        ggtitle(paste(plot_name, "aggWeek"))
                
                
                p1 = add.event.vline(p1)
                p2 = add.event.vline(p2)
                
                plots <- arrangeGrob(p1, p2)
                
                ggsave(file = paste0("plots/", plot_name, ".png"), width = 20, height = 20, dpi = 600, plots)
        }
}


### 
## 4. full_lightON counting : 24hours 
## ==> # of "over peak*(0.5~0.8)" == 96?

for(lab in 1:4){ 
        # lab_dt & name selection 
        lab_dt = dt_list[[lab]]
        lab_name = lab_names[lab]
        
        print(lab_name)
        
        light_peak = quantile(lab_dt$light, .95, na.rm = T)
        print(light_peak)
        
        full_lightON_aggDay_dt = lab_dt[, .(full_lightON_50 = sum(light > (light_peak * 0.5), na.rm = T)==96,
                                            full_lightON_40 = sum(light > (light_peak * 0.4), na.rm = T)==96,
                                            full_lightON_30 = sum(light > (light_peak * 0.3), na.rm = T)==96,
                                            full_lightON_20 = sum(light > (light_peak * 0.2), na.rm = T)==96,
                                            full_lightON_10 = sum(light > (light_peak * 0.1), na.rm = T)==96), by=aggDay]
        
        full_lightON_aggWeek_dt = full_lightON_aggDay_dt[, .(full_lightON_50 = sum(full_lightON_50, na.rm = T),
                                                             full_lightON_40 = sum(full_lightON_40, na.rm = T),
                                                             full_lightON_30 = sum(full_lightON_30, na.rm = T),
                                                             full_lightON_20 = sum(full_lightON_20, na.rm = T),
                                                             full_lightON_10 = sum(full_lightON_10, na.rm = T)), by=.(aggWeek=as.Date(cut(aggDay, breaks = "week", start.on.monday = T)))]
        
        for(s in seq(0.1, 0.5, 0.1)) {
                
                plot_name = paste(lab_name, "full(24hrs)_lightON count - span", s)  
                print(plot_name)
                
                p <- ggplot(full_lightON_aggWeek_dt, aes(x=aggWeek)) +
                        
                        #                         geom_point(aes(y=full_lightON_50, color='full_lightON_50')) +
                        #                         geom_smooth(aes(y=full_lightON_50, color='full_lightON_50'), span = s) +    
                        #                         
                        #                         geom_point(aes(y=full_lightON_40, color='full_lightON_40')) +
                        #                         geom_smooth(aes(y=full_lightON_40, color='full_lightON_40'), span = s) +             
                        
                        geom_point(aes(y=full_lightON_30, color='full_lightON_30')) +
                        geom_smooth(aes(y=full_lightON_30, color='full_lightON_30'), span = s) +             
                        
                        geom_point(aes(y=full_lightON_20, color='full_lightON_20')) +
                        geom_smooth(aes(y=full_lightON_20, color='full_lightON_20'), span = s) +    
                        
                        geom_point(aes(y=full_lightON_10, color='full_lightON_10')) +
                        geom_smooth(aes(y=full_lightON_10, color='full_lightON_10'), span = s) +  
                        
                        ggtitle(paste(plot_name, "aggWeek"))
                
                p = add.event.vline(p)
                
                ggsave(file = paste0("plots/", plot_name, ".png"), width = 20, height = 10, dpi = 600, p)
        }
}
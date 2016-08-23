# library(data.table)
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
library(gtable)
library(gridExtra)
library(scales)


## 아래처럼 15분 데이터 전처리 
## NA --> na.locf()로 채우고  
## total 갱신
## no-hvac total column 추가 

## server issue 시간대 replacement 

## 이후 과정은 그대로~ 

marg_dt = data.table(marg_defalut_table_15min, index=rep(1:96, (nrow(marg_defalut_table_15min)/96)))
hcc_dt = data.table(hcc_defalut_table_15min, index=rep(1:96, (nrow(marg_defalut_table_15min)/96)))
ux_dt = data.table(ux_defalut_table_15min, index=rep(1:96, (nrow(marg_defalut_table_15min)/96)))

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
## abnormal computer usage (HVAC usage at computer feeder)
## hcc : computer usage over 0.6 = NA
##  ux : computer usage over 0.4 = NA

hcc_dt[computer > 0.6, ':='(computer = NA)]
ux_dt[computer > 0.4, ':='(computer = NA)]


### Light 
## 
marg_dt[light > 0.5, ':='(light = NA)]
hcc_dt[light > 0.5, ':='(light = NA)]
ux_dt[light > 0.5, ':='(light = NA)]

# hist(marg_dt[light>0.01]$light, 100) # no problem
# hist(hcc_dt[light>0.01]$light, 100)  # abnormal : over 0.5
# hist(ux_dt[light>0.01]$light, 100)   # abnormal : over 0.5

# check the distributions
par(mfrow=c(3,3))
hist(marg_dt$computer, 100)
hist(hcc_dt$computer, 100)
hist(ux_dt$computer, 100)

hist(marg_dt$light, 100)
hist(hcc_dt$light, 100)
hist(ux_dt$light, 100)

hist(marg_dt$total, 100)
hist(hcc_dt$total, 100)
hist(ux_dt$total, 100)
par(mfrow=c(1,1))



## Replacement 
marg_dt[timestamp >= "2015-01-18 11:00" & timestamp <= "2015-01-19 10:45"]$computer <- marg_dt[timestamp >= "2015-02-01 11:00" & timestamp <= "2015-02-02 10:45"]$computer
marg_dt[timestamp >= "2015-01-18 11:00" & timestamp <= "2015-01-19 10:45"]$light <- marg_dt[timestamp >= "2015-02-01 11:00" & timestamp <= "2015-02-02 10:45"]$light
marg_dt[timestamp >= "2015-01-18 11:00" & timestamp <= "2015-01-19 10:45"]$hvac <- marg_dt[timestamp >= "2015-02-01 11:00" & timestamp <= "2015-02-02 10:45"]$hvac
marg_dt[timestamp >= "2015-01-18 11:00" & timestamp <= "2015-01-19 10:45"]$etc <- marg_dt[timestamp >= "2015-02-01 11:00" & timestamp <= "2015-02-02 10:45"]$etc
marg_dt[timestamp >= "2015-01-18 11:00" & timestamp <= "2015-01-19 10:45"]$total <- marg_dt[timestamp >= "2015-02-01 11:00" & timestamp <= "2015-02-02 10:45"]$total

marg_dt[timestamp >= "2015-01-18 11:00" & timestamp <= "2015-01-19 10:45"]
marg_dt[timestamp >= "2015-02-01 11:00" & timestamp <= "2015-02-02 10:45"]





aggDay = c(rep((as.Date(marg_defalut_table_15min$timestamp[1], tz="rok")-1),28), 
           as.Date(marg_defalut_table_15min$timestamp, tz='rok')[29:(nrow(marg_defalut_table_15min))-28])

marg_dt = data.table(marg_dt, aggDay)
hcc_dt = data.table(hcc_dt, aggDay)
ux_dt = data.table(ux_dt, aggDay)

# cut the date depending on addDay & index update 
marg_dt = marg_dt[aggDay >= "2014-10-01" & aggDay <= as.Date(update_end)-2]
hcc_dt = hcc_dt[aggDay >= "2014-10-01" & aggDay <= as.Date(update_end)-2]
ux_dt = ux_dt[aggDay >= "2014-10-01" & aggDay <= as.Date(update_end)-2]

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


fill.na <- function(dt){
  dt$computer = na.locf(dt$computer)
  dt$light = na.locf(dt$light)
  dt$hvac = na.locf(dt$hvac)
  dt$etc = na.locf(dt$etc)
  dt$total = na.locf(dt$total)

  return(dt)  
}

marg_dt = fill.na(marg_dt)
hcc_dt = fill.na(hcc_dt)
ux_dt = fill.na(ux_dt)

summary(marg_dt)
summary(hcc_dt)
summary(ux_dt)





## build day table (7am - 7am)

marg_day = marg_dt[, .(computer = sum(computer),
                       light = sum(light),
                       hvac = sum(hvac),
                       etc = sum(etc),
                       total = sum(total)), by=aggDay]


marg_day[, ':='(day = weekdays(aggDay, abbreviate = T), weekday = isWeekday(aggDay))]

marg_day[, ':='(total_noHVAC = total-hvac)]

marg_day[year(aggDay)==2014 & month(aggDay)==10, 
         lapply(.SD, mean, na.rm=TRUE), by=weekday, .SDcols=c("computer", "light", "total_noHVAC") ] 

marg_day[year(aggDay)==2015 & month(aggDay)==1 & (day(aggDay)>14) & (day(aggDay)<22)]




### --------------------------- ###
#### RealSense

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
RS_marg_raw = fread("../realsense/marg.csv")
RS_hcc_raw = fread("../realsense/hcc.csv")
RS_ux_raw = fread("../realsense/ux.csv")

marg_RS = table.time2string(RS_marg_raw)
hcc_RS = table.time2string(RS_hcc_raw)
ux_RS = table.time2string(RS_ux_raw)

date_adjust_parameter = 7 * 60 * 60 # 7 hours 

marg_RS[, ':='(aggDay=as.Date(joined-date_adjust_parameter, tz="rok"), weekday = isWeekday(joined-date_adjust_parameter))]
hcc_RS[, ':='(aggDay=as.Date(joined-date_adjust_parameter, tz="rok"), weekday = isWeekday(joined-date_adjust_parameter))]
ux_RS[, ':='(aggDay=as.Date(joined-date_adjust_parameter, tz="rok"), weekday = isWeekday(joined-date_adjust_parameter))]

## vaild date 
marg_RS = marg_RS[aggDay > "2015-10-14" & aggDay <= "2016-07-01"]
hcc_RS = hcc_RS[aggDay > "2015-10-14" & aggDay <= "2016-07-01"]
ux_RS = ux_RS[aggDay > "2015-10-14" & aggDay <= "2016-07-01"]


## Validate the "duration"
# marg_RS[duration > 5*60] # 4 --> Over 1000 case should be removed (single case : duration 1494.048)
marg_RS = marg_RS[duration < 1000]
# hcc_RS[duration > 5*60]  # 48 --> All are in the abnormal period (will be removed) 
# ux_RS[duration > 5*60]   # 3 --> All cases make sense 

hcc_RS <- hcc_RS[aggDay < "2015-12-11" | aggDay > "2015-12-30"]

# # Sum of duration 
# marg_SumOfDuration_day = marg_RS[, .(sum_of_duration = sum(duration)), by=aggDay]
# hcc_SumOfDuration_day = hcc_RS[, .(sum_of_duration = sum(duration)), by=aggDay]
# ux_SumOfDuration_day = ux_RS[, .(sum_of_duration = sum(duration)), by=aggDay]
# 
# marg_Freq_day = marg_RS[, .(freq = nrow(.SD)), by=aggDay]
# hcc_Freq_day = hcc_RS[, .(freq = nrow(.SD)), by=aggDay]
# ux_Freq_day = ux_RS[, .(freq = nrow(.SD)), by=aggDay]

##frequency cut
marg_SumOfDuration_day = marg_RS[duration>1, .(sum_of_duration = sum(duration)), by=aggDay]
hcc_SumOfDuration_day = hcc_RS[duration>1, .(sum_of_duration = sum(duration)), by=aggDay]
ux_SumOfDuration_day = ux_RS[duration>1, .(sum_of_duration = sum(duration)), by=aggDay]

marg_Freq_day = marg_RS[duration>1, .(freq = nrow(.SD)), by=aggDay]
hcc_Freq_day = hcc_RS[duration>1, .(freq = nrow(.SD)), by=aggDay]
ux_Freq_day = ux_RS[duration>1, .(freq = nrow(.SD)), by=aggDay]


# set max sum_of_duration as 600
max_sum_of_duration = 600
marg_SumOfDuration_day[sum_of_duration > max_sum_of_duration]
hcc_SumOfDuration_day[sum_of_duration > max_sum_of_duration]
ux_SumOfDuration_day[sum_of_duration > max_sum_of_duration]

marg_SumOfDuration_day[sum_of_duration > max_sum_of_duration, ':='(sum_of_duration = max_sum_of_duration)]
hcc_SumOfDuration_day[sum_of_duration > max_sum_of_duration, ':='(sum_of_duration = max_sum_of_duration)]
ux_SumOfDuration_day[sum_of_duration > max_sum_of_duration, ':='(sum_of_duration = max_sum_of_duration)]

marg_Freq_day[freq > 100]
hcc_Freq_day[freq > 100]
ux_Freq_day[freq > 100]

# set max freq as 300 
marg_Freq_day[freq > 300, ':='(freq = 300)]
hcc_Freq_day[freq > 300, ':='(freq = 300)]
ux_Freq_day[freq > 300, ':='(freq = 300)]

datetime.all <- data.table(marg_dt$aggDay)
setnames(datetime.all,"aggDay")
datetime.all[, ':='(sum_of_duration = NA)][, ':='(freq = NA)]
datetime.all[, sum_of_duration := as.numeric(sum_of_duration)]
datetime.all[, freq := as.numeric(freq)]

margRS_dt <- datetime.all
# margRS_dt[which(aggDay >= as.Date(marg_RS$joined[1],format="%y-%m-%d") & aggDay <= "2016-6-30"), ]$sum_of_duration = 0
# margRS_dt[which(aggDay >= as.Date(marg_RS$joined[1],format="%y-%m-%d") & aggDay <= "2016-6-30"), ]$freq = 0
uniqueDate = unique(marg_SumOfDuration_day$aggDay)

for(i in 1:length(uniqueDate)){
  margRS_dt[min(which(margRS_dt$aggDay == uniqueDate[i]))]$sum_of_duration = marg_SumOfDuration_day[marg_SumOfDuration_day[,marg_SumOfDuration_day$aggDay==uniqueDate[i]],]$sum_of_duration
  margRS_dt[min(which(margRS_dt$aggDay == uniqueDate[i]))]$freq = marg_Freq_day[marg_Freq_day[,marg_Freq_day$aggDay==uniqueDate[i]],]$freq
}

hccRS_dt <- datetime.all
# hccRS_dt[which(aggDay >= as.Date(hcc_RS$joined[1],format="%y-%m-%d") & aggDay <= "2016-6-30"), ]$sum_of_duration = 0
# hccRS_dt[which(aggDay >= as.Date(hcc_RS$joined[1],format="%y-%m-%d") & aggDay <= "2016-6-30"), ]$freq = 0
uniqueDate = unique(hcc_SumOfDuration_day$aggDay)
for(i in 1:length(uniqueDate)){
  hccRS_dt[min(which(hccRS_dt$aggDay == uniqueDate[i]))]$sum_of_duration = hcc_SumOfDuration_day[hcc_SumOfDuration_day[,hcc_SumOfDuration_day$aggDay==uniqueDate[i]],]$sum_of_duration
  hccRS_dt[min(which(hccRS_dt$aggDay == uniqueDate[i]))]$freq = hcc_Freq_day[hcc_Freq_day[,hcc_Freq_day$aggDay==uniqueDate[i]],]$freq
}

uxRS_dt <- datetime.all
# uxRS_dt[which(aggDay >= as.Date(ux_RS$joined[1],format="%y-%m-%d") & aggDay <= "2016-6-30"), ]$sum_of_duration = 0
# uxRS_dt[which(aggDay >= as.Date(ux_RS$joined[1],format="%y-%m-%d") & aggDay <= "2016-6-30"), ]$freq = 0
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


### Build list of all data tables 
##
dt_list = list(marg_dt, hcc_dt, ux_dt, AllLabs_dt)



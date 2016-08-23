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
library(gtable)
library(gridExtra)
library(scales)

## -------------------- ##
### data loading 
# load("Encored-Data-Analysis/marg_15min.RData")
# load("Encored-Data-Analysis/hcc_15min.RData")
# load("Encored-Data-Analysis/ux_15min.RData")

load("../rawData/marg_15min.RData")
load("../rawData/hcc_15min.RData")
load("../rawData/ux_15min.RData")

source("getSNUdata.R")
update_start = "2014-09-01"
update_end = "2016-08-09"

marg_defalut_table_15min <- reviseSNUData(marg_defalut_table_15min, "marg", update_start, update_end, verbose = T)
hcc_defalut_table_15min <- reviseSNUData( hcc_defalut_table_15min, "hcc",  update_start, update_end, verbose = T)
ux_defalut_table_15min <- reviseSNUData(  ux_defalut_table_15min, "ux",   update_start, update_end, verbose = T)

save(marg_defalut_table_15min, file ="../rawData/marg_15min.RData")
save( hcc_defalut_table_15min, file ="../rawData/hcc_15min.RData")
save(  ux_defalut_table_15min, file ="../rawData/ux_15min.RData")

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
# p1 = ggplot(marg_dt[, .(light_sum = sum(light)), by=index], aes(x=index, y=light_sum)) +
#   geom_point() +
#   scale_x_continuous(breaks = 1:96)
# p2 = ggplot(hcc_dt[, .(light_sum = sum(light)), by=index], aes(x=index, y=light_sum)) +
#   geom_point() +
#   scale_x_continuous(breaks = 1:96)
# p3 = ggplot(ux_dt[, .(light_sum = sum(light)), by=index], aes(x=index, y=light_sum)) +
#   geom_point() +
#   scale_x_continuous(breaks = 1:96)
# 
# grid.arrange(p1, p2, p3)

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
## abnormal computer usage (HVAC usage at computer feeder)
## hcc : computer usage over 0.6 = NA
##  ux : computer usage over 0.4 = NA

hcc_dt[computer > 0.6, ':='(computer = NA)]
ux_dt[computer > 0.4, ':='(computer = NA)]


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


### Light 
## 
hist(marg_dt[light>0.01]$light, 100) # no problem
hist(hcc_dt[light>0.01]$light, 100)  # abnormal : over 0.5
hist(ux_dt[light>0.01]$light, 100)   # abnormal : over 0.5

marg_dt[light > 0.5, ':='(light = NA)]
hcc_dt[light > 0.5, ':='(light = NA)]
ux_dt[light > 0.5, ':='(light = NA)]

# hist(marg_dt[light>0.01]$light, 100) # no problem
# hist(hcc_dt[light>0.01]$light, 100)  # abnormal : over 0.5
# hist(ux_dt[light>0.01]$light, 100)   # abnormal : over 0.5

## Replacement 
marg_dt[timestamp >= "2015-01-18 11:00" & timestamp <= "2015-01-19 10:45"]$computer <- marg_dt[timestamp >= "2015-02-01 11:00" & timestamp <= "2015-02-02 10:45"]$computer
marg_dt[timestamp >= "2015-01-18 11:00" & timestamp <= "2015-01-19 10:45"]$light <- marg_dt[timestamp >= "2015-02-01 11:00" & timestamp <= "2015-02-02 10:45"]$light
marg_dt[timestamp >= "2015-01-18 11:00" & timestamp <= "2015-01-19 10:45"]$hvac <- marg_dt[timestamp >= "2015-02-01 11:00" & timestamp <= "2015-02-02 10:45"]$hvac
marg_dt[timestamp >= "2015-01-18 11:00" & timestamp <= "2015-01-19 10:45"]$etc <- marg_dt[timestamp >= "2015-02-01 11:00" & timestamp <= "2015-02-02 10:45"]$etc
marg_dt[timestamp >= "2015-01-18 11:00" & timestamp <= "2015-01-19 10:45"]$total <- marg_dt[timestamp >= "2015-02-01 11:00" & timestamp <= "2015-02-02 10:45"]$total

marg_dt[timestamp >= "2015-01-18 11:00" & timestamp <= "2015-01-19 10:45"]
marg_dt[timestamp >= "2015-02-01 11:00" & timestamp <= "2015-02-02 10:45"]


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
RS_marg_raw = fread("../realsense/marg.csv")
RS_hcc_raw = fread("../realsense/hcc.csv")
RS_ux_raw = fread("../realsense/ux.csv")

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
marg_RS = marg_RS[aggDay > "2015-10-14" & aggDay <= "2016-07-01"]
hcc_RS = hcc_RS[aggDay > "2015-10-14" & aggDay <= "2016-07-01"]
ux_RS = ux_RS[aggDay > "2015-10-14" & aggDay <= "2016-07-01"]


## Validate the "duration"
# marg_RS[duration > 5*60] # 4 --> Over 1000 case should be removed (single case : duration 1494.048)
marg_RS = marg_RS[duration < 1000]
# hcc_RS[duration > 5*60]  # 48 --> All are in the abnormal period (will be removed) 
# ux_RS[duration > 5*60]   # 3 --> All cases make sense 

hcc_RS <- hcc_RS[aggDay < "2015-12-11" | aggDay > "2015-12-30"]


# i = 1
# sum(marg_RS$duration < i) / nrow(marg_RS)
# sum(hcc_RS$duration < i) / nrow(hcc_RS)
# sum(ux_RS$duration < i) / nrow(ux_RS)


# # Sum of duration 
# marg_SumOfDuration_day = marg_RS[, .(sum_of_duration = sum(duration)), by=aggDay]
# hcc_SumOfDuration_day = hcc_RS[, .(sum_of_duration = sum(duration)), by=aggDay]
# ux_SumOfDuration_day = ux_RS[, .(sum_of_duration = sum(duration)), by=aggDay]
# 
# marg_Freq_day = marg_RS[, .(freq = nrow(.SD)), by=aggDay]
# hcc_Freq_day = hcc_RS[, .(freq = nrow(.SD)), by=aggDay]
# ux_Freq_day = ux_RS[, .(freq = nrow(.SD)), by=aggDay]

# #frequency cut
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

# hist(marg_Freq$freq, 100)
# hist(hcc_Freq$freq, 100)
# hist(ux_Freq$freq, 100)


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





### --------------------------- ###
### Build tables and functions for extraction & aggregation

## four stat : peak, base, avg, mid --> c(1,2,3,4)
get.four.stats <- function(usage, type){
  peak = quantile(usage, .90, na.rm = T)
  base = quantile(usage, .10, na.rm = T)
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

# 2. aggregation unit 
agg_Units = c("aggWeek", "aggDay")

# 3. day selection
day_selections = c("allDay", "weekDay", "weekEnd")

# 4. feeders
feeders = c("total", "computer", "light", "hvac")
# feeders = c("hvac")


return_dts = list(0)

for(lab in 1:4){ 
# for(lab in 1){
#   lab_dt & name selection 
  lab_dt = dt_list[[lab]]
#   lab_dt = marg_dt
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
                                 avg  = get.four.stats(get(feeder), 3)),
                                 by=get(agg_Unit)]
          
        } else if(day_selection == "weekDay") {
          
          return_dt = lab_dt[weekday == T, .(peak = get.four.stats(get(feeder), 1),
                                             base = get.four.stats(get(feeder), 2),
                                             avg  = get.four.stats(get(feeder), 3)), 
                                             by=get(agg_Unit)]
          
        } else if(day_selection == "weekEnd") {
          
          return_dt = lab_dt[weekday == F, .(peak = get.four.stats(get(feeder), 1),
                                             base = get.four.stats(get(feeder), 2),
                                             avg  = get.four.stats(get(feeder), 3)),
                                             by=get(agg_Unit)]
        }
        
        light_min = 0.01
        light_peak = quantile(lab_dt$light, .90, na.rm = T)
        light_dt = lab_dt[, .(timestamp = timestamp,
                              aggDay = aggDay,
                              aggWeek = aggWeek,
                              light = na.locf(light),
                              lightON = ifelse(na.locf(light) > light_min, 1, 0),
                              peak_80 = filter.fault.partial.lightOn(ifelse((na.locf(light) < light_peak * 0.8) & (na.locf(light) > light_min), 1, 0)))]
        
        partial_light = light_dt[, .(lightON = sum(lightON),
                                     threshold80 = ifelse(sum(lightON)==0,1,sum(peak_80)/sum(lightON))), by=get(agg_Unit)]
        
        return_dt = merge(return_dt, partial_light, by="get")
        
        assign(dt_name, return_dt)
        return_dts = append(return_dts, setNames(list(return_dt),dt_name))
      }
    }
  }
}


## making MARG_return_dts for exp1,2
MARG_return_dts = list(0)

for(lab in 1){ 
  #   lab_dt & name selection 
  lab_dt = dt_list[[lab]]
  #   lab_dt = marg_dt
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
                                 avg  = get.four.stats(get(feeder), 3)),
                             by=get(agg_Unit)]
          
        } else if(day_selection == "weekDay") {
          
          return_dt = lab_dt[weekday == T, .(peak = get.four.stats(get(feeder), 1),
                                             base = get.four.stats(get(feeder), 2),
                                             avg  = get.four.stats(get(feeder), 3)), 
                             by=get(agg_Unit)]
          
        } else if(day_selection == "weekEnd") {
          
          return_dt = lab_dt[weekday == F, .(peak = get.four.stats(get(feeder), 1),
                                             base = get.four.stats(get(feeder), 2),
                                             avg  = get.four.stats(get(feeder), 3)),
                             by=get(agg_Unit)]
        }
        
        light_min = 0.01
        light_peak = quantile(lab_dt$light, .90, na.rm = T)
        light_dt = lab_dt[, .(timestamp = timestamp,
                              aggDay = aggDay,
                              aggWeek = aggWeek,
                              light = na.locf(light),
                              lightON = ifelse(na.locf(light) > light_min, 1, 0),
                              peak_80 = filter.fault.partial.lightOn(ifelse((na.locf(light) < light_peak * 0.8) & (na.locf(light) > light_min), 1, 0)))]
        
        partial_light = light_dt[, .(lightON = sum(lightON),
                                     threshold80 = ifelse(sum(lightON)==0,1,sum(peak_80)/sum(lightON))), by=get(agg_Unit)]
        
        return_dt = merge(return_dt, partial_light, by="get")
        
        assign(dt_name, return_dt)
        MARG_return_dts = append(MARG_return_dts, setNames(list(return_dt),dt_name))
      }
    }
  }
}

MARG_return_dts[[2]]$peak <- MARG_return_dts[[2]]$peak - MARG_return_dts[[5]]$peak
MARG_return_dts[[2]]$avg <- MARG_return_dts[[2]]$avg - MARG_return_dts[[5]]$avg
MARG_return_dts[[2]]$base <- MARG_return_dts[[2]]$base - MARG_return_dts[[5]]$base

MARG_return_dts[[6]]$peak <- MARG_return_dts[[6]]$peak - MARG_return_dts[[9]]$peak
MARG_return_dts[[6]]$avg <- MARG_return_dts[[6]]$avg - MARG_return_dts[[9]]$avg
MARG_return_dts[[6]]$base <- MARG_return_dts[[6]]$base - MARG_return_dts[[9]]$base

MARG_return_dts[[10]]$peak <- MARG_return_dts[[10]]$peak - MARG_return_dts[[13]]$peak
MARG_return_dts[[10]]$avg <- MARG_return_dts[[10]]$avg - MARG_return_dts[[13]]$avg
MARG_return_dts[[10]]$base <- MARG_return_dts[[10]]$base - MARG_return_dts[[13]]$base

MARG_return_dts[[14]]$peak <- MARG_return_dts[[14]]$peak - MARG_return_dts[[17]]$peak
MARG_return_dts[[14]]$avg <- MARG_return_dts[[14]]$avg - MARG_return_dts[[17]]$avg
MARG_return_dts[[14]]$base <- MARG_return_dts[[14]]$base - MARG_return_dts[[17]]$base

MARG_return_dts[[18]]$peak <- MARG_return_dts[[18]]$peak - MARG_return_dts[[21]]$peak
MARG_return_dts[[18]]$avg <- MARG_return_dts[[18]]$avg - MARG_return_dts[[21]]$avg
MARG_return_dts[[18]]$base <- MARG_return_dts[[18]]$base - MARG_return_dts[[21]]$base

MARG_return_dts[[22]]$peak <- MARG_return_dts[[22]]$peak - MARG_return_dts[[25]]$peak
MARG_return_dts[[22]]$avg <- MARG_return_dts[[22]]$avg - MARG_return_dts[[25]]$avg
MARG_return_dts[[22]]$base <- MARG_return_dts[[22]]$base - MARG_return_dts[[25]]$base

## check the returns 
# return_dts[15]
# marg_dt[, .(peak = get.four.stats(computer, 1),
#             base = get.four.stats(computer, 2),
#             avg  = get.four.stats(computer, 3),
#             med  = get.four.stats(computer, 4)), by=aggDay]


### ------------------------------------------- ###
### Plotting
windowingByExpDate <- function(data, yName, windowingWeek){
  if((plot_dt$get[2]-plot_dt$get[1]) > 3) {
    n <- windowingWeek/2
  }else {
    n <- windowingWeek/2*7
  }
  
  y<-data[[yName]]
  windowing <- data.table(matrix(rep(0,length(y)*3),ncol=3))
  setnames(windowing,c("get","mean","sd"))
  
  for (k in 1:length(y)) {
    start<-1
    end<-1
    
    #Collecting UX RealSense data starts on "2015-11-02"
    if((yName=='sum_of_duration' | yName=='freq') & (data$get[k] < as.Date("2015-11-02"))) {
      windowing$mean[k] <- NA
      windowing$sd[k] <- NA 
      next
    }
    
    if(k < rownum_expDate[2]){
      start<-k-n
      end<-k+n
      
      if(k-n <= 0) {
        start <- 1
      }
      if(k+n >= rownum_expDate[2]) {
        end <- rownum_expDate[2] - 1
      }
      
      windowing$mean[k] <- ifelse(is.nan(mean(y[start:end],na.rm=T)),NA,mean(y[start:end],na.rm=T))
      windowing$sd[k] <- ifelse(is.nan(sd(y[start:end],na.rm=T)),NA,sd(y[start:end],na.rm=T))
      
    } else if(k < rownum_expDate[3]){
      start<-k-n
      end<-k+n
      
      if(k-n <= rownum_expDate[2]) {
        start <- rownum_expDate[2]
      }
      if(k+n >= rownum_expDate[3]) {
        end <- rownum_expDate[3] - 1
      }
      
      windowing$mean[k] <- ifelse(is.nan(mean(y[start:end],na.rm=T)),NA,mean(y[start:end],na.rm=T))
      windowing$sd[k] <- ifelse(is.nan(sd(y[start:end],na.rm=T)),NA,sd(y[start:end],na.rm=T))
    } else if(k < rownum_expDate[4]){
      start<-k-n
      end<-k+n
      
      if(k-n <= rownum_expDate[3]) {
        start <- rownum_expDate[3]
      }
      if(k+n >= rownum_expDate[4]) {
        end <- rownum_expDate[4] - 1
      }
      
      windowing$mean[k] <- ifelse(is.nan(mean(y[start:end],na.rm=T)),NA,mean(y[start:end],na.rm=T))
      windowing$sd[k] <- ifelse(is.nan(sd(y[start:end],na.rm=T)),NA,sd(y[start:end],na.rm=T)) 
    } else if(k < rownum_expDate[5]){
      start<-k-n
      end<-k+n
      
      if(k-n <= rownum_expDate[4]) {
        start <- rownum_expDate[4]
      }
      if(k+n >= rownum_expDate[5]) {
        end <- rownum_expDate[5] - 1
      }
      
      windowing$mean[k] <- ifelse(is.nan(mean(y[start:end],na.rm=T)),NA,mean(y[start:end],na.rm=T))
      windowing$sd[k] <- ifelse(is.nan(sd(y[start:end],na.rm=T)),NA,sd(y[start:end],na.rm=T))
    } else if(k < rownum_expDate[6]){
      start<-k-n
      end<-k+n
      
      if(k-n <= rownum_expDate[5]) {
        start <- rownum_expDate[5]
      }
      if(k+n >= rownum_expDate[6]) {
        end <- rownum_expDate[6] - 1
      }
      
      windowing$mean[k] <- ifelse(is.nan(mean(y[start:end],na.rm=T)),NA,mean(y[start:end],na.rm=T))
      windowing$sd[k] <- ifelse(is.nan(sd(y[start:end],na.rm=T)),NA,sd(y[start:end],na.rm=T))
    } else if(k < rownum_expDate[7]){
      start<-k-n
      end<-k+n
      
      if(k-n <= rownum_expDate[6]) {
        start <- rownum_expDate[6]
      }
      if(k+n >= rownum_expDate[7]) {
        end <- rownum_expDate[7] - 1
      }
      
      windowing$mean[k] <- ifelse(is.nan(mean(y[start:end],na.rm=T)),NA,mean(y[start:end],na.rm=T))
      windowing$sd[k] <- ifelse(is.nan(sd(y[start:end],na.rm=T)),NA,sd(y[start:end],na.rm=T))
    } else {
      start<-k-n
      end<-k+n
      
      if(k-n <= rownum_expDate[7]) {
        start <- rownum_expDate[7]
      }
      if(k+n >= length(y)) {
        end <- length(y)
      }
      
      windowing$mean[k] <- ifelse(is.nan(mean(y[start:end],na.rm=T)),NA,mean(y[start:end],na.rm=T))
      windowing$sd[k] <- ifelse(is.nan(sd(y[start:end],na.rm=T)),NA,sd(y[start:end],na.rm=T)) 
    }
    print(paste(data$get[k],":",start,end, windowing$mean[k]))
  }
  windowing$sd <- ifelse(is.na(windowing$sd),0,windowing$sd)
  windowing$get <- data$get
  
  return (windowing)
}

add.window.line <- function(plot_body, data, valueName, windowingWeek) {
  window_df = windowingByExpDate(data,valueName,windowingWeek)
  result = plot_body +
    geom_line(data=window_df, aes_string(y = "mean", linetype = shQuote(valueName)), size=1) +
    geom_ribbon(data=window_df,aes(ymin = mean - sd, ymax = mean + sd), alpha = 0.2)
  
  return (result)
}

add.colorful.window.line <- function(plot_body, data, valueName, windowingWeek, colorName, ribbon=TRUE) {
  window_df = windowingByExpDate(data,valueName,windowingWeek)

  if(ribbon==TRUE) {
    result = plot_body +
      geom_line(data=window_df, aes_string(y = "mean", linetype = shQuote(valueName)), color=colorName, size=1) +
      geom_ribbon(data=window_df,aes(ymin = mean - sd, ymax = mean + sd), fill=colorName, alpha = 0.2)
  }else {
    result = plot_body +
      geom_line(data=window_df, aes_string(y = "mean", color = shQuote(valueName)), size=1)
  }
  
  return (result)
}

set.expDate.rownum <- function(plot_dt, expDate) {
  rownum_expDate <- 1
  
  for (d in expDate) {
    rownum_expDate <- append(rownum_expDate, ifelse((nrow(plot_dt[d>plot_dt$get,])+1)>nrow(plot_dt),nrow(plot_dt),(nrow(plot_dt[d>plot_dt$get,])+1)))
  }
  
  return(rownum_expDate)
}

add.event.vline <- function(plot_body){
  result = plot_body + 
    scale_x_date("Timestamp", labels = date_format("%Y-%m"), breaks = date_breaks("month")) +
    theme_bw()+
    geom_vline(aes(xintercept = as.numeric(as.Date("2015-10-08"))),color="gray40") +
    geom_vline(aes(xintercept = as.numeric(as.Date("2015-12-01"))),color="gray40") +
    geom_vline(aes(xintercept = as.numeric(as.Date("2016-01-11"))),color="gray40") +
    geom_vline(aes(xintercept = as.numeric(as.Date("2016-02-01"))),color="gray40") +
    geom_vline(aes(xintercept = as.numeric(as.Date("2016-05-16"))),color="gray40") +
    geom_vline(aes(xintercept = as.numeric(as.Date("2016-06-13"))),color="gray40")
  
  return(result)
}

set.default.theme <- function(plot_body) {
  result = plot_body + 
    theme_bw()+
    theme(axis.line = element_line(colour = "black"),
          legend.text = element_text(size=23, hjust = 1),
          plot.title = element_text(size=23),
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.key = element_rect(colour="white"),
          legend.key.size = unit(10,"cm"),
          legend.margin = unit(0, "cm"),
          axis.text = element_text(size=20),
          axis.text.x = element_text(size=15, angle = 45, hjust = 1),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size=23, vjust = 1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank())+
    guides(fill = guide_legend(keywidth = 1, keyheight = 1),
           linetype = guide_legend(keywidth = 4, keyheight = 1),
           colour = guide_legend(keywidth = 3, keyheight = 1))
  
  return(result)
}

##theme for light pattern plot; without legend box
set.colorful.theme <- function(plot_body, colorName) {
  result = plot_body + 
    theme_bw()+
    theme(axis.line = element_line(colour = "black"),
          legend.text = element_text(size=23, hjust = 1),
          plot.title = element_text(size=23),
          legend.position = "none",
          legend.title = element_blank(),
          legend.key = element_rect(colour="white"),
          legend.key.size = unit(10,"cm"),
          legend.margin = unit(0, "cm"),
          axis.text = element_text(size=20),
          axis.text.x = element_text(size=15, angle = 45, hjust = 1),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size=23, vjust = 1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank())+
    guides(fill = guide_legend(keywidth = 1, keyheight = 1),
           linetype = guide_legend(keywidth = 4, keyheight = 1),
           colour = guide_legend(keywidth = 3, keyheight = 1))+
    scale_color_manual(values=colorName)+
    scale_fill_manual(values=colorName)
  
  return(result)
}

get.expDate.3 <- function() {
  expDate<-c(as.Date("2015-10-08"),
             as.Date("2015-12-01"),
             as.Date("2016-01-11"),
             as.Date("2016-02-01"),
             as.Date("2016-05-16"),
             as.Date("2016-06-13"))
  return(expDate)
}

save.plot <- function(file, plot) {
  ggsave(file, width = 8, height = 6, dpi = 300, plot, limitsize=FALSE)
}

save.wide.plot <- function(file, plot) {
  ggsave(file, width = 10, height = 6, dpi = 300, plot, limitsize=FALSE)
}

partial_lightON_color = "orange2"
lightON_duration_color = "darkolivegreen"
full_lightON_color = "violetred4"
strong_light_color = "darkblue"

## 1. Four stats plots  +  partial_lightON & lightON duration plots 
# expDate<-c(as.Date("2015-10-08"),
#            as.Date("2015-12-01"),
#            as.Date("2016-01-11"),
#            as.Date("2016-02-01"),
#            as.Date("2016-05-16"),
#            as.Date("2016-06-13"))
expDate <- get.expDate.3()

# return_dts[[40]][get >= "2015-12-11" & get <= "2015-12-30"]$sum_of_duration <- NA
# return_dts[[40]][get >= "2015-12-11" & get <= "2015-12-30"]$freq <- NA

start.time <- Sys.time()
for(i in 2:length(return_dts)){ 
  plot_dt   = return_dts[[i]]
  
  rownum_expDate <- set.expDate.rownum(plot_dt, expDate)
  
  if(max(plot_dt$threshold80) <= 1.0) {
    plot_dt$threshold80 <- plot_dt$threshold80*100 ## partial lightON rate -> percentage  
    plot_dt$lightON <- plot_dt$lightON*15.0/60.0 ## lightOn duration 15min block -> hour
  }
  
#   plotTitle <- "daily"
  
  ##windowingWeek must be even
  for(windowingWeek in c(4)) {
    
#     if(strsplit(plot_name,"_")[[1]][2] == "aggWeek"){
#       #       ylim_RS_duration <- 2000
#       #       ylim_RS_freq <- 1000
#       plotTitle <- "weekly"
#     } else{
#       #       ylim_RS_duration <- 600
#       #       ylim_RS_freq <- 300
#       plotTitle <- "daily"
#     }  
#     
    plot_name = paste(names(return_dts[i]), "- windowing ", windowingWeek,"weeks")  
    print(plot_name)
    
    stats <- ggplot(plot_dt, aes(x=get)) +
      ggtitle(plot_name) +
      ylab("Energy use (kWh/day)")+
      scale_linetype_discrete(breaks=c("peak", "avg", "base"))
    stats = add.window.line(stats, plot_dt, "peak", windowingWeek)
    stats = add.window.line(stats, plot_dt, "base", windowingWeek)
    stats = add.window.line(stats, plot_dt, "avg", windowingWeek)
    
  
    
    if(grepl("light", plot_name) | grepl("total", plot_name)){
      partial_lightON <- ggplot(plot_dt, aes(x=get)) +
        ggtitle(plot_name)+
        ylab("Partial light-ON (%)")
      partial_lightON = add.colorful.window.line(partial_lightON, plot_dt, 'threshold80', windowingWeek, partial_lightON_color)
      
      lightON_duration <- ggplot(plot_dt, aes(x=get)) +
        ggtitle(plot_name)+
        ylim(0,24)+
        ylab("Light-ON duration (hours)")+
        scale_color_discrete(breaks = c("lightON"), labels = c("number of lightON blocks(15min)"))
      lightON_duration = add.colorful.window.line(lightON_duration, plot_dt, 'lightON',windowingWeek, lightON_duration_color)
      
      stats            = add.event.vline(stats)
      partial_lightON  = add.event.vline(partial_lightON)
      lightON_duration = add.event.vline(lightON_duration)
      stats            = set.default.theme(stats)
      partial_lightON  = set.colorful.theme(partial_lightON, partial_lightON_color)
      lightON_duration = set.colorful.theme(lightON_duration, lightON_duration_color)
      
#       plots <- arrangeGrob(stats, partial_lightON, lightON_duration, ncol=1)
      save.plot(paste0("../plots/",plot_name, "_1.png"), stats)
      save.plot(paste0("../plots/",plot_name, "_2.png"), partial_lightON)
      save.plot(paste0("../plots/",plot_name, "_3.png"), lightON_duration)
#       ggsave(file = paste0("../plots/",plot_name, "_1.png"), width = 9, height = 7, dpi = 300, stats, limitsize=FALSE)
#       ggsave(file = paste0("../plots/",plot_name, "_2.png"), width = 9, height = 7, dpi = 300, partial_lightON, limitsize=FALSE)
#       ggsave(file = paste0("../plots/",plot_name, "_3.png"), width = 9, height = 7, dpi = 300, lightON_duration, limitsize=FALSE)
#       
    } else {
      
      stats            = add.event.vline(stats)
      stats            = set.default.theme(stats)
      
      save.plot(paste0("../plots/",plot_name, ".png"), stats)
    }
  }
}
end.time <- Sys.time()
end.time-start.time


###
## 1-1. MARG computer 
# expDate<-c(as.Date("2014-11-10"),
#            as.Date("2014-11-16"),
#            as.Date("2016-11-16"),
#            as.Date("2016-11-16"),
#            as.Date("2016-11-16"),
#            as.Date("2016-11-16"))
# 
expDate<-c(as.Date("2014-11-10"),
           as.Date("2014-11-16"),
           as.Date("2015-01-15"),
           as.Date("2015-01-21"),
           as.Date("2016-11-16"),
           as.Date("2016-11-16"))

# return_dts[[40]][get >= "2015-12-11" & get <= "2015-12-30"]$sum_of_duration <- NA
# return_dts[[40]][get >= "2015-12-11" & get <= "2015-12-30"]$freq <- NA

start.time <- Sys.time()
for(i in 2:length(MARG_return_dts)){ 
  plot_dt   = MARG_return_dts[[i]]
  
  #     #Exp1
  #     plot_dt = plot_dt[get >= "2014-09-11" & get <= "2014-12-16"]
      #Exp2
      plot_dt = plot_dt[get >= "2014-09-11" & get < "2015-06-01"]
  
  rownum_expDate <- set.expDate.rownum(plot_dt, expDate)
  
#   plot_dt$threshold80 <- plot_dt$threshold80*100 ## partial lightON rate -> percentage
#   plot_dt$lightON <- plot_dt$lightON*15.0/60.0 ## lightOn duration 15min block -> hour
#   
  ##windowingWeek must be even
  for(windowingWeek in c(4,8)) {
      
    plot_name = names(MARG_return_dts[i])
    
    if((strsplit(plot_name,"_")[[1]][4] != "computer") & (strsplit(plot_name,"_")[[1]][4] != "light") & (strsplit(plot_name,"_")[[1]][4] != "total")){
      next
    }
    
    plot_name = paste(names(MARG_return_dts[i]), "- windowing ", windowingWeek,"weeks")  
    
    print(plot_name)
    
    stats <- ggplot(plot_dt, aes(x=get)) +
      ggtitle(plot_name) +
      ylab("Energy use (kWh/day)")+
      scale_linetype_discrete(breaks=c("peak", "avg", "base"))
    stats = add.window.line(stats, plot_dt, "peak", windowingWeek)
    stats = add.window.line(stats, plot_dt, "base", windowingWeek)
    stats = add.window.line(stats, plot_dt, "avg", windowingWeek)
    
#     #Exp1
#     stats = stats + 
#       scale_x_date("Timestamp", labels = date_format("%Y-%m"), breaks = date_breaks("month")) +
#       theme_bw()+
#       geom_vline(aes(xintercept = as.numeric(as.Date("2014-11-10"))),color="gray40", linetype = "longdash") +
#       geom_vline(aes(xintercept = as.numeric(as.Date("2014-11-16"))),color="gray40", linetype = "longdash")
    #Exp2
    stats = stats + 
      scale_x_date("Timestamp", labels = date_format("%Y-%m"), breaks = date_breaks("month")) +
      theme_bw()+
      geom_vline(aes(xintercept = as.numeric(as.Date("2014-11-10"))),color="gray40", linetype = "longdash") +
      geom_vline(aes(xintercept = as.numeric(as.Date("2014-11-16"))),color="gray40", linetype = "longdash") +
      geom_vline(aes(xintercept = as.numeric(as.Date("2015-01-15"))),color="gray40", linetype = "longdash") +
      geom_vline(aes(xintercept = as.numeric(as.Date("2015-01-21"))),color="gray40", linetype = "longdash")
    stats            = set.default.theme(stats)
    
    if((strsplit(plot_name,"_")[[1]][4] == "total")){
#       save.wide.plot(paste0("../plots/Exp1_",plot_name, ".png"), stats)
      save.wide.plot(paste0("../plots/Exp2_",plot_name, ".png"), stats)
    }else{
#       save.plot(paste0("../plots/Exp1_",plot_name, ".png"), stats)
      save.plot(paste0("../plots/Exp2_",plot_name, ".png"), stats)
    }
  }
}
end.time <- Sys.time()
end.time-start.time


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
  
  lab_dt$light <- na.locf(lab_dt$light)
  
  rownum_expDate <- 1
  
  lunch_dt_aggDay <- lab_dt[, .(aggWeek=as.Date(cut(aggDay, breaks = "week", start.on.monday = T)),
                                lunch_lightOFF_80 = get.lunch.lightOFF(.SD, 0.8)), by=aggDay]
  
  setnames(lunch_dt_aggDay,old="aggDay",new="get")
  
  lunch_dt_aggWeek <- lunch_dt_aggDay[, .(lunch_lightOFF_80 = sum(lunch_lightOFF_80)), by=aggWeek]
  
  setnames(lunch_dt_aggWeek,old="aggWeek",new="get")
  
  aggDay_rownum_expDate <- set.expDate.rownum(lunch_dt_aggDay, expDate)
  
  aggWeek_rownum_expDate <- set.expDate.rownum(lunch_dt_aggWeek, expDate)
  
  for(windowingWeek in c(4,8)) {
    
    plot_name = paste(lab_name, "lunch light OFF count - windowing", windowingWeek,"weeks")  
    print(plot_name)
    
    rownum_expDate <- aggDay_rownum_expDate
    
    p1 <- ggplot(lunch_dt_aggDay, aes(x=get)) +
      ggtitle(paste(plot_name, "aggDay")) +
      ylab("lunch light On = 0; Off = 1")
    
    p1 = add.window.line(p1, lunch_dt_aggDay, 'lunch_lightOFF_80', windowingWeek)
    
    rownum_expDate <- aggWeek_rownum_expDate
    
    p2 <- ggplot(lunch_dt_aggWeek, aes(x=get)) +           
      ggtitle(paste(plot_name, "aggWeek"))+
      ylab("number of lunch_lightOFF days")
    
    p2 = add.window.line(p2, lunch_dt_aggWeek, 'lunch_lightOFF_80', windowingWeek)
    
    p1 = add.event.vline(p1)
    p2 = add.event.vline(p2)
    p1 = set.default.theme(p1)
    p2 = set.default.theme(p2)
    
#     plots <- arrangeGrob(p1, p2)
    
    save.plot(paste0("../plots/",plot_name, "_1.png"), p1)
    save.plot(paste0("../plots/",plot_name, "_2.png"), p2)
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
  
#   lab_dt$light <- na.locf(lab_dt$light)
  
  light_peak = quantile(lab_dt$light, .90, na.rm = T)
  print(light_peak)
  
  strong_light_aggDay_dt = lab_dt[, .(strong_light_80 = sum(light > (light_peak * 0.8), na.rm = T)*15.0/60.0), by=aggDay]
  setnames(strong_light_aggDay_dt,old="aggDay",new="get")
  
  strong_light_aggWeek_dt = lab_dt[, .(strong_light_80 = sum(light > (light_peak * 0.8), na.rm = T)*15.0/60.0), by=aggWeek]
  setnames(strong_light_aggWeek_dt,old="aggWeek",new="get")
  
  strong_light_aggWeek_dt$strong_light_80 <- strong_light_aggWeek_dt$strong_light_80/7.0

  aggDay_rownum_expDate <- 1
  
  for (d in expDate) {
    aggDay_rownum_expDate <- append(aggDay_rownum_expDate, (nrow(lunch_dt_aggDay[d>lunch_dt_aggDay$get,])+1))
  }
  
  aggWeek_rownum_expDate <- 1
  
  for (d in expDate) {
    aggWeek_rownum_expDate <- append(aggWeek_rownum_expDate, (nrow(lunch_dt_aggWeek[d>lunch_dt_aggWeek$get,])+1))
  }
  
  for(windowingWeek in c(4,8)) {
    
    plot_name = paste(lab_name, "strong_light count - windowing", windowingWeek,"weeks")  
    print(plot_name)
    
    rownum_expDate <- aggDay_rownum_expDate
    
    p1 <- ggplot(strong_light_aggDay_dt, aes(x=get)) +         
      ggtitle(paste(plot_name, "aggDay"))+
      ylab("strong light-ON duration (hrs/day)")
    
    p1 = add.colorful.window.line(p1, strong_light_aggDay_dt, 'strong_light_80', windowingWeek, strong_light_color)
    
    rownum_expDate <- aggWeek_rownum_expDate
    
    p2 <- ggplot(strong_light_aggWeek_dt, aes(x=get)) +
      ggtitle(paste(plot_name, "aggWeek"))+
      ylab("strong light-ON average duration (hrs/day)")

    p2 = add.colorful.window.line(p2, strong_light_aggWeek_dt, 'strong_light_80', windowingWeek, strong_light_color)
    
    p1 = add.event.vline(p1)
    p2 = add.event.vline(p2)
    p1 = set.colorful.theme(p1, strong_light_color)
    p2 = set.colorful.theme(p2, strong_light_color)
    
#     plots <- arrangeGrob(p1, p2)
    
    save.plot(paste0("../plots/",plot_name, "_1.png"), p1)
    save.plot(paste0("../plots/",plot_name, "_2.png"), p2)
#     ggsave(file = paste0("../plots/", plot_name, ".png"), width = 8, height = 12, dpi = 300, plots)
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
  
#   lab_dt$light <- na.locf(lab_dt$light)
  
  light_peak = quantile(lab_dt$light, .90, na.rm = T)
  print(light_peak)
  
  full_lightON_aggDay_dt = lab_dt[, .(full_lightON_10 = sum(light > (light_peak * 0.1), na.rm = T)==96), by=aggDay]
  setnames(full_lightON_aggDay_dt,old="aggDay",new="get")
  
  full_lightON_aggWeek_dt = full_lightON_aggDay_dt[, .(full_lightON_10 = sum(full_lightON_10, na.rm = T)), by=.(aggWeek=as.Date(cut(get, breaks = "week", start.on.monday = T)))]
  setnames(full_lightON_aggWeek_dt,old="aggWeek",new="get")
  
  rownum_expDate <- 1
  
  for (d in expDate) {
    rownum_expDate <- append(rownum_expDate, (nrow(lunch_dt_aggWeek[d>lunch_dt_aggWeek$get,])+1))
  }
  
  for(windowingWeek in c(4,8)) {
    
    plot_name = paste(lab_name, "full(24hrs)_lightON count - windowing", windowingWeek,"weeks")  
    print(plot_name)
    
    p <- ggplot(full_lightON_aggWeek_dt, aes(x=get)) +
#       
#       #                         geom_point(aes(y=full_lightON_50, color='full_lightON_50')) +
#       #                         geom_smooth(aes(y=full_lightON_50, color='full_lightON_50'), span = s) +    
#       #                         
#       #                         geom_point(aes(y=full_lightON_40, color='full_lightON_40')) +
#       #                         geom_smooth(aes(y=full_lightON_40, color='full_lightON_40'), span = s) +             
#       
#       geom_point(aes(y=full_lightON_30, color='full_lightON_30')) +        
#       geom_point(aes(y=full_lightON_20, color='full_lightON_20')) +
      geom_point(aes(y=full_lightON_10), colour='gray70') +
      ggtitle(plot_name)+
      ylab("24hrs light-ON day (count/week)")
# +
#       theme(legend.text = element_text(size=20),
#             plot.title = element_text(size=20),
#             legend.title = element_blank(),
#             axis.text = element_text(size=15),
#             axis.title = element_text(size=15))+
#       guides(fill = guide_legend(keywidth = 1, keyheight = 1),
#              linetype=guide_legend(keywidth = 3, keyheight = 2),
#              colour=guide_legend(keywidth = 3, keyheight = 2))
    
#     p = add.window.line(p, full_lightON_aggWeek_dt, 'full_lightON_30', windowingWeek)
#     p = add.window.line(p, full_lightON_aggWeek_dt, 'full_lightON_20', windowingWeek)
    p = add.colorful.window.line(p, full_lightON_aggWeek_dt, 'full_lightON_10', windowingWeek, full_lightON_color)
    p = add.event.vline(p)
    p = set.colorful.theme(p, full_lightON_color)
    
    save.plot(paste0("../plots/",plot_name, ".png"), p)
#     ggsave(file = paste0("../plots/", plot_name, ".png"), width = 20, height = 10, dpi = 200, p)
  }
}



########################
## Weather table
########################
weather_dt = fread("../rawData/Suwon_weather.csv")
weather_dt$date_index = as.Date(weather_dt$date_index)
setnames(weather_dt,old="date_index",new="get")
str(weather_dt)

weather_dt <- weather_dt[, ':='(aggDay = get,
                                aggWeek = as.Date(cut(get, breaks = "week", start.on.monday = T)))]
aggWeek_weather_dt <- weather_dt[, .(get = aggWeek,
                               avg_temp = mean(avg_temp),
                               max_temp = mean(max_temp),
                               min_temp = mean(min_temp)), by=aggWeek] 
aggWeek_weather_dt <- aggWeek_weather_dt[, aggWeek:=NULL]

### 
## 5. MARG HVAC + weahter(max, avg, min temperature info) 
## 
expDate <- get.expDate.3()

start.time <- Sys.time()
for(i in 2:length(return_dts)){ 
  plot_dt   = return_dts[[i]]
  
  plot_name = names(return_dts[i])
  
  if((strsplit(plot_name,"_")[[1]][4] != "hvac") | (strsplit(plot_name,"_")[[1]][1] != "MARG")){
    next
  }
  
  if(strsplit(plot_name,"_")[[1]][2] == "aggWeek") {
    temp_weather_dt = aggWeek_weather_dt[get >= plot_dt$get[1] & get <= plot_dt$get[nrow(plot_dt)]]
  }else {
    temp_weather_dt = weather_dt[get >= plot_dt$get[1] & get <= plot_dt$get[nrow(plot_dt)]]  
  }
  
  ##windowingWeek must be even
  for(windowingWeek in c(4,8)) {
    
    rownum_expDate <- 1
    for (d in expDate) {
      rownum_expDate <- append(rownum_expDate, (nrow(temp_weather_dt[d>temp_weather_dt$get,])+1))
    }
    
    grid.newpage()
    plot_name = names(return_dts[i])
    plot_name = paste(plot_name, "+ Temperature - windowing ", windowingWeek,"weeks")  
    print(plot_name)
    
    print(strsplit(plot_name,"_")[[1]][2])
    print(rownum_expDate)
    print(head(temp_weather_dt))
    print(head(w_max_tmp))
    print(nrow(w_max_tmp))
    
    p2 <- ggplot(temp_weather_dt, aes(x=get)) +
      ylab("temperature(°C)")+
      ylim(-20,40)+
      scale_color_manual(values=c("dodgerblue2", "skyblue", "midnightblue"), 
                         breaks=c("max_temp", "avg_temp", "min_temp"))
    p2 = add.colorful.window.line(p2, temp_weather_dt, 'max_temp', windowingWeek, "dodgerblue2", ribbon=FALSE)
    p2 = add.colorful.window.line(p2, temp_weather_dt, 'avg_temp', windowingWeek, "skyblue", ribbon=FALSE)
    p2 = add.colorful.window.line(p2, temp_weather_dt, 'min_temp', windowingWeek, "midnightblue", ribbon=FALSE)
    
    p2 = p2 + 
      theme_bw()+
      theme(axis.line = element_line(colour = "black"),
            legend.text = element_text(size=23, hjust = 1),
            plot.title = element_text(size=23),
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.key = element_rect(colour="white"),
            legend.key.size = unit(10,"cm"),
            legend.margin = unit(0, "cm"),
            legend.key.height = unit(2,"cm"),
            legend.key.width = unit(2,"cm"),
            axis.text = element_text(size=20),
            axis.text.x = element_text(size=15, angle = 45, hjust = 1),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size=23, vjust = 1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_rect(colour = "black"),
            panel.background = element_rect(fill = NA))+
      guides(fill = guide_legend(keywidth = 1, keyheight = 1),
             colour = guide_legend(keywidth = 3, keyheight = 1))
    p2 <- p2 +
      guides(colour=guide_legend(override.aes = list(size=2)))
#     p2            = add.event.vline(p2)
  
    rownum_expDate <- 1
    for (d in expDate) {
      rownum_expDate <- append(rownum_expDate, (nrow(plot_dt[d>plot_dt$get,])+1))
    }
    
    print("plot_dt")
    print(rownum_expDate)

    stats <- ggplot(plot_dt, aes(x=get)) +
#       geom_point(aes(y=peak, color='peak')) +
      ylab("electricity usage(kWh)")+
      ggtitle(plot_name)
    stats = add.window.line(stats, plot_dt, "peak", windowingWeek)
    
    stats = stats + 
      scale_linetype_discrete(breaks=c("peak"), labels=c("90% of peak"))+
      scale_linetype_manual(values=c("dashed"))
    stats            = add.event.vline(stats)

    stats = set.default.theme(stats)
    stats <- stats +
      theme(plot.margin= unit(c(0,2,0,0),"lines"),
            legend.key.height = unit(2,"cm"),
            legend.key.width = unit(2,"cm"))+
      guides(fill = guide_legend(keywidth = 1, keyheight = 1),
             linetype=guide_legend(override.aes = list(size=1.5)),
             colour=guide_legend(keywidth = 3, keyheight = 1))
    
    g1<-ggplot_gtable(ggplot_build(stats))
    g2<-ggplot_gtable(ggplot_build(p2))
    
    pp<-c(subset(g1$layout, name == "panel", se = t:r))
    g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, pp$l, pp$b, pp$l)
    
    ia<-which(g2$layout$name=="axis-l")
    ga <- g2$grobs[[ia]]
    ax <- ga$children[[2]]
    ax$widths <- rev(ax$widths)
    ax$grobs <- rev(ax$grobs)
    ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(0, "npc") + unit(0, "cm")
    g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
    g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
#     g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths))
#     g <- gtable_add_grob(g, ax, pp$t, length(g$widths), pp$b)
    g <- gtable_add_grob(g, g2$grobs[[7]], pp$t, length(g$widths), pp$b)
    
    leg1 <- g1$grobs[[which(g1$layout$name == "guide-box")]]
    leg2 <- g2$grobs[[which(g2$layout$name == "guide-box")]]
    g$grobs[[which(g$layout$name == "guide-box")]] <- gtable:::cbind_gtable(leg1, leg2, "first")
    
    #     il <- which(g2$layout$name == "ylab")
    #     gl <- g2$grobs[[il]]
    #     gl$x <- gl$x - unit(1, "npc") + unit(0.15, "cm")
    #     g <- gtable_add_cols(g, g2$widths[g2$layout[il, ]$l], length(g$widths) - 1)
    #     g <- gtable_add_grob(g, gl, pp$t, length(g$widths) - 1, pp$b)
    #     
    #     grid.arrange(g, ncol=1, heights=c(10, 1),widths =c(1) ,as.table =TRUE)
    
    png(file = paste0("../plots/weather_",plot_name, ".png"), width = 800, height = 600)
    grid.draw(g)
    dev.off()
    
  }
}
end.time <- Sys.time()
end.time-start.time


########################
## RealSense table
########################

return_rs_dts = list(0)

# for(lab in 1:4){ 
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

      dt_name = paste(lab_name, agg_Unit, day_selection, sep="_")
      print(dt_name)
      
      if(day_selection == "allDay"){
        
        return_rs_dt = lab_dt[, .(sum_of_duration = sum(sum_of_duration, na.rm=T),
                               freq = sum(freq, na.rm=T))
                           , by=get(agg_Unit)]
        
      } else if(day_selection == "weekDay") {
        
        return_rs_dt = lab_dt[weekday == T, .(sum_of_duration = sum(sum_of_duration, na.rm=T), 
                                           freq = sum(freq, na.rm=T))
                           , by=get(agg_Unit)]
        
      } else if(day_selection == "weekEnd") {
        
        return_rs_dt = lab_dt[weekday == F, .(sum_of_duration = sum(sum_of_duration, na.rm=T), 
                                           freq = sum(freq, na.rm=T))
                           , by=get(agg_Unit)]
      }
      
      assign(dt_name, return_rs_dt)
      return_rs_dts = append(return_rs_dts, setNames(list(return_rs_dt),dt_name))
    }
  }
}

###
## 6. RealSense plot
##

# default.plot.theme <- function(plot) {
#   plot = plot + theme(legend.text = element_text(size=20),
#                       legend.title = element_text(size=20),
#                       plot.title = element_text(size=20),
#                       axis.text = element_text(size=15))+
#                 guides(fill = guide_legend(keywidth = 1, keyheight = 1),
#                        linetype=guide_legend(keywidth = 3, keyheight = 1),
#                        colour=guide_legend(keywidth = 3, keyheight = 1))
#   dreturn(plot)
# }

start.time <- Sys.time()
expDate <- get.expDate.3()
for(i in 2:length(return_rs_dts)){ 
  plot_dt   = return_rs_dts[[i]]
  
  plot_dt = plot_dt[get >= "2015-10-08" & get <= "2016-07-18"]
  
  rownum_expDate <- set.expDate.rownum(plot_dt, expDate)
  
  ##windowingWeek must be even, 
  ###add.window.line function will window from the point before (windowingWeek/2) weeks and the point after (windowingWeek/2) weeks
  for(windowingWeek in c(4,8)) {
    
    plot_name = paste(names(return_rs_dts[i]), "- windowing ", windowingWeek,"weeks")  
    print(plot_name)
    
    if(strsplit(plot_name,"_")[[1]][2] == "aggWeek"){
      ylim_RS_duration <- 2000
      ylim_RS_freq <- 1000
    } else{
      ylim_RS_duration <- 600
      ylim_RS_freq <- 300
    }    
    
    RS_duration <- ggplot(plot_dt, aes(x=get, ymax=ylim_RS_duration))+
#       geom_point(aes(y=sum_of_duration, color='sum_of_duration')) +
#       geom_text(aes(y=sum_of_duration, label=round(sum_of_duration, 0)), position=position_dodge(width=0.9), vjust=-0.25, colour="grey50") + 
      scale_y_continuous(limits=c(0,ylim_RS_duration), oob=rescale_none) +
      ggtitle(paste("RS", plot_name))+
      theme_bw()+
      ylab("total duration(seconds)")
    
    RS_freq <- ggplot(plot_dt, aes(x=get, ymax=ylim_RS_freq))+
#       geom_point(aes(y=freq, color='freq')) +
#       geom_text(aes(y=freq, label=round(freq, 0)), position=position_dodge(width=0.9), vjust=-0.25, colour="grey50") + 
      scale_y_continuous(limits=c(0,ylim_RS_freq), oob=rescale_none) +
      ggtitle(paste("RS", plot_name))+
      theme_bw()+
      ylab("(counts / day)")
    
    RS_duration      = add.window.line(RS_duration, plot_dt, "sum_of_duration", windowingWeek)
    RS_freq          = add.window.line(RS_freq, plot_dt, "freq", windowingWeek)
    
    RS_duration      = add.event.vline(RS_duration)
    RS_freq          = add.event.vline(RS_freq)

    RS_duration      = set.colorful.theme(RS_duration, "black")
    RS_freq          = set.colorful.theme(RS_freq, "black")    
        
    save.plot(paste0("../plots/RealSense_",plot_name, "_1.png"),RS_duration)
    save.plot(paste0("../plots/RealSense_",plot_name, "_2.png"),RS_freq)
#     plots <- arrangeGrob(RS_duration, RS_freq, ncol=1)
#     ggsave(file = paste0("../plots/RealSense_",plot_name, ".png"), width = 10, height = 10, dpi = 300, plots, limitsize=FALSE)
  }
}
end.time <- Sys.time()
end.time-start.time

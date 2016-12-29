### ------------------------------------------------------------ ###
## pre-processing for 15min data  
##
### JY, EJ @ ADSL, SNU 
###                                   last update : 2016. 8. 30.
### ------------------------------------------------------------ ###

library(data.table)
library(ggplot2)
library(gridExtra)
library(timeDate)
library(zoo)
library(lubridate)
library(plyr)
library(scales)
library(reshape2)
library(bit64)
library(gtable)
library(gridExtra)
library(scales)

source("getSNUdata.R")

## Loading saved raw data 
load("../data/raw/marg_15min.RData")
load("../data/raw/hcc_15min.RData")
load("../data/raw/ux_15min.RData")

## Update & save 15min data  
update_start = "2014-09-01"
update_end = "2016-12-01"

marg_defalut_table_15min <- reviseSNUData(marg_defalut_table_15min, "marg", update_start, update_end, verbose = T)
hcc_defalut_table_15min <- reviseSNUData( hcc_defalut_table_15min, "hcc",  update_start, update_end, verbose = T)
ux_defalut_table_15min <- reviseSNUData(  ux_defalut_table_15min, "ux",   update_start, update_end, verbose = T)

save(marg_defalut_table_15min, file ="../data/raw/marg_15min.RData")
save( hcc_defalut_table_15min, file ="../data/raw/hcc_15min.RData")
save(  ux_defalut_table_15min, file ="../data/raw/ux_15min.RData")

## Add default index for each day (1:96 per day)
marg_dt = data.table(marg_defalut_table_15min, index=rep(1:96, (nrow(marg_defalut_table_15min)/96)))
hcc_dt = data.table(hcc_defalut_table_15min, index=rep(1:96, (nrow(marg_defalut_table_15min)/96)))
ux_dt = data.table(ux_defalut_table_15min, index=rep(1:96, (nrow(marg_defalut_table_15min)/96)))

##
## Missing data 
## 컴퓨터기준으로 15분 사용량이 marg:0.1, hcc:0.05, ux:0.02 미만 시점을 NA처리 

na.missing.data <- function(dt, threshold_min){
  df = data.frame(dt)
  na_indexes = which(df$computer < threshold_min)
  print(df[na_indexes, c("timestamp", "computer", "light", "hvac", "etc", "total")])
  print(paste("missing data points :", length(na_indexes)))
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

##
## Abnormal computer usage - Computer
##  (HVAC usage at computer feeder)
## hcc : computer usage > 0.45 --> NA
##  ux : computer usage > 0.4 --> NA
hcc_dt[computer > 0.45, ':='(computer = NA)]
ux_dt[computer > 0.4, ':='(computer = NA)]

##
## Abnormal computer usage - Light 
## all labs: over 0.5 --> NA 
marg_dt[light > 0.5, ':='(light = NA)]
hcc_dt[light > 0.5, ':='(light = NA)]
ux_dt[light > 0.5, ':='(light = NA)]

##
## Abnormal computer usage - Etc 
marg_dt[etc > 0.3, ':='(etc = NA)]
hcc_dt[etc > 0.1, ':='(etc = NA)]
ux_dt[etc > 0.15, ':='(etc = NA)]

##
## Abnormal computer usage - HVAC
## marg only ... 
# marg_dt[hvac > 1.5, ':='(hvac = NA)]


# check computer & light usage distributions
{
  par(mfrow=c(1,3))
  hist(marg_dt$computer, 100)
  hist(hcc_dt$computer, 100)
  hist(ux_dt$computer, 100)
  
  hist(marg_dt$light, 100)
  hist(hcc_dt$light, 100)
  hist(ux_dt$light, 100)
  
  hist(marg_dt[hvac > 0.1]$hvac, 100)
  hist(hcc_dt[hvac > 0.01]$hvac, 100)
  hist(ux_dt[hvac > 0.01]$hvac, 100)
  
  hist(marg_dt$etc, 100)
  hist(hcc_dt$etc, 100)
  hist(ux_dt$etc, 100)
  
  hist(marg_dt$total, 100)
  hist(hcc_dt$total, 100)
  hist(ux_dt$total, 100)
  par(mfrow=c(1,1))
}


##
## Server issue
## Replacement 2015-01-18 11:00 
## 

replace.server.issue <- function(raw_dt){
  
  return_dt = raw_dt
  
  return_dt[timestamp >= "2015-01-18 11:00" & timestamp <= "2015-01-19 10:45"]$computer <- raw_dt[timestamp >= "2015-02-01 11:00" & timestamp <= "2015-02-02 10:45"]$computer
  return_dt[timestamp >= "2015-01-18 11:00" & timestamp <= "2015-01-19 10:45"]$light    <- raw_dt[timestamp >= "2015-02-01 11:00" & timestamp <= "2015-02-02 10:45"]$light
  return_dt[timestamp >= "2015-01-18 11:00" & timestamp <= "2015-01-19 10:45"]$hvac     <- raw_dt[timestamp >= "2015-02-01 11:00" & timestamp <= "2015-02-02 10:45"]$hvac
  return_dt[timestamp >= "2015-01-18 11:00" & timestamp <= "2015-01-19 10:45"]$etc      <- raw_dt[timestamp >= "2015-02-01 11:00" & timestamp <= "2015-02-02 10:45"]$etc
  return_dt[timestamp >= "2015-01-18 11:00" & timestamp <= "2015-01-19 10:45"]$total    <- raw_dt[timestamp >= "2015-02-01 11:00" & timestamp <= "2015-02-02 10:45"]$total
  
  return(return_dt)
  
}

marg_dt = replace.server.issue(marg_dt)
hcc_dt = replace.server.issue(hcc_dt)
ux_dt = replace.server.issue(ux_dt)

##
## Add 'aggDay' column
## aggDay: Key index column for aggreagtion by day
##
aggDay = c(rep((as.Date(marg_defalut_table_15min$timestamp[1], tz="rok")-1),28), 
           as.Date(marg_defalut_table_15min$timestamp, tz='rok')[29:(nrow(marg_defalut_table_15min))-28])

marg_dt = data.table(marg_dt, aggDay)
hcc_dt = data.table(hcc_dt, aggDay)
ux_dt = data.table(ux_dt, aggDay)

##
## Add 'aggWeek' column
## aggWeek: Key index column for aggreagtion by week
##

marg_dt[, ':='(aggWeek=as.Date(cut(aggDay, breaks = "week", start.on.monday = T)))]
hcc_dt[, ':='(aggWeek=as.Date(cut(aggDay, breaks = "week", start.on.monday = T)))]
ux_dt[, ':='(aggWeek=as.Date(cut(aggDay, breaks = "week", start.on.monday = T)))]


## 
## Cut-out 2014.09(Sep) & the last day data
##
marg_dt = marg_dt[aggDay >= "2014-09-01" & aggDay <= as.Date(update_end)-2]
hcc_dt = hcc_dt[aggDay >= "2014-09-01" & aggDay <= as.Date(update_end)-2]
ux_dt = ux_dt[aggDay >= "2014-09-01" & aggDay <= as.Date(update_end)-2]

##
## Update columns depending on addDay & index update
## --> index, day, workindday
## 
Sys.setlocale("LC_TIME", "English")
marg_dt[, ':='(index=rep(1:96, nrow(marg_dt)/96),
               day = weekdays(aggDay, abbreviate = T), workingday = isWeekday(aggDay))]
hcc_dt[, ':='(index=rep(1:96, nrow(marg_dt)/96),
               day = weekdays(aggDay, abbreviate = T), workingday = isWeekday(aggDay))]
ux_dt[, ':='(index=rep(1:96, nrow(marg_dt)/96),
               day = weekdays(aggDay, abbreviate = T), workingday = isWeekday(aggDay))]

##
## Update 'holidays' to 'workingday = F' 
##
HOLIDAYS = c('2014-10-03', '2015-10-03', '2016-10-03', 
             '2014-10-09', '2015-10-09',
             '2014-12-25', '2015-12-25',
             '2015-01-01', '2016-01-01', 
             '2015-02-18', '2015-02-19', '2015-02-20',
             '2015-05-05', 
             '2015-05-25',
             '2015-06-06',
             '2015-08-14',
             '2015-09-28', '2015-09-29',
             '2016-02-08', '2016-02-09', '2016-02-10',
             '2015-03-01', '2016-03-01',
             '2016-04-13', 
             '2016-05-05', '2016-05-06',
             '2016-06-06',
             '2016-08-15',
             '2016-09-14', '2016-09-15', '2016-09-16')
HOLIDAYS = as.Date(HOLIDAYS)

marg_dt[aggDay %in% HOLIDAYS, ':='(workingday = FALSE)] 
hcc_dt[aggDay %in% HOLIDAYS, ':='(workingday = FALSE)] 
ux_dt[aggDay %in% HOLIDAYS, ':='(workingday = FALSE)] 


##
## Fill NA of 15min data 
## MARG: 83 NA's
## HCC: 257 NA's
## UX: 176 NA's
##
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

## Add total_woHVAC(total without hvac) column
# marg_dt = marg_dt[, ':='(total_woHVAC = total - hvac)]
# hcc_dt = hcc_dt[, ':='(total_woHVAC = total - hvac)]
# ux_dt = ux_dt[, ':='(total_woHVAC = total - hvac)]

marg_dt = marg_dt[, ':='(total_woHVAC = total - hvac,
                         total_woETC = total - etc)]
hcc_dt = hcc_dt[, ':='(total_woHVAC = total - hvac,
                       total_woETC = total - etc)]
ux_dt = ux_dt[, ':='(total_woHVAC = total - hvac,
                     total_woETC = total - etc)]



summary(marg_dt)
summary(hcc_dt)
summary(ux_dt)




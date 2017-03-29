### ------------------------------------------------------------ ###
## pre-processing for RealSense data  
##
### JY, EJ @ ADSL, SNU 
###                                   last update : 2016. 8. 30.
### ------------------------------------------------------------ ###

## 
## Functions for RealSense pre-processing 
##
table.time2string <- function(RS_raw_data){
  RS_raw_data$joined = as.POSIXct(RS_raw_data$joined/1000, origin = "1970-01-01", tz="ROK")
  RS_raw_data$leaved = as.POSIXct(RS_raw_data$leaved/1000, origin = "1970-01-01", tz="ROK")
  
  duration = as.numeric(difftime(RS_raw_data$leaved, RS_raw_data$joined, units = "secs"))
  
  return(cbind(RS_raw_data, duration))
}
# 
# make.quarter.label <- function(input){ 
#   # 15min data label maker 
#   quarter_label = seq(as.POSIXct(as.numeric(input$joined[1])%/%(15*60)*15*60, origin="1970-01-01"),
#                       as.POSIXct(as.numeric(input$joined[nrow(input)])%/%(15*60)*15*60, origin="1970-01-01"),
#                       by = 15*60)
#   
#   dt_quarter_label = data.table(timestamp = quarter_label)
#   
#   return(dt_quarter_label)
# }

build.quarter.table <- function(input){
  # 15min data label maker 
  quarter_label = seq(as.POSIXct(as.numeric(input$joined[1])%/%(15*60)*15*60, origin="1970-01-01"),
                      as.POSIXct(as.numeric(input$joined[nrow(input)])%/%(15*60)*15*60, origin="1970-01-01"),
                      by = 15*60)
  
  dt_quarter_label = data.table(timestamp = quarter_label)
  
  input[, ':='(timestamp = as.POSIXct(as.numeric(joined)%/%(15*60)*15*60, origin="1970-01-01"))]
  
  return_table <- merge(x = dt_quarter_label, y = input, by= "timestamp", all.x = TRUE)
  return_table <- return_table[, .(timestamp = timestamp,
                                   duration = duration)]
  
  
  return_table <- return_table[duration > 1, .(RS_duration = sum(duration),
                                               RS_count = nrow(.SD)),
                               by = timestamp]

  return(return_table)
}

##
## Load RealSense raw data 
##
RS_marg_raw = fread("../data/raw/marg_RS.csv")
RS_hcc_raw = fread("../data/raw/hcc_RS.csv")
RS_ux_raw = fread("../data/raw/ux_RS.csv")

marg_RS = table.time2string(RS_marg_raw)
hcc_RS = table.time2string(RS_hcc_raw)
ux_RS = table.time2string(RS_ux_raw)



## Add aggDay column & Update 'holydays' to 'workingday = F' 
##
## aggDay: key index column for data aggregation 'by day'
## date_adjust_parameter: for local time 
##
# date_adjust_parameter = 7 * 60 * 60 # 7 hours 
# 
# marg_RS[, ':='(aggDay=as.Date(joined-date_adjust_parameter, tz="rok"), workingday = isWeekday(joined-date_adjust_parameter))]
# hcc_RS[, ':='(aggDay=as.Date(joined-date_adjust_parameter, tz="rok"), workingday = isWeekday(joined-date_adjust_parameter))]
# ux_RS[, ':='(aggDay=as.Date(joined-date_adjust_parameter, tz="rok"), workingday = isWeekday(joined-date_adjust_parameter))]
# 
# marg_RS[aggDay %in% HOLIDAYS, ':='(workingday = FALSE)] 
# hcc_RS[aggDay %in% HOLIDAYS, ':='(workingday = FALSE)] 
# ux_RS[aggDay %in% HOLIDAYS, ':='(workingday = FALSE)] 
# 

## 
## Cut-out the inVaild period of data  
## 
# 1. very beginning of RealSense H/W installation 
# 2. HCC invalid period: 2015-12-11 ~ 2015-12-30
MARG_VALID_RS_START_DATE = '2015-10-09'
HCC_VALID_RS_START_DATE = '2015-10-14'
UX_VALID_RS_START_DATE = '2015-11-01'

marg_RS = marg_RS[as.Date(joined) > MARG_VALID_RS_START_DATE]
hcc_RS  =  hcc_RS[as.Date(joined) > HCC_VALID_RS_START_DATE]
ux_RS   =   ux_RS[as.Date(joined) > UX_VALID_RS_START_DATE]
hcc_RS  =  hcc_RS[as.Date(joined) < "2015-12-11" | as.Date(joined) > "2015-12-30"]

marg_RS = marg_RS[duration < 1000]

marg_RS_dt = build.quarter.table(marg_RS)
hcc_RS_dt = build.quarter.table(hcc_RS)
ux_RS_dt = build.quarter.table(ux_RS)

# quarter_label = seq(as.POSIXct(as.numeric(marg_RS$joined[1])%/%(15*60)*15*60, origin="1970-01-01"),
#                     as.POSIXct(as.numeric(marg_RS$joined[nrow(marg_RS)])%/%(15*60)*15*60, origin="1970-01-01"),
#                     by = 15*60)
# 
# 
# dt_quarter_label = data.table(timestamp = quarter_label)
# 
# tmp = marg_RS
# tmp[, ':='(timestamp = as.POSIXct(as.numeric(joined)%/%(15*60)*15*60, origin="1970-01-01"))]
# 
# tmp<-merge(x = dt_quarter_label, y = tmp, by="timestamp", all.x=TRUE)
# tmp<-tmp[, .(timestamp = timestamp,
#         duration = ifelse(is.na(duration), 0, duration))]
# 
# tmp2<-tmp[duration > 1, .(sum_of_duration = sum(duration),
#                          count_per_day = nrow(.SD)), by=timestamp]

##
## Build sum_of_duration & count_per_day table 
## -- valid observation: duration > 1
## -- VALID_SUM_OF_DURATION = 600
## -- MAX_COUNT_PER_DAY = 300


## Validation of "duration"
# marg_RS[duration > 5*60] # 4 --> Over 1000 case should be removed (only one case : duration 1494.048)
# hcc_RS[duration > 5*60]  # 48 --> All are in the abnormal period (will be removed) 
# ux_RS[duration > 5*60]   # 3 --> All cases make sense 
# marg_RS = marg_RS[duration < 1000]

## Build basic table 
# marg_sum_of_duration_day = marg_RS[duration>1, .(sum_of_duration = sum(duration)), by=aggDay]
# hcc_sum_of_duration_day = hcc_RS[duration>1, .(sum_of_duration = sum(duration)), by=aggDay]
# ux_sum_of_duration_day = ux_RS[duration>1, .(sum_of_duration = sum(duration)), by=aggDay]
# 
# marg_count_per_day = marg_RS[duration>1, .(count_per_day = nrow(.SD)), by=aggDay]
# hcc_count_per_day = hcc_RS[duration>1, .(count_per_day = nrow(.SD)), by=aggDay]
# ux_count_per_day = ux_RS[duration>1, .(count_per_day = nrow(.SD)), by=aggDay]

## Build 'valid' table 
# VALID_SUM_OF_DURATION = 600
# MAX_COUNT_PER_DAY = 300
# 
# marg_sum_of_duration_day[sum_of_duration > VALID_SUM_OF_DURATION]
# hcc_sum_of_duration_day[sum_of_duration > VALID_SUM_OF_DURATION]
# ux_sum_of_duration_day[sum_of_duration > VALID_SUM_OF_DURATION]
# 
# marg_sum_of_duration_day[sum_of_duration > VALID_SUM_OF_DURATION, ':='(sum_of_duration = VALID_SUM_OF_DURATION)]
# hcc_sum_of_duration_day[sum_of_duration > VALID_SUM_OF_DURATION, ':='(sum_of_duration = VALID_SUM_OF_DURATION)]
# ux_sum_of_duration_day[sum_of_duration > VALID_SUM_OF_DURATION, ':='(sum_of_duration = VALID_SUM_OF_DURATION)]
# 
# marg_count_per_day[count_per_day > 100]
# hcc_count_per_day[count_per_day > 100]
# ux_count_per_day[count_per_day > 100]
# 
# marg_count_per_day[count_per_day > MAX_COUNT_PER_DAY, ':='(count_per_day = MAX_COUNT_PER_DAY)]
# hcc_count_per_day[count_per_day > MAX_COUNT_PER_DAY, ':='(count_per_day = MAX_COUNT_PER_DAY)]
# ux_count_per_day[count_per_day > MAX_COUNT_PER_DAY, ':='(count_per_day = MAX_COUNT_PER_DAY)]


# 
# 
# ##
# ## Make 'datetime.all' for x-axis
# ##
# 
# datetime.all <- data.table(marg_dt$aggDay)
# setnames(datetime.all,"aggDay")
# datetime.all[, ':='(sum_of_duration = NA)][, ':='(count_per_day = NA)]
# datetime.all[, sum_of_duration := as.numeric(sum_of_duration)]
# datetime.all[, count_per_day := as.numeric(count_per_day)]
# 
# margRS_dt <- datetime.all
# # margRS_dt[which(aggDay >= as.Date(marg_RS$joined[1],format="%y-%m-%d") & aggDay <= "2016-6-30"), ]$sum_of_duration = 0
# # margRS_dt[which(aggDay >= as.Date(marg_RS$joined[1],format="%y-%m-%d") & aggDay <= "2016-6-30"), ]$count_per_day = 0
# uniqueDate = unique(marg_sum_of_duration_day$aggDay)
# 
# for(i in 1:length(uniqueDate)){
#   margRS_dt[min(which(margRS_dt$aggDay == uniqueDate[i]))]$sum_of_duration = marg_sum_of_duration_day[marg_sum_of_duration_day[,marg_sum_of_duration_day$aggDay==uniqueDate[i]],]$sum_of_duration
#   margRS_dt[min(which(margRS_dt$aggDay == uniqueDate[i]))]$count_per_day = marg_count_per_day[marg_count_per_day[,marg_count_per_day$aggDay==uniqueDate[i]],]$count_per_day
# }
# 
# hccRS_dt <- datetime.all
# # hccRS_dt[which(aggDay >= as.Date(hcc_RS$joined[1],format="%y-%m-%d") & aggDay <= "2016-6-30"), ]$sum_of_duration = 0
# # hccRS_dt[which(aggDay >= as.Date(hcc_RS$joined[1],format="%y-%m-%d") & aggDay <= "2016-6-30"), ]$count_per_day = 0
# uniqueDate = unique(hcc_sum_of_duration_day$aggDay)
# for(i in 1:length(uniqueDate)){
#   hccRS_dt[min(which(hccRS_dt$aggDay == uniqueDate[i]))]$sum_of_duration = hcc_sum_of_duration_day[hcc_sum_of_duration_day[,hcc_sum_of_duration_day$aggDay==uniqueDate[i]],]$sum_of_duration
#   hccRS_dt[min(which(hccRS_dt$aggDay == uniqueDate[i]))]$count_per_day = hcc_count_per_day[hcc_count_per_day[,hcc_count_per_day$aggDay==uniqueDate[i]],]$count_per_day
# }
# 
# uxRS_dt <- datetime.all
# # uxRS_dt[which(aggDay >= as.Date(ux_RS$joined[1],format="%y-%m-%d") & aggDay <= "2016-6-30"), ]$sum_of_duration = 0
# # uxRS_dt[which(aggDay >= as.Date(ux_RS$joined[1],format="%y-%m-%d") & aggDay <= "2016-6-30"), ]$count_per_day = 0
# uniqueDate = unique(ux_sum_of_duration_day$aggDay)
# for(i in 1:length(uniqueDate)){
#   uxRS_dt[min(which(uxRS_dt$aggDay == uniqueDate[i]))]$sum_of_duration = ux_sum_of_duration_day[ux_sum_of_duration_day[,ux_sum_of_duration_day$aggDay==uniqueDate[i]],]$sum_of_duration
#   uxRS_dt[min(which(uxRS_dt$aggDay == uniqueDate[i]))]$count_per_day = ux_count_per_day[ux_count_per_day[,ux_count_per_day$aggDay==uniqueDate[i]],]$count_per_day
# }
# 
# marg_dt[,sum_of_duration := margRS_dt$sum_of_duration][,count_per_day := margRS_dt$count_per_day]
# hcc_dt[,sum_of_duration := hccRS_dt$sum_of_duration][,count_per_day := hccRS_dt$count_per_day]
# ux_dt[,sum_of_duration := uxRS_dt$sum_of_duration][,count_per_day := uxRS_dt$count_per_day]
# 
# 
# ### -------------------------------- ###
# ### Build AllLabs data.table  
# ### -------------------------------- ### 
# 
# AllLabs_dt = marg_dt
# AllLabs_dt$computer = AllLabs_dt$computer + hcc_dt$computer + ux_dt$computer
# AllLabs_dt$light = AllLabs_dt$light + hcc_dt$light + ux_dt$light
# AllLabs_dt$etc = AllLabs_dt$etc + hcc_dt$etc + ux_dt$etc
# AllLabs_dt$total = AllLabs_dt$total + hcc_dt$total + ux_dt$total
# AllLabs_dt$sum_of_duration = AllLabs_dt$sum_of_duration + hcc_dt$sum_of_duration + ux_dt$sum_of_duration
# AllLabs_dt$count_per_day = AllLabs_dt$count_per_day + hcc_dt$count_per_day + ux_dt$count_per_day


### -------------------------------- ###
### Join with existing data.table to make whole data.table
### -------------------------------- ### 

marg_dt = merge(x = marg_dt, y = marg_RS_dt, by = "timestamp", all = TRUE)
hcc_dt = merge(x = hcc_dt, y = hcc_RS_dt, by = "timestamp", all.x = TRUE)
ux_dt = merge(x = ux_dt, y = ux_RS_dt, by = "timestamp", all.x = TRUE)


### -------------------------------- ###
### Build AllLabs data.table  
### -------------------------------- ### 

AllLabs_dt = marg_dt
AllLabs_dt$computer = AllLabs_dt$computer + hcc_dt$computer + ux_dt$computer
AllLabs_dt$light = AllLabs_dt$light + hcc_dt$light + ux_dt$light
AllLabs_dt$etc = AllLabs_dt$etc + hcc_dt$etc + ux_dt$etc
AllLabs_dt$total = AllLabs_dt$total + hcc_dt$total + ux_dt$total
AllLabs_dt$total_woHVAC = AllLabs_dt$total_woHVAC + hcc_dt$total_woHVAC + ux_dt$total_woHVAC
AllLabs_dt$total_woETC = AllLabs_dt$total_woETC + hcc_dt$total_woETC + ux_dt$total_woETC
AllLabs_dt$RS_duration = AllLabs_dt$RS_duration + hcc_dt$RS_duration + ux_dt$RS_duration
AllLabs_dt$RS_count = AllLabs_dt$RS_count + hcc_dt$RS_count + ux_dt$RS_count

### Build list of all data tables 
##
dt_list = list(marg_dt, hcc_dt, ux_dt, AllLabs_dt)
# dt_list = list(marg_dt, hcc_dt, ux_dt)

summary(marg_dt)
summary(hcc_dt)
summary(ux_dt)
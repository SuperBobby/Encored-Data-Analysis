library(ggplot2)
library(scales)
# library(zoo)
library(smoother)
library(data.table)

## -------------------- ##
### data loading 
load("marg_15min.RData")
load("hcc_15min.RData")
load("ux_15min.RData")

source("getSNUdata.R")
update_start = "2014-10-01"
update_end = "2016-7-2"

marg_defalut_table_15min = reviseSNUData(marg_defalut_table_15min, "marg", update_start, update_end, verbose = T)
hcc_defalut_table_15min = reviseSNUData( hcc_defalut_table_15min, "hcc",  update_start, update_end, verbose = T)
ux_defalut_table_15min = reviseSNUData(  ux_defalut_table_15min, "ux",   update_start, update_end, verbose = T)

## -------------------- ##
#### 기본 data.table 빌드 
marg_dt = data.table(marg_defalut_table_15min, index=rep(1:96, (nrow(marg_defalut_table_15min)/96)))
hcc_dt = data.table(hcc_defalut_table_15min, index=rep(1:96, (nrow(marg_defalut_table_15min)/96)))
ux_dt = data.table(ux_defalut_table_15min, index=rep(1:96, (nrow(marg_defalut_table_15min)/96)))

#### Seeking new date timming ==> 7am 
## 오전 7시를 기점으로 하루를 자르면 될 듯 (3개 랩 패턴 매우 유사함)
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

marg_dt = na.missing.data(marg_dt, 0.1)
hcc_dt = na.missing.data(hcc_dt, 0.05)
ux_dt = na.missing.data(ux_dt, 0.02)

### Computer
## abnormal computer usage : 
## hcc : computer usage over 0.5 = NA
##  ux : computer usage over 0.35 = NA

hcc_dt[computer > 0.5, ':='(computer = NA)]
ux_dt[computer > 0.35, ':='(computer = NA)]

### Light 
## 
hcc_dt[light > 0.5, ':='(light = NA)]
ux_dt[light > 0.5, ':='(light = NA)]


#------------------------------------------------------------------------------------preprocessing end!

expDate<-c(as.Date("2015-10-08"),
           as.Date("2015-12-01"),
           as.Date("2016-01-11"),
           as.Date("2016-02-01"),
           as.Date("2016-05-16"),
           as.Date("2016-06-13"))

rownum_expDate <- 1

for (d in expDate) {
  print(nrow(plot_dt[d>=plot_dt$get,]))
  rownum_expDate <- append(rownum_expDate, nrow(plot_dt[d>=plot_dt$get,]))
}

windowingWeek <- 2

# n: the number of samples for windowing
n <- windowingWeek*7+1

centered = TRUE   #centered=TRUE이면 windowingWeek은 짝수여야함


if (centered) {
  before <- floor  ((n-1)/2)
  after  <- ceiling((n-1)/2)
} else {
  before <- n-1
  after  <- 0
}

k <- 1

plot_dt <- return_dts[[40]]

# plot_dt$get[k] < expDate[1]

windowing <- rep(0,length(nrow(plot_dt)))



if(k < nrow_expDate[2]) {
  if(k <= before){
    windowing[k] <- mean(plot_dt$peak[1:k+after])
  } 
}














lab<-"MARG"

dataColumn <- "total"
# dataColumn <- "computer"
# dataColumn <- "light"
# dataColumn <- "hvac"
dt <- data.table(data_raw)


expDate<-c(as.numeric(as.Date("2015-10-08")),
           as.numeric(as.Date("2015-12-01")),
           as.numeric(as.Date("2016-01-11")),
           as.numeric(as.Date("2016-02-01")),
           as.numeric(as.Date("2016-05-16")),
           as.numeric(as.Date("2016-06-13")))

aggDay = c(rep((as.Date(dt$timestamp[1], tz="rok")-1),28), 
           as.Date(dt$timestamp, tz='rok')[29:(nrow(dt))-28])

dt = data.table(dt, aggDay)










# #weekday+weekend
# myPlot<-ggplot(data_raw, aes(x = timestamp, y=light)) + 
#   geom_point() +
# #   geom_line() +
#   geom_line(aes(y=simpleWindow,colour="simpleWindow")) + 
#   geom_line(aes(y=gaussianWindow,colour="gaussianWindow")) + 
#   scale_color_manual(name="",values=c("simpleWindow"="red", "gaussianWindow"="blue"))+
#   scale_x_date("timestamp", labels = date_format("%y-%m"), breaks = date_breaks("month")) +
#   theme(axis.text.x=element_text(size=8, angle=45, colour="black"))+
#   geom_vline(aes(xintercept=c(expDate)),color="green4")+
#   ggtitle(paste("MARG ", dataColumn," Windowing - Simple : before&after ",
#                 averagingWeeks,"weeks(=",averagingWeeks*2*7+1,"days) Gaussian : ",nrow(data_raw)*windowRatio," days"))
# 
# #weekday
# ggplot(data_raw, aes(x = timestamp, y=hvac)) + 
#   geom_point() +
#   #   geom_line() +
#   geom_line(aes(y=simpleWindow,colour="simpleWindow")) + 
#   geom_line(aes(y=gaussianWindow,colour="gaussianWindow")) + 
#   scale_color_manual(name="",values=c("simpleWindow"="red", "gaussianWindow"="blue"))+
#   scale_x_date("timestamp", labels = date_format("%y-%m"), breaks = date_breaks("month")) +
#   geom_vline(data=expDate,aes(xintercept=expDate),color="green4")+
#   ggtitle(paste("MARG weekend ", dataColumn," Windowing - Simple : before&after ",
#                 averagingWeeks,"weeks(=",averagingWeeks*2*7+1,"days) Gaussian : ",nrow(data_raw)*windowRatio," days"))
# 
# ggsave(filename=paste(lab,"_",dataColumn,".png"),plot=myPlot, width = 26.61, height = 18.2, units = "cm")
# 
# 
# 


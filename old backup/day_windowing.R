# marg_raw <- read.csv(file = "../rawData/marg_table_day.csv",header = TRUE)
# marg_raw$timestamp<-as.Date(strptime(marg_raw$timestamp, "%Y-%m-%d"))
# 
# #Plot the unsmoothe data(gray)
# dataColumn <- "total"
# # dataColumn <- "computer"
# # dataColumn <- "light"
# # dataColumn <- "hvac"


library(ggplot2)
library(scales)
library(zoo)
library(smoother)

# data_raw <- read.csv(file = "../rawData/marg_table_day.csv",header = TRUE)
# data_raw <- data_raw[data_raw$weekday==TRUE,]
# data_raw$timestamp<-as.Date(data_raw$timestamp)
# 
# lab<-"MARG"
# 
# # dataColumn <- "total"
# # dataColumn <- "computer"
# # dataColumn <- "light"
# dataColumn <- "hvac"
# y_raw <- data_raw[,c(dataColumn)]
# 
# y_zoo <- zoo(y_raw, data_raw$timestamp)
# averagingWeeks <- 2
# data_raw$simpleWindow <- coredata(rollmean(y_zoo, averagingWeeks*2*7+1, fill = list(NA, NULL, NA)))
# 
# windowRatio <- (averagingWeeks*2*7+1)/nrow(data_raw)
# data_raw$gaussianWindow <- smth(y_raw, window=windowRatio, method="gaussian")
# 
# expDate<-c(as.numeric(as.Date("2015-10-08")),
#            as.numeric(as.Date("2015-12-01")),
#            as.numeric(as.Date("2016-01-11")),
#            as.numeric(as.Date("2016-02-01")),
#            as.numeric(as.Date("2016-05-16")),
#            as.numeric(as.Date("2016-06-13")))
# expDate <- data.frame(expDate)
# 
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


expDate<-c(as.numeric(as.Date("2015-10-08")),
           as.numeric(as.Date("2015-12-01")),
           as.numeric(as.Date("2016-01-11")),
           as.numeric(as.Date("2016-02-01")),
           as.numeric(as.Date("2016-05-16")),
           as.numeric(as.Date("2016-06-13")))

windowPlot <- function(lab, dataColumn, when, averagingWeeks) {
  
  data_raw <- read.csv(file = paste("../rawData/",lab,"_table_day.csv",sep=""),header = TRUE)

  if(when=="weekday"){
    data_raw <- data_raw[data_raw$weekday==TRUE,]
  } else if(when=="weekend"){
    data_raw <- data_raw[data_raw$weekday==FALSE,]
  }
  
  data_raw$timestamp<-as.Date(data_raw$timestamp)
  
  y_raw <- data_raw[,c(dataColumn)]
  
  y_zoo <- zoo(y_raw, data_raw$timestamp)
  data_raw$simpleWindow <- coredata(rollmean(y_zoo, averagingWeeks*2*7+1, fill = list(NA, NULL, NA)))
  
  windowRatio <- (averagingWeeks*2*7+1)/nrow(data_raw)
  data_raw$gaussianWindow <- smth(y_raw, window=windowRatio, method="gaussian")
  
#   expDate<-c(as.numeric(as.Date("2015-10-08")),
#              as.numeric(as.Date("2015-12-01")),
#              as.numeric(as.Date("2016-01-11")),
#              as.numeric(as.Date("2016-02-01")),
#              as.numeric(as.Date("2016-05-16")),
#              as.numeric(as.Date("2016-06-13")))
  
  
  if(dataColumn=="computer"){
    myPlot<-ggplot(data_raw, aes(x = timestamp, y=computer))
  }else if(dataColumn=="light"){
    myPlot<-ggplot(data_raw, aes(x = timestamp, y=light))
  }else if(dataColumn=="hvac"){
    myPlot<-ggplot(data_raw, aes(x = timestamp, y=hvac))
  }else {
    myPlot<-ggplot(data_raw, aes(x = timestamp, y=total))
  }
  
  myPlot <- myPlot+ 
    geom_point(color="grey50") +
    #   geom_line() +
    geom_line(aes(y=simpleWindow,colour="simpleWindow")) + 
    geom_line(aes(y=gaussianWindow,colour="gaussianWindow")) + 
    scale_color_manual(name="",values=c("simpleWindow"="red", "gaussianWindow"="blue"))+
    scale_x_date("timestamp", labels = date_format("%y-%m"), breaks = date_breaks("month")) +
    theme(axis.text.x=element_text(size=8, angle=45, colour="black"))+
    ggtitle(paste(lab," ", dataColumn,"_",when," Windowing - Simple : before&after ",
                  averagingWeeks,"weeks(=",averagingWeeks*2*7+1,"days) Gaussian : ",nrow(data_raw)*windowRatio," days"))
  
  if(when=="weekday" || when=="weekend" ){
    df_expDate <- data.frame(expDate)    
    myPlot <- myPlot + geom_vline(data=df_expDate, aes(xintercept=expDate),color="green4")
  } else {
    myPlot <- myPlot + geom_vline(aes(xintercept=expDate),color="green4")
  }

  ggsave(filename=paste("../",lab,"_plot/",lab,"_",dataColumn,"_",when,"_window",averagingWeeks,"Weeks",".png",sep=""),plot=myPlot, width = 26.61, height = 18.2, units = "cm")
  
}

labName <- "marg"
labName <- "hcc"
labName <- "ux"

windowPlot(labName,"total","week",averagingWeeks=2)
windowPlot(labName,"total","weekday",averagingWeeks=2)
windowPlot(labName,"total","weekend",averagingWeeks=2)

windowPlot(labName,"computer","week",averagingWeeks=2)
windowPlot(labName,"computer","weekday",averagingWeeks=2)
windowPlot(labName,"computer","weekend",averagingWeeks=2)

windowPlot(labName,"light","week",averagingWeeks=2)
windowPlot(labName,"light","weekday",averagingWeeks=2)
windowPlot(labName,"light","weekend",averagingWeeks=2)

windowPlot(labName,"hvac","week",averagingWeeks=2)
windowPlot(labName,"hvac","weekday",averagingWeeks=2)
windowPlot(labName,"hvac","weekend",averagingWeeks=2)



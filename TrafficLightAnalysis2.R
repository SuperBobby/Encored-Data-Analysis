#install.packages("rjson")
require("rjson")
source("getSNUdata.R")
library(ggplot2)
library(reshape2)

## Functions #########################################################

accumulateUsage <- function(x, unit=24, verbose=T) {
  timestamp = x[,1]
  data = x[,2]
  accUsage = numeric(0)
  accSum = 0
  i = 1
  
  for(i in 1:length(data)){
    accSum = accSum + data[i]
    accUsage = c(accUsage, accSum)
    
    if(i %% unit == 0) {
      accSum = 0
    }
    if(i == length(data)){
      break
    }
    if(verbose){
      print(paste(i, timestamp[i]))
    }
  }
  return_table = data.frame(timestamp, accUsage)
  return(return_table)
}


getSavingRate <- function(x, shift, verbose=T) {
  last = x[1:(nrow(x)-shift),]
  this = x[(1+shift):nrow(x),]
  
  timestamp = this[,1]
  SavingRate = this[,2]/last[,2]
  
  return_table = data.frame(timestamp, SavingRate)
  print(summary(return_table))
  
  return(return_table)
}


plotTrafficLightHeatmap <- function(x, firstCut, secondCut, startDate, endDate) {
  
  if(missing(firstCut)){
    cat(paste("firstCut is missing. used defalut 0.95\n"))
    firstCut = 0.95
  }
  if(missing(secondCut)){
    cat(paste("secondCut is missing. used defalut 1.05\n"))
    secondCut = 1.05
  }
  
  x$section <- cut(x$SavingRate,breaks = c(-Inf,firstCut,secondCut,Inf),right = FALSE)
  x$hour<-rep(x=1:24)
  x$date<-as.factor(format(x$timestamp, "%Y%m%d"))
  x$date<-factor(x$date,levels=rev(levels(x$date)))
  
  startRow = which(x$timestamp==paste(startDate,"00:00:00"))
  endRow = which(x$timestamp==paste(endDate,"23:00:00"))
  
  print(startRow)
  print(endRow)
  
  p <- ggplot(data = x[startRow:endRow,], aes(x = hour, y = date)) + 
    geom_tile(aes(fill = section),colour="white") +
    scale_fill_manual(breaks=c("\\[-Inf,1)", "\\[1,1.2)", "\\[1.2,Inf)"), 
                      values = c("#3e721f", "#f7cb00", "#a50a0a")) +
    scale_x_discrete(limits=c(1:24)) +
    scale_y_discrete()
  
  print(p)
}


## Implements ########################################################

start = "2014-10-01"
cut = "2015-11-01"


# data loading from mongodb
marg_hours = getSNUData.sum("marg", "hours", start, cut, verbose = T)
hcc_hours = getSNUData.sum("hcc", "hours", start, cut, verbose = T)
ux_hours = getSNUData.sum("ux", "hours", start, cut, verbose = T)

marg_hours_acc = accumulateUsage(marg_hours, 24)
hcc_hours_acc = accumulateUsage(hcc_hours, 24)
ux_hours_acc = accumulateUsage(ux_hours, 24)

marg_hours_saving_rate = getSavingRate(marg_hours_acc, 24*7) # 24*7 : compare with 1 week before usage
hcc_hours_saving_rate = getSavingRate(hcc_hours_acc, 24*7)
ux_hours_saving_rate = getSavingRate(ux_hours_acc, 24*7)


fst = 1.0
snd = 1.20

# marg_hours_saving_rate2 <- marg_hours_saving_rate
# marg_hours_saving_rate2$section <- cut(marg_hours_saving_rate$SavingRate,breaks = c(-Inf,1.0,1.2,Inf),right = FALSE) # rate <1.0 , 1.0 <= rate <1.2 , 1.2< rate
# 
# 
# temp<-marg_hours_saving_rate2
# temp$hour<-rep(x=1:24)
# temp$date<-as.factor(format(temp$timestamp, "%Y%m%d"))
# 
# 
# temp2<-temp2[order(-temp2[,5],temp2[,1]),] #temp2[,5] : date, temp2[,1]: timestamp
# 
# 
# p <- ggplot(data =  temp[8593:nrow(temp),], aes(x = hour, y = date)) + 
#   geom_tile(aes(fill = section),colour="white") +
#   scale_fill_manual(breaks=c("\\[-Inf,1)", "\\[1,1.2)", "\\[1.2,Inf)"), 
#                               values = c("#3e721f", "#f7cb00", "#a50a0a")) +
#   scale_x_discrete(limits=c(1:24)) +
#   scale_y_discrete()
# 

temp3<-marg_hours_saving_rate
temp3$section <- cut(temp3$SavingRate,breaks = c(-Inf,1.0,1.2,Inf),right = FALSE)
temp3$hour<-rep(x=1:24)
temp3$date<-as.factor(format(temp3$timestamp, "%Y%m%d"))
temp3$date<-factor(temp3$date,levels=rev(levels(temp3$date)))

temp3<-temp3[nrow(temp3):1,]

p2 <- ggplot(data =  temp3[8593:9336,], aes(x = hour, y = date)) + 
  geom_tile(aes(fill = section),colour="white") +
  scale_fill_manual(breaks=c("\\[-Inf,1)", "\\[1,1.2)", "\\[1.2,Inf)"), 
                    values = c("#3e721f", "#f7cb00", "#a50a0a")) +
  scale_x_discrete(limits=c(1:24)) +
  scale_y_discrete()

plotTrafficLightHeatmap(marg_hours_saving_rate, fst, snd, "2015-10-01", "2015-10-31")
plotTrafficLightHeatmap(hcc_hours_saving_rate, fst, snd, "2015-10-01", "2015-10-31")
plotTrafficLightHeatmap(ux_hours_saving_rate, fst, snd, "2015-10-01", "2015-10-31")

temp3<-temp2
temp3$date <- factor(temp3$date,levels = rev(levels(temp3$date)),ordered = TRUE)

temp3$date <- reorder(temp3$date, temp3$timestamp)

temp3$date <- factor(temp3$date, levels=sort(temp3$date), decreasing=TRUE)

which(temp3$timestamp=="2015-10-01 00:00:00")
which(temp3$timestamp=="2015-10-31 23:00:00")

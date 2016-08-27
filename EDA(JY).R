par(mfrow=c(1,3))
pie(colSums(marg_defalut_table_15min[, 2:5]), main="MARG")
pie(colSums(hcc_defalut_table_15min[, 2:5]), main="HCC")
pie(colSums(ux_defalut_table_15min[, 2:5]), main="UX")

par(mfrow=c(1,1))

library(data.table)
library(lubridate)

marg_day <- fread("../rawData/marg_table_day.csv")
hcc_day <- fread("../rawData/hcc_table_day.csv")
ux_day <- fread("../rawData/ux_table_day.csv")

marg_day[, ':='(total_noHVAC = total-hvac)]
hcc_day[, ':='(total_noHVAC = total-hvac)]
ux_day[, ':='(total_noHVAC = total-hvac)]




marg_day[year(timestamp)==2014 & month(timestamp)==10]
marg_day[year(timestamp)==2015 & month(timestamp)==1 & (day(timestamp)>14) & (day(timestamp)<22)]






marg_day[year(timestamp)==2014 & month(timestamp)==10, 
         lapply(.SD, mean, na.rm=TRUE), by=weekday, .SDcols=c("computer", "light", "total_noHVAC") ] 


marg_day[year(timestamp)==2014 & month(timestamp)==10, 
         lapply(.SD, mean, na.rm=TRUE), by=day, .SDcols=c("computer", "light", "total_noHVAC") ] 



plot(marg_day[year(timestamp)==2016 & month(timestamp)==1]$total_noHVAC, type='l')
plot(hcc_day[year(timestamp)==2016 & month(timestamp)==1]$total_noHVAC, type='l')
plot(ux_day[year(timestamp)==2016 & month(timestamp)==1]$total_noHVAC, type='l')









tmp <- marg_dt[(aggDay > '2014-11-30') & (aggDay < '2015-04-01')]

tmp

plot(tmp$timestamp, tmp$computer)

tmp$aggDay <- as.factor(tmp$aggDay)

boxplot(computer ~ aggDay, tmp)






t1 = marg_dt[aggDay == '2015-01-06']
t2 = marg_dt[aggDay == '2015-01-07']
t3 = marg_dt[aggDay == '2015-01-15']
t4 = marg_dt[aggDay == '2015-01-16']




plot(t1$timestamp, t1$computer)
var(which(t1$computer > quantile(t1$computer, 0.9)))
var(which(t1$computer < quantile(t1$computer, 0.1)))

length(which(t1$computer < quantile(t1$computer, 0.1)))




sum(t2$computer > quantile(t2$computer, 0.9))

quantile(t2$computer, 0.9)


par(mfrow = c(4,1))
hist(t1$computer, 96, main="tmp")
hist(t2$computer, 96)
hist(t3$computer, 96)
hist(t4$computer, 96)




readkey <- function()
{
  cat ("Press [enter] to continue")
  line <- readline()
}






tmp = marg_dt[aggDay == '2015-01-07']

peak = quantile(tmp$computer, 0.9)
avg = mean(tmp$computer)
base = quantile(tmp$computer, 0.1)

plot(tmp$timestamp, tmp$computer)
abline(h=peak, col=4)
abline(h=avg, col=2)
abline(h=base, col=3)

text(90,3, labels=peak)


for(i in seq(0, 10, 0.01)){
  text(x = i, y = peak, labels=peak)
  
}





tmp1 <- MARG_aggDay_allDay_computer
tmp2 <- HCC_aggDay_allDay_computer
tmp3 <- UX_aggDay_allDay_computer


par(mfrow = c(3,1))
tmp1[, ':='(gap = base/peak)]
plot(tmp1$get, tmp1$gap)
tmp2[, ':='(gap = base/peak)]
plot(tmp2$get, tmp2$gap)
tmp3[, ':='(gap = base/peak)]
plot(tmp3$get, tmp3$gap)




tmp <- MARG_aggDay_allDay_computer[(get > '2014-11-30') & (get < '2015-09-01')& (isWeekend(get))]
tmp[,':='(base_ratio = base / avg)]
plot(as.Date(tmp$get), tmp$base_ratio)
mean(tmp$base_ratio)
tmp[which(tmp$base_ratio > 0.9)]



tmp <- MARG_aggDay_allDay_computer[(get > '2014-11-30') & (get < '2015-09-01')& (isWeekday(get))]
tmp[,':='(base_ratio = base / avg)]
plot(as.Date(tmp$get), tmp$base_ratio)
mean(tmp$base_ratio)




tmp <- HCC_aggDay_allDay_computer[(get > '2014-11-30') & (get < '2015-09-01') & (isWeekend(get))]
tmp[,':='(base_ratio = base / avg)]
plot(as.Date(tmp$get), tmp$base_ratio)
mean(tmp$base_ratio)

tmp <- HCC_aggDay_allDay_computer[(get > '2014-11-30') & (get < '2015-09-01') & (isWeekday(get))]
tmp[,':='(base_ratio = base / avg)]
tmp = na.omit(tmp)
plot(as.Date(tmp$get), tmp$base_ratio)
mean(tmp$base_ratio)





tmp <- UX_aggDay_allDay_computer[(get > '2014-11-30') & (get < '2015-09-01') & (isWeekend(get))]
tmp[,':='(base_ratio = base / avg)]
plot(as.Date(tmp$get), tmp$base_ratio)
mean(tmp$base_ratio)
tmp[which(tmp$base_ratio > 0.9)]


tmp <- UX_aggDay_allDay_computer[(get > '2014-11-30') & (get < '2015-09-01') & (isWeekday(get))]
tmp[,':='(base_ratio = base / avg)]
plot(as.Date(tmp$get), tmp$base_ratio)
mean(tmp$base_ratio)



library(data.table)

plot.point.with.3stats <- function(target_dt, target_folder, start_date, date_len){
  
  start_date = as.Date(start_date)
  
  for(i in c(0,1:date_len)){
    
    t_date = start_date + i
    
    print(t_date)
    dt = target_dt[aggDay == t_date]
    
    peak = quantile(dt$computer, 0.9)
    avg = mean(dt$computer)
    base = quantile(dt$computer, 0.1)
    
    # Base Something
    bs = (base) / avg
    # Peak Something
    ps = (avg) / peak
    
    
    png(paste0("../plots/",target_folder,"/", paste(t_date, weekdays(t_date)), ".png"))
    
    # par(mfrow = c(1,1))  
    # hist(dt$computer, 96, main=paste(t_date, weekdays(t_date)))
    
    plot_title = paste(target_folder, t_date, weekdays(t_date), round(bs,2), round(ps,2), '\n\n', 
                       round(peak,5), round(avg,5), round(base,5))
    
    plot(dt$timestamp, dt$computer, main=plot_title, ylim=c(0, peak))
    abline(h=peak, col=4)
    abline(h=avg, col=2)
    abline(h=base, col=3)
    # readkey()
    
    dev.off()
  }
}

plot.point.with.3stats(marg_dt, 'marg', '2015-01-05', 365)
plot.point.with.3stats(hcc_dt, 'hcc', '2015-01-05', 365)
plot.point.with.3stats(ux_dt, 'ux', '2015-01-05', 365)

---
title: "R Notebook"
output: html_notebook
---

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook. When you execute code within the notebook, the results appear beneath the code. 

Try executing this chunk by clicking the *Run* button within the chunk or by placing your cursor inside it and pressing *Ctrl+Shift+Enter*. 
```{r}
### HVAC something
## 2015-11-18 JY
library(data.table)

get.max.consecutive.on <- function(input, verbose=F){
  rle_return = rle(input)
  
  true_sequence = rle_return$lengths[rle_return$values]
  
  if(length(true_sequence) != 0) {
    result = max(true_sequence, na.rm = T)  
  } else {
    result = integer(0)
  }
  
  if(verbose == T) print((result))
  
  return(result)
}

tmp = marg_dt

tmp_hvac_cons = tmp[, .(hvac_on_duration = sum(hvac > 0.1),
                        max_consecutive_on_hvac = get.max.consecutive.on(hvac > 0.1)), by=aggDay]

tmp_hvac_cons[, (max_cons_ratio = max_consecutive_on_hvac/hvac_on_duration), by=aggDay]

plot(tmp_hvac_cons$aggDay, tmp_hvac_cons$hvac_on_duration)
plot(tmp_hvac_cons[, (max_cons_ratio = max_consecutive_on_hvac), by=aggDay])

# 
# summary(tmp$hvac > 0.1)
# 
# tmp[, .(hvac_on_duration = sum(hvac > 0.1)), by=aggDay]
# 
# 
# 
# get.max.consecutive.on(tmp$hvac > 0.1, T)
# 
# 
# plot(hvac_tmp)
# 
# tmp_x = c(F, F)
# 
# tmp_x = rle(tmp_x)
# 
# length(tmp_x$lengths[tmp_x$values])
# 
# max(tmp_x$lengths[tmp_x$values], na.rm = T)
# 
# get.max.consecutive.on(tmp_x)
#
```

```{r}






### ------------------------------------- ###
## plot.point.with.3stats
## 
## find the matrix(or index) for COMPUTER usage pattern 
## 2016. 8. 30. JY
### ------------------------------------- ###

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




plot(marg_dt[aggDay > '2015-01-05' & aggDay < '2015-01-20']$computer)



### ------------------------------------------------------ ###


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

```

```{r}
## sec data 전처리 시도
## 마땅히 하기 어려움
## riging, falling detection 알고리즘 상에서 해결봐야 할 듯 

library(data.table)

marg_sec = fread("../data/sec_tidy/marg_2015-09-02(com).csv")
hcc_sec = fread("../data/sec_tidy/hcc_2015-09-02(com).csv")

#

MARG_COM_THRES = c(1300, 800, 800) * 1000
HCC_COM_THRES = c(1800, 600) * 1000
UX_COM_THRES = c(1500) * 1000

marg_sec[marg_com1 > MARG_COM_THRES[1], ':='(marg_com1=NA)]
marg_sec[marg_com2 > MARG_COM_THRES[2], ':='(marg_com2=NA)]
marg_sec[marg_com3 > MARG_COM_THRES[3], ':='(marg_com3=NA)]

hcc_sec[hcc_com1 > HCC_COM_THRES[1], ':='(hcc_com1=NA)]
hcc_sec[hcc_com2 > HCC_COM_THRES[2], ':='(hcc_com2=NA)]

ux_sec[ux_com1 > UX_COM_THRES[1], ':='(ux_com1=NA)]


del.anomalies <- function(sec_dt, lab_label){
  # COM_THRES = list(marg = c(1300, 800, 800) * 1000,
  #                  hcc = c(1000, 600) * 1000,
  #                  ux = c(1500) * 1000)
  
  n_cols = length(sec_dt)
  
  for(index in 2:n_cols){
    sec_dt
    
  }
  COM_THRES[[lab_label]][1]
  
}

del.anomalies(marg_sec_raw, 'marg')


# par(mfrow = c(1,1))

for(i in 1:30){
  
  if(i < 10){
    day = paste0('0',i)  
  } else {
    day = as.character(i)
  }
  
  fname = paste0("../data/sec_tidy/marg_2015-09-", day, "(com).csv")
  sec_dt = fread(fname)
  com_usage = sec_dt$marg_com1
  hist(com_usage/1000, 1000, main=fname)
}



quantile(com_usage, .99, na.rm = T) / 1000
#


0. preprocessing - hist, set a threshold to cut the anomalies 
1. plot point color fix : explicit level define as a parameter of the function 'factor'?
2. y-axis scale : not fixed!
  3. magnitude 170 out of 180 secs 
4. make plot wider (180 * 180 + 1)

```

Add a new chunk by clicking the *Insert Chunk* button on the toolbar or by pressing *Ctrl+Alt+I*.

When you save the notebook, an HTML file containing the code and output will be saved alongside it (click the *Preview* button or press *Ctrl+Shift+K* to preview the HTML file).

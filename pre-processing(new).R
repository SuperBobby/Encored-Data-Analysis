
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

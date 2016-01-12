# data table 
lastYear_winter_marg_hours <- reviseSNUData(marg_defalut_table_hours, "marg", "2014-12-01", "2015-03-01", verbose = T)
thisYear_winter_marg_hours <- reviseSNUData(marg_defalut_table_hours, "marg", "2015-12-01", "2016-01-09", verbose = T)

lastYear_winter_hcc_hours <- reviseSNUData(hcc_defalut_table_hours, "hcc", "2014-12-01", "2015-03-01", verbose = T)
thisYear_winter_hcc_hours <- reviseSNUData(hcc_defalut_table_hours, "hcc", "2015-12-01", "2016-01-09", verbose = T)

lastYear_winter_ux_hours <- reviseSNUData(ux_defalut_table_hours, "ux", "2014-12-01", "2015-03-01", verbose = T)
thisYear_winter_ux_hours <- reviseSNUData(ux_defalut_table_hours, "ux", "2015-12-01", "2016-01-09", verbose = T)


## 95th percentile
quantile(lastYear_winter_marg_hours$total, .95) * 1.1   # 9.916728 --> 10.0
quantile(thisYear_winter_marg_hours$total, .95)         # 7.118

quantile(lastYear_winter_hcc_hours$total, .95) * 1.1    # 5.452001 --> 5.5
quantile(thisYear_winter_hcc_hours$total, .95)          # 4.41065

quantile(lastYear_winter_ux_hours$total, .95) * 1.1     # 2.633891 --> 2.6
quantile(thisYear_winter_ux_hours$total, .95)           # 2.6078 

# quantile(lastYear_winter_marg_hours$total, 1.0) # 9.015208 
# quantile(thisYear_winter_marg_hours$total, .95) # 7.118


## 90th percentile
quantile(lastYear_winter_marg_hours$total, .90) * 1.1   # 6.845204 --> 7
quantile(thisYear_winter_marg_hours$total, .90)         # 6.172

quantile(lastYear_winter_hcc_hours$total, .90) * 1.1    # 4.955059 --> 5
quantile(thisYear_winter_hcc_hours$total, .90)          # 3.9055

quantile(lastYear_winter_ux_hours$total, .90) * 1.1     # 2.358736 --> 2.5
quantile(thisYear_winter_ux_hours$total, .90)           #  2.4435



### LastYear vs ThisYear

## MARG
lastYear_winter_marg_15min <- reviseSNUData(marg_defalut_table_15min, "marg", "2014-12-01", "2015-03-01", verbose = T)
thisYear_winter_marg_15min <- reviseSNUData(marg_defalut_table_15min, "marg", "2015-12-01", "2016-01-06", verbose = T)

# weekDAY vs weekEND
weekDAY.vs.weekEND <- function(data) {
        
        weekDAY = subset(data, subset=data$weekday == T)
        weekEND = subset(data, subset=data$weekday == F)
                
        print(paste("weekDAY", mean(weekDAY$total)))
        print(paste("weekEND", mean(weekEND$total)))
}

weekDAY.vs.weekEND(lastYear_winter_marg_15min)
[1] "weekDAY 0.822951928044872"
[1] "weekEND 0.7218912975"
weekDAY.vs.weekEND(thisYear_winter_marg_15min)
[1] "weekDAY 0.958700806891026"
[1] "weekEND 0.772461458333333"


weekDAY.vs.weekEND(lastYear_winter_hcc_15min)
[1] "weekDAY 0.631999643589744"
[1] "weekEND 0.503465277916667"
weekDAY.vs.weekEND(thisYear_winter_hcc_15min)
[1] "weekDAY 0.656088963333333"
[1] "weekEND 0.533945833333333"


weekDAY.vs.weekEND(lastYear_winter_ux_15min)
[1] "weekDAY 0.299478010416667"
[1] "weekEND 0.20316178875"
weekDAY.vs.weekEND(thisYear_winter_ux_15min)
[1] "weekDAY 0.3320739225"
[1] "weekEND 0.252071875"


# EACH day of the week
eachDAY.of.week <- function(data){
        
        Mon = subset(data, subset=data$day == "월")
        Tue = subset(data, subset=data$day == "화")
        Wed = subset(data, subset=data$day == "수")
        Thu = subset(data, subset=data$day == "목")
        Fri = subset(data, subset=data$day == "금")
        Sat = subset(data, subset=data$day == "토")
        Sun = subset(data, subset=data$day == "일")
        
        print(mean(Mon$total)) 
        print(mean(Tue$total)) 
        print(mean(Wed$total))
        print(mean(Thu$total))
        print(mean(Fri$total))
        print(mean(Sat$total))
        print(mean(Sun$total))
}

eachDAY.of.week(lastYear_winter_marg_15min)
[1] 0.8926314
[1] 0.8098206
[1] 0.7541812
[1] 0.8091981
[1] 0.8489284
___________________
[1] 0.6609803
[1] 0.7878782
eachDAY.of.week(thisYear_winter_marg_15min)
[1] 0.9645625
[1] 1.016722
[1] 0.9232234
[1] 0.9450479
[1] 0.9323438
___________________
[1] 0.7544167
[1] 0.7905062


eachDAY.of.week(lastYear_winter_hcc_15min)
[1] 0.6754294
[1] 0.6738894
[1] 0.6429694
[1] 0.6047296
[1] 0.5629805
___________________
[1] 0.4979144
[1] 0.5094787
eachDAY.of.week(thisYear_winter_hcc_15min)
[1] 0.6658479
[1] 0.6722521
[1] 0.7400781
[1] 0.6679979
___________________
[1] 0.5342687
[1] 0.4705063
[1] 0.5973854


eachDAY.of.week(lastYear_winter_ux_15min)
[1] 0.3448405
[1] 0.32114
[1] 0.2965545
[1] 0.2624919
[1] 0.2723631
___________________
[1] 0.200995
[1] 0.2055092
eachDAY.of.week(thisYear_winter_ux_15min)
[1] 0.4190125
[1] 0.3420646
[1] 0.3720259
[1] 0.2933292
___________________
[1] 0.2339375
[1] 0.2204792
[1] 0.2836646





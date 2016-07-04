library(data.table)
library(ggplot2)
library(gridExtra)

library(timeDate)
library(zoo)

## -------------------- ##
### data loading 
source("Encored-Data-Analysis/getSNUdata.R")
update_start = "2014-10-01"
update_end = "2016-7-2"

marg_defalut_table_15min = reviseSNUData(marg_defalut_table_15min, "marg", update_start, update_end, verbose = T)
hcc_defalut_table_15min = reviseSNUData( hcc_defalut_table_15min, "hcc",  update_start, update_end, verbose = T)
ux_defalut_table_15min = reviseSNUData(  ux_defalut_table_15min, "ux",   update_start, update_end, verbose = T)

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
p1 = ggplot(marg_dt[, .(light_sum = sum(light)), by=index], aes(x=index, y=light_sum)) +
        geom_point() +
        scale_x_continuous(breaks = 1:96)
p2 = ggplot(hcc_dt[, .(light_sum = sum(light)), by=index], aes(x=index, y=light_sum)) +
        geom_point() +
        scale_x_continuous(breaks = 1:96)
p3 = ggplot(ux_dt[, .(light_sum = sum(light)), by=index], aes(x=index, y=light_sum)) +
        geom_point() +
        scale_x_continuous(breaks = 1:96)

grid.arrange(p1, p2, p3)

aggDay = c(rep((as.Date(marg_defalut_table_15min$timestamp[1], tz="rok")-1),28), 
           as.Date(marg_defalut_table_15min$timestamp, tz='rok')[29:(nrow(marg_defalut_table_15min))-28])

marg_dt = data.table(marg_dt, aggDay)
hcc_dt = data.table(hcc_dt, aggDay)
ux_dt = data.table(ux_dt, aggDay)

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
## abnormal computer usage : 
## hcc : computer usage over 0.5 = NA
##  ux : computer usage over 0.35 = NA

hcc_dt[computer > 0.5, ':='(computer = NA)]
ux_dt[computer > 0.35, ':='(computer = NA)]

# check the distributions
par(mfrow=c(2,3))
hist(marg_dt$computer, 100)
hist(hcc_dt$computer, 100)
hist(ux_dt$computer, 100)

hist(marg_dt$total, 100)
hist(hcc_dt$total, 100)
hist(ux_dt$total, 100)
par(mfrow=c(1,1))


### Light 
## 
hist(marg_dt[light>0.01]$light, 100) # no problem
hist(hcc_dt[light>0.01]$light, 100)  # abnormal : over 0.5
hist(ux_dt[light>0.01]$light, 100)   # abnormal : over 0.5

hcc_dt[light > 0.5, ':='(light = NA)]
ux_dt[light > 0.5, ':='(light = NA)]

light_min <- 0.1

### --------------------------- ###
### Build tables and functions for extraction & aggregation

return_dts = list(0)

## four stat : peak, base, avg, mid --> c(1,2,3,4)
get.four.stats <- function(usage, type){
        peak = quantile(usage, .95, na.rm = T)
        base = quantile(usage, .05, na.rm = T)
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
lab_names = c("MARG", "HCC", "UX")
dt_list = list(marg_dt, hcc_dt, ux_dt)

# 2. aggregation unit 
agg_Units = c("aggWeek", "aggDay")

# 3. day selection
day_selections = c("allDay", "weekDay", "weekEnd")

# 4. feeders
feeders = c("total", "computer", "light", "hvac")

for(lab in 1:3){ 
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
                        
                        for(feeder in feeders){
                                # feeder selection
                                # get(feeder)
                                
                                dt_name = paste(lab_name, agg_Unit, day_selection, feeder, sep="_")
                                print(dt_name)
                                
                                if(day_selection == "allDay"){
                                        
                                        return_dt = lab_dt[, .(peak = get.four.stats(get(feeder), 1),
                                                               base = get.four.stats(get(feeder), 2),
                                                               avg  = get.four.stats(get(feeder), 3),
                                                               med  = get.four.stats(get(feeder), 4)), by=get(agg_Unit)]
                                        
                                } else if(day_selection == "weekDay") {
                                        
                                        return_dt = lab_dt[weekday == T, .(peak = get.four.stats(get(feeder), 1),
                                                                           base = get.four.stats(get(feeder), 2),
                                                                           avg  = get.four.stats(get(feeder), 3),
                                                                           med  = get.four.stats(get(feeder), 4)), by=get(agg_Unit)]
                                        
                                } else if(day_selection == "weekEnd") {
                                        
                                        return_dt = lab_dt[weekday == F, .(peak = get.four.stats(get(feeder), 1),
                                                                           base = get.four.stats(get(feeder), 2),
                                                                           avg  = get.four.stats(get(feeder), 3),
                                                                           med  = get.four.stats(get(feeder), 4)), by=get(agg_Unit)]
                                }
                                
                                light_min = 0.01
                                light_peak = quantile(lab_dt$light, .95, na.rm = T)
                                light_dt = lab_dt[, .(timestamp = timestamp,
                                                      aggDay = aggDay,
                                                      aggWeek = aggWeek,
                                                      light = na.locf(light),
                                                      lightON = ifelse(na.locf(light) > light_min, 1, 0),
                                                      peak_50 = filter.fault.partial.lightOn(ifelse((na.locf(light) < light_peak * 0.5) & (na.locf(light) > light_min), 1, 0)),
                                                      peak_60 = filter.fault.partial.lightOn(ifelse((na.locf(light) < light_peak * 0.6) & (na.locf(light) > light_min), 1, 0)),
                                                      peak_70 = filter.fault.partial.lightOn(ifelse((na.locf(light) < light_peak * 0.7) & (na.locf(light) > light_min), 1, 0)),
                                                      peak_80 = filter.fault.partial.lightOn(ifelse((na.locf(light) < light_peak * 0.8) & (na.locf(light) > light_min), 1, 0)),
                                                      peak_90 = filter.fault.partial.lightOn(ifelse((na.locf(light) < light_peak * 0.9) & (na.locf(light) > light_min), 1, 0)))]
                                
                                partial_light = light_dt[, .(lightON = sum(lightON),
                                                             threshold50 = sum(peak_50)/sum(lightON),
                                                             threshold60 = sum(peak_60)/sum(lightON),
                                                             threshold70 = sum(peak_70)/sum(lightON),
                                                             threshold80 = sum(peak_80)/sum(lightON),
                                                             threshold90 = sum(peak_90)/sum(lightON)), by=get(agg_Unit)]
                                
                                return_dt = merge(return_dt, partial_light, by="get")
                                
                                assign(dt_name, return_dt)
                                return_dts = append(return_dts, setNames(list(return_dt),dt_name))
                        }
                }
        }
}


## check the returns 
# return_dts[15]
# marg_dt[, .(peak = get.four.stats(computer, 1),
#             base = get.four.stats(computer, 2),
#             avg  = get.four.stats(computer, 3),
#             med  = get.four.stats(computer, 4)), by=aggDay]


### ------------------------------------------- ###
### Plotting

## 1. partial_lightON & lightON duration plots 

for(i in 2:(length(return_dts))){
        # (length(return_dts)) 
        plot_dt   = return_dts[[i]]
        # plot_name = names(return_dts[i])
        # plot_name = paste0(names(return_dts[i]), "_partial_lightON")
        
        for(s in seq(0.1, 0.5, 0.1)) {
                      
                plot_name = paste(names(return_dts[i]), "- span", s)  
                print(plot_name)
                
                stats <- ggplot(plot_dt, aes(x=get)) +
                        geom_point(aes(y=peak, color='peak')) +
                        geom_smooth(aes(y=peak, color='peak'), span = s) +
                        
                        geom_point(aes(y=base, color='base')) +
                        geom_smooth(aes(y=base, color='base'), span = s) +
                        
                        geom_point(aes(y=avg, color='avg')) +
                        geom_smooth(aes(y=avg, color='avg'), span = s) +
                        
                        geom_point(aes(y=med, color='med')) +
                        geom_smooth(aes(y=med, color='med'), span = s) +
                        
                        scale_x_date("Timestamp", labels = date_format("%y-%m"), breaks = date_breaks("month")) +
                        
                        geom_vline(aes(xintercept = as.numeric(as.Date("2015-10-08"))),color="green4") +
                        geom_vline(aes(xintercept = as.numeric(as.Date("2015-12-01"))),color="green4") +
                        geom_vline(aes(xintercept = as.numeric(as.Date("2016-01-11"))),color="green4") +
                        geom_vline(aes(xintercept = as.numeric(as.Date("2016-02-01"))),color="green4") +
                        geom_vline(aes(xintercept = as.numeric(as.Date("2016-05-16"))),color="green4") +
                        geom_vline(aes(xintercept = as.numeric(as.Date("2016-06-13"))),color="green4") +
                        theme(legend.position = "bottom") + 
                        ggtitle(plot_name)
        
                
                if(grepl("light", plot_name) | grepl("total", plot_name)){
        
                        partial_lightON <- ggplot(plot_dt, aes(x=get)) +
                                
                                geom_point(aes(y=threshold50, color='threshold50')) +
                                geom_smooth(aes(y=threshold50, color='threshold50'), span = s) +    
                                
                                geom_point(aes(y=threshold60, color='threshold60')) +
                                geom_smooth(aes(y=threshold60, color='threshold60'), span = s) +             
                                
                                geom_point(aes(y=threshold70, color='threshold70')) +
                                geom_smooth(aes(y=threshold70, color='threshold70'), span = s) +             
                                
                                geom_point(aes(y=threshold80, color='threshold80')) +
                                geom_smooth(aes(y=threshold80, color='threshold80'), span = s) +  
                                
                                geom_point(aes(y=threshold90, color='threshold90')) +
                                geom_smooth(aes(y=threshold90, color='threshold90'), span = s) +  
                                
                                scale_x_date("Timestamp", labels = date_format("%y-%m"), breaks = date_breaks("month")) +
                                
                                geom_vline(aes(xintercept = as.numeric(as.Date("2015-10-08"))),color="green4") +
                                geom_vline(aes(xintercept = as.numeric(as.Date("2015-12-01"))),color="green4") +
                                geom_vline(aes(xintercept = as.numeric(as.Date("2016-01-11"))),color="green4") +
                                geom_vline(aes(xintercept = as.numeric(as.Date("2016-02-01"))),color="green4") +
                                geom_vline(aes(xintercept = as.numeric(as.Date("2016-05-16"))),color="green4") +
                                geom_vline(aes(xintercept = as.numeric(as.Date("2016-06-13"))),color="green4") +         
                                theme(legend.position = "bottom") +
                                ggtitle("partial_lightON")
                        
                        lightON_duration <- ggplot(plot_dt, aes(x=get, y=lightON)) +
                                
                                geom_point(aes(y=lightON, color='lightON')) +
                                geom_smooth(aes(y=lightON, color='lightON'), span = s) +          
                                
                                scale_x_date("Timestamp", labels = date_format("%y-%m"), breaks = date_breaks("month")) +
                                
                                geom_vline(aes(xintercept = as.numeric(as.Date("2015-10-08"))),color="red") +
                                geom_vline(aes(xintercept = as.numeric(as.Date("2015-12-01"))),color="red") +
                                geom_vline(aes(xintercept = as.numeric(as.Date("2016-01-11"))),color="red") +
                                geom_vline(aes(xintercept = as.numeric(as.Date("2016-02-01"))),color="red") +
                                geom_vline(aes(xintercept = as.numeric(as.Date("2016-05-16"))),color="red") +
                                geom_vline(aes(xintercept = as.numeric(as.Date("2016-06-13"))),color="red") +         
                                theme(legend.position = "bottom") +
                                ggtitle("lightON duration")
                        
                        plots <- arrangeGrob(stats, partial_lightON, lightON_duration)
                        ggsave(file = paste0("plots/",plot_name, ".png"), width = 25, height = 20, dpi = 300, plots)
                        
                } else {
                        plots <- stats
                        ggsave(file = paste0("plots/",plot_name, ".png"), width = 25, height = 10, dpi = 300, plots)
                }
        }
}


###
## 2. lunch light OFF plots 

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

for(lab in 1:3){ 
        # lab_dt & name selection 
        lab_dt = dt_list[[lab]]
        lab_name = lab_names[lab]
        
        lunch_dt_aggDay <- lab_dt[, .(aggWeek=as.Date(cut(aggDay, breaks = "week", start.on.monday = T)),
                                      lunch_lightOFF_50 = get.lunch.lightOFF(.SD, 0.5),
                                      lunch_lightOFF_60 = get.lunch.lightOFF(.SD, 0.6),
                                      lunch_lightOFF_70 = get.lunch.lightOFF(.SD, 0.7),
                                      lunch_lightOFF_80 = get.lunch.lightOFF(.SD, 0.8),
                                      lunch_lightOFF_90 = get.lunch.lightOFF(.SD, 0.9)), by=aggDay]
        
        lunch_dt_aggWeek <- lunch_dt_aggDay[, .(lunch_lightOFF_50 = sum(lunch_lightOFF_50),
                                                lunch_lightOFF_60 = sum(lunch_lightOFF_60),
                                                lunch_lightOFF_70 = sum(lunch_lightOFF_70),
                                                lunch_lightOFF_80 = sum(lunch_lightOFF_80),
                                                lunch_lightOFF_90 = sum(lunch_lightOFF_90)), by=aggWeek]
        
        for(s in seq(0.1, 0.5, 0.1)) {
                
                plot_name = paste(lab_name, "lunch light OFF count - span", s)  
                print(plot_name)
                
                p1 <- ggplot(lunch_dt_aggDay, aes(x=aggDay)) +
                        
                        geom_point(aes(y=lunch_lightOFF_50, color='lunch_lightOFF_50')) +
                        geom_smooth(aes(y=lunch_lightOFF_50, color='lunch_lightOFF_50'), span = s) +    
                        
                        geom_point(aes(y=lunch_lightOFF_60, color='lunch_lightOFF_60')) +
                        geom_smooth(aes(y=lunch_lightOFF_60, color='lunch_lightOFF_60'), span = s) +             
                        
                        geom_point(aes(y=lunch_lightOFF_70, color='lunch_lightOFF_70')) +
                        geom_smooth(aes(y=lunch_lightOFF_70, color='lunch_lightOFF_70'), span = s) +             
                        
                        geom_point(aes(y=lunch_lightOFF_80, color='lunch_lightOFF_80')) +
                        geom_smooth(aes(y=lunch_lightOFF_80, color='lunch_lightOFF_80'), span = s) +   
                        
                        geom_point(aes(y=lunch_lightOFF_90, color='lunch_lightOFF_90')) +
                        geom_smooth(aes(y=lunch_lightOFF_90, color='lunch_lightOFF_90'), span = s) +   
                        
                        scale_x_date("Timestamp", labels = date_format("%y-%m"), breaks = date_breaks("month")) +
                        
                        geom_vline(aes(xintercept = as.numeric(as.Date("2015-10-08"))),color="green4") +
                        geom_vline(aes(xintercept = as.numeric(as.Date("2015-12-01"))),color="green4") +
                        geom_vline(aes(xintercept = as.numeric(as.Date("2016-01-11"))),color="green4") +
                        geom_vline(aes(xintercept = as.numeric(as.Date("2016-02-01"))),color="green4") +
                        geom_vline(aes(xintercept = as.numeric(as.Date("2016-05-16"))),color="green4") +
                        geom_vline(aes(xintercept = as.numeric(as.Date("2016-06-13"))),color="green4") +         
                        theme(legend.position = "bottom") +
                        
                        ggtitle(plot_name)
                
                ggsave(file = paste0("plots/", plot_name, ".png"), width = 25, height = 10, dpi = 600, p1)
        }
}

###
## 3. light peak * (0.5~0.8) / 96:15mins ==> strong_light counting 

for(lab in 1:3){ 
        # lab_dt & name selection 
        lab_dt = dt_list[[lab]]
        lab_name = lab_names[lab]
        
        print(lab_name)
        
        light_peak = quantile(lab_dt$light, .95, na.rm = T)
        print(light_peak)
        
        strong_light_dt = lab_dt[, .(strong_light_50 = sum(light > (light_peak * 0.5)),
                                     strong_light_60 = sum(light > (light_peak * 0.6)),
                                     strong_light_70 = sum(light > (light_peak * 0.7)),
                                     strong_light_80 = sum(light > (light_peak * 0.8)),
                                     strong_light_90 = sum(light > (light_peak * 0.9))), by=aggDay]
        
        for(s in seq(0.1, 0.5, 0.1)) {
                
                plot_name = paste(lab_name, "strong_light count - span", s)  
                print(plot_name)
                
                p1 <- ggplot(strong_light_dt, aes(x=aggDay)) +
                        
                        geom_point(aes(y=strong_light_50, color='strong_light_50')) +
                        geom_smooth(aes(y=strong_light_50, color='strong_light_50'), span = s) +    
                        
                        geom_point(aes(y=strong_light_60, color='strong_light_60')) +
                        geom_smooth(aes(y=strong_light_60, color='strong_light_60'), span = s) +             
                        
                        geom_point(aes(y=strong_light_70, color='strong_light_70')) +
                        geom_smooth(aes(y=strong_light_70, color='strong_light_70'), span = s) +             
                        
                        geom_point(aes(y=strong_light_80, color='strong_light_80')) +
                        geom_smooth(aes(y=strong_light_80, color='strong_light_80'), span = s) +    
                        
                        geom_point(aes(y=strong_light_90, color='strong_light_90')) +
                        geom_smooth(aes(y=strong_light_90, color='strong_light_90'), span = s) +    
                        
                        scale_x_date("Timestamp", labels = date_format("%y-%m"), breaks = date_breaks("month")) +
                        
                        geom_vline(aes(xintercept = as.numeric(as.Date("2015-10-08"))),color="green4") +
                        geom_vline(aes(xintercept = as.numeric(as.Date("2015-12-01"))),color="green4") +
                        geom_vline(aes(xintercept = as.numeric(as.Date("2016-01-11"))),color="green4") +
                        geom_vline(aes(xintercept = as.numeric(as.Date("2016-02-01"))),color="green4") +
                        geom_vline(aes(xintercept = as.numeric(as.Date("2016-05-16"))),color="green4") +
                        geom_vline(aes(xintercept = as.numeric(as.Date("2016-06-13"))),color="green4") +         
                        theme(legend.position = "bottom") +
                        
                        ggtitle(plot_name)
                
                ggsave(file = paste0("plots/", plot_name, ".png"), width = 25, height = 10, dpi = 600, p1)
        }
}


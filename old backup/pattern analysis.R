###############################
#### showing pattern functions
#### barplot with errors 2015.3.12 Bob
###############################
source("Encored public functions.R")
library("ggplot2")
library("gridExtra")
library("scales")

barplot.withError = function(data, main_title, y_range){
        
        means  = sapply(data, mean, na.rm=T)
        stDevs = sapply(data, sd, na.rm=T)
        
        peak = quantile(data, 0.95, na.rm = T)
        base = quantile(data, 0.05, na.rm = T)
        print(paste("peak(95th percentiles):", peak))
        print(paste("base( 5th percentiles):", base))
        
        n = length(means)
        avg = mean(stack(data)$values, na.rm=T) * 96 
        mp = barplot(means, 
                     col=rep(topo.colors(n/4), each = 4),
                     main=main_title,
                     ylim=y_range,
                     xlab=paste("average usage/day:", round(avg,2)),
                     ylab="Usage (kW/15min)")
        box()
        
        segments(mp, means - stDevs, mp, means + stDevs, lwd=2)
        segments(mp - 0.1, means - stDevs, mp + 0.1, means - stDevs, lwd=2)
        segments(mp - 0.1, means + stDevs, mp + 0.1, means + stDevs, lwd=2)
        
        lines(c(-4,116), c(base,base), col="blue")
        text(x = -2, y = base*1.05, labels = round(base,2), lty = 2, offset=0.3)
        
        lines(c(-4,116), c(peak,peak), col="red")
        text(x = -2, y = peak*1.05, labels = round(peak,2), lty = 2, offset=0.3)
        
}

show.pattern = function(raw_data, target, start_date, weekend_index = c(3,4), weeks = 4, ymax, return_data = F){
        
        #     ## setting the mode: 15min/hour .... NO need for hour for pattern analysis...
        #     if(resolution == '15min'){
        #         data = raw_data
        #         unit = 96
        #     } else if(resolution == 'hour') {
        #         data = resolution.processor(raw_data = data, jump = 0, merge = 4)
        #         unit = 24
        #     } else {
        #         print(paste("resolution : 15min, hour"))
        #         break()
        #     }
        
        data = raw_data
        unit = 96
        
        # indexing
        start = which(data$X == start_date)
        indexes = seq(from = start, by = unit*7, length.out = weeks)
        
        ##  preparation for loop
        par(mfrow=c(weeks,2))
        
        sub_data = data[-1]
        result = matrix(rep(0, 7*unit), 7, unit)
        
        #for the return set
        return_row_names = character(0)
        return_result = matrix(0, 0, unit)
        
        ## auto y range ... estimation from the whole... need to adjust manually after the first try
        if(missing(ymax)){
                
                print("ymax missing... auto y_range")
                means  = sapply(raw_data[target], mean, na.rm=T)
                stDevs = sapply(raw_data[target], sd, na.rm=T)
                
                y_range = c(0, max(means + stDevs) * 1.4)
                #print(max(means + stDevs))
                print(y_range)
        } 
        else {
                y_range = c(0, ymax)
        }
        
        
        for (start_index in indexes){
                
                row_names = character(0)
                col_names = character(0)
                #print(start_index)
                
                # for a week -- 7days
                result = matrix(rep(0, 7*unit), 7, unit)
                
                for (i in 1:7){ 
                        
                        from = (start_index+unit*(i-1))
                        to = (start_index-1+unit*(i))    
                        
                        result[i,] = as.numeric(sub_data[from:to,target])
                        
                        row_names = c(row_names, strtrim(data[from,'X'], 10))
                        return_row_names = c(return_row_names, strtrim(data[from,'X'], 10))
                }
                
                col_names = substr(data[from:to,'X'], 12,16)
                
                # return set
                return_result = rbind(return_result, result)
                
                # local loop set
                result = as.data.frame(x = result, row.names = row_names)
                names(result) = col_names
                
                plot_title = paste(target, row_names[1], "~")
                
                # weekday, weekend by indexing
                weekday = result[-weekend_index,]
                weekend = result[ weekend_index,]
                
                #weekday = result[c(1,2,5,6,7),]
                #weekend = result[c(3,4),]
                
                if(!return_data) {
                        barplot.withError(weekday, main_title = paste(plot_title, "weekday"), y_range) 
                        barplot.withError(weekend, main_title = paste(plot_title, "weekend"), y_range) 
                }
        }
        
        return_result = as.data.frame(return_result)
        return_result = cbind(return_row_names , return_result)
        names(return_result) = c("timestamp", col_names)
        
        par(mfrow=c(1,1))
        
        if(return_data) {    
                return(return_result)
        }
}

avg.peak.base.plot = function(data, target, start_date, weekend_index = c(3,4), weeks = 4, average_only = F, ylims){
        
        unit = 96
        sub_data = data[,-1] # remove timestamp column
        
        # indexing
        start = which(data$X == start_date)
        indexes = seq(from = start, by = unit*7, length.out = weeks)
        
        #par(mfrow=c(3,1))
        
        #     weekday = data.frame(avg = numeric(), peak = numeric(), base = numeric())
        #     weekend = data.frame(avg = numeric(), peak = numeric(), base = numeric())
        weekday = matrix(rep(0, 3*weeks), weeks, 3)
        weekend = matrix(rep(0, 3*weeks), weeks, 3)
        
        weekday_timestamp = character(0)
        weekend_timestamp = character(0)
        week = 1
        
        for (start_index in indexes){
                row_names = character(0)
                col_names = character(0)
                
                result = matrix(rep(0, 7*unit), 7,unit)
                
                for (i in 1:7){ 
                        
                        from = (start_index+unit*(i-1))
                        to = (start_index-1+unit*(i))    
                        
                        result[i,] = as.numeric(sub_data[from:to,target])
                        
                        row_names = c(row_names, strtrim(data[from,'X'], 10))
                }
                
                col_names = substr(data[from:to,'X'], 12,16)
                #print(col_names)
                
                result = as.data.frame(x = result, row.names = row_names)
                names(result) = col_names
                
                # weekday, weekend... start from Thursday, so 1,2,5,6,7 are the weekdays
                weekday_data = result[-weekend_index,]
                weekend_data = result[ weekend_index,]
                
                avg_weekday = mean(stack(weekday_data)$values, na.rm=T) * 96 
                peak_weekday = quantile(weekday_data, 0.95, na.rm = T)
                base_weekday = quantile(weekday_data, 0.05, na.rm = T)
                ts_weekday = row.names(weekday_data)[1]
                
                avg_weekend = mean(stack(weekend_data)$values, na.rm=T) * 96 
                peak_weekend = quantile(weekend_data, 0.95, na.rm = T)
                base_weekend = quantile(weekend_data, 0.05, na.rm = T)
                ts_weekend = row.names(weekend_data)[1]        
                
                weekday[week, ] = c(avg_weekday, peak_weekday, base_weekday)
                weekend[week, ] = c(avg_weekend, peak_weekend, base_weekend)
                weekday_timestamp = c(weekday_timestamp, ts_weekday)
                weekend_timestamp = c(weekend_timestamp, ts_weekend)
                week = week + 1
        }
        
        ## tidy dataset containing the avg, peak, base with timestamp
        weekday = cbind(as.Date(weekday_timestamp), as.data.frame(weekday))
        weekend = cbind(as.Date(weekend_timestamp), as.data.frame(weekend))
        names(weekday) = c("weekday_timestamp", "avg", "peak", "base")
        names(weekend) = c("weekend_timestamp", "avg", "peak", "base")
        
        weekday_day_lable = weekdays(weekday$weekday_timestamp)
        weekend_day_lable = weekdays(weekend$weekend_timestamp)
        
        weekday = cbind(weekday, weekday_day_lable)
        weekend = cbind(weekend, weekend_day_lable)
        
        ## auto y range ... estimation from the whole... need to adjust manually after the first try
        ## ylims should contain c(ymax for average, ymax for peak_base)
        if(missing(ylims)){
                #if(is.na(ylims)) {
                print("ylims missing... auto y_range")
                avg_y_range = c(0, max(weekday$avg)*1.1)
                peak_y_range = c(0, max(weekday$peak)*1.1)
                print(paste("avg: 0~", round(avg_y_range[2],1), ", peak: 0~",round(peak_y_range[2],1), sep=""))
        } else {
                avg_y_range  = c(0, ylims[1])
                peak_y_range = c(0, ylims[2])
        }
        
        # tidy dataset containing avg, peak and base with timestamp
        average = cbind(weekday[,1:2], weekend[,2]); names(average) <- c("time_stamp", "weekday", "weekend")
        
        print(average)    
        print(weekday)
        print(weekend)
        #write.csv(weekday, file="data/weekday.csv")
        #write.csv(weekend, file="data/weekend.csv")
        #return(weekday)
        
        average_title = paste("Average Usage (", target, ")", sep="")
        weekday_title = paste("Weekday Peak & Base (", target, ")", sep="")
        weekend_title = paste("Weekend Peak & Base (", target, ")", sep="")
        
        ## plots
        #1. average
        aveg_plot <- ggplot(average, aes(x = time_stamp)) +
                ggtitle(average_title) +
                scale_x_date("Timestamp", labels = date_format("%m/%d"), breaks = date_breaks("week")) +
                scale_y_continuous("Average Usage (KW/h)", limits = avg_y_range) +
                geom_point(aes(y= weekday, color="weekday")) +
                geom_text(aes(y= weekday, label=round(weekday,2), vjust=-1)) + 
                geom_line(aes(y= weekday, color="weekday")) +
                geom_point(aes(y= weekend, color="weekend")) +
                geom_text(aes(y= weekend, label=round(weekend,2), vjust=+2)) + 
                geom_line(aes(y= weekend, color="weekend")) +
                theme(legend.title=element_blank())
        print(aveg_plot)
        
        if(!average_only){
                #2. weekday peak/base
                weekday_plot <- ggplot(weekday, aes(x = weekday_timestamp)) +
                        ggtitle(weekday_title) +
                        scale_x_date("Timestamp", labels = date_format("%m/%d"), breaks = date_breaks("week")) +
                        scale_y_continuous("Average Usage (kW/15min)", limits = peak_y_range) +
                        geom_point(aes(y= peak, color="peak")) +
                        geom_text(aes(y= peak, label=round(peak,2), vjust=-1)) + 
                        geom_line(aes(y= peak, color="peak")) +
                        geom_point(aes(y= base, color="base")) +
                        geom_text(aes(y= base, label=round(base,2), vjust=+2)) + 
                        geom_line(aes(y= base, color="base")) +
                        theme(legend.title=element_blank())
                print(weekday_plot)
                
                #3. weekend peak/base
                weekend_plot <- ggplot(weekend, aes(x = weekend_timestamp)) +
                        ggtitle(weekend_title) +
                        scale_x_date("Timestamp", labels = date_format("%m/%d"), breaks = date_breaks("week")) +
                        scale_y_continuous("Average Usage (kW/15min)", limits = peak_y_range) +
                        geom_point(aes(y= peak, color="peak")) +
                        geom_text(aes(y= peak, label=round(peak,2), vjust=-1)) + 
                        geom_line(aes(y= peak, color="peak")) +
                        geom_point(aes(y= base, color="base")) +
                        geom_text(aes(y= base, label=round(base,2), vjust=+2)) + 
                        geom_line(aes(y= base, color="base")) +
                        theme(legend.title=element_blank())
                print(weekend_plot)
                
                grid.arrange(aveg_plot, weekday_plot, weekend_plot, nrow=3)
        }
        
}


####################################
## data loading & preprocessing  ##
####################################
marg_15min = read.csv("../raw_data/marg_15min_raw.csv")
hcc_15min = read.csv("../raw_data/hcc_15min_raw.csv")
ux_15min = read.csv("../raw_data/ux_15min_raw.csv")

## com missing -> NA (rule: < 0.1)
marg_15min = missing.to.NA(data = marg_15min, indicator = "D406.3", 0.1)

hcc_15min$D410.5[hcc_15min$D410.5 > 300000] = 0
hcc_15min$D410.8[hcc_15min$D410.8 > 300000] = 0
hcc_15min = missing.to.NA(data = hcc_15min, indicator = "D410.5", 0.1)

## 15min -> hour
#marg_hour = resolution.processor(data = marg_15min, jump = 0, merge = 4)

## mW/h -> kW/h
marg_15min[-1] = marg_15min[-1]/1000000
hcc_15min[-1] = hcc_15min[-1]/1000000

marg_15min = marg.add.summation(marg_15min)
hcc_15min = hcc.add.summation(hcc_15min)


##################
### analysis  ####
##################
## computer
# marg
show.pattern(marg_15min, "marg_com_sum", "2014-12-25 0:00", weeks = 4)
show.pattern(marg_15min, "marg_com_sum", "2015-02-26 0:00", weeks = 8)
avg.peak.base.plot(marg_15min, "marg_com_sum", "2014-11-06 0:00", weeks = 4)
# hcc
show.pattern(hcc_15min, "hcc_com_sum", "2014-12-25 00:00", weeks = 9, ymax = .4)
show.pattern(hcc_15min, "hcc_com_sum", "2015-02-26 00:00", weeks = 9, ymax = .4)
avg.peak.base.plot(hcc_15min, "hcc_com_sum", "2014-12-25 00:00", weeks = 18)

## light
avg.peak.base.plot(marg_15min, "D406.light", "2014-12-25 0:00", weeks = 18, average_only=T)
avg.peak.base.plot(hcc_15min, "D410.light", "2014-12-25 00:00", weeks = 18, average_only=T)

## all
avg.peak.base.plot(marg_15min, "marg_total", "2014-10-02 0:00", weeks = 30, average_only=T)
avg.peak.base.plot(hcc_15min, "hcc_total", "2014-10-02 00:00", weeks = 30, average_only=T)



## clustering ... just try... weekday vs weekend?
com_dataset = show.pattern(marg_15min, "marg_com_sum", "2014-08-05 00:00", weeks = 30, return_data = T)
com_dataset = add.daylabel(com_dataset, start ="Tue")
km.out = kmeans(na.omit(com_dataset[-c(1, 98, 99)]), centers = 2, nstart = 20)
table(com_dataset$day[km.out$cluster == 2])
table(com_dataset$type[km.out$cluster == 2])

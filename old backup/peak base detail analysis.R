## by J.Y.Han 2015.5.30.

source("public functions for Encored log data.R")

library(plyr)
library(stringr)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(scales)

## functions
peak.base.detail.analysis = function(data, start_date, target, weeks){
        unit = 96
        
        if(target == "MARG"){
                light_label        = "D406.light"
                com_sum_label      = "marg_com_sum"
                total_column_label = "marg_total"
        }else if(target == "HCC"){
                light_label        = "D410.light"
                com_sum_label      = "hcc_com_sum"
                total_column_label = "hcc_total"
        }else if(target == "UX"){
                light_label        = "D409.light"
                com_sum_label      = "ux_com_sum"
                total_column_label = "ux_total"
        }
        
        # smart index finder (auto selection 00:00 or 0:00)
        start_date_time = paste(start_date, "00:00")
        start = which(data$X == start_date_time)
        
        if(length(start)){
                return  
        } else { 
                start_date_time = paste(start_date, "0:00")
                start = which(data$X == start_date_time)
        }
        
        indexes = seq(from = start, by = unit*7, length.out = weeks)
        
        # a list to collect day_peck & day_base data.frame
        list_for_return = list()
        
        cat("<< Peak & Base data frame >>\n")
        for (start_index in indexes){
                
                ## find peak&base for each day during a week
                for (i in 1:7){ 
                        
                        from = (start_index+unit*(i-1))
                        to = (start_index-1+unit*(i))    
                        
                        a_day_data = data[from:to,]
                        
                        day_peak = quantile(a_day_data[,total_column_label], 0.95, na.rm = T)
                        day_base = quantile(a_day_data[,total_column_label], 0.05, na.rm = T)
                        
                        # preventing from choice of NA for peak || base
                        total_vector = a_day_data[[total_column_label]]
                        total_vector[is.na(total_vector)] = 10000
                        
                        day_peak_index = which.min(abs(total_vector - day_peak))
                        day_base_index = which.min(abs(total_vector - day_base))
                        
                        #print(paste("day_peak:", a_day_data[day_peak_index,1], day_peak, "(", a_day_data[day_peak_index, total_column_label], ")"))
                        #print(paste("day_base:", a_day_data[day_base_index,1], day_base, "(", a_day_data[day_base_index, total_column_label], ")"))
                        #cat("\n")
                        
                        peak_row = c(type       = "day_peak",
                                     timestamp  = as.character(a_day_data[day_peak_index,1]),
                                     time_label = str_sub(as.character(a_day_data[day_peak_index,1]), 12, 16),
                                     day_label  = weekdays(as.Date(a_day_data[day_peak_index,1])),
                                     light      = a_day_data[day_peak_index, light_label], 
                                     computer   = a_day_data[day_peak_index, com_sum_label], 
                                     etc        = a_day_data[day_peak_index, total_column_label]-a_day_data[day_peak_index, light_label]-a_day_data[day_peak_index, com_sum_label],
                                     total      = a_day_data[day_peak_index, total_column_label])
                        
                        
                        base_row = c(type       = "day_base",
                                     timestamp  = as.character(a_day_data[day_base_index,1]),
                                     time_label = str_sub(as.character(a_day_data[day_base_index,1]), 12, 16),
                                     day_label  = weekdays(as.Date(a_day_data[day_base_index,1])),
                                     light      = a_day_data[day_base_index, light_label], 
                                     computer   = a_day_data[day_base_index, com_sum_label], 
                                     etc        = a_day_data[day_base_index, total_column_label]-a_day_data[day_base_index, light_label]-a_day_data[day_base_index, com_sum_label], 
                                     total      = a_day_data[day_base_index, total_column_label])
                        
                        list_for_return = append(list_for_return, list(rbind(peak_row, base_row)))
                }
        }
        
        # combine day_peak&day_base data to form a final peak&base data.frame
        peak_base_data_frame=ldply(list_for_return, data.frame)
        
        # data type setting
        peak_base_data_frame$type       = as.factor(peak_base_data_frame$type)
        peak_base_data_frame$timestamp  = as.Date(peak_base_data_frame$timestamp)
        peak_base_data_frame$time_label = as.character(peak_base_data_frame$time_label)
        peak_base_data_frame$day_label  = as.character(peak_base_data_frame$day_label)
        peak_base_data_frame$light      = as.numeric(as.character(peak_base_data_frame$light))
        peak_base_data_frame$computer   = as.numeric(as.character(peak_base_data_frame$computer))
        peak_base_data_frame$etc        = as.numeric(as.character(peak_base_data_frame$etc))
        row.names(peak_base_data_frame) = NULL
        
        print(summary(peak_base_data_frame))
        
        ## drawing some detail plots using peak&base data.frame
        #1. cumulative bar plot
        cumulative.bar.plot(peak_base_data_frame, target)
        #2. peak & base timming plot
        time_levels = str_sub(data$X[1:96], 12, 16)
        peak.basetime.plot(peak_base_data_frame, time_levels, target)
        
}

cumulative.bar.plot = function(data, target){
        cat("<< Detail Peak & Base Plotting >>\n")
        
        # subset: weekday vs weekend
        # weekend index
        Sat_index = which(data$day_label == "Saturday")
        Sun_index = which(data$day_label == "Sunday")
        weedEND_index = c(Sat_index, Sun_index)
        
        weekDAY_data = data[-weedEND_index,]
        weekEND_data = data[ weedEND_index,]
        
        # subset: peak vs base
        peak_data_weekDAY = subset(weekDAY_data, subset=data$type=="day_peak")
        peak_data_weekEND = subset(weekEND_data, subset=data$type=="day_peak")
        
        base_data_weekDAY = subset(weekDAY_data, subset=data$type=="day_base")
        base_data_weekEND = subset(weekEND_data, subset=data$type=="day_base")
        
        # 1. peak plotting
        DF_peak_weekDAY = melt(peak_data_weekDAY[,c(2,5,6,7)], id.var="timestamp")
        DF_peak_weekEND = melt(peak_data_weekEND[,c(2,5,6,7)], id.var="timestamp")
        
        peak_weekDAY = ggplot(DF_peak_weekDAY, aes(x=timestamp, y=value, fill=variable))+
                scale_x_date("Timestamp", labels = date_format("%b %d"), breaks = date_breaks("week")) +
                theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                xlab("\nTime Stamp") +
                ylab("kW/15min\n") +    
                scale_y_continuous(limits=c(0, 1.0)) +
                geom_bar(stat="identity") +
                guides(fill=FALSE) +
                ggtitle(paste("[Figure 1] Peak Details (WeekDay) -", target))
        
        
        peak_weekEND = ggplot(DF_peak_weekEND, aes(x=timestamp, y=value, fill=variable))+
                scale_x_date("Timestamp", labels = date_format("%b %d"), breaks = date_breaks("week")) +
                theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                xlab("\nTime Stamp") +
                ylab("kW/15min\n") +    
                scale_y_continuous(limits=c(0, 1.0)) +
                geom_bar(stat="identity") +
                ggtitle(paste("[Figure 2] Peak Details (WeekEND) -", target))
        
        
        ## 2. base plotting
        DF_base_weekDAY = melt(base_data_weekDAY[,c(2,5,6,7)], id.var="timestamp")
        DF_base_weekEND = melt(base_data_weekEND[,c(2,5,6,7)], id.var="timestamp")
        
        base_weekDAY = ggplot(DF_base_weekDAY, aes(x=timestamp, y=value, fill=variable))+
                scale_x_date("Timestamp", labels = date_format("%b %d"), breaks = date_breaks("week")) +
                theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                xlab("\nTime Stamp") +
                ylab("kW/15min\n") + 
                scale_y_continuous(limits=c(0, 1.0)) +
                geom_bar(stat="identity") +
                guides(fill=FALSE) +
                ggtitle(paste("[Figure 3] Base Details (WeekDay)", target))
        
        
        base_weekEND = ggplot(DF_base_weekEND, aes(x=timestamp, y=value, fill=variable))+
                scale_x_date("Timestamp", labels = date_format("%b %d"), breaks = date_breaks("week")) +
                theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                xlab("\nTime Stamp") +
                ylab("kW/15min\n") + 
                scale_y_continuous(limits=c(0, 1.0)) +
                geom_bar(stat="identity") +
                ggtitle(paste("[Figure 4] Base Details (WeekEND)", target))
        
        
        grid.arrange(peak_weekDAY, peak_weekEND, base_weekDAY, base_weekEND, nrow=2, ncol=2)
}

peak.basetime.plot = function(data, time_levels, target){
        
        # 1. before, during, after the experiment
        when = rep("during", nrow(data))
        when[data$timestamp < as.Date("2015-01-15")] = "before"
        when[data$timestamp > as.Date("2015-01-22")] = "in 2 weeks"
        when[data$timestamp > as.Date("2015-01-29")] = "in 4 weeks"
        when[data$timestamp > as.Date("2015-02-05")] = "after 4 weeks"
        
        
        df_for_plot = data.frame(type = data$type, 
                                 time_label = factor(data$time_label, levels=time_levels),
                                 day_label = data$day_label,
                                 when = factor(when, levels=c("before", "during", "in 2 weeks", "in 4 weeks", "after 4 weeks")))
        
        # subset: weekday vs weekend
        # weekend index
        Sat_index = which(df_for_plot$day_label == "Saturday")
        Sun_index = which(df_for_plot$day_label == "Sunday")
        weedEND_index = c(Sat_index, Sun_index)
        
        weekDAY_data = df_for_plot[-weedEND_index,]
        weekEND_data = df_for_plot[ weedEND_index,]
        
        
        peak_hist_weekDAY = ggplot(weekDAY_data[weekDAY_data$type=="day_peak",], aes(x=time_label, fill=when, orger=when)) +
                scale_x_discrete(drop=FALSE) +
                theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                geom_histogram() +
                guides(fill=FALSE) +
                ggtitle(paste("[Figure 1] Peak Timming (WeekDay) -", target))
        
        peak_hist_weekEND = ggplot(weekEND_data[weekEND_data$type=="day_peak",], aes(x=time_label, fill=when, orger=when)) +
                scale_x_discrete(drop=FALSE) +
                theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                geom_histogram() +
                ggtitle(paste("[Figure 2] Peak Timming (WeekEnd) -", target))
        
        base_hist_weekDAY = ggplot(weekDAY_data[weekDAY_data$type=="day_base",], aes(x=time_label, fill=when, , orger=when)) + 
                scale_x_discrete(drop=FALSE) +
                theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                geom_histogram() +
                guides(fill=FALSE) +
                ggtitle(paste("[Figure 3] Base Timming (WeekDay) -", target))
        
        base_hist_weekEND = ggplot(weekEND_data[weekEND_data$type=="day_base",], aes(x=time_label, fill=when, , orger=when)) + 
                scale_x_discrete(drop=FALSE) +
                theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                geom_histogram() +
                ggtitle(paste("[Figure 4] Base Timming (WeekEnd) -", target))
        
        
        grid.arrange(peak_hist_weekDAY, peak_hist_weekEND,
                     base_hist_weekDAY, base_hist_weekEND, nrow=2, ncol=2)
}

####################################
## data loading & preprocessing  ##
####################################
marg_15min = read.csv("data/raw/marg_15min_raw.csv")
hcc_15min = read.csv("data/raw/hcc_15min_raw.csv")
ux_15min = read.csv("data/raw/ux_15min_raw.csv")

## com missing -> NA (rule: < 0.1)
marg_15min = missing.to.NA(data = marg_15min, indicator = "D406.3", 0.1)
ux_15min = missing.to.NA(data = ux_15min, indicator = "D409.5", 0.1)

hcc_15min$D410.5[hcc_15min$D410.5 > 300000] = 0
hcc_15min$D410.8[hcc_15min$D410.8 > 300000] = 0
hcc_15min = missing.to.NA(data = hcc_15min, indicator = "D410.5", 0.1)

## 15min -> hour
#marg_hour = resolution.processor(data = marg_15min, jump = 0, merge = 4)

## mW/h -> kW/h
marg_15min[-1] = marg_15min[-1]/1000000
hcc_15min[-1]  =  hcc_15min[-1]/1000000
ux_15min[-1]  =  ux_15min[-1]/1000000

marg_15min = marg.add.summation(marg_15min)
hcc_15min = hcc.add.summation(hcc_15min)
ux_15min = ux.add.summation(ux_15min)

######################################
## analysis
######################################
# peak.base.detail.analysis(marg_15min, "2014-12-18", target = "MARG",  weeks = 14)
# peak.base.detail.analysis(hcc_15min, "2014-12-18", target = "HCC",  weeks = 14)

peak.base.detail.analysis(marg_15min, "2014-11-01", target = "MARG",  weeks = 4)
#peak.base.detail.analysis(hcc_15min, "2014-11-06", target = "HCC",  weeks = 25)
#peak.base.detail.analysis(ux_15min, "2014-11-06", target = "UX",  weeks = 25)


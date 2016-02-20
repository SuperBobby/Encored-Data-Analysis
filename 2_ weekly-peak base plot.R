###############################
#### showing pattern functions
#### barplot with errors with SNU API
#### 2015.10.30 Han.J.Y.
###############################
source("Encored-Data-Analysis/getSNUdata.R")

library(plyr)
library(stringr)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(scales)
library(timeDate)

avg.peak.base.plot = function(data, target, label, start_date, weeks = 4, average_only = F, ylims){
        
        unit = 96
        sub_data = data[,-1] # remove timestamp column
        
        # indexing
        start = which(data[,1] == start_date)
        indexes = seq(from = start, by = unit*7, length.out = weeks)
        
        #par(mfrow=c(3,1))
        
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
                        
                        row_names = c(row_names, strtrim(data[from, 1], 10))
                }
                
                col_names = substr(data[from:to, 1], 12,16)
                
                result = as.data.frame(x = result, row.names = row_names)
                names(result) = col_names
                
                weekday_data = result[isWeekday(row_names),]
                weekend_data = result[isWeekend(row_names),]
                
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
        
        average_title = paste(label, ": Average Usage (", target, " : ", average$time_stamp[1], " ~ for ", weeks, " weeks)", sep="")
        weekday_title = paste(label, ": Weekday Peak & Base (", target, ")", sep="")
        weekend_title = paste(label, ": Weekend Peak & Base (", target, ")", sep="")
        
        ## plots
        # The palette with grey:
        cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
        # The palette with black:
        cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
        
        #1. average
        aveg_plot <- ggplot(average, aes(x = time_stamp)) +
                ggtitle(average_title) +
                scale_x_date("Timestamp", labels = date_format("%m/%d"), breaks = date_breaks("week")) +
                scale_y_continuous("Average Usage (KW/h)", limits = avg_y_range) +
                scale_colour_manual(values=cbPalette) + 
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


##################
### analysis  ####
##################
from_date = "2015-11-02"
plot_weeks = 16

from_date = "2014-09-08"
plot_weeks = 75

avg.peak.base.plot(marg_defalut_table_15min, target="total", label = "MARG", from_date, weeks = plot_weeks)
avg.peak.base.plot( hcc_defalut_table_15min, target="total", label = "HCC",  from_date, weeks = plot_weeks)
avg.peak.base.plot(  ux_defalut_table_15min, target="total", label = "UX",   from_date, weeks = plot_weeks)


avg.peak.base.plot(marg_defalut_table_15min, target="computer", label = "MARG", from_date, weeks = plot_weeks)
avg.peak.base.plot(marg_defalut_table_15min, target="light",    label = "MARG", from_date, weeks = plot_weeks)
avg.peak.base.plot(marg_defalut_table_15min, target="hvac",     label = "MARG", from_date, weeks = plot_weeks)



avg.peak.base.plot(hcc_defalut_table_15min, target="computer", label = "HCC", from_date, weeks = plot_weeks)
avg.peak.base.plot(hcc_defalut_table_15min, target="light",    label = "HCC", from_date, weeks = plot_weeks)


avg.peak.base.plot(ux_defalut_table_15min, target="computer", label = "UX", from_date, weeks = plot_weeks)
avg.peak.base.plot(ux_defalut_table_15min, target="light",    label = "UX", from_date, weeks = plot_weeks)


# install.packages("lubridate")
# install.packages("bit64")

library(lubridate)
library(plyr)
library(ggplot2)
library(scales)
library(reshape2)
library(gridExtra)
library(data.table)
library(timeDate)
require(bit64)

table.time2string <- function(RS_raw_data){
        RS_raw_data$joined = as.POSIXct(RS_raw_data$joined/1000, origin = "1970-01-01", tz="ROK")
        RS_raw_data$leaved = as.POSIXct(RS_raw_data$leaved/1000, origin = "1970-01-01", tz="ROK")
        
        duration = difftime(RS_raw_data$leaved, RS_raw_data$joined, units = "secs")
        
        return(cbind(RS_raw_data, duration))
}

make.quarter.label = function(input){ 
        # 15min data label maker 
        h = floor(((input-1) * 15)/60)
        m = (input-1)*15 - h*60
        l = paste0(h,":",m)
        return(l)
}

fill.empty.date <- function(input_dt){
        input_dt$date = as.Date(input_dt$date)
        
        date_start = input_dt$date[1]
        date_long  = as.numeric(as.Date(input_dt$date[nrow(input_dt)]) - as.Date(input_dt$date[1]))
        
        # print(paste(date_start, date_long))
        full_date_index = data.frame(date = date_start + (0:date_long))
        
        result_df = merge(full_date_index, input_dt, all.x=T)
        result_df[is.na(result_df)] = 0
        
        result_df = cbind(result_df, weekday = as.factor(isWeekday(result_df$date)))
        print(head(result_df)); print(tail(result_df))
        return(result_df)
}


build.RealSense.table = function(data){
        # convert data format
        joined = as.POSIXct(data$joined/1000, format="%Y-%m-%d", origin='1970-01-01', tz="ROK")
        leaved = as.POSIXct(data$leaved/1000, format="%Y-%m-%d", origin='1970-01-01', tz="ROK")
        duration = as.numeric(data$leaved - data$joined)
        
        basic_table = data.frame(joined, duration)
        
        #         basic_table = basic_table[basic_table$duration > threshold, ]
        #         basic_table = basic_table[basic_table$duration < threshold2, ]
        # print(nrow(basic_table))
        print(paste("range:", range(duration)))
        
        # after = nrow(basic_table)
        
        # print(paste((after/before)*100), "%")
        # basic_table = basic_table[order(joined, leaved),]
        
        # set every timestamp second to '0'
        second(basic_table$joined) = 0
        
        # minite
        minute(basic_table$joined)[minute(basic_table$joined) < 15] = 0
        minute(basic_table$joined)[minute(basic_table$joined) >= 15 & minute(basic_table$joined) < 30] = 15
        minute(basic_table$joined)[minute(basic_table$joined) >= 30 & minute(basic_table$joined) < 45] = 30
        minute(basic_table$joined)[minute(basic_table$joined) >= 45] = 45
        
        # aggregation
        basic_table$joined = as.factor(basic_table$joined)
        counting = count(basic_table, "joined")
        sum_of_duration = aggregate(duration ~ joined, data = basic_table, sum)
        
        return_table = merge(counting, sum_of_duration)
        return_table$joined = as.POSIXct(return_table$joined, origin='1970-01-01', tz="ROK")
        return_table$duration = return_table$duration/1000
        
        return(return_table)
}
#####################
# raw data loading
RS_adsl_raw = fread("realsense/adsl.csv")
RS_marg_raw = fread("realsense/marg.csv")
RS_hcc_raw = fread("realsense/hcc.csv")
RS_ux_raw = fread("realsense/ux.csv")

RS_adsl = table.time2string(RS_adsl_raw)
RS_marg = table.time2string(RS_marg_raw)
RS_hcc = table.time2string(RS_hcc_raw)
RS_ux = table.time2string(RS_ux_raw)

###################
## preprocessing 

duration.filter <- function(dt, threshold_min, threshold_max){
        filtered_min = nrow(dt[duration <= threshold_min])
        filtered_max = nrow(dt[duration >= threshold_max])
        
        print(paste("filtered", filtered_min, "and", filtered_max, "rows (by min & max, respectively"))
        
        return(dt[duration > threshold_min & duration < threshold_max])
}

RS_marg = duration.filter(RS_marg, 1, 300)
RS_hcc  = duration.filter(RS_hcc,  1, 300)
RS_ux   = duration.filter(RS_ux,   1, 300)

#######################
## Build a 15min table 

RS_marg_table = build.RealSense.table(RS_marg_raw)
RS_hcc_table  = build.RealSense.table(RS_hcc_raw)
RS_ux_table   = build.RealSense.table(RS_ux_raw)


###################
## plot functions

lineplot.RealSense.data <- function(lab, data, ylim, date_cut, type) {
        
        dt = data.table(data)
        
        dt[, ':='(date = as.Date(joined))]
        
        # print(dt)        
        # dt = dt[duration > threshold]
        # print(dt)
        dt = dt[date > as.Date(date_cut)]
        # print(dt)
        
        
        if(type == "freq") {
                sub_dt = dt[, .(day_freq_sum = sum(freq)), by=date]
                sub_dt <- fill.empty.date(sub_dt)
                
                plotting <- ggplot(sub_dt, aes(x=date, y=day_freq_sum, label=day_freq_sum)) +
                        geom_line() +
                        geom_point() +
                        geom_text(vjust = 0, nudge_y = 1) +
                        scale_x_date(date_labels = "%y-%b-%d", breaks = date_breaks("days")) +
                        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                        ggtitle(paste(lab, "RealSense daily", type))
                
        } else if(type == "duration_mean") {
                sub_dt = dt[, .(day_duration_mean = round(mean(duration))), by=date]
                sub_dt <- fill.empty.date(sub_dt)
                
                plotting <- ggplot(sub_dt, aes(x=date, y=day_duration_mean, label=day_duration_mean)) +
                        geom_line() +
                        geom_point() +
                        geom_text(vjust = 0, nudge_y = 1) +
                        scale_x_date(date_labels = "%y-%b-%d", breaks = date_breaks("days")) +
                        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                        ggtitle(paste(lab, "RealSense daily", type))
                
        } else if(type == "duration_sum") {
                sub_dt = dt[, .(day_duration_sum = round(sum(duration))), by=date]
                sub_dt <- fill.empty.date(sub_dt)
                
                # thresholding
                # sub_dt[which(sub_dt$day_duration_sum > max_threshold), "day_duration_sum"] = NA                
                
                print(sub_dt)
                
                plotting <- ggplot(sub_dt, aes(x=date, y=day_duration_sum, label=day_duration_sum)) +
                        geom_line() +
                        geom_point(aes(color=weekday)) +
                        geom_text(aes(color=weekday), vjust = 0, nudge_y = 1) +
                        scale_colour_manual(name = 'weekday', values = setNames(c('blue','red'), c(T, F))) +
                        scale_x_date(date_labels = "%y-%b-%d", breaks = date_breaks("days")) +
                        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                        ggtitle(paste(lab, "RealSense daily", type)) + 
                        scale_y_continuous(limits = c(0, ylim))
        } else {
                print("error the type")
                return()
        }
        print(plotting)
        return(plotting)
}

heatmap.RealSense.data <- function(lab, data, date_cut, threshold) {
        
        dt = data.table(data)
        names(dt) <- c("quarter_index", "freq", "sum_of_duration")
        
        dt = dt[sum_of_duration > threshold]
        dt = dt[quarter_index > date_cut]
        
        start_ts = as.numeric(dt$quarter_index[1])
        end_ts   = as.numeric(dt$quarter_index[nrow(dt)])
        
        full_quarter_index = data.table(quarter_index = as.POSIXct(seq(from=start_ts, to=end_ts, by = 15*60), 
                                                                   origin="1970-01-01", tz="ROK"))
        setkey(full_quarter_index, quarter_index)
        setkey(dt, quarter_index)
        
        # tables();
        
        dt = merge(full_quarter_index, dt, all.x=T)
        dt = cbind(dt, date_index = as.Date(dt$quarter_index, tz="ROK"))
        dt[, ':='(quarter = ((hour(quarter_index) * 60 + minute(quarter_index)) / 15) + 1), by=.(date_index)]
        
        # View(dt)
        
        plotting <- ggplot(dt, aes(x=date_index, y=quarter, fill=sum_of_duration)) +
                scale_x_date(date_labels = "%y-%b-%d", breaks = date_breaks("days")) +
                scale_y_continuous(breaks=1:96, labels=make.quarter.label(1:96)) + 
                theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                geom_raster() +
                ggtitle(paste(lab, "RealSense heatmap ( threshold:", threshold, ")" ))
        
        print(plotting)
}


##################
## plotting 

# max_threshold = 500
date_cut = "2015-10-30"
# date_cut = "2015-12-31"
type = "duration_sum" # freq, duration_mean, duration_sum

marg <- lineplot.RealSense.data("MARG", RS_marg_table, ylim = 600, "2015-10-07", "duration_sum")
hcc <- lineplot.RealSense.data("HCC", RS_hcc_table, ylim = 600, "2015-10-28", "duration_sum")
ux <- lineplot.RealSense.data("UX", RS_ux_table, ylim = 600, "2015-09-30", "duration_sum")

grid.arrange(marg, hcc, ux)


heatmap.RealSense.data("MARG", RS_marg_table, date_cut = "2015-10-07", threshold = 1)
heatmap.RealSense.data("HCC", RS_hcc_table, date_cut = "2015-10-07", threshold = 1)
heatmap.RealSense.data("UX", RS_ux_table, date_cut = "2015-10-07", threshold = 1)



# install.packages("lubridate")
library(lubridate)
library(plyr)
library(ggplot2)
library(scales)
library(reshape2)
library(gridExtra)
library(data.table)

make.RealSense.Table = function(data){
        # convert data format
        joined = as.POSIXct(data$joined/1000, format="%Y-%m-%d", origin='1970-01-01', tz="ROK")
        leaved = as.POSIXct(data$leaved/1000, format="%Y-%m-%d", origin='1970-01-01', tz="ROK")
        duration = as.numeric(data$leaved - data$joined)
        
        basic_table = data.frame(joined, duration)
        basic_table = basic_table[order(joined, leaved),]
        
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

time2string <- function(input){
        output = as.POSIXct(input/1000, origin = "1970-01-01", tz="ROK")
}

#####################
# raw data loading
RS_adsl_raw = read.csv("realsense/adsl.csv")
RS_marg_raw = read.csv("realsense/marg.csv")
RS_ux_raw = read.csv("realsense/hcc.csv")
RS_ux_raw = read.csv("realsense/ux.csv")

RS_marg_table = make.RealSense.Table(RS_marg_raw)
RS_hcc_table  = make.RealSense.Table(RS_hcc_raw)
RS_ux_table   = make.RealSense.Table(RS_ux_raw)

###################
## preprocessing 

# nrow(RS_marg_table[RS_marg_table$duration > 1,])
# nrow(RS_hcc_table[RS_hcc_table$duration > 1,])
# nrow(RS_ux_table[RS_ux_table$duration > 1,])

# View(RS_marg)
# View(RS_hcc)
# View(RS_ux)

##############
## plotting

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

plot.RealSense.data <- function(lab, data, ylim, date_cut, type) {
        
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
                
                # print(sub_dt)
                
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


# max_threshold = 500
date_cut = "2015-10-30"
# date_cut = "2015-12-31"
type = "duration_sum" # freq, duration_mean, duration_sum

marg <- plot.RealSense.data("MARG", RS_marg_table, ylim = 500, "2015-10-07", "duration_sum")
hcc <- plot.RealSense.data("HCC", RS_hcc_table, ylim = 500, "2015-10-28", "duration_sum")
ux <- plot.RealSense.data("UX", RS_ux_table, ylim = 500, "2015-09-30", "duration_sum")

grid.arrange(marg, hcc, ux)


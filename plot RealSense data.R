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
        
        duration = as.numeric(difftime(RS_raw_data$leaved, RS_raw_data$joined, units = "secs"))
        
        return(cbind(RS_raw_data, duration))
}

make.quarter.label = function(input){ 
        # 15min data label maker 
        h = floor(((input-1) * 15)/60)
        m = (input-1)*15 - h*60
        l = paste0(h,":",m)
        return(l)
}



#####################
# raw data loading
RS_adsl_raw = fread("realsense/adsl.csv")
RS_marg_raw = fread("realsense/marg.csv")
RS_hcc_raw = fread("realsense/hcc.csv")
RS_ux_raw = fread("realsense/ux.csv")

adsl_RS = table.time2string(RS_adsl_raw)
marg_RS = table.time2string(RS_marg_raw)
hcc_RS = table.time2string(RS_hcc_raw)
ux_RS = table.time2string(RS_ux_raw)

date_adjust_parameter = 7 * 60 * 60 # 7 hours 

marg_RS[, ':='(date=as.Date(joined-date_adjust_parameter, tz="rok"), weekday = isWeekday(joined))]
hcc_RS[, ':='(date=as.Date(joined-date_adjust_parameter, tz="rok"), weekday = isWeekday(joined))]
ux_RS[, ':='(date=as.Date(joined-date_adjust_parameter, tz="rok"), weekday = isWeekday(joined))]

write.csv(marg_RS, file ="data/marg_RS.csv")
write.csv(hcc_RS, file ="data/hcc_RS.csv")
write.csv(ux_RS, file ="data/ux_RS.csv")

# raw data histogram

hist.by.day <- function(lab, dt, duration_cut, type = c("gt, le"), date_cut, j=F){

        dt[, ':='(date=as.Date(joined), weekday = isWeekday(joined))]
        
        dt = dt[date > as.Date(date_cut)]
        if(type == "gt"){
                dt = dt[duration > duration_cut]
        } else if(type == "le") {
                dt = dt[duration <= duration_cut]
        } else {
                print("type = gt, le")
                return()
        }
        
        box <- ggplot(dt, aes(factor(date), as.numeric(duration))) + 
                geom_boxplot(aes(fill = factor(weekday))) +
                theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
                ggtitle(paste("Distribution of individual sample's duration -", toupper(lab)))
        
        bar_freq <- ggplot(dt, aes(factor(date), as.numeric(duration))) + 
                stat_summary(aes(fill = factor(weekday)), fun.y=length, geom="bar") +
                stat_summary(aes(label=round(..y..,2)), fun.y=length, geom="text", size=3, vjust = -0.5) + 
                theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                ggtitle(paste("Frequency of samples - ", toupper(lab)))
        
        bar_duration <- ggplot(dt, aes(factor(date), as.numeric(duration))) + 
                stat_summary(aes(fill = factor(weekday)), fun.y=sum, geom="bar") +
                stat_summary(aes(label=round(..y..,2)), fun.y=sum, geom="text", size=3, vjust = -0.5) + 
                scale_y_continuous(limits = c(0,500)) +
                theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                ggtitle(paste("Sum of duration -", toupper(lab)))
        
        if(j) box <- box + geom_jitter()
        
        # x11()
        # print(box)
        # print(bar_freq)
        print(bar_duration)
        # grid.arrange(box, bar_freq, bar_duration)
        return(dt)
}        

# hist(log(as.numeric(RS_marg$duration)))

sum(RS_marg$duration <= 10) / length(RS_marg$duration)
sum(RS_hcc$duration <= 0.5) / length(RS_hcc$duration)
sum(RS_ux$duration <= 10) / length(RS_ux$duration)
# 약 97%의 sample이 30초 이하 
# 20초 미만 : 94~95%
# 10초 미만 : 90%

cut_date = "2015-10-8"

hist.by.day("marg", RS_marg, 300, "le", cut_date, j=F)
hist.by.day("marg", RS_marg, 300, "gt", cut_date, j=T)

hist.by.day("hcc", RS_hcc, 300, "le", cut_date, j=F)
hist.by.day("hcc", RS_hcc, 300, "gt", cut_date, j=T)

hist.by.day("ux", RS_ux, 300, "le", cut_date, j=F)
hist.by.day("ux", RS_ux, 300, "gt", cut_date, j=T)

hist.by.day("hcc", RS_hcc, 300, "le", cut_date, j=F)

#









###################
## pre-processing 

duration.filter <- function(dt, threshold_min, threshold_max){
        filtered_min = nrow(dt[duration <= threshold_min])
        filtered_max = nrow(dt[duration >= threshold_max])
        
        print(paste("filtered", filtered_min, "and", filtered_max, "rows (by min & max, respectively)"))
        
        return(dt[duration > threshold_min & duration < threshold_max])
}


# 
RS_marg = duration.filter(RS_marg, 1, 300)
RS_hcc  = duration.filter(RS_hcc,  1, 300)
RS_ux   = duration.filter(RS_ux,   1, 300)

# 
# RS_marg = RS_marg[duration<1]
# RS_hcc = RS_hcc[duration<1]
# RS_ux = RS_ux[duration<1]


# RS_marg = RS_marg[duration > 60]
# RS_hcc = RS_hcc[duration > 60]
# RS_ux = RS_ux[duration > 60]




#######################
## Build a 15min table 
build.RealSense.table = function(data, unit="quarter"){
        
        data$duration = as.numeric(data$duration)
        
        print("range:")
        print(range(data$duration))
        print(paste("unit:", unit))
        
        # set every timestamp second to '0'
        second(data$joined) = 0
        
        if(unit == "quarter") {
                minute(data$joined)[minute(data$joined) < 15] = 0
                minute(data$joined)[minute(data$joined) >= 15 & minute(data$joined) < 30] = 15
                minute(data$joined)[minute(data$joined) >= 30 & minute(data$joined) < 45] = 30
                minute(data$joined)[minute(data$joined) >= 45] = 45
        } 
        
        print(data)
        
        return_table = data[, .(freq = nrow(.SD), sum_of_duration = sum(duration)), by=joined]
        as.POSIXct(return_table$joined, origin='1970-01-01', tz="ROK")
        
        return(return_table)
        
}

RS_marg_table = build.RealSense.table(RS_marg)
RS_hcc_table  = build.RealSense.table(RS_hcc)
RS_ux_table   = build.RealSense.table(RS_ux)




###################
## line plot 

lineplot.RealSense.data <- function(lab, data, ylim, date_cut, type) {
        
        # dt = data.table(data)
        print("Duration range:")
        print(range(data$sum_of_duration))
        
        data[, ':='(date = as.Date(joined))]
        data = data[date > as.Date(date_cut)]
        
        print(data)
        
        if(type == "freq") {
                sub_dt = data[, .(day_freq_sum = sum(freq)), by=date]
                sub_dt <- fill.empty.date(sub_dt)
                
                plotting <- ggplot(sub_dt, aes(x=date, y=day_freq_sum, label=day_freq_sum)) +
                        geom_line() +
                        geom_point(aes(color=weekday)) +
                        geom_text(aes(color=weekday), vjust = 0, nudge_y = 1) +
                        scale_colour_manual(name = 'weekday', values = setNames(c('blue','red'), c(T, F))) +
                        scale_x_date(date_labels = "%y-%b-%d", breaks = date_breaks("days")) +
                        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                        ggtitle(paste(lab, "RealSense daily", type))
                
        } else if(type == "duration_mean") {
                sub_dt = data[, .(day_duration_mean = round(mean(sum_of_duration))), by=date]
                sub_dt <- fill.empty.date(sub_dt)
                
#                 plotting <- ggplot(sub_dt, aes(x=date, y=day_duration_mean, label=day_duration_mean)) +
#                         geom_line() +
#                         geom_point() +
#                         geom_text(vjust = 0, nudge_y = 1) +
#                         scale_x_date(date_labels = "%y-%b-%d", breaks = date_breaks("days")) +
#                         theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#                         ggtitle(paste(lab, "RealSense daily", type))
                
        } else if(type == "duration_sum") {
                
                sub_dt = data[, .(day_duration_sum = round(sum(as.numeric(sum_of_duration)))), by=date]
                sub_dt <- fill.empty.date(sub_dt)
                
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

# max_threshold = 500
date_cut = "2015-10-30"
# date_cut = "2015-12-31"
type = "duration_sum" # freq, duration_mean, duration_sum

marg <- lineplot.RealSense.data("MARG", RS_marg_table, ylim = 600, "2015-10-07", type)
hcc <- lineplot.RealSense.data("HCC", RS_hcc_table, ylim = 600, "2015-10-28", type)
ux <- lineplot.RealSense.data("UX", RS_ux_table, ylim = 600, "2015-09-30", type)

grid.arrange(marg, hcc, ux)





###################
## heatmap

heatmap.RealSense.data <- function(lab, data, date_cut, threshold) {
        
        dt = data.table(data)
        names(dt)[1] <- "quarter_index"
        names(dt)[2] <- "freq"
        names(dt)[3] <- "sum_of_duration"
        
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


heatmap.RealSense.data("MARG", RS_marg_table, date_cut = "2015-10-07", threshold = 1)
heatmap.RealSense.data("HCC", RS_hcc_table, date_cut = "2015-10-07", threshold = 1)
heatmap.RealSense.data("UX", RS_ux_table, date_cut = "2015-10-07", threshold = 1)





###################
## bar plot 

barplot.watched = function(RS_table){
        table = data.table(Date = as.Date(RS_table$joined), watched = as.logical(RS_table$sum_of_duration))
        
        for_plot = table[, .(watched_sum = sum(watched)), by=Date]
        return_plot <- ggplot(for_plot, aes(x=Date, y=watched_sum)) +
                geom_bar(stat = "identity") +
                geom_text(aes(label=watched_sum), vjust=-1) +
                scale_x_date(date_labels = "%y-%b-%d", breaks = date_breaks("days")) +
                theme(axis.text.x = element_text(angle = 90, hjust = 1))
        
        print(return_plot)
        return(return_plot)
}

marg <- barplot.watched(RS_marg_table)
hcc <- barplot.watched(RS_hcc_table)
ux <- barplot.watched(RS_ux_table)

grid.arrange(marg, hcc, ux)





######################################################################
# short gap merging


data = RS_hcc
allowance = 2

class(RS_marg$joined)


# data$joined = as.numeric(data$joined)
# data$leaved = as.numeric(data$leaved)

data = cbind(data, used = rep(0,nrow(data)))

updated_joined = numeric(0)
updated_leaved = numeric(0)

merged_raw = numeric(0)

for(i in 1:(nrow(data)-1)){
        # for(i in 1:100){
        
        print(paste(i, "/", nrow(data)))
        #         print(data$leaved[i])
        #         print(data$joined[i+1])
        
        
        if(data$used[i] == 1){
                next
        } else {
                data$used[i] = 1
                tmp_joined = data$joined[i]
                tmp_leaved = data$leaved[i]
                
                j = i + 1
                
                for(j in (i+1):nrow(data)){
                        # cat(paste(j, " "))
                        
                        if(data$used[j] == 1){
                                next
                        } else {
                                gap = difftime(data$joined[j], tmp_leaved, units = "secs")
                                
                                #                                 gap = data$joined[j] - tmp_leaved
                                
                                if(gap > 0 & gap < allowance){
                                        print(paste("merged gap:", gap))
                                        print(paste(tmp_leaved, "as", data$leaved[j], "@", i))
                                        
                                        tmp_leaved = data$leaved[j]
                                        merged_raw = c(merged_raw, i)
                                        data$used[j] = 1
                                        
                                }
                        }
                }
                updated_joined = c(updated_joined, tmp_joined)
                updated_leaved = c(updated_leaved, tmp_leaved)
                cat("\n")
        }
}



result_dt = data.table(joined = as.POSIXct(updated_joined, origin = "1970-01-01", tz = "ROK"),
                       leaved = as.POSIXct(updated_leaved, origin = "1970-01-01", tz = "ROK"),
                       id = 1:nrow(result_dt),
                       duration = difftime(as.POSIXct(updated_leaved, origin = "1970-01-01", tz = "ROK"),
                                           as.POSIXct(updated_joined, origin = "1970-01-01", tz = "ROK"), units = "secs"))

length(merged_raw) / nrow(data)


# day segment plot
library(data.table)
library(ggplot2)
library(lubridate)

# target_date = "2016-02-24"
date_from = "2016-02-15"
date_to = "2016-02-21"


# RS_ux[, ':='(daily_id = NULL)]

convert2secs <- function(joined){
        result = (hour(joined) * 60 * 60) + (minute(joined) * 60) + second(joined)
        return(result)
}


RS_ux[, ':='(daily_id = 1:nrow(.SD)), by=as.Date(joined, tz="ROK")]
RS_ux[, ':='(secs = convert2secs(joined))]


dt = RS_ux

sub_dt = dt[as.Date(joined, tz="ROK") >= as.Date(date_from, tz="ROK") & as.Date(joined, tz="ROK") <= as.Date(date_to, tz="ROK")]
# sub_dt = dt[as.Date(joined, tz="ROK") == as.Date(target_date, tz="ROK")]


# boxplot(sub_dt)

x11()
ggplot(sub_dt) + 
        geom_segment(aes(x=joined, y=daily_id, xend=leaved, yend=daily_id), color="blue", size=5) +
        geom_text(aes(x=joined, y=daily_id, label=paste(daily_id,":",round(duration,1)), color = ifelse(duration < 1, TRUE, FALSE)), vjust=0) +
        scale_color_manual(values = c("red", "black")) +
        scale_y_reverse() + 
        ggtitle(paste("UX :", date_from, date_to))



# 
# data = RS_marg
# gaps = numeric(0)
# 
# for(i in 1:(nrow(data)-1)){
#         print(paste(i, "/", nrow(data)))
#         #         print(data$leaved[i])
#         #         print(data$joined[i+1])
#         j = i + 1
#         gap = difftime(data$joined[j], data$leaved[i], units = "secs")
#         gaps = c(gaps, gap)
# }
# 





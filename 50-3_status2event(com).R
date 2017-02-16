library(data.table)
library(lubridate)
library(ggplot2)
library(scales)
library(gridExtra)
library(timeDate)

VALID_STAY_DURATION = 60 # secs

STATUS_AGG_PATH = "../data/status/aggregated/"

PLOTTING = F
# PLOT_PATH = "../plots/milli/com/"
PLOT_PATH = "../plots/"
PLOT_END_DATE = "2016-10-01"

LAB_LABLES = c("marg", "hcc", "ux") 
# LAB_LABLES = c("marg") 

get.officehour <- function(dts, office_hour_start = 7){
  new_dts = as.POSIXct(dts)
  new_dts = new_dts - office_hour_start*60*60
  new_dts = as.Date(new_dts, tz='ROK')
  
  return(new_dts)
}

get.dinner.label <- function(dts){
  hh = hour(dts)
  mm = minute(dts)
  
  as_minute = hh*60 + mm
  
  dinner_label = rep(0, length(as_minute))
  
  # for lunch itme: 11:30 ~ 13:00
  dinner_label[ as_minute >= (11*60+30) & as_minute <= (13*60+00) ] = 1
  
  # for supper time: 17:30 ~ 19:00
  dinner_label[ as_minute >= (17*60+30) & as_minute <= (19*60+00) ] = 1
  
  return(dinner_label)
}

get.com.event <- function(com_usage_diff){
  event = rep("ON", length(com_usage_diff))
  event[com_usage_diff < 0] = "OFF"
  
  return(event)
}

get.com.tidy.event.dt <- function(agg_status_dt, valid_stay_duration, valid_diff){
  
  agg_event_dt = agg_status_dt
  
  # subset valid status only
  # 1. 'stay' with 'valid duration'
  # 2. 'com_usage_diff' with valid change of com usage (over COM_PRE_POST_GAP_THRE) 
  #
#   repeat{
#     agg_event_dt[, ':='(com_usage_diff = c(valid_diff, diff(agg_event_dt$com)))]
#     agg_event_dt = agg_event_dt[status=='stay' 
#                                 & duration > valid_stay_duration
#                                 & abs(com_usage_diff) > valid_diff]
#     
#     agg_event_dt[, ':='(com_usage_diff = c(valid_diff, diff(agg_event_dt$com)))]
# 
#     if(sum(abs(agg_event_dt$com_usage_diff) < valid_diff)){
#       invalid_stay_index = which(abs(agg_event_dt$com_usage_diff) < valid_diff)
#       print(agg_event_dt[invalid_stay_index])
#       
#     } else {
#       break
#     }
#   }

  agg_event_dt[, ':='(com_usage_diff = c(valid_diff, diff(agg_event_dt$com)))]
  agg_event_dt = agg_event_dt[status=='stay' 
                              & duration > 30]

  
  # --- new columns --- #
  ##  'aggDay' & 'aggWeek' for aggregation (considering an office hour 7AM to 7AM)
  agg_event_dt[, ':='(aggDay = get.officehour(dts))]
  agg_event_dt[, ':='(aggWeek=as.Date(cut(aggDay, breaks = "week", start.on.monday = T)))]
  
  ## 'event' for ON, OFF labeling 
  agg_event_dt[, ':='(event = get.com.event(com_usage_diff))]
  
  ## 'lunch_label' for dinner 
  agg_event_dt[, ':='(dinner_label = get.dinner.label(dts))]
  
  # ## 'initial_on': right after zero-usage status 
  # no_use_index = which(agg_event_dt$com == 0)
  # initial_on_index  = no_use_index[-length(no_use_index)] + 1
  # 
  # initial_on = rep(0, nrow(agg_event_dt))
  # initial_on[initial_on_index] = 1
  # 
  # agg_event_dt = cbind(agg_event_dt, initial_on)
  
  return(agg_event_dt)
}

f = paste0(STATUS_AGG_PATH, 'marg', "_com_aggregated_status_dt.csv")
tmp = get.com.tidy.event.dt(fread(f), VALID_STAY_DURATION, 40)

View(tmp)

which(abs(tmp$com_usage_diff) < 40)

range((tmp$com_usage_diff))

## -------------------------- ##
## plot

# 1. average_com_on_lift: User's fine control for com-on event  
for(lab in LAB_LABLES){
  
  file_name = paste0(STATUS_AGG_PATH, lab, "_com_aggregated_status_dt.csv")
  agg_event_dt = get.com.tidy.event.dt(fread(file_name), VALID_STAY_DURATION, COM_PRE_POST_GAP_THRE)
  agg_event_dt = agg_event_dt[dts < PLOT_END_DATE]
  print(file_name)
  
  # # # average lift for 'ON' event 
  average_com_on_lift_dt = agg_event_dt[event == 'ON', 
                                        .(com_on_lift_average = mean(com_usage_diff)), by=aggDay]
  
  setnames(average_com_on_lift_dt, "aggDay", "timestamp")
  average_com_on_lift_dt[, timestamp := as.Date(timestamp)]
  
  average_com_on_lift_dt = merge(DAY_LABEL, average_com_on_lift_dt, by = "timestamp", all.x = T)
  
  p <- ggplot(average_com_on_lift_dt, aes(x = timestamp)) +
    ggtitle(paste0(lab, ": average lift for 'ON' event "))
  
  p = add.window.line(p, average_com_on_lift_dt, "com_on_lift_average", windowingWeek, get.expDate.2())
  p = add.event.vline.exp2(p)
  p = set.default.theme(p)
  
  save.plot(paste0(PLOT_PATH, lab, "_average lift for ON event.png"), p)
  
  
#   p <- ggplot(average_com_on_lift_dt, aes(aggDay, com_on_lift_average)) + 
#     stat_smooth() +
#     geom_point() + 
#     scale_x_date("Timestamp", labels = date_format("%Y-%m"), breaks = date_breaks("month")) +
#     ggtitle(paste(lab, ": average lift for 'ON' event "))
#   
#   print(p)
}


# 2. dinner time OFF : dinner-time saving count per week 
for(lab in LAB_LABLES){
  
  file_name = paste0(STATUS_AGG_PATH, lab, "_com_aggregated_status_dt.csv")
  agg_event_dt = get.com.tidy.event.dt(fread(file_name), VALID_STAY_DURATION, COM_PRE_POST_GAP_THRE)
  agg_event_dt = agg_event_dt[dts < PLOT_END_DATE]
  print(file_name)
  
  # # # dinner time OFF event counting 
  dinner_time_off_dt = agg_event_dt[dinner_label == 1 & event=='OFF', 
                                    .(dinner_time_off_count = .N), by=aggWeek]
  dinner_time_on_dt = agg_event_dt[dinner_label == 1 & event=='ON', 
                                   .(dinner_time_on_count = .N), by=aggWeek]
  
  setnames(dinner_time_off_dt, "aggWeek", "timestamp")
  setnames(dinner_time_on_dt, "aggWeek", "timestamp")
  dinner_time_on_dt[, timestamp := as.Date(timestamp)]
  dinner_time_off_dt[, timestamp := as.Date(timestamp)]
  
  dinner_time_dt = merge(WEEK_LABEL, dinner_time_off_dt, by = "timestamp", all.x = T)
  dinner_time_dt = merge(dinner_time_dt, dinner_time_on_dt, by = "timestamp", all.x = T)
  
  p <- ggplot(dinner_time_dt, aes(x = timestamp)) +
    ggtitle(paste0(lab, ": dinner time ONOFF count"))
  
  p = add.colorful.window.line(p, dinner_time_dt, "dinner_time_on_count", windowingWeek, "orange3",get.expDate.2())
  p = add.colorful.window.line(p, dinner_time_dt, "dinner_time_off_count", windowingWeek, "darkorchid4", get.expDate.2())
  
  p = add.event.vline.exp2(p)
  p = set.default.theme(p)
  
  save.plot(paste0(PLOT_PATH, lab, "_dinner time ONOFF com_count.png"), p)
  
  
#   p <- ggplot(dinner_time_off_dt, aes(aggWeek, dinner_time_saving_count)) +
#     stat_smooth() +
#     geom_point() +
#     scale_x_date("Timestamp", labels = date_format("%Y-%m"), breaks = date_breaks("month")) +
#     ggtitle(paste0(lab, ": dinner time OFF count"))
#   
#   print(p)
#   
}


# 3. initial com-on lifting
for(lab in LAB_LABLES){
  
  file_name = paste0(STATUS_AGG_PATH, lab, "_com_aggregated_status_dt.csv")
  agg_event_dt = get.com.tidy.event.dt(fread(file_name), VALID_STAY_DURATION, COM_PRE_POST_GAP_THRE)
  agg_event_dt = agg_event_dt[dts < PLOT_END_DATE]
  print(file_name)
  
  # # # dinner time OFF event counting 
  initial_com_on_dt = agg_event_dt[initial_on == 1 & event=='ON', 
                                   .(initial_com_on = mean(com_usage_diff)), by=aggDay]
  
  p <- ggplot(initial_com_on_dt, aes(aggDay, initial_com_on)) +
    stat_smooth() +
    geom_point() +
    scale_x_date("Timestamp", labels = date_format("%Y-%m"), breaks = date_breaks("month")) +
    ggtitle(paste0(lab, ": initial com-on lifting"))
  
  print(p)
  
}

# 
# f = "../data/status/aggregated/ux_com_aggregated_status_dt.csv"
# tmp = get.com.tidy.event.dt(fread(f), VALID_STAY_DURATION)
# tmp_dt = tmp[com_usage_diff > 700 & event=='ON', .(tmp = .N), by=aggDay]
# ggplot(tmp_dt, aes(aggDay, tmp)) +
#   stat_smooth() +
#   geom_point() +
#   scale_x_date("Timestamp", labels = date_format("%Y-%m"), breaks = date_breaks("month")) +
#   ggtitle(paste0(f))



for(lab in LAB_LABLES){
  
  file_name = paste0(STATUS_AGG_PATH, lab, "_com_aggregated_status_dt.csv")
  agg_event_dt = get.com.tidy.event.dt(fread(file_name), VALID_STAY_DURATION, COM_PRE_POST_GAP_THRE)
  agg_event_dt = agg_event_dt[dts < PLOT_END_DATE]
  print(file_name)
  
  # # # dinner time OFF event counting 
  daily_off_dt = agg_event_dt[event=='OFF', 
                                    .(daily_off_count = .N), by=aggDay]
  daily_on_dt = agg_event_dt[event=='ON', 
                                   .(daily_on_count = .N), by=aggDay]
  
  setnames(daily_off_dt, "aggDay", "timestamp")
  setnames(daily_on_dt, "aggDay", "timestamp")
  daily_on_dt[, timestamp := as.Date(timestamp)]
  daily_off_dt[, timestamp := as.Date(timestamp)]
  
  daily_dt = merge(WEEK_LABEL, daily_off_dt, by = "timestamp", all.x = T)
  daily_dt = merge(daily_dt, daily_on_dt, by = "timestamp", all.x = T)
  
  p <- ggplot(daily_dt, aes(x = timestamp)) +
    ggtitle(paste0(lab, ": dinner time ONOFF count"))
  
  p = add.colorful.window.line(p, daily_dt, "daily_on_count", windowingWeek, "orange3",get.expDate.2())
  p = add.colorful.window.line(p, daily_dt, "daily_off_count", windowingWeek, "darkorchid4", get.expDate.2())
  
  p = add.event.vline.exp2(p)
  p = set.default.theme(p)
  
  save.plot(paste0(PLOT_PATH, lab, "_daily ONOFF com_count.png"), p)
  
  
  #   p <- ggplot(dinner_time_off_dt, aes(aggWeek, dinner_time_saving_count)) +
  #     stat_smooth() +
  #     geom_point() +
  #     scale_x_date("Timestamp", labels = date_format("%Y-%m"), breaks = date_breaks("month")) +
  #     ggtitle(paste0(lab, ": dinner time OFF count"))
  #   
  #   print(p)
  #   
}
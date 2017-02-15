library(data.table)
library(lubridate)
library(ggplot2)
library(scales)
library(gridExtra)
library(timeDate)

# marg_agg_status_dt = fread("../data/status/aggregated/marg_light_aggregated_status_dt.csv")
# hcc_agg_status_dt = fread("../data/status/aggregated/hcc_light_aggregated_status_dt.csv")
# ux_agg_status_dt = fread("../data/status/aggregated/ux_light_aggregated_status_dt.csv")
# 
# hist(marg_agg_status_dt$light, 100)
# hist(hcc_agg_status_dt$light, 100)
# hist(ux_agg_status_dt$light, 100)

VALID_STAY_DURATION = 60 # secs

STATUS_AGG_PATH = "../data/status/aggregated/"

# PLOTTING = F
PLOT_PATH = "../plots/milli/light/"
PLOT_END_DATE = "2016-10-01"

LAB_LABLES = c("marg", "hcc", "ux") 

get.officehour.date <- function(dts, office_hour_start = 7){
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

get.light.event <- function(light_usage_diff){
  event = rep("ON", length(light_usage_diff))
  event[light_usage_diff < 0] = "OFF"
  
  return(event)
}

get.light.tidy.event.dt <- function(agg_status_dt, valid_stay_duration){

  agg_event_dt = agg_status_dt

  # subset valid status only: 'stay' with over 'valid_stay_duration secs'
  agg_event_dt = agg_event_dt[status=='stay' & duration > valid_stay_duration]

  # --- new columns --- #
  ##  'aggDay' & 'aggWeek' for aggregation (considering an office hour 7AM to 7AM)
  agg_event_dt[, ':='(aggDay = get.officehour.date(dts))]
  agg_event_dt[, ':='(aggWeek=as.Date(cut(aggDay, breaks = "week", start.on.monday = T)))]
  
  ## 'light_usage_diff' to consider a valid change (over 10W) of light usage
  agg_event_dt[, ':='(light_usage_diff = c(0, diff(agg_event_dt$light)))]
  agg_event_dt = agg_event_dt[abs(light_usage_diff) > 10]
  
  ## 'event' for ON, OFF labeling 
  agg_event_dt[, ':='(event = get.light.event(light_usage_diff))]
  
  ## 'lunch_label' for dinner 
  agg_event_dt[, ':='(dinner_label = get.dinner.label(dts))]
  
  ## 'initial_on': right after zero-usage status 
  no_use_index = which(agg_event_dt$light == 0)
  initial_on_index  = no_use_index[-length(no_use_index)] + 1
  
  initial_on = rep(0, nrow(agg_event_dt))
  initial_on[initial_on_index] = 1
  
  agg_event_dt = cbind(agg_event_dt, initial_on)
  
  return(agg_event_dt)
}

## -------------------------- ##
## plot

# 1. average_light_on_lift: User's fine control for light-on event  
for(lab in LAB_LABLES){
  
  file_name = paste0(STATUS_AGG_PATH, lab, "_light_aggregated_status_dt.csv")
  agg_event_dt = get.light.tidy.event.dt(fread(file_name) , VALID_STAY_DURATION)
  agg_event_dt = agg_event_dt[dts < PLOT_END_DATE]
  print(file_name)
  
  # # # average lift for 'ON' event 
  average_light_on_lift_dt = agg_event_dt[event == 'ON', 
                                              .(light_on_lift_average = mean(light_usage_diff)), by=aggDay]


  p <- ggplot(average_light_on_lift_dt, aes(aggDay, light_on_lift_average)) + 
    stat_smooth() +
    geom_point() + 
    scale_x_date("Timestamp", labels = date_format("%Y-%m"), breaks = date_breaks("month")) +
    ggtitle(paste(lab, ": average lift for 'ON' event "))
  
  print(p)
}


# 2. dinner time OFF : dinner-time saving count per week 
for(lab in LAB_LABLES){
  
  file_name = paste0(STATUS_AGG_PATH, lab, "_light_aggregated_status_dt.csv")
  agg_event_dt = get.light.tidy.event.dt(fread(file_name), VALID_STAY_DURATION)
  agg_event_dt = agg_event_dt[dts < PLOT_END_DATE]
  print(file_name)
  
  # # # dinner time OFF event counting 
  dinner_time_off_dt = agg_event_dt[dinner_label == 1 & event=='OFF', 
                                        .(dinner_time_saving_count = .N), by=aggWeek]
   
  p <- ggplot(dinner_time_off_dt, aes(aggWeek, dinner_time_saving_count)) +
    stat_smooth() +
    geom_point() +
    scale_x_date("Timestamp", labels = date_format("%Y-%m"), breaks = date_breaks("month")) +
    ggtitle(paste0(lab, ": dinner time OFF count"))

  print(p)
  
}


# 3. initial light-on lifting
for(lab in LAB_LABLES){
  
  file_name = paste0(STATUS_AGG_PATH, lab, "_light_aggregated_status_dt.csv")
  agg_event_dt = get.light.tidy.event.dt(fread(file_name), VALID_STAY_DURATION)
  agg_event_dt = agg_event_dt[dts < PLOT_END_DATE]
  print(file_name)
  
  # # # dinner time OFF event counting 
  initial_light_on_dt = agg_event_dt[initial_on == 1 & event=='ON', 
                                        .(initial_light_on = mean(light_usage_diff)), by=aggDay]
  
  p <- ggplot(initial_light_on_dt, aes(aggDay, initial_light_on)) +
    stat_smooth() +
    geom_point() +
    scale_x_date("Timestamp", labels = date_format("%Y-%m"), breaks = date_breaks("month")) +
    ggtitle(paste0(lab, ": initial light-on lifting"))
  
  print(p)
  
}

# 
# f = "../data/status/aggregated/ux_light_aggregated_status_dt.csv"
# tmp = get.light.tidy.event.dt(fread(f), VALID_STAY_DURATION)
# tmp_dt = tmp[light_usage_diff > 700 & event=='ON', .(tmp = .N), by=aggDay]
# ggplot(tmp_dt, aes(aggDay, tmp)) +
#   stat_smooth() +
#   geom_point() +
#   scale_x_date("Timestamp", labels = date_format("%Y-%m"), breaks = date_breaks("month")) +
#   ggtitle(paste0(f))


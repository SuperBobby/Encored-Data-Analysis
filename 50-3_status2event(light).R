library(data.table)
library(lubridate)
library(ggplot2)
library(scales)
library(gridExtra)
library(timeDate)

VALID_STAY_DURATION = 10 # secs

STATUS_AGG_PATH = "../data/status/aggregated/"

PLOT_PATH = "../plots/"
PLOT_END_DATE = "2016-12-01"

LAB_LABLES = c("marg", "hcc", "ux")
# LAB_LABLES = c("marg")

DAY_LABEL = as.Date(seq(as.Date("2015-09-01"), as.Date(PLOT_END_DATE), by = "days"))
DAY_LABEL = data.table(timestamp = DAY_LABEL)

WEEK_LABEL = as.Date(seq(as.Date("2015-08-31"), as.Date(PLOT_END_DATE), by = "weeks"))
WEEK_LABEL = data.table(timestamp = WEEK_LABEL)

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
  no_use_index = which(agg_event_dt$light_usage == 0)
  initial_on_index  = no_use_index[-length(no_use_index)] + 1
  
  initial_on = rep(0, nrow(agg_event_dt))
  initial_on[initial_on_index] = 1
  
  agg_event_dt = cbind(agg_event_dt, initial_on)
  
  return(agg_event_dt)
}


marg_agg_status_dt = fread("../data/status/aggregated/marg_light_aggregated_status_dt.csv")
hcc_agg_status_dt = fread("../data/status/aggregated/hcc_light_aggregated_status_dt.csv")
ux_agg_status_dt = fread("../data/status/aggregated/ux_light_aggregated_status_dt.csv")

# View(marg_agg_status_dt)
# hist(marg_agg_status_dt$light, 100)
# hist(hcc_agg_status_dt$light, 100)
# hist(ux_agg_status_dt$light, 100)


# for(i in seq(10, 100, 10)){
#   tmp = get.light.tidy.event.dt(hcc_agg_status_dt, i)
#   hist(tmp$light_usage_diff, 100, main=paste0('hcc, valid_stay = ',i))
#   
# }


par(mfrow = c(1,3))
event_dt = get.light.tidy.event.dt(marg_agg_status_dt, 10)
daily_init_on_count = event_dt[, .(daily_init_on_count = sum(initial_on)), by=aggDay]
hist((daily_init_on_count$daily_init_on_count), max(daily_init_on_count$daily_init_on_count), 
     main = 'marg daily inital-light-on count')


event_dt = get.light.tidy.event.dt(hcc_agg_status_dt, 10)
daily_init_on_count = event_dt[, .(daily_init_on_count = sum(initial_on)), by=aggDay]
hist((daily_init_on_count$daily_init_on_count), max(daily_init_on_count$daily_init_on_count), 
     main = 'hcc daily inital-light-on count' )



event_dt = get.light.tidy.event.dt(marg_agg_status_dt, 10)
daily_init_on_count = event_dt[, .(daily_init_on_count = sum(initial_on)), by=aggDay]
hist((daily_init_on_count$daily_init_on_count), max(daily_init_on_count$daily_init_on_count), 
     main = 'ux daily inital-light-on count')
par(mfrow = c(1,1))


# 
# tmp = get.light.tidy.event.dt(marg_agg_status_dt, 30)
# hist(tmp$light_usage_diff, 100)
# 
# tmp = get.light.tidy.event.dt(hcc_agg_status_dt, 30)
# hist(tmp$light_usage_diff, 100)
# 
# tmp = get.light.tidy.event.dt(ux_agg_status_dt, 30)
# hist(tmp$light_usage_diff, 100)
# 
# 
# tmp = get.light.tidy.event.dt(marg_agg_status_dt, 30)
# # View(tmp[event == 'ON' & initial_on == 1])
# 
# tmp[event == 'ON' & light_usage_diff > 600, .(all_switch_on = .N), by=aggDay]


## -------------------------- ##
## plot

# 1. average_light_on_lift: User's fine control for light-on event  

# 
# for(lab in LAB_LABLES){
#   
#   file_name = paste0(STATUS_AGG_PATH, lab, "_light_aggregated_status_dt.csv")
#   agg_event_dt = get.light.tidy.event.dt(fread(file_name) , VALID_STAY_DURATION)
#   agg_event_dt = agg_event_dt[dts < PLOT_END_DATE]
#   print(file_name)
#   
#   # # # average lift for 'ON' event 
#   average_light_on_lift_dt = agg_event_dt[event == 'ON', 
#                                           .(light_on_lift_average = mean(light_usage_diff, na.rm = T)), by=aggDay]
# 
#   setnames(average_light_on_lift_dt, "aggDay", "timestamp")
#   average_light_on_lift_dt[, timestamp := as.Date(timestamp)]
#   
#   average_light_on_lift_dt = merge(DAY_LABEL, average_light_on_lift_dt, by = "timestamp", all.x = T)
#   
#   p <- ggplot(average_light_on_lift_dt, aes(x = timestamp)) + 
#     ggtitle(paste(lab, ": average lift for 'ON' event "))
#   
#   p = add.window.line(p, average_light_on_lift_dt, "light_on_lift_average", windowingWeek, get.expDate.2())
#   p = add.event.vline.exp2(p)
#   p = set.default.theme(p)
#   
#   save.plot(paste0(PLOT_PATH, lab, "_average lift for ON event.png"), p)
# }

# 2. dinner time OFF : dinner-time saving count per week 
# 
# for(lab in LAB_LABLES){
#   
#   file_name = paste0(STATUS_AGG_PATH, lab, "_light_aggregated_status_dt.csv")
#   agg_event_dt = get.light.tidy.event.dt(fread(file_name), VALID_STAY_DURATION)
#   agg_event_dt = agg_event_dt[dts < PLOT_END_DATE]
#   print(file_name)
#   
#   # # # dinner time OFF event counting 
#   dinner_time_off_dt = agg_event_dt[dinner_label == 1 & event=='OFF', 
#                                     .(dinner_time_off_count = .N), by=aggWeek]
#   # dinner_time_on_dt = agg_event_dt[dinner_label == 1 & event=='ON', 
#   #                                   .(dinner_time_on_count = .N), by=aggWeek]
#   
#   # fill the empty date to '0'
#   aggWeek_vec = unique(agg_event_dt$aggWeek)
#   dinner_time_off_dt = merge(data.frame(aggWeek = aggWeek_vec), dinner_time_off_dt, all = T)
#   dinner_time_off_dt$dinner_time_off_count[is.na(dinner_time_off_dt$dinner_time_off_count)] = 0
#   dinner_time_off_dt = data.table(dinner_time_off_dt)
#   
#   setnames(dinner_time_off_dt, "aggWeek", "timestamp")
#   # setnames(dinner_time_on_dt, "aggWeek", "timestamp")
#   # dinner_time_on_dt[, timestamp := as.Date(timestamp)]
#   dinner_time_off_dt[, timestamp := as.Date(timestamp)]
#   
#   dinner_time_dt = merge(WEEK_LABEL, dinner_time_off_dt, by = "timestamp", all.x = T)
#   # dinner_time_dt = merge(dinner_time_dt, dinner_time_on_dt, by = "timestamp", all.x = T)
#   
#   p <- ggplot(dinner_time_dt, aes(x = timestamp)) +
#     ggtitle(paste0(lab, ": dinner time light ONOFF count"))
#   
#   # p = add.colorful.window.line(p, dinner_time_dt, "dinner_time_on_count", windowingWeek, "orange3",get.expDate.2())
#   p = add.colorful.window.line(p, dinner_time_dt, "dinner_time_off_count", windowingWeek, "darkorchid4", get.expDate.2())
# 
#   p = add.event.vline.exp2(p)
#   p = set.default.theme(p)
#   
#   save.plot(paste0(PLOT_PATH, lab, "_dinner time light ON_OFF count.png"), p)
#   
# }


# 3. Maximum initial light-on lifting
PLOT_PATH = "../plots/milli/"

LABEL = 'daily_max_of_light_off-to-on_ration'
windowingWeek = 4

p1 = ggplot()
p2 = ggplot()
p3 = ggplot()

for(lab in LAB_LABLES){

  file_name = paste0(STATUS_AGG_PATH, lab, "_light_aggregated_status_dt.csv")
  agg_event_dt = get.light.tidy.event.dt(fread(file_name), VALID_STAY_DURATION)
  agg_event_dt = agg_event_dt[dts < PLOT_END_DATE]
  print(file_name)
  
  denominator = quantile(agg_event_dt$light_usage, 1)/2 ## divided by 2, because the half of the lab is the "full light switch on" event  
  print(denominator)
  
  agg_event_dt[light_usage_diff >= denominator, ':='(light_usage_diff = denominator)]
  agg_event_dt[light_usage_diff <= -denominator, ':='(light_usage_diff = -denominator)]
  
  # # # dinner time OFF event counting 
  max_initial_light_on_dt = agg_event_dt[initial_on == 1 & event=='ON', 
                                     .(initial_light_switch_on_ratio = max(light_usage_diff)/denominator), by=aggDay]
  setnames(max_initial_light_on_dt, "aggDay", "timestamp")
  max_initial_light_on_dt[, timestamp := as.Date(timestamp)]
  
  max_initial_light_on_dt = merge(DAY_LABEL, max_initial_light_on_dt, by = "timestamp", all.x = T)
  max_initial_light_on_dt$initial_light_switch_on_ratio = na.locf(max_initial_light_on_dt$initial_light_switch_on_ratio)
  
  p <- ggplot() + 
    geom_path() +
    ylab("Light switch operation (%/day)") +
    scale_y_continuous(labels = percent) +
    coord_cartesian(ylim=c(0,1)) +
    ggtitle(paste0(lab, ": initial light-on lifting (daily max)\n"))
  
  p = add.window.line(p, max_initial_light_on_dt, "initial_light_switch_on_ratio", windowingWeek, get.expDate.2(), shadowing=T)
  # p = add.colorful.window.line(p, max_initial_light_on_dt, "initial_light_on", windowingWeek, 'black', get.expDate.2(), shadowing=T)
  p = add.event.vline.exp2(p)
  p = set.default.theme(p)
  
  # print(p)
  save.plot(paste0(PLOT_PATH, lab, "_initial light-on lifting.png"), p)

  if (lab == "marg") {
    p1 = p
  } else if (lab == "hcc") {
    p2 = p
  } else {
    p3 = p
  }
  
  write.csv(max_initial_light_on_dt, paste0('../data/max_initial_light_switch_on_ratio(', lab, ').csv'))

}

plot_name = "MARG_HCC_UX"
p4 <- grid.arrange(arrangeGrob(p1 + theme(legend.position="none", plot.title = element_blank()),
                               p2 + theme(legend.position="none", plot.title = element_blank(), axis.title.y = element_blank()),
                               p3 + theme(legend.position="none", plot.title = element_blank(), axis.title.y = element_blank()),
                               nrow=1),
                   nrow=2, heights=c(10, 1), top = paste0(plot_name, "_", LABEL, "\n"))

save.plot(paste0(PLOT_PATH, plot_name, "_", LABEL, ".png"), p4, width_ = 24, height_ = 7)

print(paste0(PLOT_PATH, plot_name, "_", LABEL, ".png saved"))

##
## 4. inital light on event statistics
##    -- daily initial light on duration (unit: hours)
##    -- ratio of daily initial light on duration to daily total light on duration 
for(lab in LAB_LABLES){
  
  file_name = paste0(STATUS_AGG_PATH, lab, "_light_aggregated_status_dt.csv")
  
  tmp = get.light.tidy.event.dt(fread(file_name), 10)
  
  daily_light_on_duration = tmp[light_usage != 0, .(daily_light_on_duration = sum(duration)/60/60), by=aggDay]
  daily_inital_light_on_duration = tmp[initial_on == 1, .(daily_initial_light_on_duration = sum(duration)/60/60), by=aggDay]
  
  # fill the empty date to '0'
  aggDay_vec = unique(tmp$aggDay)
  light_on_duration_dt = merge(data.frame(aggDay = aggDay_vec), daily_light_on_duration, all = T)
  light_on_duration_dt = merge(light_on_duration_dt, daily_inital_light_on_duration)
  
  print(lab)
  print(mean(light_on_duration_dt$daily_initial_light_on_duration))
  print(mean(light_on_duration_dt$daily_initial_light_on_duration / light_on_duration_dt$daily_light_on_duration))
  
}
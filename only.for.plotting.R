# source('3_partial_light_on_ratio.R')

LABEL = "partial_light_on_ratio"

PLOT_PATH = "../plots/"

if(PLOTTING){
  # for(lab in 1:length(PARTIAL_LIGHT_ON_RATIO_list)){
  #   plot_partial_lightOn <- plot.partial.lightOn(PARTIAL_LIGHT_ON_RATIO_list[lab], get.expDate.1.1(), PLOT_PATH)  
  # }
  
  # for(lab in 1:length(PARTIAL_LIGHT_ON_RATIO_list)){
  #   plot_partial_lightOn <- plot.partial.lightOn(PARTIAL_LIGHT_ON_RATIO_list[lab], get.expDate.1.2(), PLOT_PATH)  
  # }
  
  # for(lab in 1:length(PARTIAL_LIGHT_ON_RATIO_list)){
  #   plot_partial_lightOn <- plot.partial.lightOn(PARTIAL_LIGHT_ON_RATIO_list[lab], get.expDate.2(), PLOT_PATH)  
  # }
  
  for(lab in 1:(length(PARTIAL_LIGHT_ON_RATIO_list)/3)){
    combined.plot(plot.partial.lightOn, PARTIAL_LIGHT_ON_RATIO_list[lab], PARTIAL_LIGHT_ON_RATIO_list[lab + (length(PARTIAL_LIGHT_ON_RATIO_list)/3)], PARTIAL_LIGHT_ON_RATIO_list[lab + 2*(length(PARTIAL_LIGHT_ON_RATIO_list)/3)], get.expDate.2(), PLOT_PATH, "partial_light_on_ratio", individualPlotting = F)
  }
}




# source("50-3_status2event(light).R")
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
  
  denominator = quantile(agg_event_dt$light_usage, 1)/2
  print(denominator)
  
  # # # dinner time OFF event counting 
  max_initial_light_on_dt = agg_event_dt[initial_on == 1 & event=='ON', 
                                         .(initial_light_switch_on_ratio = max(light_usage_diff)/denominator), by=aggDay]
  setnames(max_initial_light_on_dt, "aggDay", "timestamp")
  max_initial_light_on_dt[, timestamp := as.Date(timestamp)]
  
  max_initial_light_on_dt = merge(DAY_LABEL, max_initial_light_on_dt, by = "timestamp", all.x = T)
  
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
  
  # write.csv(max_initial_light_on_dt, paste0('../data/max_initial_light_switch_on_ratio(', lab, ').csv'))
  
}

plot_name = "MARG_HCC_UX"
p4 <- grid.arrange(arrangeGrob(p1 + theme(legend.position="none", plot.title = element_blank()),
                               p2 + theme(legend.position="none", plot.title = element_blank(), axis.title.y = element_blank()),
                               p3 + theme(legend.position="none", plot.title = element_blank(), axis.title.y = element_blank()),
                               nrow=1),
                   nrow=2, heights=c(10, 1), top = paste0(plot_name, "_", LABEL, "\n"))

save.plot(paste0(PLOT_PATH, plot_name, "_", LABEL, ".png"), p4, width_ = 24, height_ = 7)

print(paste0(PLOT_PATH, plot_name, "_", LABEL, ".png saved"))



# source("4_light_off_afterwork_ratio.R")
LABEL = "light_off_afterwork_ratio"
PLOT_PATH = "../plots/"

if(PLOTTING){
  # for(lab in 1:length(WHOLE_DAY_LIGHT_ON_COUNT_list)){
  #   plot_24hr_lightOn_counting <- plot.24hr.lightOn.counting(WHOLE_DAY_LIGHT_ON_COUNT_list[lab], get.expDate.1.1(), PLOT_PATH)  
  # }
  
  # for(lab in 1:length(WHOLE_DAY_LIGHT_ON_COUNT_list)){
  #   plot_24hr_lightOn_counting <- plot.24hr.lightOn.counting(WHOLE_DAY_LIGHT_ON_COUNT_list[lab], get.expDate.1.2(), PLOT_PATH)  
  # }
  
  # for(lab in 1:length(WHOLE_DAY_LIGHT_ON_COUNT_list)){
  #   plot_24hr_lightOn_counting <- plot.24hr.lightOn.counting(WHOLE_DAY_LIGHT_ON_COUNT_list[lab], get.expDate.2(), PLOT_PATH)  
  # }
  
  for(lab in 1:(length(WHOLE_DAY_LIGHT_ON_COUNT_list)/3)){
    combined.plot(plot.24hr.lightOn.counting, WHOLE_DAY_LIGHT_ON_COUNT_list[lab], WHOLE_DAY_LIGHT_ON_COUNT_list[lab + (length(WHOLE_DAY_LIGHT_ON_COUNT_list)/3)], WHOLE_DAY_LIGHT_ON_COUNT_list[lab + 2*(length(WHOLE_DAY_LIGHT_ON_COUNT_list)/3)], get.expDate.2(), PLOT_PATH, "light_off_afterwork_ratio", individualPlotting = F)
  }
}


# source("5_lunch_saving.R")
LABEL = "lunch_time_saving_ratio"
PLOT_PATH = "../plots/"

PLOTTING = T
#plot
if(PLOTTING){
  # for(lab in 1:length(LUNCH_TIME_SAVING_RATIO_list)){
  #   plot_lunch_saving <- plot.lunch.saving(LUNCH_TIME_SAVING_RATIO_list[lab], get.expDate.1.1(), PLOT_PATH)
  # }
  # for(lab in 1:(length(LUNCH_TIME_SAVING_RATIO_list))){
  #   plot_lunch_saving <- plot.lunch.saving(LUNCH_TIME_SAVING_RATIO_list[lab], get.expDate.1.2(), PLOT_PATH)
  #   plot_lunch_saving <- plot.lunch.saving(LUNCH_TIME_SAVING_RATIO_list[lab], get.expDate.2(), PLOT_PATH)
  # }
  for(lab in 1:(length(LUNCH_TIME_SAVING_RATIO_list)/3)){
    combined.plot(plot.lunch.saving, LUNCH_TIME_SAVING_RATIO_list[lab], LUNCH_TIME_SAVING_RATIO_list[lab + (length(LUNCH_TIME_SAVING_RATIO_list)/3)], LUNCH_TIME_SAVING_RATIO_list[lab + 2*(length(LUNCH_TIME_SAVING_RATIO_list)/3)], get.expDate.2(), PLOT_PATH, "lunch_time_saving_ratio", individualPlotting = F)
  }
}

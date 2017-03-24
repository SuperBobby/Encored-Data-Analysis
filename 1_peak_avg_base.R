### ------------------------------------------------------------ ###
### basic_statistics(peak, base, average) of each feeder
### 
### JY, EJ @ ADSL, SNU 
###                                   last update : 2016. 11. 19.
### ------------------------------------------------------------ ###

# * peak : 90th percentile of usage in the aggregate unit 
# * base : 10th percentile of usage in the aggregate unit 
# * average : average of usage in the aggregate unit 


#' @title The functions for describing basic statistics
#' (peak, base, average) of each feeder.
#'
#' @description retrieving or plotting basic statistics of a feeder's usage.
#'
#' @author Jeongyoon Han \email{hanjy@snu.ac.kr}
#' @author Eunjung Lee \email{ej-lee@snu.ac.kr}
#' @author Hyunghun Cho \email{webofthink@snu.ac.kr}
#'
#' @exportMethod get.basic.stats
#' @exportMethod plot.basic.stats
#'
#' @include 0-1_pre-processing(15min).R
#' @include 0-2_pre-processing(RealSense).R
#' @include 0-3_functions_for_table.R
#' @include 0-4_funtions_for_plot.R

### --------------------------------------------------------------------- ###
### Build list of tables : STATS_list
### 
### table_name = {lab}_{agg_unit}_{day_type}_{feeder} (with peak, base, avg columns)
### --------------------------------------------------------------------- ### 


STATS_list = list()

PEAK_PERCENTILE = 0.9
BASE_PERCENTILE = 0.1

PLOT_PATH = "../plots/"

LABEL = 'peak_avg_base'

## four stat : peak, base, avg 
get.four.stats <- function(usage, thre_peak, thre_base){
  peak = quantile(usage, thre_peak, na.rm = T)
  avg  = mean(usage, na.rm = T)
  base = quantile(usage, thre_base, na.rm = T)

  return(list(peak=peak, base=base, avg=avg))
}

## Loop parameters 
# 1. lab
# 2. aggregation unit 
# 3. types of day (working day?)
# 4. target feeder

LABS = c("MARG", "HCC", "UX")                                  # lab
# LABS = c("MARG", "HCC", "UX", "All_Labs")                    # lab
AGG_UNITS = c("aggDay")                                        # agg_unit
# AGG_UNITS = c("aggWeek", "aggDay")                           # agg_unit
# TYPES_OF_DAY = c("allDay", "workingday", "nonworkingday")      # day_type
TYPES_OF_DAY = c("allDay")      # day_type

FEEDERS = c("total", "total_woHVAC", "computer", "light", "hvac")                         # feeder
# FEEDERS = c("total", "total_woHVAC", "total_woETC", "computer", "light", "hvac")            # feeder


dt_list = setNames(dt_list, LABS)

# 1. lab
for(lab in LABS){ 
  lab_dt = dt_list[[lab]]
  
  # 2. Aggregation unit
  for(agg_unit in AGG_UNITS){
    
    # 3. Types of day
    for(day_type in TYPES_OF_DAY){
      
      # 4. Target feeder ----> feeder_stats = label
      for(feeder in FEEDERS){
        
        # table name
        dt_name = paste(lab, agg_unit, day_type, feeder, sep="_")
        print(paste("Build table:", dt_name))

        if(day_type == "allDay"){
          stats_dt = lab_dt[, as.list(get.four.stats(get(feeder), PEAK_PERCENTILE, BASE_PERCENTILE)), 
                            by=.(timestamp=get(agg_unit))]
          
        } else if(day_type == "workingday") {
          stats_dt = lab_dt[workingday == T, as.list(get.four.stats(get(feeder), PEAK_PERCENTILE, BASE_PERCENTILE)), 
                            by=.(timestamp=get(agg_unit))]
          
        } else if(day_type == "nonworkingday") {
          stats_dt = lab_dt[workingday == F, as.list(get.four.stats(get(feeder), PEAK_PERCENTILE, BASE_PERCENTILE)), 
                            by=.(timestamp=get(agg_unit))]
        }
        STATS_list = append(STATS_list, setNames(list(stats_dt),dt_name))
      }
    }
  }
}


### -------------------------------- ###
### Plot: stats    
### -------------------------------- ### 

plot.stats <- function(dt, expDate, PLOT_PATH, plotting = T) {
  plot_dt = dt[[1]]
  
  if(expDate[length(expDate)] == "2014-11-17"){
    #exp1-1
    plot_dt = cut.expDate.1.1(plot_dt)
    plot_name = paste('exp1-1', names(dt), sep="_")
  } else if(expDate[length(expDate)] == "2015-01-22"){
    #exp1-2
    plot_dt = cut.expDate.1.2(plot_dt)
    plot_name = paste('exp1-2', names(dt), sep="_")
  } else{
    #exp3
    plot_dt = cut.expDate.2(plot_dt)
    plot_name = paste('exp2', names(dt), sep="_")
  }
 
  windowingWeek = 4
  
  stats <- ggplot() + 
    geom_path() +
    ggtitle(paste0(plot_name,'\n')) +
    # ylab("Energy use (kWh/day)")+
    ylab("Power comsumption (Watt/15min)\n") +
    scale_linetype_discrete(breaks=c("peak", "avg", "base"))
  
  stats = add.window.line(stats, plot_dt, "avg", windowingWeek, expDate, shadowing=T)
  stats = add.window.line(stats, plot_dt, "peak", windowingWeek, expDate)
  stats = add.window.line(stats, plot_dt, "base", windowingWeek, expDate)
  
  # feeder = strsplit(plot_name, "_")[[1]][5]
  # if (feeder == "computer") {
  #   line.palette = c("#3D5799", "#3D5799", "#3D5799")
  # } else if (feeder == "light") {
  #   line.palette = c("#F0960E", "#F0960E", "#F0960E")
  # } else if (feeder == "hvac") {
  #   line.palette = c("#CA3E34", "#CA3E34", "#CA3E34")
  # } else {
  #   line.palette = c("#444A45", "#444A45", "#444A45")
  # }
  # 
  # stats = add.colorful.window.line(stats, plot_dt, "peak", windowingWeek, line.palette[1], expDate)
  # stats = add.colorful.window.line(stats, plot_dt, "avg", windowingWeek, line.palette[2], expDate, shadowing=T)
  # stats = add.colorful.window.line(stats, plot_dt, "base", windowingWeek, line.palette[3], expDate)
  
    
  if(expDate[length(expDate)] == "2014-11-17"){
    #exp1-1
    stats = add.event.vline.exp1.1(stats)
  } else if(expDate[length(expDate)] == "2015-01-22"){
    #exp1-2
    stats = add.event.vline.exp1.2(stats)
  } else{
    #exp2
    stats = add.event.vline.exp2(stats)
  }
  
  stats = set.default.theme(stats)
  
  # if(feeder == "hvac") {
  #   stats = stats + scale_y_continuous(limits=c(0, 3), oob=rescale_none)
  # }

  if (plotting) {
    save.plot(paste0(PLOT_PATH, plot_name, "_peak_avg_base.png"), stats)
  }
  
  print(paste("plot:", plot_name))
  return(stats)
}



### plot
if(PLOTTING){
  # for(lab in 1:length(STATS_list)){
  #   plot_stats <- plot.stats(STATS_list[lab], get.expDate.1.1(), PLOT_PATH)
  # }
  
  # for(lab in 1:length(STATS_list)){
  #   plot_stats <- plot.stats(STATS_list[lab], get.expDate.1.2(), PLOT_PATH)
  # }

  # for(lab in 1:length(STATS_list)){
  #   plot_stats <- plot.stats(STATS_list[lab], get.expDate.2(), PLOT_PATH)
  # }
  
  for(lab in 1:(length(STATS_list)/3)){
    combined.plot(plot.stats, STATS_list[lab], STATS_list[lab + (length(STATS_list)/3)], STATS_list[lab + 2*(length(STATS_list)/3)], get.expDate.1.2(), PLOT_PATH, 'peak_avg_base', individualPlotting = F)
    # plot.stats(STATS_list[lab], get.expDate.2(), PLOT_PATH)
  }

  for(lab in 1:(length(STATS_list)/3)){
    combined.plot(plot.stats, STATS_list[lab], STATS_list[lab + (length(STATS_list)/3)], STATS_list[lab + 2*(length(STATS_list)/3)], get.expDate.2(), PLOT_PATH, 'peak_avg_base', individualPlotting = F)
      # plot.stats(STATS_list[lab], get.expDate.2(), PLOT_PATH)
  }

}

### ------------------------------------------------------------ ###
### Update summary_list
###
### category = {lab} + {day_type} + {label} 
###                                 {label} = {feeder}_{stats}
### ------------------------------------------------------------ ### 

## initialize summary list 
# summary_list = list()
# 
# target_summary_list = STATS_list
# 
# target_labs         = LABS                    # lab 
# target_types_of_day = TYPES_OF_DAY            # day_type
# target_feeders      = FEEDERS                 # feeder  
# target_stats        = c('peak','avg','base')  # stats ----> {feeder}_{stats} = label
# 
# agg_unit = "aggDay"
# 
# for(lab in target_labs){
#   
#   for(day_type in target_types_of_day){
#     
#     for(feeder in target_feeders){
#       
#       for(stats in target_stats){
#         
#         dt_name = paste(lab, agg_unit, day_type, feeder, sep="_")
#         target_dt = target_summary_list[[dt_name]][,c('timestamp',stats),with=F]
#         # print(head(target_dt))
#         
#         label = paste(feeder, stats, sep='_')
#         category_name = paste(lab, day_type, label, sep='_')
#         
#         print(category_name)
#         
#         split_list = split.table.by.expDate(target_dt, all_expDate)
#         
#         summary_list = append(summary_list, setNames(list(split_list), category_name))
#       }
#     }
#   }
# }

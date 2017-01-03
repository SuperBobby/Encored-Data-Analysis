### ------------------------------------------------------------ ###
### basic_statistics(peak, base, average) of each feeder
### 
### JY, EJ @ ADSL, SNU 
###                                   last update : 2016. 11. 19.
### ------------------------------------------------------------ ###

# * peak : 90th percentile of usage in the aggregate unit 
# * base : 10th percentile of usage in the aggregate unit 
# * average : average of usage in the aggregate unit 

### --------------------------------------------------------------------- ###
### Build list of tables : STATS_list
### 
### table_name = {lab}_{agg_unit}_{day_type}_{feeder} (with peak, base, avg columns)
### --------------------------------------------------------------------- ### 

STATS_list = list()

PEAK_PERCENTILE = 0.9
BASE_PERCENTILE = 0.1

PLOT_PATH = "../plots/"

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
TYPES_OF_DAY = c("allDay", "workingday", "non_workingday")     # day_type
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
          
        } else if(day_type == "non_workingday") {
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

plot.stats <- function(dt, expDate, PLOT_PATH) {
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
  
  stats <- ggplot(plot_dt, aes(x=timestamp)) +
    ggtitle(plot_name) +
    ylab("Energy use (kWh/day)")+
    scale_linetype_discrete(breaks=c("peak", "avg", "base"))
  
  stats = add.window.line(stats, plot_dt, plot_name, "peak", windowingWeek, expDate)
  stats = add.window.line(stats, plot_dt, plot_name, "base", windowingWeek, expDate)
  stats = add.window.line(stats, plot_dt, plot_name, "avg", windowingWeek, expDate)
    
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
  
  save.plot(paste0(PLOT_PATH, plot_name, "_peak_avg_base.png"), stats)
  
  print(paste("plot:", plot_name))
  return(stats)
}


### plot
if(PLOTTING){
  # for(lab in 1:length(STATS_list)){
  #   plot_stats <- plot.stats(STATS_list[lab], get.expDate.1.1(), PLOT_PATH)
  # }
  
  for(lab in 1:length(STATS_list)){
    plot_stats <- plot.stats(STATS_list[lab], get.expDate.1.2(), PLOT_PATH)
  }
  
  for(lab in 1:length(STATS_list)){
    plot_stats <- plot.stats(STATS_list[lab], get.expDate.2(), PLOT_PATH)
  }
  
}

### ------------------------------------------------------------ ###
### Update summary_list
###
### category = {lab} + {day_type} + {label} 
###                                 {label} = {feeder}_{stats}
### ------------------------------------------------------------ ### 

## initialize summary list 
summary_list = list()

target_summary_list = STATS_list

target_labs         = LABS                    # lab 
target_types_of_day = TYPES_OF_DAY            # day_type
target_feeders      = FEEDERS                 # feeder  
target_stats        = c('peak','avg','base')  # stats ----> {feeder}_{stats} = label

agg_unit = "aggDay"

for(lab in target_labs){
  
  for(day_type in target_types_of_day){
    
    for(feeder in target_feeders){
      
      for(stats in target_stats){
        
        dt_name = paste(lab, agg_unit, day_type, feeder, sep="_")
        target_dt = target_summary_list[[dt_name]][,c('timestamp',stats),with=F]
        # print(head(target_dt))
        
        label = paste(feeder, stats, sep='_')
        category_name = paste(lab, day_type, label, sep='_')
        
        print(category_name)
        
        split_list = split.table.by.expDate(target_dt, all_expDate)
        
        summary_list = append(summary_list, setNames(list(split_list), category_name))
      }
    }
  }
}
### ------------------------------------------------------------ ###
### Basic statistics(peak, base, average, median) of 15min data
### 
### JY, EJ @ ADSL, SNU 
###                                   last update : 2016. 8. 30.
### ------------------------------------------------------------ ###

### ------------------------------------------------------- ###
### Build list: STATS_list
### 
### category = {lab} + {day_type} + {label} 
###                                 {label} = {feeder} + {stats}
### ------------------------------------------------------- ### 

PEAK_PERCENTILE = 0.9
BASE_PERCENTILE = 0.1

## four stat : peak, base, avg, med 
get.four.stats <- function(usage, thre_peak, thre_base){
  peak = quantile(usage, thre_peak, na.rm = T)
  base = quantile(usage, thre_base, na.rm = T)
  avg  = mean(usage, na.rm = T)
  # med  = median(usage, na.rm = T)
  
  return(list(peak=peak, base=base, avg=avg))
}

## Loop parameters 
# 1. lab
# 2. aggregation unit 
# 3. types of day (working day?)
# 4. target feeder

LABS = c("MARG", "HCC", "UX", "All_Labs")                    # lab
AGG_UNITS = c("aggWeek", "aggDay")                           # agg_unit
TYPES_OF_DAY = c("allDay", "workingday", "non_workingday")   # day_type
FEEDERS = c("total", "computer", "light", "hvac")            # feeder

dt_list = setNames(dt_list, LABS)
STATS_list = list()

for(lab in LABS){ 
  # 1. lab
  lab_dt = dt_list[[lab]]
  
  # 2. Aggregation unit
  for(agg_unit in AGG_UNITS){
    
    # 3. Types of day
    for(day_type in TYPES_OF_DAY){
      
      # 4. Target feeder ----> feeder_stats = label
      for(feeder in FEEDERS){
        
        dt_name = paste(lab, agg_unit, day_type, feeder, sep="_")
        
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
        
        print(paste("Build table:", dt_name))
        # assign(dt_name, stats_dt)
        
        STATS_list = append(STATS_list, setNames(list(stats_dt),dt_name))
      }
    }
  }
}


### -------------------------------- ###
### Plot: stats    
### -------------------------------- ### 

plot.stats <- function(dt, expDate) {
  plot_dt = dt[[1]]
  
  if(expDate[4] == "2016-11-16"){
    #exp1-1
    plot_dt = cut.expDate.1.1(plot_dt)
    plot_name = paste('exp1-1', names(dt), sep="_")
  } else if(expDate[4] == "2015-01-22"){
    #exp1-2
    plot_dt = cut.expDate.1.2(plot_dt)
    plot_name = paste('exp1-2', names(dt), sep="_")
  } else{
    #exp3
    plot_dt = cut.expDate.2(plot_dt)
    plot_name = paste('exp2', names(dt), sep="_")
  }
  
  rownum_expDate = get.expDate.rownum(plot_dt, expDate)
  
  windowingWeek = 4
  
  stats <- ggplot(plot_dt, aes(x=timestamp)) +
    ggtitle(plot_name) +
    ylab("Energy use (kWh/day)")+
    scale_linetype_discrete(breaks=c("peak", "avg", "base"))
  
  stats = add.window.line(stats, plot_dt, plot_name, "peak", windowingWeek, rownum_expDate, expDate)
  stats = add.window.line(stats, plot_dt, plot_name, "base", windowingWeek, rownum_expDate, expDate)
  stats = add.window.line(stats, plot_dt, plot_name, "avg", windowingWeek, rownum_expDate, expDate)
    
  if(expDate[4] == "2016-11-16"){
    #exp1-1
    stats = add.event.vline.exp1.1(stats)
  } else if(expDate[4] == "2015-01-22"){
    #exp1-2
    stats = add.event.vline.exp1.2(stats)
  } else{
    #exp3
    stats = add.event.vline.exp2(stats)
  }
  
  stats = set.default.theme(stats)
  
  save.plot(paste0("../plots/stats/", plot_name, ".png"), stats)
  
  print(paste("plot:", plot_name))
  return(stats)
}


### plot
for(lab in 1:length(STATS_list)){
  plot_stats <- plot.stats(STATS_list[lab], get.expDate.1.1())
}

for(lab in 1:length(STATS_list)){
  plot_stats <- plot.stats(STATS_list[lab], get.expDate.1.2())
}

for(lab in 1:length(STATS_list)){
  plot_stats <- plot.stats(STATS_list[lab], get.expDate.2())
}



### -------------------------------- ###
### summary_list
### Added category: feeders + stats    
### -------------------------------- ### 

## initialize summary list 
summary_list = list()

target_summary_list = STATS_list
all_expDate = get.expDate.all()


target_labs         = LABS                    # lab 
target_types_of_day = TYPES_OF_DAY            # day_type
target_feeders      = FEEDERS                 # feeder  
target_stats        = c('peak','base','avg')  # stats ----> feeder + stats = label

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


### -------------------------------- ###
### representation_table
### -------------------------------- ### 

## initialize summary list 
representation_table = data.frame(exp_label = names(get.expDate.all()))
representation_func = mean

for(category in names(summary_list)){
  
  one_category = summary_list[[category]]
  
  category_values = numeric(0)
  
  for(exp_label in names(one_category)){
    
    exp_dt = one_category[[exp_label]]
    
    exp_data = data.frame(exp_dt)[,2]
    
    representative_value = representation_func(exp_data)
    
    category_values = c(category_values, representative_value)
    # names(category_values)[length(category_values)] = exp_label
    
    print(paste(category, exp_label))
    # print(exp_dt)
    # print(exp_data)
    print(representative_value)
    
  }
  print(category_values)
  representation_table = cbind(representation_table, category_values)
  names(representation_table)[length(representation_table)] = category
}

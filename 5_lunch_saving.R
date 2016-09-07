### ------------------------------------------------------------ ###
### lunch_time_saving_count per week (computer & light feeder) 
### 
### JY, EJ @ ADSL, SNU 
###                                   last update : 2016. 8. 30.
### ------------------------------------------------------------ ###

# * Def: number of ‘target-off(including partial off)’ action during lunch-time per week 
# * ‘target-off’ action: (minimum of ‘during lunch’) < (maximum of ‘before & after lunch’) * 80% when (maximum of before lunch > 10% of peak)
# * before lunch usage: 11:00 ~ 12:00 — index 17:20
# * during lunch usage: 11:30 ~ 13:30 — index 19:26
# * after lunch usage: 13:00 ~ 14:00    — index 25:28
# * Unit: # of action / week

### --------------------------------------------------------------------- ###
### Build list of tables : LUNCH_TIME_SAVING_COUNT_list
### 
### table_name = {lab}_{agg_unit}_{day_type}_{feeder} (with lunch_time_saving_count column)
### --------------------------------------------------------------------- ### 

LUNCH_TIME_SAVING_COUNT_list = list()

LABEL = "lunch_time_saving_count"

## Loop parameters 
# 1. lab
# 2. aggregation unit 
# 3. types of day (working day?)
# 4. target feeder

LABS = c("MARG", "HCC", "UX", "All_Labs")                    # lab
AGG_UNITS = c("aggWeek")                                     # agg_unit
TYPES_OF_DAY = c("allDay", "workingday", "non_workingday")   # day_type
FEEDERS = c("computer", "light")                             # feeder

dt_list = setNames(dt_list, LABS)


get.lunch.time.saving.count <- function(sub_dt, feeder){
  
  # * before lunch usage: 11:00 ~ 12:00 — index 17:20
  # * during lunch usage: 11:30 ~ 13:30 — index 19:26
  # *  after lunch usage: 13:00 ~ 14:00 — index 25:28
  before_lunch_index = 17:20
  during_lunch_index = 19:26
  after_lunch_index  = 25:28
  
  lunch_saving_threshold_ratio = 0.8
  target_on_usage = 0.01
  
  before_dt = sub_dt[index %in% before_lunch_index, .(before_lunch_max = max(get(feeder))), by=aggDay]
  during_dt = sub_dt[index %in% during_lunch_index, .(during_lunch_min = min(get(feeder))), by=aggDay]
   after_dt = sub_dt[index %in%  after_lunch_index, .( after_lunch_max = max(get(feeder))), by=aggDay]
  
  lunch_dt = merge(before_dt, during_dt, by='aggDay')
  lunch_dt = merge(lunch_dt, after_dt, by='aggDay')
  
  
  lunch_dt[, ':='(lunch_saving = 0)]
  lunch_dt[    (during_lunch_min < (before_lunch_max * lunch_saving_threshold_ratio)) 
             & (during_lunch_min < ( after_lunch_max * lunch_saving_threshold_ratio)) 
             & (before_lunch_max > target_on_usage), ':='(lunch_saving = 1), by=aggDay]
  
  lunch_time_saving_count = sum(lunch_dt$lunch_saving)
  
  return(lunch_time_saving_count)
}


# 1. lab
for(lab in LABS){ 
  lab_dt = dt_list[[lab]]
  
  # 2. Aggregation unit
  for(agg_unit in AGG_UNITS){
    
    # 3. Types of day
    for(day_type in TYPES_OF_DAY){
      
      # 4. Target feeder ----> {feeder}_lunch_time_saving_count = label
      for(feeder in FEEDERS){
        
        # table name
        dt_name = paste(lab, agg_unit, day_type, feeder, LABEL, sep="_")
        print(paste("Build table:", dt_name))
        
        # # subset by day_type ------------------> Why it doesn't work? should be figured out!
        # if(day_type == "workingday"){ 
        #   lab_dt = lab_dt[workingday == T] 
        # } else if(day_type == "non_workingday"){ 
        #   lab_dt = lab_dt[workingday == F] 
        # } 

        if(day_type == "allDay"){
          
          lunch_time_saving_count_dt = lab_dt[, .(get.lunch.time.saving.count(.SD, feeder)), by=agg_unit]
          
        } else if(day_type == "workingday") {
          
          lunch_time_saving_count_dt = lab_dt[workingday == T, .(get.lunch.time.saving.count(.SD, feeder)), by=agg_unit]
          
        } else if(day_type == "non_workingday") {
          
          lunch_time_saving_count_dt = lab_dt[workingday == F, .(get.lunch.time.saving.count(.SD, feeder)), by=agg_unit]
        }
        
        # change column name
        names(lunch_time_saving_count_dt) = c("timestamp", paste(feeder, LABEL, sep='_'))
        
        LUNCH_TIME_SAVING_COUNT_list = append(LUNCH_TIME_SAVING_COUNT_list, setNames(list(lunch_time_saving_count_dt),dt_name))
      }
    }
  }
}



### -------------------------------- ###
### Plot: lunch_time_saving_count     
### -------------------------------- ### 


plot.lunch.saving <- function(dt, expDate){
  
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
    #exp2
    plot_dt = cut.expDate.2(plot_dt)
    plot_name = paste('exp2', names(dt), sep="_")
  }
  
  windowingWeek <- 4
  
  #   print(plot_name)
  
  p1 = ggplot(plot_dt, aes(x=timestamp)) +
    ggtitle(plot_name)

  target_col = colnames(plot_dt)[2]
  p1 = add.window.line(p1, plot_dt, plot_name, target_col, windowingWeek, expDate)
  
  if(expDate[length(expDate)] == "2014-11-17"){
    #exp1-1
    p1 = add.event.vline.exp1.1(p1)
  } else if(expDate[length(expDate)] == "2015-01-22"){
    #exp1-2
    p1 = add.event.vline.exp1.2(p1)
  } else{
    #exp2
    p1 = add.event.vline.exp2(p1)
  }
  
  p1 = set.default.theme(p1)
  
  save.plot(paste0("../plots/lunch_saving/", plot_name, ".png"), p1)
  
  return(p1)
}


#plot
for(lab in 1:length(LUNCH_TIME_SAVING_COUNT_list)){
  plot_lunch_saving <- plot.lunch.saving(LUNCH_TIME_SAVING_COUNT_list[lab], get.expDate.1.1())  
}
for(lab in 1:length(LUNCH_TIME_SAVING_COUNT_list)){
  plot_lunch_saving <- plot.lunch.saving(LUNCH_TIME_SAVING_COUNT_list[lab], get.expDate.1.2())  
}
for(lab in 1:length(LUNCH_TIME_SAVING_COUNT_list)){
  plot_lunch_saving <- plot.lunch.saving(LUNCH_TIME_SAVING_COUNT_list[lab], get.expDate.2())  
}




### ------------------------------------------------------------ ###
### Update summary_list
###
### category = {lab} + {day_type} + {label} 
###                                 {label} = {feeder}_lunch_time_saving_count
### ------------------------------------------------------------ ### 

target_summary_list = LUNCH_TIME_SAVING_COUNT_list
all_expDate = get.expDate.all()

target_labs         = LABS                    # lab 
target_types_of_day = TYPES_OF_DAY            # day_type
target_feeders      = FEEDERS                 # feeder  

agg_unit = "aggWeek"

for(lab in target_labs){
  
  for(day_type in target_types_of_day){
    
    for(feeder in target_feeders){
      
      local_label = paste(feeder, LABEL, sep='_')
      
      dt_name = paste(lab, agg_unit, day_type, local_label, sep="_")
      target_dt = target_summary_list[[dt_name]][,c('timestamp', local_label),with=F]
      
      category_name = paste(lab, day_type, local_label, sep='_')
      
      print(category_name)
      
      split_list = split.table.by.expDate(target_dt, all_expDate)
      
      ## update summary_list 
      summary_list = append(summary_list, setNames(list(split_list), category_name))
    }
  }
}

# source('10_representation_table.R')

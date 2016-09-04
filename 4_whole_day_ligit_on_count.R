### ------------------------------------------------------------ ###
### whole_day_ligit_on_count per week 
### 
### JY, EJ @ ADSL, SNU 
###                                   last update : 2016. 9. 3.
### ------------------------------------------------------------ ###

# * Def: number of days which is ‘light-on duration == 24hrs’ per week 
# * Unit: # of days / week 
# * Down is good

### -------------------------------------------------------------------- ###
### Build list of tables : WHOLE_DAY_LIGHT_ON_COUNT_list
### 
### table_name = {lab}_{agg_unit}_{day_type}_'whole_day_ligit_on_count' 
### -------------------------------------------------------------------- ### 

WHOLE_DAY_LIGHT_ON_COUNT_list = list()
LIGHT_ON_MIN_USAGE = 0.01

LABEL = "whole_day_ligit_on_count"

## Loop parameters 
# 1. lab
# 2. aggregation unit 
# 3. types of day (working day?)
# 4. target feeder

LABS = c("MARG", "HCC", "UX", "All_Labs")                    # lab
AGG_UNITS = c("aggWeek")                                     # agg_unit
TYPES_OF_DAY = c("allDay", "workingday", "non_workingday")   # day_type

dt_list = setNames(dt_list, LABS)


get.whole.day.ligit.on.count <- function(sub_dt){
  whole_day_ligit_on_dt = sub_dt[, .(whole_day_ligit_on = (sum(light > LIGHT_ON_MIN_USAGE)==96)), by=aggDay]
  
  return(sum(whole_day_ligit_on_dt$whole_day_ligit_on))
}


# 1. lab
for(lab in LABS){ 
  lab_dt = dt_list[[lab]]
  
  # 2. Aggregation unit
  for(agg_unit in AGG_UNITS){
    
    # 3. Types of day
    for(day_type in TYPES_OF_DAY){
      
      # table name 
      dt_name = paste(lab, agg_unit, day_type, LABEL, sep="_")
      print(paste("Build table:", dt_name))
      
      if(day_type == "allDay"){
        
        whole_day_ligit_on_count_dt = lab_dt[, .(whole_day_ligit_on_count = get.whole.day.ligit.on.count(.SD)), by=aggWeek]
        
      } else if(day_type == "workingday") {
        
        whole_day_ligit_on_count_dt = lab_dt[workingday == T, .(whole_day_ligit_on_count = get.whole.day.ligit.on.count(.SD)), by=aggWeek]
        
      } else if(day_type == "non_workingday") {
        
        whole_day_ligit_on_count_dt = lab_dt[workingday == F, .(whole_day_ligit_on_count = get.whole.day.ligit.on.count(.SD)), by=aggWeek]
      }
      
      # change column name
      names(whole_day_ligit_on_count_dt) = c("timestamp", LABEL)
      
      # append data.table to list 
      WHOLE_DAY_LIGHT_ON_COUNT_list = append(WHOLE_DAY_LIGHT_ON_COUNT_list, setNames(list(whole_day_ligit_on_count_dt),dt_name))
    }
  }
}


### -------------------------------- ###
### Plot: whole_day_ligit_on_count     
### -------------------------------- ### 
# 
# plot.24hr.lightOn.counting <- function(dt, expDate, full_lightON_color = "violetred4"){
#   
#   plot_dt = dt[[1]]
#   
#   if(expDate[4] == "2016-11-16"){
#     #exp1-1
#     plot_dt = set.expDate.1.1(plot_dt)
#     plot_name = paste('exp1-1', names(dt), sep="_")
#   } else if(expDate[4] == "2015-01-22"){
#     #exp1-2
#     plot_dt = set.expDate.1.2(plot_dt)
#     plot_name = paste('exp1-2', names(dt), sep="_")
#   } else{
#     #exp3
#     plot_dt = set.expDate.2(plot_dt)
#     plot_name = paste('exp2', names(dt), sep="_")
#   }
# 
#   windowingWeek <- 4
# 
#   print(plot_name)
#   
#   p <- ggplot(plot_dt, aes(x=get)) +
#     geom_point(aes(y=full_lightON_10), colour='gray70') +
#     ggtitle(plot_name)+
#     ylab("24hrs light-ON day (count/week)")
#   
#   p = add.colorful.window.line(p, plot_dt, plot_name, 'full_lightON_10', windowingWeek, full_lightON_color, expDate)
#   
#   if(expDate[4] == "2016-11-16"){
#     #exp1-1
#     p = add.event.vline.exp1.1(p)
#   } else if(expDate[4] == "2015-01-22"){
#     #exp1-2
#     p = add.event.vline.exp1.2(p)
#   } else{
#     #exp3
#     p = add.event.vline.exp2(p)
#   }
#   
#   p = set.colorful.theme(p, full_lightON_color)
# 
#   save.plot(paste0("../plots/", plot_name, ".png"), p)
#   
#   return(p)
# }

#plot
# for(lab in 1:length(table_full_lightOn)){
#   plot_full_lightOn <- plot.24hr.lightOn.counting(table_full_lightOn[lab], get.expDate.2())  
# }


### ------------------------------------------------------------ ###
### Update summary_list
###
### category = {lab} + {day_type} + {label} 
###                                 {label} = 'whole_day_ligit_on_count' 
### ------------------------------------------------------------ ### 

target_summary_list = WHOLE_DAY_LIGHT_ON_COUNT_list
all_expDate = get.expDate.all()

target_labs         = LABS                    # lab 
target_types_of_day = TYPES_OF_DAY            # day_type

agg_unit = "aggWeek"

for(lab in target_labs){
  
  for(day_type in target_types_of_day){
    
    dt_name = paste(lab, agg_unit, day_type, LABEL, sep="_")
    target_dt = target_summary_list[[dt_name]][,c('timestamp', LABEL),with=F]
    
    category_name = paste(lab, day_type, LABEL, sep='_')
    
    print(category_name)
    
    split_list = split.table.by.expDate(target_dt, all_expDate)
    
    ## update summary_list 
    summary_list = append(summary_list, setNames(list(split_list), category_name))
  }
}

# source('10_representation_table.R')

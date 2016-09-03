### ------------------------------------------------------------ ###
### Partial Light-on ratio per day
### 
### JY, EJ @ ADSL, SNU 
###                                   last update : 2016. 9. 3.
### ------------------------------------------------------------ ###

# * Def: below 80% of light peak usage duration per day / light-on duration per day 
#       (Excluded every-light-off)
# * Unit: % 
# * Up is good

### -------------------------------------------------------------------- ###
### Build list of tables : PARTIAL_LIGHT_ON_RATIO_list
### 
### table_name = {lab}_{agg_unit}_{day_type}_'partial_light_on_ratio' 
### -------------------------------------------------------------------- ### 

PARTIAL_LIGHT_ON_RATIO_list = list()

LIGHT_ON_MIN_USAGE = 0.01
PARTIAL_ON_RATIO = 0.8

LABEL = "partial_light_on_ratio"

## Loop parameters 
# 1. lab
# 2. aggregation unit 
# 3. types of day (working day?)
# 4. target feeder

LABS = c("MARG", "HCC", "UX", "All_Labs")                    # lab
AGG_UNITS = c("aggWeek", "aggDay")                           # agg_unit
TYPES_OF_DAY = c("allDay", "workingday", "non_workingday")   # day_type

dt_list = setNames(dt_list, LABS)

get.partial.light.on.ratio <- function(light, LIGHT_ON_MIN_USAGE, light_peak, PARTIAL_ON_RATIO){
  
  # filter '010' partial-light-on pattern
  filter.fault.partial.light.on <- function(raw_partial_light_on){
    filtered_partial_light_on = raw_partial_light_on    
    for(i in 2:(length(raw_partial_light_on)-1)){
      if(sum(raw_partial_light_on[(i-1):(i+1)]) == 1){
        filtered_partial_light_on[i] = F
      }
    }
    return(filtered_partial_light_on)
  }
  
  light_on = light > LIGHT_ON_MIN_USAGE
  raw_partial_light_on = (light > LIGHT_ON_MIN_USAGE) & (light < light_peak * PARTIAL_ON_RATIO)
  
  partial_light_on = filter.fault.partial.light.on(raw_partial_light_on)
  
  # No use of light, partial-light-on-ratio = 0
  if(sum(light_on) == 0){
    return(0)
  } else {
    partial_light_on_ratio = sum(partial_light_on) / sum(light_on)
  }
  return(partial_light_on_ratio)
}

{
# #---#
# tmp <- dt_list[[LABS[3]]]
# tmp_light_peak = quantile(tmp$light, .9)
# 
# tmp[, .(partial_light_on_ratio = sum(light > LIGHT_ON_MIN_USAGE & light < (tmp_light_peak*.8)) / sum(light > 0)), by=aggDay]
# 
# tmp_dt = tmp[, .(light = light, 
#                  light_on = light > LIGHT_ON_MIN_USAGE, 
#                  partial = light > LIGHT_ON_MIN_USAGE & light < tmp_light_peak*0.8,
#                  filtered_partial = filter.fault.partial.light.on(light > LIGHT_ON_MIN_USAGE & light < tmp_light_peak*0.8) ),
#              by=aggDay]
# 
# # 
# tmp_dt = tmp[, .(get.partial.light.on.ratio(light, LIGHT_ON_MIN_USAGE, tmp_light_peak, PARTIAL_ON_RATIO)), 
#     by=aggDay]
# names(tmp_dt) <- c("timestamp", LABEL)

# 
# aggDay partial_ratio
# 1: 2014-10-01     1.0000000
# 2: 2014-10-02     0.3414634
# 3: 2014-10-03     0.1186441
# 4: 2014-10-04     0.2142857
# 5: 2014-10-05     0.2131148
# 694: 2016-08-24     0.6052632
# 695: 2016-08-25     0.7763158
# 696: 2016-08-26     0.7500000
# 697: 2016-08-27     1.0000000
# 698: 2016-08-28     1.0000000
# 
# 
# tmp_date = "2014-12-06"
# tmp_dt[aggDay == tmp_date]
# 
# sum(tmp_dt[aggDay == tmp_date]$filtered_partial) / sum(tmp_dt[aggDay == tmp_date]$light_on)
# 
# #---#
}

# 1. lab
for(lab in LABS){ 
  lab_dt = dt_list[[lab]]
  light_peak = quantile(lab_dt$light, .9)
  
  # 2. Aggregation unit
  for(agg_unit in AGG_UNITS){
    
    # 3. Types of day
    for(day_type in TYPES_OF_DAY){
      
      # table name 
      dt_name = paste(lab, agg_unit, day_type, LABEL, sep="_")
      print(paste("Build table:", dt_name))
      
      if(day_type == "allDay"){
        
        partial_light_on_ratio_dt = lab_dt[, .(get.partial.light.on.ratio(light, LIGHT_ON_MIN_USAGE, light_peak, PARTIAL_ON_RATIO)), 
                                           by=get(agg_unit)]
        
      } else if(day_type == "workingday") {
        
        partial_light_on_ratio_dt = lab_dt[workingday == T, .(get.partial.light.on.ratio(light, LIGHT_ON_MIN_USAGE, light_peak, PARTIAL_ON_RATIO)),
                                           by=get(agg_unit)]
        
        
      } else if(day_type == "non_workingday") {
        
        partial_light_on_ratio_dt = lab_dt[workingday == F, .(get.partial.light.on.ratio(light, LIGHT_ON_MIN_USAGE, light_peak, PARTIAL_ON_RATIO)), 
                                           by=get(agg_unit)]
      }
      names(partial_light_on_ratio_dt) = c("timestamp", LABEL)
#       print(summary(partial_light_on_ratio_dt))
      PARTIAL_LIGHT_ON_RATIO_list = append(PARTIAL_LIGHT_ON_RATIO_list, setNames(list(partial_light_on_ratio_dt),dt_name))
    }
  }
}


### -------------------------------- ###
### Plot: partial_light_on_ratio     
### -------------------------------- ### 
# 
# plot.partial.lightOn <- function(dt, expDate, partial_lightON_color = "orange2"){
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
#   #   print(plot_name)
#   partial_lightON <- ggplot(plot_dt, aes(x=get)) +
#     ggtitle(plot_name)+
#     ylab("Partial light-ON (%)")
#   partial_lightON = add.colorful.window.line(partial_lightON, plot_dt, plot_name, 'threshold80', windowingWeek, partial_lightON_color, expDate)
#   
#   if(expDate[4] == "2016-11-16"){
#     #exp1-1
#     partial_lightON = add.event.vline.exp1.1(partial_lightON)
#   } else if(expDate[4] == "2015-01-22"){
#     #exp1-2
#     partial_lightON = add.event.vline.exp1.2(partial_lightON)
#   } else{
#     #exp3
#     partial_lightON = add.event.vline.exp2(partial_lightON)
#   }
#   
#   partial_lightON  = set.colorful.theme(partial_lightON, partial_lightON_color)
#   
#   save.plot(paste0("../plots/",plot_name, ".png"), partial_lightON)
#   
#   return(partial_lightON)
# }
# 
# 
# #plot
# for(lab in 1:length(table_partial_lightOn)){
#   plot_partial_lightOn <- plot.partial.lightOn(table_partial_lightOn[lab], get.expDate.2())  
# }
# 




### ------------------------------------------------------------ ###
### Update summary_list
###
### category = {lab} + {day_type} + {label} 
###                                 {label} = 'partial_light_on_ratio' 
### ------------------------------------------------------------ ### 

target_summary_list = PARTIAL_LIGHT_ON_RATIO_list
all_expDate = get.expDate.all()

target_labs         = LABS                    # lab 
target_types_of_day = TYPES_OF_DAY            # day_type

agg_unit = "aggDay"

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

source('10_representation_table.R')
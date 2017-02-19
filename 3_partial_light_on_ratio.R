### ------------------------------------------------------------ ###
### partial_light_on_ratio per day
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

PLOT_PATH = "../plots/"

## Loop parameters 
# 1. lab
# 2. aggregation unit 
# 3. types of day (working day?)
# 4. target feeder

LABS = c("MARG", "HCC", "UX")                                # lab
AGG_UNITS = c("aggWeek", "aggDay")[2]                           # agg_unit
TYPES_OF_DAY = c("allDay", "workingday", "nonworkingday")[1]   # day_type

dt_list = setNames(dt_list, LABS)

get.partial.light.on.ratio <- function(light, LIGHT_ON_MIN_USAGE, light_peak, PARTIAL_ON_RATIO){
  
  # filter 'FTF' partial-light-on pattern  <--- update needed~!
  filter.fault.partial.light.on <- function(raw_partial_light_on){
    filtered_partial_light_on = raw_partial_light_on    
    for(i in 2:(length(raw_partial_light_on)-1)){
      if(raw_partial_light_on[(i-1)] == FALSE & raw_partial_light_on[(i+1)] == FALSE){
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
    partial_light_on_ratio = (sum(partial_light_on) / sum(light_on)) * 100
  }
  return(partial_light_on_ratio)
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
        
        
      } else if(day_type == "nonworkingday") {
        
        partial_light_on_ratio_dt = lab_dt[workingday == F, .(get.partial.light.on.ratio(light, LIGHT_ON_MIN_USAGE, light_peak, PARTIAL_ON_RATIO)), 
                                           by=get(agg_unit)]
      }
      names(partial_light_on_ratio_dt) = c("timestamp", LABEL)

            PARTIAL_LIGHT_ON_RATIO_list = append(PARTIAL_LIGHT_ON_RATIO_list, setNames(list(partial_light_on_ratio_dt),dt_name))
    }
  }
}


### -------------------------------- ###
### Plot: partial_light_on_ratio     
### -------------------------------- ### 

plot.partial.lightOn <- function(dt, expDate, PLOT_PATH, partial_lightON_color = "orange2"){
  
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
  
  windowingWeek = 4
  
  #   print(plot_name)
  partial_lightON <- ggplot(plot_dt, aes(x=timestamp)) +
    ggtitle(plot_name)+
    ylab("Partial light-ON (%)")
  
  partial_lightON = add.colorful.window.line(partial_lightON, plot_dt, 'partial_light_on_ratio', windowingWeek, partial_lightON_color, expDate)
  
  if(expDate[length(expDate)] == "2014-11-17"){
    #exp1-1
    partial_lightON = add.event.vline.exp1.1(partial_lightON)
  } else if(expDate[length(expDate)] == "2015-01-22"){
    #exp1-2
    partial_lightON = add.event.vline.exp1.2(partial_lightON)
  } else{
    #exp2
    partial_lightON = add.event.vline.exp2(partial_lightON)
  }
  
  partial_lightON = set.colorful.theme(partial_lightON)
  
  save.plot(paste0(PLOT_PATH, plot_name, ".png"), partial_lightON)
  
  print(paste("plot:", plot_name))
  return(partial_lightON)
}


#plot
if(PLOTTING){
  # for(lab in 1:length(PARTIAL_LIGHT_ON_RATIO_list)){
  #   plot_partial_lightOn <- plot.partial.lightOn(PARTIAL_LIGHT_ON_RATIO_list[lab], get.expDate.1.1(), PLOT_PATH)  
  # }
  
  # for(lab in 1:length(PARTIAL_LIGHT_ON_RATIO_list)){
  #   plot_partial_lightOn <- plot.partial.lightOn(PARTIAL_LIGHT_ON_RATIO_list[lab], get.expDate.1.2(), PLOT_PATH)  
  # }
  
  for(lab in 1:length(PARTIAL_LIGHT_ON_RATIO_list)){
    plot_partial_lightOn <- plot.partial.lightOn(PARTIAL_LIGHT_ON_RATIO_list[lab], get.expDate.2(), PLOT_PATH)  
  }
}


### ------------------------------------------------------------ ###
### Update summary_list
###
### category = {lab} + {day_type} + {label} 
###                                 {label} = 'partial_light_on_ratio' 
### ------------------------------------------------------------ ### 

target_summary_list = PARTIAL_LIGHT_ON_RATIO_list

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

# source('10_representation_table.R')
### ------------------------------------------------------------ ###
### max_consecutive_hvac per day
### 
### JY, EJ @ ADSL, SNU 
###                                   last update : 2016. 9. 3.
### ------------------------------------------------------------ ###

# * Def: 
# * Unit: hours 
# * Down is good

### -------------------------------------------------------------------- ###
### Build list of tables : MAX_CONSECUTIVE_HVAC_list
### 
### table_name = {lab}_{agg_unit}_{day_type}_'max_consecutive_hvac' 
### -------------------------------------------------------------------- ### 

MAX_CONSECUTIVE_HVAC_list = list()

LABEL = "max_consecutive_hvac"

PLOT_PATH = "../plots/"

## Loop parameters 
# 1. lab
# 2. aggregation unit 
# 3. types of day (working day?)
# 4. target feeder

LABS = c("MARG")                                            # lab
AGG_UNITS = c("aggDay")                                     # agg_unit
TYPES_OF_DAY = c("allDay", "workingday", "nonworkingday")  # day_type

dt_list = setNames(dt_list, LABS)

get.max.consecutive.hvac <- function(hvac_usage, verbose=T){
  rle_return = rle(hvac_usage > 0.1)
  
  true_sequence = rle_return$lengths[rle_return$values]
  
  if(length(true_sequence) != 0) {
    result = max(true_sequence, na.rm = T)  
  } else {
    result = 0
  }
  
  if(verbose == T) print((result))
  
  return(result*15/60)
}

# tmp = c(1,1,1,2,3,4,4,4,4,4,4,6,0,0,0,0,2,3,4,5,7,9,0,0,0,1)
# 
# tmptmptmp = get.max.consecutive.hvac(tmp)
# 
# tmptmp = rle(tmp > 3)
# 
# tmptmp$lengths[tmptmp$values]


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
        
        max_consecutive_hvac_dt = lab_dt[, .(get.max.consecutive.hvac(hvac)), by=get(agg_unit)]
        
      } else if(day_type == "workingday") {
        
        max_consecutive_hvac_dt = lab_dt[workingday == T, .(get.max.consecutive.hvac(hvac)), by=get(agg_unit)]
        
        
      } else if(day_type == "nonworkingday") {
        
        max_consecutive_hvac_dt = lab_dt[workingday == F, .(get.max.consecutive.hvac(hvac)), by=get(agg_unit)]
      }
      names(max_consecutive_hvac_dt) = c("timestamp", LABEL)
      
      MAX_CONSECUTIVE_HVAC_list = append(MAX_CONSECUTIVE_HVAC_list, setNames(list(max_consecutive_hvac_dt),dt_name))
    }
  }
}


### -------------------------------- ###
### Plot: max_consecutive_hvac     
### -------------------------------- ### 

plot.max.consecutive.hvac <- function(dt, expDate, PLOT_PATH, max_consecutive_hvac_color = 'red'){
  
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
  
  max_consecutive_hvac <- ggplot(plot_dt, aes(x=timestamp)) +
    ggtitle(plot_name) +
    ylab("Max Consecutive HVAC duration (hrs)")
  
  max_consecutive_hvac = add.colorful.window.line(max_consecutive_hvac, plot_dt, 'max_consecutive_hvac', windowingWeek, max_consecutive_hvac_color, expDate)
  
  if(expDate[length(expDate)] == "2014-11-17"){
    #exp1-1
    max_consecutive_hvac = add.event.vline.exp1.1(max_consecutive_hvac)
  } else if(expDate[length(expDate)] == "2015-01-22"){
    #exp1-2
    max_consecutive_hvac = add.event.vline.exp1.2(max_consecutive_hvac)
  } else{
    #exp2
    max_consecutive_hvac = add.event.vline.exp2(max_consecutive_hvac)
  }
  
  max_consecutive_hvac = set.colorful.theme(max_consecutive_hvac)
  
  save.plot(paste0(PLOT_PATH, plot_name, ".png"), max_consecutive_hvac)
  
  print(paste("plot:", plot_name))
  return(max_consecutive_hvac)
}


#plot
if(PLOTTING){
  # for(lab in 1:length(MAX_CONSECUTIVE_HVAC_list)){
  #   plot_max_consecutive_hvac <- plot.max.consecutive.hvac(MAX_CONSECUTIVE_HVAC_list[lab], get.expDate.1.1(), PLOT_PATH)  
  # }
  
  for(lab in 1:length(MAX_CONSECUTIVE_HVAC_list)){
    plot_max_consecutive_hvac <- plot.max.consecutive.hvac(MAX_CONSECUTIVE_HVAC_list[lab], get.expDate.1.2(), PLOT_PATH)  
  }
  
  for(lab in 1:length(MAX_CONSECUTIVE_HVAC_list)){
    plot_max_consecutive_hvac <- plot.max.consecutive.hvac(MAX_CONSECUTIVE_HVAC_list[lab], get.expDate.2(), PLOT_PATH)  
  }
}


### ------------------------------------------------------------ ###
### Update summary_list
###
### category = {lab} + {day_type} + {label} 
###                                 {label} = 'max_consecutive_hvac' 
### ------------------------------------------------------------ ### 

target_summary_list = MAX_CONSECUTIVE_HVAC_list

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
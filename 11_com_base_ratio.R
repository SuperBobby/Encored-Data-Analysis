### ------------------------------------------------------------ ###
### com_base_ratio per day
### 
### JY, EJ @ ADSL, SNU 
###                                   last update : 2016. 9. 3.
### ------------------------------------------------------------ ###

# * Def: computer base / computer average in A working day 
# * Unit: % 
# * Down is good

### -------------------------------------------------------------------- ###
### Build list of tables : COM_BASE_RATIO_list
### 
### table_name = {lab}_{agg_unit}_{day_type}_'com_base_ratio' 
### -------------------------------------------------------------------- ### 

COM_BASE_RATIO_list = list()

LABEL = "com_base_ratio"

PLOT_PATH = "../plots/"

## Loop parameters 
# 1. lab
# 2. aggregation unit 
# 3. types of day (working day?)
# 4. target feeder

LABS = c("MARG", "HCC", "UX")                                # lab
AGG_UNITS = c("aggDay")                                      # agg_unit
TYPES_OF_DAY = c("allDay", "workingday", "nonworkingday")   # day_type

dt_list = setNames(dt_list, LABS)

get.com.base.ratio <- function(usage){
  avg  = mean(usage, na.rm = T)
  base = quantile(usage, 0.1, na.rm = T)
  
  return((base/avg)*100)
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
        
        com_base_ratio_dt = lab_dt[, .(get.com.base.ratio(computer)), by=get(agg_unit)]
        
      } else if(day_type == "workingday") {
        
        com_base_ratio_dt = lab_dt[workingday == T, .(get.com.base.ratio(computer)), by=get(agg_unit)]
        
        
      } else if(day_type == "nonworkingday") {
        
        com_base_ratio_dt = lab_dt[workingday == F, .(get.com.base.ratio(computer)), by=get(agg_unit)]
      }
      names(com_base_ratio_dt) = c("timestamp", LABEL)
      
      COM_BASE_RATIO_list = append(COM_BASE_RATIO_list, setNames(list(com_base_ratio_dt),dt_name))
    }
  }
}


### -------------------------------- ###
### Plot: com_base_ratio     
### -------------------------------- ### 

plot.com.base.ratio <- function(dt, expDate, PLOT_PATH, com_base_ratio_color = 'green'){
  
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
  com_base_ratio <- ggplot(plot_dt, aes(x=timestamp)) +
    ggtitle(plot_name)+
    ylab("Computer Base Ratio (%)")
  
  com_base_ratio = add.colorful.window.line(com_base_ratio, plot_dt, 'com_base_ratio', windowingWeek, com_base_ratio_color, expDate)
  
  if(expDate[length(expDate)] == "2014-11-17"){
    #exp1-1
    com_base_ratio = add.event.vline.exp1.1(com_base_ratio)
  } else if(expDate[length(expDate)] == "2015-01-22"){
    #exp1-2
    com_base_ratio = add.event.vline.exp1.2(com_base_ratio)
  } else{
    #exp2
    com_base_ratio = add.event.vline.exp2(com_base_ratio)
  }
  
  com_base_ratio = set.colorful.theme(com_base_ratio)
  
  save.plot(paste0(PLOT_PATH, plot_name, ".png"), com_base_ratio)
  
  print(paste("plot:", plot_name))
  return(com_base_ratio)
}


#plot
if(PLOTTING){
  # for(lab in 1:length(COM_BASE_RATIO_list)){
  #   plot_com_base <- plot.com.base.ratio(COM_BASE_RATIO_list[lab], get.expDate.1.1(), PLOT_PATH)  
  # }
  
  for(lab in 1:length(COM_BASE_RATIO_list)){
    plot_com_base <- plot.com.base.ratio(COM_BASE_RATIO_list[lab], get.expDate.1.2(), PLOT_PATH)  
  }
  
  for(lab in 1:length(COM_BASE_RATIO_list)){
    plot_com_base <- plot.com.base.ratio(COM_BASE_RATIO_list[lab], get.expDate.2(), PLOT_PATH)  
  }
}


### ------------------------------------------------------------ ###
### Update summary_list
###
### category = {lab} + {day_type} + {label} 
###                                 {label} = 'com_base_ratio' 
### ------------------------------------------------------------ ### 

target_summary_list = COM_BASE_RATIO_list

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
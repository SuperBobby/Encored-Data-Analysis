### ------------------------------------------------------------ ###
### hvac_on_duration per day
### 
### JY, EJ @ ADSL, SNU 
###                                   last update : 2016. 11. 19.
### ------------------------------------------------------------ ###

# * HVAC-on_duration: sum of 'HVAC-on' duration per day
#   ('HVAC-on': over 0.1 consumption(for 15min))
# * unit: hours / day
# * Down is good

### -------------------------------------------------------------------- ###
### Build list of tables : HVAC_ON_DURATION_list
### 
### table_name = {lab}_{agg_unit}_{day_type}_'hvac_on_duration'
### -------------------------------------------------------------------- ### 

HVAC_ON_DURATION_list = list()
HVAC_ON_MIN_USAGE = 0.1

LABEL = 'hvac_on_duration'

PLOT_PATH = "../plots/"

## Loop parameters 
# 1. lab
# 2. aggregation unit 
# 3. types of day (working day?)
# 4. target feeder

LABS = c("MARG", "HCC", "UX")                                # lab
AGG_UNITS = c("aggDay")                                      # agg_unit
TYPES_OF_DAY = c("allDay", "workingday", "non_workingday")   # day_type

dt_list = setNames(dt_list, LABS)

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
        
        hvac_on_duration_dt = lab_dt[, .(sum(hvac > HVAC_ON_MIN_USAGE)), 
                                      by=get(agg_unit)]
        
      } else if(day_type == "workingday") {
        
        hvac_on_duration_dt = lab_dt[workingday == T, .(sum(hvac > HVAC_ON_MIN_USAGE)), 
                                      by=get(agg_unit)]
        
      } else if(day_type == "non_workingday") {
        
        hvac_on_duration_dt = lab_dt[workingday == F, .(sum(hvac > HVAC_ON_MIN_USAGE)), 
                                      by=get(agg_unit)]
      }
      
      # change column name
      names(hvac_on_duration_dt) = c("timestamp", LABEL)
      
      # change unit: '(count of 15min) / (day or week)' to 'hours/day'
      if(agg_unit == 'aggDay'){
        
        hvac_on_duration_dt$hvac_on_duration = hvac_on_duration_dt$hvac_on_duration * 15 / 60
        
      } else {
        
        hvac_on_duration_dt$hvac_on_duration = hvac_on_duration_dt$hvac_on_duration * 15 / 60 / 7
      }
      
      # append data.table to list 
      HVAC_ON_DURATION_list = append(HVAC_ON_DURATION_list, setNames(list(hvac_on_duration_dt),dt_name))
    }
  }
}



### -------------------------------- ###
### Plot: hvac_on duration     
### -------------------------------- ### 

plot.hvac.On.duration <- function(dt, expDate, PLOT_PATH, hvacON_duration_color = "darkolivegreen"){
  
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
  
  hvac_on_duration <- ggplot(plot_dt, aes(x=timestamp)) +
    ggtitle(plot_name)+
    scale_y_continuous(limits=c(0,24), oob=rescale_none) +
    ylab("HVAC-ON duration (hours)")+
    scale_color_discrete(breaks = c("hvac_on_duration"), labels = c("hvac on duration (hours/day)"))
  
  hvac_on_duration = add.colorful.window.line(hvac_on_duration, plot_dt, plot_name, 'hvac_on_duration', 
                                               windowingWeek, hvacON_duration_color, expDate)
  
  if(expDate[length(expDate)] == "2014-11-17"){
    #exp1-1
    hvac_on_duration = add.event.vline.exp1.1(hvac_on_duration)
  } else if(expDate[length(expDate)] == "2015-01-22"){
    #exp1-2
    hvac_on_duration = add.event.vline.exp1.2(hvac_on_duration)
  } else{
    #exp2
    hvac_on_duration = add.event.vline.exp2(hvac_on_duration)
  }
  
  hvac_on_duration = set.colorful.theme(hvac_on_duration)
  
  save.plot(paste0(PLOT_PATH, plot_name, ".png"), hvac_on_duration)
  
  print(paste("plot:", plot_name))
  return(hvac_on_duration)
}



#plot
# for(lab in 1:length(HVAC_ON_DURATION_list)){
#   plot_hvacOn_duration <- plot.hvac.On.duration(HVAC_ON_DURATION_list[lab], get.expDate.1.1(), PLOT_PATH)  
# }

for(lab in 1:length(HVAC_ON_DURATION_list)){
  plot_hvacOn_duration <- plot.hvac.On.duration(HVAC_ON_DURATION_list[lab], get.expDate.1.2(), PLOT_PATH)  
}

for(lab in 1:length(HVAC_ON_DURATION_list)){
  plot_hvacOn_duration <- plot.hvac.On.duration(HVAC_ON_DURATION_list[lab], get.expDate.2(), PLOT_PATH)  
}




### ------------------------------------------------------------ ###
### Update summary_list
###
### category = {lab} + {day_type} + {label} 
###                                 {label} = 'hvac_on_duration' 
### ------------------------------------------------------------ ### 

target_summary_list = HVAC_ON_DURATION_list
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

# source('10_representation_table.R')
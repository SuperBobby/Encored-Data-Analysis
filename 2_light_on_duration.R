### ------------------------------------------------------------ ###
### light_on_duration per day
### 
### JY, EJ @ ADSL, SNU 
###                                   last update : 2016. 11. 19.
### ------------------------------------------------------------ ###

# * Light-on_duration: sum of 'light-on' duration per day
#   ('light-on': over 0.01 consumption(for 15min))
# * unit: hours / day
# * Down is good

### -------------------------------------------------------------------- ###
### Build list of tables : LIGHT_ON_DURATION_list
### 
### table_name = {lab}_{agg_unit}_{day_type}_'light_on_duration'
### -------------------------------------------------------------------- ### 

LIGHT_ON_DURATION_list = list()
LIGHT_ON_MIN_USAGE = 0.01

LABEL = 'light_on_duration'

PLOT_PATH = "../plots/"

## Loop parameters 
# 1. lab
# 2. aggregation unit 
# 3. types of day (working day?)
# 4. target feeder

LABS = c("MARG", "HCC", "UX")                                # lab
AGG_UNITS = c("aggWeek", "aggDay")                           # agg_unit
TYPES_OF_DAY = c("allDay", "workingday", "nonworkingday")   # day_type

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
        
        light_on_duration_dt = lab_dt[, .(sum(light > LIGHT_ON_MIN_USAGE)), 
                                      by=get(agg_unit)]
        
      } else if(day_type == "workingday") {
        
        light_on_duration_dt = lab_dt[workingday == T, .(sum(light > LIGHT_ON_MIN_USAGE)), 
                                      by=get(agg_unit)]
        
      } else if(day_type == "nonworkingday") {
        
        light_on_duration_dt = lab_dt[workingday == F, .(sum(light > LIGHT_ON_MIN_USAGE)), 
                                      by=get(agg_unit)]
      }
      
      # change column name
      names(light_on_duration_dt) = c("timestamp", LABEL)
      
      # change unit: '(count of 15min) / (day or week)' to 'hours/day'
      if(agg_unit == 'aggDay'){
        
        light_on_duration_dt$light_on_duration = light_on_duration_dt$light_on_duration * 15 / 60
          
      } else {
        
        light_on_duration_dt$light_on_duration = light_on_duration_dt$light_on_duration * 15 / 60 / 7
      }
      
      # append data.table to list 
      LIGHT_ON_DURATION_list = append(LIGHT_ON_DURATION_list, setNames(list(light_on_duration_dt),dt_name))
    }
  }
}




### -------------------------------- ###
### Plot: light_on duration     
### -------------------------------- ### 


plot.light.On.duration <- function(dt, expDate, PLOT_PATH, lightON_duration_color = "darkolivegreen", plotting = T){
  
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
 
  light_on_duration <- ggplot() + 
    geom_path() +
    ggtitle(plot_name)+
    scale_y_continuous(limits=c(0,24), oob=rescale_none) +
    ylab("Light-ON duration (hours)")+
    scale_color_discrete(breaks = c("light_on_duration"), labels = c("light on duration (hours/day)"))
  
  light_on_duration = add.colorful.window.line(light_on_duration, plot_dt, 'light_on_duration', 
                                               windowingWeek, 'black', expDate)
  
  if(expDate[length(expDate)] == "2014-11-17"){
    #exp1-1
    light_on_duration = add.event.vline.exp1.1(light_on_duration)
  } else if(expDate[length(expDate)] == "2015-01-22"){
    #exp1-2
    light_on_duration = add.event.vline.exp1.2(light_on_duration)
  } else{
    #exp2
    light_on_duration = add.event.vline.exp2(light_on_duration)
  }

  light_on_duration = set.default.theme(light_on_duration)
  
  if (plotting) {
    save.plot(paste0(PLOT_PATH, plot_name, ".png"), light_on_duration)
  }

  print(paste("plot:", plot_name))
  return(light_on_duration)
}



#plot
if(PLOTTING){
  # for(lab in 1:length(LIGHT_ON_DURATION_list)){
  #   plot_lightOn_duration <- plot.light.On.duration(LIGHT_ON_DURATION_list[lab], get.expDate.1.1(), PLOT_PATH)  
  # }
  
  # for(lab in 1:length(LIGHT_ON_DURATION_list)){
  #   plot_lightOn_duration <- plot.light.On.duration(LIGHT_ON_DURATION_list[lab], get.expDate.1.2(), PLOT_PATH)  
  # }
  # 
  # for(lab in 1:length(LIGHT_ON_DURATION_list)){
  #   plot_lightOn_duration <- plot.light.On.duration(LIGHT_ON_DURATION_list[lab], get.expDate.2(), PLOT_PATH)  
  # }
  for(lab in 1:(length(LIGHT_ON_DURATION_list)/3)){
    combined.plot(plot.light.On.duration, LIGHT_ON_DURATION_list[lab], LIGHT_ON_DURATION_list[lab + (length(LIGHT_ON_DURATION_list)/3)], LIGHT_ON_DURATION_list[lab + 2*(length(LIGHT_ON_DURATION_list)/3)], get.expDate.2(), PLOT_PATH, 'light_on_duration', individualPlotting = F)
  }
}




### ------------------------------------------------------------ ###
### Update summary_list
###
### category = {lab} + {day_type} + {label} 
###                                 {label} = 'light_on_duration' 
### ------------------------------------------------------------ ### 

target_summary_list = LIGHT_ON_DURATION_list

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

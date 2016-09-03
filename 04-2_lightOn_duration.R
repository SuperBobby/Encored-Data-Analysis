### ------------------------------------------------------------ ###
### Light-on duration per day
### 
### JY, EJ @ ADSL, SNU 
###                                   last update : 2016. 8. 30.
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

## Loop parameters 
# 1. lab
# 2. aggregation unit 
# 3. types of day (working day?)
# 4. target feeder

LABS = c("MARG", "HCC", "UX", "All_Labs")                    # lab
AGG_UNITS = c("aggWeek", "aggDay")                           # agg_unit
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
      dt_name = paste(lab, agg_unit, day_type,"light_on_duration", sep="_")
      print(paste("Build table:", dt_name))
      
      if(day_type == "allDay"){
        
        light_on_duration_dt = lab_dt[, .(light_on_duration = sum(light > LIGHT_ON_MIN_USAGE)), 
                                      by=.(timestamp=get(agg_unit))]
        
      } else if(day_type == "workingday") {
        
        light_on_duration_dt = lab_dt[workingday == T, .(light_on_duration = sum(light > LIGHT_ON_MIN_USAGE)), 
                                      by=.(timestamp=get(agg_unit))]
        
        
      } else if(day_type == "non_workingday") {
        
        light_on_duration_dt = lab_dt[workingday == F, .(light_on_duration = sum(light > LIGHT_ON_MIN_USAGE)), 
                                      by=.(timestamp=get(agg_unit))]
      }
      
      # change unit: '(count of 15min) / (day or week)' to 'hours/day'
      if(agg_unit == 'aggDay'){
        
        light_on_duration_dt$light_on_duration = light_on_duration_dt$light_on_duration * 15 / 60
          
      } else {
        
        light_on_duration_dt$light_on_duration = light_on_duration_dt$light_on_duration * 15 / 60 / 7
      }
      
      LIGHT_ON_DURATION_list = append(LIGHT_ON_DURATION_list, setNames(list(light_on_duration_dt),dt_name))
    }
  }
}




### -------------------------------- ###
### Plot: light_on duration     
### -------------------------------- ### 


plot.light.On.duration <- function(dt, expDate, lightON_duration_color = "darkolivegreen"){
  
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
  
  rownum_expDate <- get.expDate.rownum(plot_dt, expDate)
  
  windowingWeek <- 4
  
#   print(plot_name)
  
  light_on_duration <- ggplot(plot_dt, aes(x=timestamp)) +
    ggtitle(plot_name)+
    scale_y_continuous(limits=c(0,24), oob=rescale_none) +
    ylab("Light-ON duration (hours)")+
    scale_color_discrete(breaks = c("light_on_duration"), labels = c("light on duration (hours/day)"))
  
  light_on_duration = add.colorful.window.line(light_on_duration, plot_dt, plot_name, 'light_on_duration', 
                                               windowingWeek, lightON_duration_color, rownum_expDate)
  
  if(expDate[4] == "2016-11-16"){
    #exp1-1
    light_on_duration = add.event.vline.exp1.1(light_on_duration)
  } else if(expDate[4] == "2015-01-22"){
    #exp1-2
    light_on_duration = add.event.vline.exp1.2(light_on_duration)
  } else{
    #exp3
    light_on_duration = add.event.vline.exp2(light_on_duration)
  }

  light_on_duration = set.colorful.theme(light_on_duration, lightON_duration_color)
  
  
  save.plot(paste0("../plots/light_on_duration/", plot_name, ".png"), light_on_duration)
  
  return(light_on_duration)
}



#plot
for(lab in 1:length(LIGHT_ON_DURATION_list)){
  plot_lightOn_duration <- plot.light.On.duration(LIGHT_ON_DURATION_list[lab], get.expDate.1.1())  
}

for(lab in 1:length(LIGHT_ON_DURATION_list)){
  plot_lightOn_duration <- plot.light.On.duration(LIGHT_ON_DURATION_list[lab], get.expDate.1.2())  
}

for(lab in 1:length(LIGHT_ON_DURATION_list)){
  plot_lightOn_duration <- plot.light.On.duration(LIGHT_ON_DURATION_list[lab], get.expDate.2())  
}




### ------------------------------------------------------------ ###
### summary_list
###
### category = {lab} + {day_type} + {label} 
###                                 {label} = 'light_on_duration' 
### ------------------------------------------------------------ ### 

target_summary_list = LIGHT_ON_DURATION_list
all_expDate = get.expDate.all()

target_labs         = LABS                    # lab 
target_types_of_day = TYPES_OF_DAY            # day_type

agg_unit = "aggDay"

for(lab in target_labs){
  
  for(day_type in target_types_of_day){
    
    dt_name = paste(lab, agg_unit, day_type, "light_on_duration", sep="_")
    target_dt = target_summary_list[[dt_name]][,c('timestamp','light_on_duration'),with=F]

    label = "light_on_duration"
    category_name = paste(lab, day_type, label, sep='_')
    
    print(category_name)
    
    split_list = split.table.by.expDate(target_dt, all_expDate)
    
    ## update summary_list 
    summary_list = append(summary_list, setNames(list(split_list), category_name))
  }
}


### -------------------------------- ###
### representation_table
### -------------------------------- ### 

## initialize representation_table 
representation_table = data.frame(exp_label = names(get.expDate.all()))
representation_func  = mean

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


View(t(representation_table))

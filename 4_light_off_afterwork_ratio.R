### ------------------------------------------------------------ ###
### light_off_afterwork_ratio per week 
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
### table_name = {lab}_{agg_unit}_{day_type}_'light_off_afterwork_ratio' 
### -------------------------------------------------------------------- ### 

WHOLE_DAY_LIGHT_ON_COUNT_list = list()
LIGHT_ON_MIN_USAGE = 0.01

LABEL = "light_off_afterwork_ratio"

PLOT_PATH = "../plots/"

## Loop parameters 
# 1. lab
# 2. aggregation unit 
# 3. types of day (working day?)
# 4. target feeder

LABS = c("MARG", "HCC", "UX")                                # lab
AGG_UNITS = c("aggWeek")                                     # agg_unit
TYPES_OF_DAY = c("allDay", "workingday", "nonworkingday")[1]   # day_type

dt_list = setNames(dt_list, LABS)

get.light.off.afterwork.ratio <- function(sub_dt){
  whole_day_ligit_on_dt = sub_dt[, .(whole_day_ligit_on = (sum(light > LIGHT_ON_MIN_USAGE)==96)), by=aggDay]
  
  return((7-sum(whole_day_ligit_on_dt$whole_day_ligit_on))/7)
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
        
        light_off_afterwork_ratio_dt = lab_dt[, .(light_off_afterwork_ratio = get.light.off.afterwork.ratio(.SD)), by=aggWeek]
        
      } else if(day_type == "workingday") {
        
        light_off_afterwork_ratio_dt = lab_dt[workingday == T, .(light_off_afterwork_ratio = get.light.off.afterwork.ratio(.SD)), by=aggWeek]
        
      } else if(day_type == "nonworkingday") {
        
        light_off_afterwork_ratio_dt = lab_dt[workingday == F, .(light_off_afterwork_ratio = get.light.off.afterwork.ratio(.SD)), by=aggWeek]
      }
      
      # change column name
      names(light_off_afterwork_ratio_dt) = c("timestamp", LABEL)
      
      # append data.table to list 
      WHOLE_DAY_LIGHT_ON_COUNT_list = append(WHOLE_DAY_LIGHT_ON_COUNT_list, setNames(list(light_off_afterwork_ratio_dt),dt_name))
    }
  }
}


### -------------------------------- ###
### Plot: light_off_afterwork_ratio     
### -------------------------------- ### 

plot.24hr.lightOn.counting <- function(dt, expDate, PLOT_PATH, light_off_afterwork_ratio_color = "violetred4"){
  
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
  
  p <- ggplot(plot_dt, aes(x=timestamp)) +
    geom_point(aes(y=light_off_afterwork_ratio), colour='gray70') +
    ggtitle(paste0(plot_name, "\n"))+
    scale_y_continuous(labels = percent) +
    coord_cartesian(ylim=c(0,1)) +
    ylab("light-off afterwork (%/week)\n")
  
  p = add.window.line(p, plot_dt, 'light_off_afterwork_ratio', windowingWeek, expDate)
  
  if(expDate[length(expDate)] == "2014-11-17"){
    #exp1-1
    p = add.event.vline.exp1.1(p)
  } else if(expDate[length(expDate)] == "2015-01-22"){
    #exp1-2
    p = add.event.vline.exp1.2(p)
  } else{
    #exp2
    p = add.event.vline.exp2(p)
  }
  
  p = set.colorful.theme(p)

  save.plot(paste0(PLOT_PATH, plot_name, ".png"), p)
  
  print(paste("plot:", plot_name))
  return(p)
}

##plot
if(PLOTTING){
  # for(lab in 1:length(WHOLE_DAY_LIGHT_ON_COUNT_list)){
  #   plot_24hr_lightOn_counting <- plot.24hr.lightOn.counting(WHOLE_DAY_LIGHT_ON_COUNT_list[lab], get.expDate.1.1(), PLOT_PATH)  
  # }
  
  # for(lab in 1:length(WHOLE_DAY_LIGHT_ON_COUNT_list)){
  #   plot_24hr_lightOn_counting <- plot.24hr.lightOn.counting(WHOLE_DAY_LIGHT_ON_COUNT_list[lab], get.expDate.1.2(), PLOT_PATH)  
  # }
  
  for(lab in 1:length(WHOLE_DAY_LIGHT_ON_COUNT_list)){
    plot_24hr_lightOn_counting <- plot.24hr.lightOn.counting(WHOLE_DAY_LIGHT_ON_COUNT_list[lab], get.expDate.2(), PLOT_PATH)  
  }
}


### ------------------------------------------------------------ ###
### Update summary_list
###
### category = {lab} + {day_type} + {label} 
###                                 {label} = 'light_off_afterwork_ratio' 
### ------------------------------------------------------------ ### 

target_summary_list = WHOLE_DAY_LIGHT_ON_COUNT_list

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

write.csv(WHOLE_DAY_LIGHT_ON_COUNT_list$MARG_aggWeek_allDay_light_off_afterwork_ratio, 
          '../data/light_off_afterwork_weekly_ratio(Lab_A).csv')
write.csv(WHOLE_DAY_LIGHT_ON_COUNT_list$HCC_aggWeek_allDay_light_off_afterwork_ratio, 
          '../data/light_off_afterwork_weekly_ratio(Lab_B).csv')
write.csv(WHOLE_DAY_LIGHT_ON_COUNT_list$UX_aggWeek_allDay_light_off_afterwork_ratio, 
          '../data/light_off_afterwork_weekly_ratio(Lab_C).csv')


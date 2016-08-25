# 1. lab
lab_names = c("MARG", "HCC", "UX", "All Labs")

# 2. aggregation unit 
agg_Units = c("aggWeek", "aggDay")

# 3. day selection
day_selections = c("allDay", "weekDay", "weekEnd")

build.table.lunch.saving <- function(dt_list, target){
  return_dts = list()
  
  for(lab in 1:length(dt_list)){ 
    #   lab_dt & name selection 
    lab_dt = dt_list[[lab]]
    lab_name = lab_names[lab]
      
      for(day_selection in day_selections){
        
        dt_name = paste(lab_name, "aggWeek", day_selection, target, "lunch_saving", sep="_")
        print(dt_name)
        
        # * before lunch usage: 11:00 ~ 12:00 — index 17:20
        # * during lunch usage: 11:30 ~ 13:30 — index 19:26
        # *  after lunch usage: 13:00 ~ 14:00 — index 25:28
        before_lunch_index = 17:20
        during_lunch_index = 19:26
        after_lunch_index  = 25:28
        
        peak_of_target = quantile(data.frame(lab_dt[, .(get(target))])[,1], .90)
        target_on_threshold = peak_of_target * 0.1
        
        lunch_saving_threshold_ratio = 0.8
        target_on_threshold_usage = peak_of_target * 0.1
        
        before_dt = lab_dt[index %in% before_lunch_index, .(before_lunch = max(get(target))), by=aggDay]
        during_dt = lab_dt[index %in% during_lunch_index, .(during_lunch = min(get(target))), by=aggDay]
        after_dt = lab_dt[index %in%  after_lunch_index, .(after_lunch = max(get(target))), by=aggDay]
        
        lunch_dt = merge(before_dt, during_dt, by='aggDay')
        lunch_dt = merge(lunch_dt, after_dt, by='aggDay')
        
        lunch_dt = lunch_dt[, ':='(weekday=isWeekday(aggDay))]
        
        print(head(lunch_dt))
        
        lunch_dt = lunch_dt[, ':='(lunch_saving = 0)]
        
        if(day_selection == "allDay"){
          ## Conditions of lunch saving 
          lunch_dt = lunch_dt[(during_lunch < (before_lunch * lunch_saving_threshold_ratio)) 
                   & (during_lunch < (after_lunch * lunch_saving_threshold_ratio)) 
                   & (before_lunch > target_on_threshold_usage), 
                   ':='(lunch_saving = 1), by=aggDay]
          
        } else if(day_selection == "weekDay") {
          
          lunch_dt = lunch_dt[(weekday == T)
                              &(during_lunch < (before_lunch * lunch_saving_threshold_ratio)) 
                              & (during_lunch < (after_lunch * lunch_saving_threshold_ratio)) 
                              & (before_lunch > target_on_threshold_usage), 
                              ':='(lunch_saving = 1), by=aggDay]
          
        } else if(day_selection == "weekEnd") {
          
          lunch_dt = lunch_dt[(weekday == F)
                              & (during_lunch < (before_lunch * lunch_saving_threshold_ratio)) 
                              & (during_lunch < (after_lunch * lunch_saving_threshold_ratio)) 
                              & (before_lunch > target_on_threshold_usage), 
                              ':='(lunch_saving = 1), by=aggDay]
        }

        ## aggregation: week table 'lunch_saving_per_week' 
        lunch_dt[, ':='(aggWeek=as.Date(cut(aggDay, breaks = "week", start.on.monday = T)))]
        lunch_saving_per_week = lunch_dt[, .(lunch_saving_count = sum(lunch_saving, na.rm = T)), by=aggWeek]
        setnames(lunch_saving_per_week, old="aggWeek", new="get")
        
        assign(dt_name, lunch_saving_per_week)
        return_dts = append(return_dts, setNames(list(lunch_saving_per_week),dt_name))
        
      }#day selection 
    }
  
  return(return_dts)
}

get.plot.lunch.saving <- function(dt, expDate){
  
  plot_dt = dt[[1]]
  
  if(expDate[4] == "2016-11-16"){
    #exp1-1
    plot_dt = set.expDate.1.1(plot_dt)
    plot_name = paste('exp1-1', names(dt), sep="_")
  } else if(expDate[4] == "2015-01-22"){
    #exp1-2
    plot_dt = set.expDate.1.2(plot_dt)
    plot_name = paste('exp1-2', names(dt), sep="_")
  } else{
    #exp3
    plot_dt = set.expDate.2(plot_dt)
    plot_name = paste('exp2', names(dt), sep="_")
  }
  
  rownum_expDate <- set.expDate.rownum(plot_dt, expDate)
  
  windowingWeek <- 4
  
  #   print(plot_name)
  
  p1 = ggplot(plot_dt, aes(x=get)) +
    ggtitle(plot_name)

  p1 = add.window.line(p1, plot_dt, plot_name, 'lunch_saving_count',windowingWeek, rownum_expDate)
  
  if(expDate[4] == "2016-11-16"){
    #exp1-1
    p1 = add.event.vline.exp1.1(p1)
  } else if(expDate[4] == "2015-01-22"){
    #exp1-2
    p1 = add.event.vline.exp1.2(p1)
  } else{
    #exp3
    p1 = add.event.vline.exp2(p1)
  }
  
  p1 = set.default.theme(p1)
  
  save.plot(paste0("../plots/", plot_name, ".png"), p1)
  
  return(p1)
}


#build table
table_lunch_saving_light <- build.table.lunch.saving(dt_list, 'light')
table_lunch_saving_computer <- build.table.lunch.saving(dt_list, 'computer')

#plot
for(lab in 1:length(table_lunch_saving_light)){
  plot_lunch_saving <- get.plot.lunch.saving(table_lunch_saving_light[lab], get.expDate.2())  
}
for(lab in 1:length(table_lunch_saving_computer)){
  plot_lunch_saving <- get.plot.lunch.saving(table_lunch_saving_computer[lab], get.expDate.2())  
}

#statistics
all_expDate <- get.expDate()

# print.mean <- function(dt,colName,all_expDate){
#   for(lab in 1:length(dt)){
#       print(names(dt[lab]))
#       
#       for(i in 1:length(all_expDate)){
#         dt = dt[[lab]][get >= all_expDate[[i]][1] & get <= all_expDate[[i]][2]]
#         print(mean(dt[[colName]]))
#       }
#   }
# }
# 
# print.mean(table_lunch_saving_light, 'lunch_saving_count', all_expDate)

for(lab in 1:length(table_lunch_saving_light)){
  if(grepl("allDay", names(table_lunch_saving_light[lab]))){
    print(names(table_lunch_saving_light[lab]))
    
    for(i in 1:length(all_expDate)){
      dt = table_lunch_saving_light[[lab]][get >= all_expDate[[i]][1] & get <= all_expDate[[i]][2]]
      print(mean(dt$lunch_saving_count))
    }
  }
}

for(lab in 1:length(table_lunch_saving_computer)){
  if(grepl("allDay", names(table_lunch_saving_computer[lab]))){
    print(names(table_lunch_saving_computer[lab]))
    
    for(i in 1:length(all_expDate)){
      dt = table_lunch_saving_computer[[lab]][get >= all_expDate[[i]][1] & get <= all_expDate[[i]][2]]
      print(mean(dt$lunch_saving_count))
    }
  }
}
# 1. lab
lab_names = c("MARG", "HCC", "UX", "All Labs")

# 2. aggregation unit 
agg_Units = c("aggWeek", "aggDay")

# 3. day selection
day_selections = c("allDay", "workingday", "non_workingday")

build.table.partial.lightOn <- function(dt_list){
  return_dts = list()
  
  for(lab in 1:length(dt_list)){ 
    #   lab_dt & name selection 
    lab_dt = dt_list[[lab]]
    lab_name = lab_names[lab]
    
    light_min = 0.01
    light_peak = quantile(lab_dt$light, .90, na.rm = T)
    
    light_dt = lab_dt[, .(timestamp = timestamp,
                          workingday = workingday,
                          aggDay = aggDay,
                          aggWeek = aggWeek,
                          light = na.locf(light),
                          lightON = ifelse(na.locf(light) > light_min, 1, 0),
                          peak_80 = filter.fault.partial.lightOn(ifelse((na.locf(light) < light_peak * 0.8) & (na.locf(light) > light_min), 1, 0)))]
    
    for(agg_Unit in agg_Units){
      
      for(day_selection in day_selections){
        
        dt_name = paste(lab_name, agg_Unit, day_selection,"partial_lightOn", sep="_")
        print(dt_name)
        
        if(day_selection == "allDay"){

          partial_light = light_dt[, .(threshold80 = ifelse(sum(lightON)==0,1,sum(peak_80)/sum(lightON))), by=get(agg_Unit)]
          
        } else if(day_selection == "workingday") {
          
          partial_light = light_dt[workingday == T, .(threshold80 = ifelse(sum(lightON)==0,1,sum(peak_80)/sum(lightON))), by=get(agg_Unit)]

        } else if(day_selection == "non_workingday") {

          partial_light = light_dt[workingday == F, .(threshold80 = ifelse(sum(lightON)==0,1,sum(peak_80)/sum(lightON))), by=get(agg_Unit)]
        }
        
        partial_light = partial_light[, .(get = get,
                                          threshold80 = threshold80*100)]
        assign(dt_name, partial_light)
        return_dts = append(return_dts, setNames(list(partial_light),dt_name))
        
      }#day selection 
    }#agg Unit
  }
  return(return_dts)
}

plot.partial.lightOn <- function(dt, expDate, partial_lightON_color = "orange2"){
  
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
  partial_lightON <- ggplot(plot_dt, aes(x=get)) +
    ggtitle(plot_name)+
    ylab("Partial light-ON (%)")
  partial_lightON = add.colorful.window.line(partial_lightON, plot_dt, plot_name, 'threshold80', windowingWeek, partial_lightON_color, rownum_expDate)
  
  if(expDate[4] == "2016-11-16"){
    #exp1-1
    partial_lightON = add.event.vline.exp1.1(partial_lightON)
  } else if(expDate[4] == "2015-01-22"){
    #exp1-2
    partial_lightON = add.event.vline.exp1.2(partial_lightON)
  } else{
    #exp3
    partial_lightON = add.event.vline.exp2(partial_lightON)
  }
  
  partial_lightON  = set.colorful.theme(partial_lightON, partial_lightON_color)
  
  save.plot(paste0("../plots/",plot_name, ".png"), partial_lightON)
  
  return(partial_lightON)
}


#build table
table_partial_lightOn <- build.table.partial.lightOn(dt_list)

#plot
for(lab in 1:length(table_partial_lightOn)){
  plot_partial_lightOn <- plot.partial.lightOn(table_partial_lightOn[lab], get.expDate.2())  
}

#statistics
all_expDate <- get.expDate()

for(lab in 1:length(table_partial_lightOn)){
  if(grepl("allDay", names(table_partial_lightOn[lab])) & grepl("aggDay", names(table_partial_lightOn[lab]))){
    print(names(table_partial_lightOn[lab]))
    
    for(i in 1:length(all_expDate)){
      dt = table_partial_lightOn[[lab]][get >= all_expDate[[i]][1] & get <= all_expDate[[i]][2]]
#       print(paste(all_expDate[[i]][1],"~",all_expDate[[i]][2],":",mean(dt$threshold80)))
      print(mean(dt$threshold80))
    }
  }
}

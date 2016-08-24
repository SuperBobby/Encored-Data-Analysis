# 1. lab
lab_names = c("MARG", "HCC", "UX", "All Labs")

# 2. aggregation unit 
agg_Units = c("aggWeek", "aggDay")

# 3. day selection
day_selections = c("allDay", "weekDay", "weekEnd")

build.table.partial.lightOn <- function(dt_list){
  return_dts = list()
  
  for(lab in 1:length(dt_list)){ 
    #   lab_dt & name selection 
    lab_dt = dt_list[[lab]]
    lab_name = lab_names[lab]
    
    for(agg_Unit in agg_Units){
      
      for(day_selection in day_selections){
        
        dt_name = paste(lab_name, agg_Unit, day_selection,"partial_lightOn", sep="_")
        print(dt_name)
        
        light_min = 0.01
        light_peak = quantile(lab_dt$light, .90, na.rm = T)
        
        light_dt = lab_dt[, .(timestamp = timestamp,
                              weekday = weekday,
                              aggDay = aggDay,
                              aggWeek = aggWeek,
                              light = na.locf(light),
                              lightON = ifelse(na.locf(light) > light_min, 1, 0),
                              peak_80 = filter.fault.partial.lightOn(ifelse((na.locf(light) < light_peak * 0.8) & (na.locf(light) > light_min), 1, 0)))]
        
        if(day_selection == "allDay"){
          
          partial_light = light_dt[, .(threshold80 = ifelse(sum(lightON)==0,1,sum(peak_80)/sum(lightON))), by=get(agg_Unit)]
          
          if(agg_Unit == "aggWeek"){
            partial_light = partial_light[, .(get = get,
                                              threshold80 = threshold80*100)]
          } else{
            partial_light = partial_light[, .(get = get,
                                              threshold80 = threshold80*100)]
          }
          
          assign(dt_name, partial_light)
          return_dts = append(return_dts, setNames(list(partial_light),dt_name))
          
        } else if(day_selection == "weekDay") {
          
          partial_light = light_dt[weekday == T, .(threshold80 = ifelse(sum(lightON)==0,1,sum(peak_80)/sum(lightON))), by=get(agg_Unit)]
          
          if(agg_Unit == "aggWeek"){
            partial_light = partial_light[, .(get = get,
                                              threshold80 = threshold80*100)]
          } else{
            partial_light = partial_light[, .(get = get,
                                              threshold80 = threshold80*100)]
          }
          
          assign(dt_name, partial_light)
          return_dts = append(return_dts, setNames(list(partial_light),dt_name))
          
        } else if(day_selection == "weekEnd") {
          
          partial_light = light_dt[weekday == F, .(threshold80 = ifelse(sum(lightON)==0,1,sum(peak_80)/sum(lightON))), by=get(agg_Unit)]
          
          if(agg_Unit == "aggWeek"){
            partial_light = partial_light[, .(get = get,
                                              threshold80 = threshold80*100)]
          } else{
            partial_light = partial_light[, .(get = get,
                                              threshold80 = threshold80*100)]
          }
          
          assign(dt_name, partial_light)
          return_dts = append(return_dts, setNames(list(partial_light),dt_name))
        }
      }#day selection 
    }#agg Unit
  }
  
  return(return_dts)
}

get.plot.partial.lightOn <- function(dt, expDate, partial_lightON_color = "orange2"){
  
  plot_dt = dt[[1]]
  plot_name = names(dt)
  rownum_expDate <- set.expDate.rownum(plot_dt, expDate)
  
  windowingWeek <- 4
  
  #   print(plot_name)
  partial_lightON <- ggplot(plot_dt, aes(x=get)) +
    ggtitle(plot_name)+
    ylab("Partial light-ON (%)")
  partial_lightON = add.colorful.window.line(partial_lightON, plot_dt, 'threshold80', windowingWeek, partial_lightON_color, rownum_expDate)
  
  partial_lightON  = add.event.vline.exp3(partial_lightON)
  partial_lightON  = set.colorful.theme(partial_lightON, partial_lightON_color)
  
  save.plot(paste0("../plots2/",plot_name, ".png"), partial_lightON)
  
  return(lightON_duration)
}


#build table
table_partial_lightOn <- build.table.partial.lightOn(dt_list)

#plot
for(lab in 1:length(table_lightOn_duration)){
  plot_partial_lightOn <- get.plot.partial.lightOn(table_lightOn_duration[lab], get.expDate.3())  
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

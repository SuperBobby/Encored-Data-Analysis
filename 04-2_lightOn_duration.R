# 1. lab
lab_names = c("MARG", "HCC", "UX", "All Labs")

# 2. aggregation unit 
agg_Units = c("aggWeek", "aggDay")

# 3. day selection
day_selections = c("allDay", "weekDay", "weekEnd")

build.table.lightOn.duration <- function(dt_list){
  return_dts = list()
  
  for(lab in 1:length(dt_list)){ 
    #   lab_dt & name selection 
    lab_dt = dt_list[[lab]]
    lab_name = lab_names[lab]
    
    light_min = 0.01
    light_peak = quantile(lab_dt$light, .90, na.rm = T)
    
    light_dt = lab_dt[, .(timestamp = timestamp,
                          weekday = weekday,
                          aggDay = aggDay,
                          aggWeek = aggWeek,
                          light = na.locf(light),
                          lightON = ifelse(na.locf(light) > light_min, 1, 0),
                          peak_80 = filter.fault.partial.lightOn(ifelse((na.locf(light) < light_peak * 0.8) & (na.locf(light) > light_min), 1, 0)))]
    
    for(agg_Unit in agg_Units){
      
      for(day_selection in day_selections){

          dt_name = paste(lab_name, agg_Unit, day_selection,"lightOn-duration", sep="_")
          print(dt_name)
          
          if(day_selection == "allDay"){
            
            partial_light = light_dt[, .(lightON = sum(lightON),
                                         threshold80 = ifelse(sum(lightON)==0,1,sum(peak_80)/sum(lightON))), by=get(agg_Unit)]
            
            if(agg_Unit == "aggWeek"){
              partial_light = partial_light[, .(get = get,
                                                lightON = lightON*15.0/60.0/7.0,
                                                threshold80 = threshold80*100)]
            } else{
              partial_light = partial_light[, .(get = get,
                                                lightON = lightON*15.0/60.0,
                                                threshold80 = threshold80*100)]
            }
            
            assign(dt_name, partial_light)
            return_dts = append(return_dts, setNames(list(partial_light),dt_name))
            
          } else if(day_selection == "weekDay") {
            
            partial_light = light_dt[weekday == T, .(lightON = sum(lightON),
                                         threshold80 = ifelse(sum(lightON)==0,1,sum(peak_80)/sum(lightON))), by=get(agg_Unit)]
            
            if(agg_Unit == "aggWeek"){
              partial_light = partial_light[, .(get = get,
                                                lightON = lightON*15.0/60.0/5.0,
                                                threshold80 = threshold80*100)]
            } else{
              partial_light = partial_light[, .(get = get,
                                                lightON = lightON*15.0/60.0,
                                                threshold80 = threshold80*100)]
            }
            
            assign(dt_name, partial_light)
            return_dts = append(return_dts, setNames(list(partial_light),dt_name))
            
          } else if(day_selection == "weekEnd") {
            
            partial_light = light_dt[weekday == F, .(lightON = sum(lightON),
                                         threshold80 = ifelse(sum(lightON)==0,1,sum(peak_80)/sum(lightON))), by=get(agg_Unit)]
            
            if(agg_Unit == "aggWeek"){
              partial_light = partial_light[, .(get = get,
                                                lightON = lightON*15.0/60.0/2.0,
                                                threshold80 = threshold80*100)]
            } else{
              partial_light = partial_light[, .(get = get,
                                                lightON = lightON*15.0/60.0,
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

get.plot.lightOn.duration <- function(dt, expDate, lightON_duration_color = "darkolivegreen"){
  
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
  
  lightON_duration <- ggplot(plot_dt, aes(x=get)) +
    ggtitle(plot_name)+
    scale_y_continuous(limits=c(0,24), oob=rescale_none) +
    ylab("Light-ON duration (hours)")+
    scale_color_discrete(breaks = c("lightON"), labels = c("number of lightON blocks(15min)"))
  lightON_duration = add.colorful.window.line(lightON_duration, plot_dt, plot_name, 'lightON',windowingWeek, lightON_duration_color, rownum_expDate)
  
  if(expDate[4] == "2016-11-16"){
    #exp1-1
    lightON_duration = add.event.vline.exp1.1(lightON_duration)
  } else if(expDate[4] == "2015-01-22"){
    #exp1-2
    lightON_duration = add.event.vline.exp1.2(lightON_duration)
  } else{
    #exp3
    lightON_duration = add.event.vline.exp2(lightON_duration)
  }

#   lightON_duration = add.event.vline.exp2(lightON_duration)
  lightON_duration = set.colorful.theme(lightON_duration, lightON_duration_color)
  
  save.plot(paste0("../plots/",plot_name, ".png"), lightON_duration)
  
  return(lightON_duration)
}


#build table
table_lightOn_duration <- build.table.lightOn.duration(dt_list)

#plot
for(lab in 1:length(table_lightOn_duration)){
  plot_lightOn_duration <- get.plot.lightOn.duration(table_lightOn_duration[lab], get.expDate.1.1())  
}

for(lab in 1:length(table_lightOn_duration)){
  plot_lightOn_duration <- get.plot.lightOn.duration(table_lightOn_duration[lab], get.expDate.1.2())  
}

for(lab in 1:length(table_lightOn_duration)){
  plot_lightOn_duration <- get.plot.lightOn.duration(table_lightOn_duration[lab], get.expDate.2())  
}

#statistics
all_expDate <- get.expDate()

for(lab in 1:length(table_lightOn_duration)){
  if(grepl("allDay", names(table_lightOn_duration[lab])) & grepl("aggDay", names(table_lightOn_duration[lab]))){
    print(names(table_lightOn_duration[lab]))
    
    for(i in 1:length(all_expDate)){
      dt = table_lightOn_duration[[lab]][get >= all_expDate[[i]][1] & get <= all_expDate[[i]][2]]
#       print(paste(all_expDate[[i]][1],"~",all_expDate[[i]][2],":",mean(dt$lightON)))
      print(mean(dt$lightON))
    }
  }
}


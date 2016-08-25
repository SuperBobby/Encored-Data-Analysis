# 1. lab
lab_names = c("MARG", "HCC", "UX", "All Labs")

# 2. aggregation unit 
agg_Units = c("aggWeek", "aggDay")

# 3. day selection
day_selections = c("allDay", "weekDay", "weekEnd")

# 4. feeders
feeders = c("total", "computer", "light", "hvac")
# feeders = c("hvac")


build.table.stats <- function(dt_list){
  return_dts = list()
  
  for(lab in 1:4){ 
    # for(lab in 1){
    #   lab_dt & name selection 
    lab_dt = dt_list[[lab]]
    #   lab_dt = marg_dt
    lab_name = lab_names[lab]
    
    for(agg_Unit in agg_Units){
      # aggregation unit selection
      # agg_Units = c("aggWeek", "aggDay")
      # get(agg_Unit)
      
      for(day_selection in day_selections){
        
        # day selection
        # print(day_selection)
        
        for(feeder in feeders){
          # feeder selection
          # get(feeder)
          
          dt_name = paste(lab_name, agg_Unit, day_selection, feeder, sep="_")
          print(dt_name)
          
          if(day_selection == "allDay"){
            
            return_dt = lab_dt[, .(peak = get.four.stats(get(feeder), 1),
                                   base = get.four.stats(get(feeder), 2),
                                   avg  = get.four.stats(get(feeder), 3)),
                               by=get(agg_Unit)]
            
          } else if(day_selection == "weekDay") {
            
            return_dt = lab_dt[weekday == T, .(peak = get.four.stats(get(feeder), 1),
                                               base = get.four.stats(get(feeder), 2),
                                               avg  = get.four.stats(get(feeder), 3)), 
                               by=get(agg_Unit)]
            
          } else if(day_selection == "weekEnd") {
            
            return_dt = lab_dt[weekday == F, .(peak = get.four.stats(get(feeder), 1),
                                               base = get.four.stats(get(feeder), 2),
                                               avg  = get.four.stats(get(feeder), 3)),
                               by=get(agg_Unit)]
          }
          
          light_min = 0.01
          light_peak = quantile(lab_dt$light, .90, na.rm = T)
          light_dt = lab_dt[, .(timestamp = timestamp,
                                aggDay = aggDay,
                                aggWeek = aggWeek,
                                light = na.locf(light),
                                lightON = ifelse(na.locf(light) > light_min, 1, 0),
                                peak_80 = filter.fault.partial.lightOn(ifelse((na.locf(light) < light_peak * 0.8) & (na.locf(light) > light_min), 1, 0)))]
          
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
          
          return_dt = merge(return_dt, partial_light, by="get")
          
          assign(dt_name, return_dt)
          return_dts = append(return_dts, setNames(list(return_dt),dt_name))
        }
      }
    }
  }
  
  return(return_dts)
}

get.plot.stats <- function(dt, expDate) {
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
  
  stats <- ggplot(plot_dt, aes(x=get)) +
    ggtitle(plot_name) +
    ylab("Energy use (kWh/day)")+
    scale_linetype_discrete(breaks=c("peak", "avg", "base"))
  
  stats = add.window.line(stats, plot_dt, plot_name, "peak", windowingWeek, rownum_expDate)
  stats = add.window.line(stats, plot_dt, plot_name, "base", windowingWeek, rownum_expDate)
  stats = add.window.line(stats, plot_dt, plot_name, "avg", windowingWeek, rownum_expDate)
    
  if(expDate[4] == "2016-11-16"){
    #exp1-1
    stats = add.event.vline.exp1.1(stats)
  } else if(expDate[4] == "2015-01-22"){
    #exp1-2
    stats = add.event.vline.exp1.2(stats)
  } else{
    #exp3
    stats = add.event.vline.exp2(stats)
  }
  
  stats = set.default.theme(stats)
  
  save.plot(paste0("../plots/", plot_name, ".png"), stats)
  
  return(stats)
}


#build table
table_stats <- build.table.stats(dt_list)

#plot
for(lab in 1:length(table_stats)){
  plot_stats <- get.plot.stats(table_stats[lab], get.expDate.1.1())
}

for(lab in 1:length(table_stats)){
  plot_stats <- get.plot.stats(table_stats[lab], get.expDate.1.2())
}

for(lab in 1:length(table_stats)){
  plot_stats <- get.plot.stats(table_stats[lab], get.expDate.2())
}

#statistics
all_expDate <- get.expDate()

for(lab in 1:length(table_stats)){  
  if(grepl("allDay", names(table_stats[lab])) & grepl("aggDay", names(table_stats[lab]))){
    print(names(table_stats[lab]))
    
    for(i in 1:length(all_expDate)){
      dt = table_stats[[lab]][get >= all_expDate[[i]][1] & get <= all_expDate[[i]][2]]
#       print(paste(all_expDate[[i]][1],"~",all_expDate[[i]][2],":",mean(dt$peak),",",mean(dt$base),",",mean(dt$avg)))
      print(paste(mean(dt$peak),",",mean(dt$base),",",mean(dt$avg)))
    }
  }
}



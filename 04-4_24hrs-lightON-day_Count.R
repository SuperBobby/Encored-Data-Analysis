### 
## 4. full_lightON counting : 24hours 
## ==> # of "over peak*(0.5~0.8)" == 96?

day_selections = c("allDay", "weekDay", "weekEnd")

build.table.24hr.lightOn.counting <- function(dt_list){
  return_dts = list()
  
  for(lab in 1:length(dt_list)){ 
    # lab_dt & name selection 
    lab_dt = dt_list[[lab]]
    lab_name = lab_names[lab]
    
    print(lab_name)
    
    #   lab_dt$light <- na.locf(lab_dt$light)
    
    light_peak = quantile(lab_dt$light, .90, na.rm = T)
    print(light_peak)
    
    for(day_selection in day_selections){
      if(day_selection == "allDay"){
        
        full_lightON_aggDay_dt = lab_dt[, .(full_lightON_10 = sum(light > (light_peak * 0.1), na.rm = T)==96), by=aggDay]
        setnames(full_lightON_aggDay_dt,old="aggDay",new="get")
        
      } else if(day_selection == "weekDay") {
        
        full_lightON_aggDay_dt = lab_dt[weekday == T, .(full_lightON_10 = sum(light > (light_peak * 0.1), na.rm = T)==96), by=aggDay]
        setnames(full_lightON_aggDay_dt,old="aggDay",new="get")
        
      } else if(day_selection == "weekEnd") {
        
        full_lightON_aggDay_dt = lab_dt[weekday == F, .(full_lightON_10 = sum(light > (light_peak * 0.1), na.rm = T)==96), by=aggDay]
        setnames(full_lightON_aggDay_dt,old="aggDay",new="get")
        
      }
      
      full_lightON_aggWeek_dt = full_lightON_aggDay_dt[, .(full_lightON_10 = sum(full_lightON_10, na.rm = T)), by=.(aggWeek=as.Date(cut(get, breaks = "week", start.on.monday = T)))]
      setnames(full_lightON_aggWeek_dt,old="aggWeek",new="get")
      
      dt_name = paste(lab_name, "24hr_lightON", "aggWeek", day_selection, sep="_")
      
      assign(dt_name, full_lightON_aggWeek_dt)
      return_dts = append(return_dts, setNames(list(full_lightON_aggWeek_dt),dt_name)) 
    }
  }
  
  return(return_dts)
}

get.plot.24hr.lightOn.counting <- function(dt, expDate, full_lightON_color = "violetred4"){
  
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

  print(plot_name)
  
  p <- ggplot(plot_dt, aes(x=get)) +
    geom_point(aes(y=full_lightON_10), colour='gray70') +
    ggtitle(plot_name)+
    ylab("24hrs light-ON day (count/week)")
  
  p = add.colorful.window.line(p, plot_dt, plot_name, 'full_lightON_10', windowingWeek, full_lightON_color, rownum_expDate)
  
  if(expDate[4] == "2016-11-16"){
    #exp1-1
    p = add.event.vline.exp1.1(p)
  } else if(expDate[4] == "2015-01-22"){
    #exp1-2
    p = add.event.vline.exp1.2(p)
  } else{
    #exp3
    p = add.event.vline.exp2(p)
  }
  
  p = set.colorful.theme(p, full_lightON_color)

  save.plot(paste0("../plots/", plot_name, ".png"), p)
  
  return(p)
}


#build table
table_full_lightOn <- build.table.24hr.lightOn.counting(dt_list)

#plot
for(lab in 1:length(table_full_lightOn)){
  plot_full_lightOn <- get.plot.24hr.lightOn.counting(table_full_lightOn[lab], get.expDate.2())  
}

#statistics
all_expDate <- get.expDate()

for(lab in 1:length(table_full_lightOn)){
  if(grepl("allDay", names(table_full_lightOn[lab]))){
    print(names(table_full_lightOn[lab]))
    
    for(i in 1:length(all_expDate)){
      dt = table_full_lightOn[[lab]][get >= all_expDate[[i]][1] & get <= all_expDate[[i]][2]]
      print(paste(all_expDate[[i]][1],"~",all_expDate[[i]][2],":",mean(dt$full_lightON_10)))
    }
  }
}



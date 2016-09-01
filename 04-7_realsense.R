# 1. lab
lab_names = c("MARG", "HCC", "UX", "All Labs")

# 2. aggregation unit 
agg_Units = c("aggWeek", "aggDay")

# 3. day selection
day_selections = c("allDay", "workingday", "non_workingday")

# 4. feeders
feeders = c("total", "computer", "light", "hvac")
# feeders = c("hvac")


build.table.realsense <- function(dt_list){
  
  return_rs_dts = list()

  for(lab in 1:length(dt_list)){
    # lab_dt & name selection 
    lab_dt = dt_list[[lab]]
    lab_name = lab_names[lab]
    
    for(agg_Unit in agg_Units){
      # aggregation unit selection
      # agg_Units = c("aggWeek", "aggDay")
      # get(agg_Unit)
      
      for(day_selection in day_selections){
        
        # day selection
        # print(day_selection)
        
        dt_name = paste(lab_name, agg_Unit, day_selection, sep="_")
        print(dt_name)
        
        if(day_selection == "allDay"){
          
          return_rs_dt = lab_dt[, .(sum_of_duration = sum(sum_of_duration, na.rm = T),
                                    freq = sum(freq, na.rm = T))
                                , by=get(agg_Unit)]   
          
        } else if(day_selection == "workingday") {
          
          return_rs_dt = lab_dt[workingday == T, .(sum_of_duration = sum(sum_of_duration, na.rm = T), 
                                                freq = sum(freq, na.rm = T))
                                , by=get(agg_Unit)]
          
        } else if(day_selection == "non_workingday") {
          
          return_rs_dt = lab_dt[workingday == F, .(sum_of_duration = sum(sum_of_duration, na.rm = T), 
                                                freq = sum(freq, na.rm = T))
                                , by=get(agg_Unit)]
        }
        
        if((agg_Unit=="aggWeek") & (day_selection=="allDay")){
          return_rs_dt = return_rs_dt[, .(get = get,
                                          sum_of_duration = sum_of_duration/7.0,
                                          freq = freq/7.0)]
        } else if((agg_Unit=="aggWeek") & (day_selection=="weekDay")){
          return_rs_dt = return_rs_dt[, .(get = get,
                                          sum_of_duration = sum_of_duration/5.0,
                                          freq = freq/5.0)]
        } else if((agg_Unit=="aggWeek") & (day_selection=="weekEnd")){
          return_rs_dt = return_rs_dt[, .(get = get,
                                          sum_of_duration = sum_of_duration/5.0,
                                          freq = freq/2.0)]
        }
        
        return_rs_dt[get <= "2015-10-14"]$sum_of_duration <- NA
        return_rs_dt[get <= "2015-10-14"]$freq <- NA
        
        if(lab_name == "HCC"){
          return_rs_dt <- return_rs_dt[get < "2015-12-11" | get > "2015-12-30"]
        }
        
        assign(dt_name, return_rs_dt)
        return_rs_dts = append(return_rs_dts, setNames(list(return_rs_dt),dt_name))
      }
    }
  }
  return(return_rs_dts)
}

get.plot.realsense <- function(dt, expDate) {
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
  
#   ylim_RS_duration <- 600
#   ylim_RS_freq <- 300
  
  RS_duration <- ggplot(plot_dt, aes(x=get))+
    geom_point(aes(y=sum_of_duration), colour="grey50") +
    #       geom_text(aes(y=sum_of_duration, label=round(sum_of_duration, 0)), position=position_dodge(width=0.9), vjust=-0.25, colour="grey50") + 
    scale_y_continuous(limits=c(0, 600), oob=rescale_none) +
    ggtitle(paste("RS", plot_name,"sum of duration"))+
    theme_bw()+
    ylab("total duration(seconds)")
  
  RS_freq <- ggplot(plot_dt, aes(x=get))+
    geom_point(aes(y=freq), colour="grey50") +
    #       geom_text(aes(y=freq, label=round(freq, 0)), position=position_dodge(width=0.9), vjust=-0.25, colour="grey50") + 
    scale_y_continuous(limits=c(0, 300), oob=rescale_none) +
    ggtitle(paste("RS", plot_name,"freq"))+
    theme_bw()+
    ylab("(counts / day)")
  
  RS_duration      = add.window.line(RS_duration, plot_dt, plot_name, "sum_of_duration", windowingWeek, expDate)
  RS_freq          = add.window.line(RS_freq, plot_dt, plot_name, "freq", windowingWeek, expDate)
  
  if(expDate[4] == "2016-11-16"){
    #exp1-1
    RS_duration      = add.event.vline.exp1.1(RS_duration)
    RS_freq          = add.event.vline.exp1.1(RS_freq)
  } else if(expDate[4] == "2015-01-22"){
    #exp1-2
    RS_duration      = add.event.vline.exp1.2(RS_duration)
    RS_freq          = add.event.vline.exp1.2(RS_freq)
  } else{
    #exp3
    RS_duration      = add.event.vline.exp2(RS_duration)
    RS_freq          = add.event.vline.exp2(RS_freq)
  }
  
  RS_duration      = set.colorful.theme(RS_duration, "black")
  RS_freq          = set.colorful.theme(RS_freq, "black")    
  
  save.plot(paste0("../plots/",plot_name, "_realsense_duration.png"),RS_duration)
  save.plot(paste0("../plots/",plot_name, "_realsense_freq.png"),RS_freq)
  
}

#build table
table_realsense <- build.table.realsense(dt_list)

#plot
for(i in 1:length(table_realsense)){
  plot_realsense <- get.plot.realsense(table_realsense[i], get.expDate.2())  
}

#statistics
all_expDate <- get.expDate()

for(lab in 1:length(table_realsense)){
  if(grepl("allDay", names(table_realsense[lab])) & grepl("aggDay", names(table_realsense[lab]))){
    print(names(table_realsense[lab]))
    print("sum_of_duration")
    
    for(i in 1:length(all_expDate)){
      dt = table_realsense[[lab]][get >= all_expDate[[i]][1] & get <= all_expDate[[i]][2]]
      #       print(paste(all_expDate[[i]][1],"~",all_expDate[[i]][2],":",mean(dt$lightON)))
      print(mean(dt$sum_of_duration))
    }
    
    print(names(table_realsense[lab]))
    print("freq")
    
    for(i in 1:length(all_expDate)){
      dt = table_realsense[[lab]][get >= all_expDate[[i]][1] & get <= all_expDate[[i]][2]]
      #       print(paste(all_expDate[[i]][1],"~",all_expDate[[i]][2],":",mean(dt$lightON)))
      print(mean(dt$freq))
    }
    
  }
}



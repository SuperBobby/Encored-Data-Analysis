# 1. lab
lab_names = c("MARG", "HCC", "UX", "All Labs")

# 2. aggregation unit 
agg_Units = c("aggWeek", "aggDay")

# 3. day selection
day_selections = c("allDay", "workingday", "non_workingday")

# 4. feeders
feeders = c("total", "computer", "light", "hvac")
# feeders = c("hvac")

MARG_VALID_RS_START_DATE = '2015-10-09'
HCC_VALID_RS_START_DATE = '2015-10-14'
UX_VALID_RS_START_DATE = '2015-11-01'

VALID_SUM_OF_DURATION = 600
MAX_COUNT_PER_DAY = 300

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
          
          return_rs_dt = lab_dt[, .(sum_of_duration = sum(RS_duration, na.rm = T)/nrow(.SD)*96,
                                    count_per_day = sum(RS_count, na.rm = T)/nrow(.SD)*96)
                                , by=get(agg_Unit)]   
          
        } else if(day_selection == "workingday") {
          
          return_rs_dt = lab_dt[workingday == T, .(sum_of_duration = sum(RS_duration, na.rm = T)/nrow(.SD)*96, 
                                                   count_per_day = sum(RS_count, na.rm = T)/nrow(.SD)*96)
                                , by=get(agg_Unit)]
          
        } else if(day_selection == "non_workingday") {
          
          return_rs_dt = lab_dt[workingday == F, .(sum_of_duration = sum(RS_duration, na.rm = T)/nrow(.SD)*96, 
                                                   count_per_day = sum(RS_count, na.rm = T)/nrow(.SD)*96)
                                , by=get(agg_Unit)]
        }
        
        names(return_rs_dt) = c("timestamp", "sum_of_duration", "count_per_day")
        
        if(lab_name == "MARG"){
          return_rs_dt <- return_rs_dt[timestamp <= MARG_VALID_RS_START_DATE, ':='(sum_of_duration = NA,
                                                                                 count_per_day = NA)]
        } else if(lab_name == "HCC"){
          return_rs_dt <- return_rs_dt[(timestamp >= "2015-12-11" & timestamp <= "2015-12-30") | timestamp <= HCC_VALID_RS_START_DATE, ':='(sum_of_duration = NA,
                                                                                                                                        count_per_day = NA)]
        } else{
          return_rs_dt <- return_rs_dt[timestamp <= UX_VALID_RS_START_DATE, ':='(sum_of_duration = NA,
                                                                                 count_per_day = NA)]
        }

        return_rs_dt[sum_of_duration > VALID_SUM_OF_DURATION, ':='(sum_of_duration = VALID_SUM_OF_DURATION)]
        return_rs_dt[count_per_day > MAX_COUNT_PER_DAY, ':='(count_per_day = MAX_COUNT_PER_DAY)]
        
        assign(dt_name, return_rs_dt)
        return_rs_dts = append(return_rs_dts, setNames(list(return_rs_dt),dt_name))
      }
    }
  }
  return(return_rs_dts)
}

get.plot.realsense <- function(dt, expDate) {
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
  
  #   rownum_expDate <- set.expDate.rownum(plot_dt, expDate)
  
  windowingWeek = 4
  
  #   ylim_RS_duration <- 600
  #   ylim_RS_count_per_day <- 300
  
  RS_duration <- ggplot(plot_dt, aes(x=timestamp))+
    geom_point(aes(y=sum_of_duration), colour="grey50") +
    #       geom_text(aes(y=sum_of_duration, label=round(sum_of_duration, 0)), position=position_dodge(width=0.9), vjust=-0.25, colour="grey50") + 
    scale_y_continuous(limits=c(0, 600), oob=rescale_none) +
    ggtitle(paste("RS", plot_name,"sum of duration"))+
    theme_bw()+
    ylab("total duration(seconds)")
  
  RS_count_per_day <- ggplot(plot_dt, aes(x=timestamp))+
    geom_point(aes(y=count_per_day), colour="grey50") +
    #       geom_text(aes(y=count_per_day, label=round(count_per_day, 0)), position=position_dodge(width=0.9), vjust=-0.25, colour="grey50") + 
    scale_y_continuous(limits=c(0, 300), oob=rescale_none) +
    ggtitle(paste("RS", plot_name,"count_per_day"))+
    theme_bw()+
    ylab("(counts / day)")
  
  RS_duration      = add.window.line(RS_duration, plot_dt, plot_name, "sum_of_duration", windowingWeek, expDate)
  RS_count_per_day          = add.window.line(RS_count_per_day, plot_dt, plot_name, "count_per_day", windowingWeek, expDate)
  
  if(expDate[length(expDate)] == "2014-11-17"){
    #exp1-1
    RS_duration      = add.event.vline.exp1.1(RS_duration)
    RS_count_per_day          = add.event.vline.exp1.1(RS_count_per_day)
  } else if(expDate[length(expDate)] == "2015-01-22"){
    #exp1-2
    RS_duration      = add.event.vline.exp1.2(RS_duration)
    RS_count_per_day          = add.event.vline.exp1.2(RS_count_per_day)
  } else{
    #exp2
    RS_duration      = add.event.vline.exp2(RS_duration)
    RS_count_per_day          = add.event.vline.exp2(RS_count_per_day)
  }
  
  RS_duration      = set.colorful.theme(RS_duration)
  RS_count_per_day          = set.colorful.theme(RS_count_per_day)    
  
  save.plot(paste0("../plots/realsense/",plot_name, "_duration.png"),RS_duration)
  save.plot(paste0("../plots/realsense/",plot_name, "_count_per_day.png"),RS_count_per_day)
  
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
    print("count_per_day")
    
    for(i in 1:length(all_expDate)){
      dt = table_realsense[[lab]][get >= all_expDate[[i]][1] & get <= all_expDate[[i]][2]]
      #       print(paste(all_expDate[[i]][1],"~",all_expDate[[i]][2],":",mean(dt$lightON)))
      print(mean(dt$count_per_day))
    }
    
  }
}


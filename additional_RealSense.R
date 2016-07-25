
##HCC data 이상구간 NA처리해서 그리기

temp<-return_dts[[40]]
temp_name<-names(return_dts[40])

for (windowingWeek in c(4,8,12,16)) {
  
  window_df<-windowingByExpDate(temp,"freq",windowingWeek)
  window_df[get >= "2015-12-11" & get <= "2015-12-30"]$mean = NA
  window_df[get >= "2015-12-11" & get <= "2015-12-30"]$sd = NA
  

  RS_freq <- ggplot(temp, aes(x=get, ymax=ylim_RS_freq))+
    geom_point(aes(y=freq, color='freq')) +
    geom_text(aes(y=freq, label=round(freq, 0)), position=position_dodge(width=0.9), vjust=-0.25, colour="grey50") + 
    scale_y_continuous(limits=c(0,ylim_RS_freq), oob=rescale_none) +
    ggtitle("RealSense Freq")  


  RS_freq = RS_freq +
    geom_line(data=window_df, aes_string(y = "mean", color = shQuote("freq")), size=1) +
    geom_ribbon(data=window_df,aes(ymin = mean - sd, ymax = mean + sd), alpha = 0.2) 
    
    
  

  
  plot_name = paste(temp_name, "- windowing ", windowingWeek,"weeks")  
  
  ggsave(file = paste0("../plots/rmNA_",plot_name, ".png"), width = 20, height = 10, dpi = 300, RS_freq, limitsize=FALSE)
  
}


##frequency 1초로 cut해서 그려보기

ylim_RS_duration <- 600
ylim_RS_freq <- 300

for (i in c(16,40,64,88)) {
  temp<-return_dts[[i]]
  temp_name<-names(return_dts[i])
  
  rownum_expDate<-1
  
  for (d in expDate) {
    rownum_expDate <- append(rownum_expDate, (nrow(temp[d>temp$get,])+1))
  }
  
  temp$avg_sum_of_duration=ifelse(is.nan(temp$sum_of_duration/temp$freq),0,temp$sum_of_duration/temp$freq)
  
  for(windowingWeek in c(4,8,12,16)) {
    duration_window_df<-windowingByExpDate(temp,"sum_of_duration",windowingWeek)
    freq_window_df<-windowingByExpDate(temp,"freq",windowingWeek)
    avg_duration_window_df<-windowingByExpDate(temp,"avg_sum_of_duration",windowingWeek)
    if(i==40) { ##HCC인 경우
      duration_window_df[get >= "2015-12-11" & get <= "2015-12-30"]$mean = NA
      duration_window_df[get >= "2015-12-11" & get <= "2015-12-30"]$sd = NA
      freq_window_df[get >= "2015-12-11" & get <= "2015-12-30"]$mean = NA
      freq_window_df[get >= "2015-12-11" & get <= "2015-12-30"]$sd = NA
      avg_duration_window_df[get >= "2015-12-11" & get <= "2015-12-30"]$mean = NA
      avg_duration_window_df[get >= "2015-12-11" & get <= "2015-12-30"]$sd = NA
    }
    
    RS_duration <- ggplot(temp, aes(x=get, ymax=ylim_RS_duration))+
      geom_point(aes(y=sum_of_duration, color='sum_of_duration')) +
      geom_text(aes(y=sum_of_duration, label=round(sum_of_duration, 0)), position=position_dodge(width=0.9), vjust=-0.25, colour="grey50") + 
      scale_y_continuous(limits=c(0,ylim_RS_duration), oob=rescale_none) +
      ggtitle("RealSense Sum of duration")
    RS_duration = RS_duration +
      geom_line(data=duration_window_df, aes_string(y = "mean", color = shQuote("sum_of_duration")), size=1) +
      geom_ribbon(data=duration_window_df,aes(ymin = mean - sd, ymax = mean + sd), alpha = 0.2) 
    
    
    RS_freq <- ggplot(temp, aes(x=get, ymax=ylim_RS_freq))+
      geom_point(aes(y=freq, color='freq')) +
      geom_text(aes(y=freq, label=round(freq, 0)), position=position_dodge(width=0.9), vjust=-0.25, colour="grey50") + 
      scale_y_continuous(limits=c(0,ylim_RS_freq), oob=rescale_none) +
      ggtitle("RealSense Freq")
    RS_freq = RS_freq +
      geom_line(data=freq_window_df, aes_string(y = "mean", color = shQuote("freq")), size=1) +
      geom_ribbon(data=freq_window_df,aes(ymin = mean - sd, ymax = mean + sd), alpha = 0.2)
    
    RS_avg_duration <- ggplot(temp, aes(x=get, ymax=60))+
      geom_point(aes(y=avg_sum_of_duration, color='avg_sum_of_duration')) +
      geom_text(aes(y=avg_sum_of_duration, label=round(avg_sum_of_duration, 0)), position=position_dodge(width=0.9), vjust=-0.25, colour="grey50") + 
      scale_y_continuous(limits=c(0,60), oob=rescale_none) +
      ggtitle("RealSense Avg Sum of duration")
    RS_avg_duration = RS_avg_duration +
      geom_line(data=avg_duration_window_df, aes_string(y = "mean", color = shQuote("avg_sum_of_duration")), size=1) +
      geom_ribbon(data=avg_duration_window_df,aes(ymin = mean - sd, ymax = mean + sd), alpha = 0.2)
    
    RS_duration      = add.event.vline(RS_duration)
    RS_freq          = add.event.vline(RS_freq)
    RS_avg_duration      = add.event.vline(RS_avg_duration)
    
    plot_name = paste(temp_name, "- windowing ", windowingWeek,"weeks")  
    
    plots <- arrangeGrob(RS_duration, RS_freq, RS_avg_duration, ncol=1)
    
    ggsave(file = paste0("../plots/freqCut1s_",plot_name, ".png"), width = 20, height = 30, dpi = 300, plots, limitsize=FALSE)
    
  }
}



##HVAC pattern

hvac_return_dts = list(0)

temp<-dt_list[[1]]

hist(temp[hvac>0.1]$hvac, 100)   # abnormal : over 1.5
# temp[hvac > 1.5, ':='(hvac = 1.5)] # max(ON) : 1.4
temp[hvac <= 0.1, ':='(hvac = 0)] # min(ON) : > 0.1

hvac_min <- 0.1



for(lab in 1:1){ 
  # lab_dt & name selection 
  lab_dt = temp
  lab_name = lab_names[lab]
  
  for(agg_Unit in agg_Units){
    # aggregation unit selection
    # agg_Units = c("aggWeek", "aggDay")
    # get(agg_Unit)
    
    for(day_selection in day_selections){
      
      # day selection
      # print(day_selection)
      
      for(feeder in c("hvac")){
        # feeder selection
        # get(feeder)
        
        dt_name = paste(lab_name, agg_Unit, day_selection, feeder, sep="_")
        print(dt_name)
        
        if(day_selection == "allDay"){
          
          return_dt = lab_dt[, .(peak = get.four.stats(get(feeder), 1),
                                 base = get.four.stats(get(feeder), 2),
                                 avg  = get.four.stats(get(feeder), 3),
                                 med  = get.four.stats(get(feeder), 4),
                                 sum_of_duration = sum(sum_of_duration), freq = sum(freq)), by=get(agg_Unit)]
          
        } else if(day_selection == "weekDay") {
          
          return_dt = lab_dt[weekday == T, .(peak = get.four.stats(get(feeder), 1),
                                             base = get.four.stats(get(feeder), 2),
                                             avg  = get.four.stats(get(feeder), 3),
                                             med  = get.four.stats(get(feeder), 4),
                                             sum_of_duration = sum(sum_of_duration), freq = sum(freq)), by=get(agg_Unit)]
          
        } else if(day_selection == "weekEnd") {
          
          return_dt = lab_dt[weekday == F, .(peak = get.four.stats(get(feeder), 1),
                                             base = get.four.stats(get(feeder), 2),
                                             avg  = get.four.stats(get(feeder), 3),
                                             med  = get.four.stats(get(feeder), 4),
                                             sum_of_duration = sum(sum_of_duration), freq = sum(freq)), by=get(agg_Unit)]
        }
        
        hvac_min = 0.01
        hvac_peak = quantile(lab_dt$hvac, .95, na.rm = T)
        hvac_dt = lab_dt[, .(timestamp = timestamp,
                              aggDay = aggDay,
                              aggWeek = aggWeek,
                              hvac = na.locf(hvac),
                              hvacON = ifelse(na.locf(hvac) > hvac_min, 1, 0),
                              peak_50 = filter.fault.partial.lightOn(ifelse((na.locf(hvac) < hvac_peak * 0.5) & (na.locf(hvac) > hvac_min), 1, 0)),
                              peak_60 = filter.fault.partial.lightOn(ifelse((na.locf(hvac) < hvac_peak * 0.6) & (na.locf(hvac) > hvac_min), 1, 0)),
                              peak_70 = filter.fault.partial.lightOn(ifelse((na.locf(hvac) < hvac_peak * 0.7) & (na.locf(hvac) > hvac_min), 1, 0)),
                              peak_80 = filter.fault.partial.lightOn(ifelse((na.locf(hvac) < hvac_peak * 0.8) & (na.locf(hvac) > hvac_min), 1, 0)),
                              peak_90 = filter.fault.partial.lightOn(ifelse((na.locf(hvac) < hvac_peak * 0.9) & (na.locf(hvac) > hvac_min), 1, 0)))]
        
        partial_hvac = hvac_dt[, .(hvacON = sum(hvacON),
                                     threshold50 = ifelse(sum(hvacON)==0,1,sum(peak_50)/sum(hvacON)),
                                     threshold60 = ifelse(sum(hvacON)==0,1,sum(peak_60)/sum(hvacON)),
                                     threshold70 = ifelse(sum(hvacON)==0,1,sum(peak_70)/sum(hvacON)),
                                     threshold80 = ifelse(sum(hvacON)==0,1,sum(peak_80)/sum(hvacON)),
                                     threshold90 = ifelse(sum(hvacON)==0,1,sum(peak_90)/sum(hvacON))), by=get(agg_Unit)]
        
        return_dt = merge(return_dt, partial_hvac, by="get")
        
        assign(dt_name, return_dt)
        hvac_return_dts = append(hvac_return_dts, setNames(list(return_dt),dt_name))
      }
    }
  }
}

for(i in 2:length(hvac_return_dts)){ 
  plot_dt   = hvac_return_dts[[i]]
  
  rownum_expDate <- 1
  
  for (d in expDate) {
    rownum_expDate <- append(rownum_expDate, (nrow(plot_dt[d>plot_dt$get,])+1))
  }
  
  ##windowingWeek must be even
  for(windowingWeek in c(4,8,12,16)) {
    
    plot_name = paste(names(hvac_return_dts[i]), "- windowing ", windowingWeek,"weeks")  
    print(plot_name)
    
    stats <- ggplot(plot_dt, aes(x=get)) +
      geom_point(aes(y=peak, color='peak')) +  
      geom_point(aes(y=base, color='base')) +
      geom_point(aes(y=avg, color='avg')) +
      geom_point(aes(y=med, color='med')) +
      ggtitle(plot_name) 
    stats = add.window.line(stats, plot_dt, "peak", windowingWeek)
    stats = add.window.line(stats, plot_dt, "base", windowingWeek)
    stats = add.window.line(stats, plot_dt, "avg", windowingWeek)
    stats = add.window.line(stats, plot_dt, "med", windowingWeek)
    
    if(strsplit(plot_name,"_")[[1]][2] == "aggWeek"){
      ylim_RS_duration <- 2000
      ylim_RS_freq <- 1000
    } else{
      ylim_RS_duration <- 600
      ylim_RS_freq <- 300
    }    
    
    RS_duration <- ggplot(plot_dt, aes(x=get, ymax=ylim_RS_duration))+
      geom_point(aes(y=sum_of_duration, color='sum_of_duration')) +
      geom_text(aes(y=sum_of_duration, label=round(sum_of_duration, 0)), position=position_dodge(width=0.9), vjust=-0.25, colour="grey50") + 
      scale_y_continuous(limits=c(0,ylim_RS_duration), oob=rescale_none) +
      ggtitle("RealSense Sum of duration")
    RS_duration = add.window.line(RS_duration, plot_dt, "sum_of_duration", windowingWeek)
    
    
    RS_freq <- ggplot(plot_dt, aes(x=get, ymax=ylim_RS_freq))+
      geom_point(aes(y=freq, color='freq')) +
      geom_text(aes(y=freq, label=round(freq, 0)), position=position_dodge(width=0.9), vjust=-0.25, colour="grey50") + 
      scale_y_continuous(limits=c(0,ylim_RS_freq), oob=rescale_none) +
      ggtitle("RealSense Freq")
    RS_freq = add.window.line(RS_freq, plot_dt, "freq", windowingWeek)
    
    if(grepl("hvac", plot_name) | grepl("total", plot_name)){
      
      partial_hvacON <- ggplot(plot_dt, aes(x=get)) +
        geom_point(aes(y=threshold50, color='threshold50'), alpha = 0.5) +
        geom_point(aes(y=threshold60, color='threshold60'), alpha = 0.5) +
        geom_point(aes(y=threshold70, color='threshold70'), alpha = 0.5) +
        geom_point(aes(y=threshold80, color='threshold80'), alpha = 0.5) +
        geom_point(aes(y=threshold90, color='threshold90'), alpha = 0.5) +
        ggtitle("partial_hvacON")
      partial_hvacON = add.window.line(partial_hvacON, plot_dt, 'threshold50', windowingWeek)
      partial_hvacON = add.window.line(partial_hvacON, plot_dt, 'threshold60', windowingWeek)
      partial_hvacON = add.window.line(partial_hvacON, plot_dt, 'threshold70', windowingWeek)
      partial_hvacON = add.window.line(partial_hvacON, plot_dt, 'threshold80', windowingWeek)
      partial_hvacON = add.window.line(partial_hvacON, plot_dt, 'threshold90', windowingWeek)
      
      hvacON_duration <- ggplot(plot_dt, aes(x=get)) +
        geom_point(aes(y=hvacON, color='hvacON'), alpha = 0.5) +     
        ggtitle("hvacON duration")
      hvacON_duration = add.window.line(hvacON_duration, plot_dt, 'hvacON',windowingWeek)
      
      
      stats            = add.event.vline(stats)
      partial_hvacON  = add.event.vline(partial_hvacON)
      hvacON_duration = add.event.vline(hvacON_duration)
      RS_duration      = add.event.vline(RS_duration)
      RS_freq          = add.event.vline(RS_freq)
      
      plots <- arrangeGrob(stats, partial_hvacON, hvacON_duration, RS_duration, RS_freq, ncol=1)
      ggsave(file = paste0("../plots/HVACpattern_",plot_name, ".png"), width = 20, height = 50, dpi = 300, plots, limitsize=FALSE)
      
    } else {
      
      stats            = add.event.vline(stats)
      RS_duration      = add.event.vline(RS_duration)
      RS_freq          = add.event.vline(RS_freq)
      
      plots <- arrangeGrob(stats, RS_duration, RS_freq, ncol=1)
      ggsave(file = paste0("../plots/HVACpattern_",plot_name, ".png"), width = 20, height = 30, dpi = 300, plots)
    }
  }
}


### 
## 3. strong_light counting light 
## ==> # of over "peak * (0.5~0.8)" in 15mins 

for(lab in 1:1){ 
  # lab_dt & name selection 
  lab_dt = dt_list[[lab]]
  lab_name = lab_names[lab]
  
  print(lab_name)
  
  hvac_peak = quantile(lab_dt$hvac, .95, na.rm = T)
  print(hvac_peak)
  
  strong_hvac_aggDay_dt = lab_dt[, .(strong_hvac_50 = sum(hvac > (hvac_peak * 0.5)),
                                      strong_hvac_60 = sum(hvac > (hvac_peak * 0.6)),
                                      strong_hvac_70 = sum(hvac > (hvac_peak * 0.7)),
                                      strong_hvac_80 = sum(hvac > (hvac_peak * 0.8)),
                                      strong_hvac_90 = sum(hvac > (hvac_peak * 0.9))), by=aggDay]
  
  setnames(strong_hvac_aggDay_dt,old="aggDay",new="get")
  
  strong_hvac_aggWeek_dt = lab_dt[, .(strong_hvac_50 = sum(hvac > (hvac_peak * 0.5)),
                                       strong_hvac_60 = sum(hvac > (hvac_peak * 0.6)),
                                       strong_hvac_70 = sum(hvac > (hvac_peak * 0.7)),
                                       strong_hvac_80 = sum(hvac > (hvac_peak * 0.8)),
                                       strong_hvac_90 = sum(hvac > (hvac_peak * 0.9))), by=aggWeek]
  
  setnames(strong_hvac_aggWeek_dt,old="aggWeek",new="get")
  
  aggDay_rownum_expDate <- 1
  
  for (d in expDate) {
    aggDay_rownum_expDate <- append(aggDay_rownum_expDate, (nrow(strong_hvac_aggDay_dt[d>strong_hvac_aggDay_dt$get,])+1))
  }
  
  aggWeek_rownum_expDate <- 1
  
  for (d in expDate) {
    aggWeek_rownum_expDate <- append(aggWeek_rownum_expDate, (nrow(strong_hvac_aggWeek_dt[d>strong_hvac_aggWeek_dt$get,])+1))
  }
  
  for(windowingWeek in c(4,8,12,16)) {
    
    plot_name = paste(lab_name, "strong_hvac count - windowing", windowingWeek,"weeks")  
    print(plot_name)
    
    rownum_expDate <- aggDay_rownum_expDate
    
    p1 <- ggplot(strong_hvac_aggDay_dt, aes(x=get)) +
      geom_point(aes(y=strong_hvac_50, color='strong_hvac_50')) +  
      geom_point(aes(y=strong_hvac_60, color='strong_hvac_60')) +          
      geom_point(aes(y=strong_hvac_70, color='strong_hvac_70')) +            
      geom_point(aes(y=strong_hvac_80, color='strong_hvac_80')) +
      geom_point(aes(y=strong_hvac_90, color='strong_hvac_90')) +
      ggtitle(paste(plot_name, "aggDay"))
    
    p1 = add.window.line(p1, strong_hvac_aggDay_dt, 'strong_hvac_50', windowingWeek)
    p1 = add.window.line(p1, strong_hvac_aggDay_dt, 'strong_hvac_60', windowingWeek)
    p1 = add.window.line(p1, strong_hvac_aggDay_dt, 'strong_hvac_70', windowingWeek)
    p1 = add.window.line(p1, strong_hvac_aggDay_dt, 'strong_hvac_80', windowingWeek)
    p1 = add.window.line(p1, strong_hvac_aggDay_dt, 'strong_hvac_90', windowingWeek)
    
    rownum_expDate <- aggWeek_rownum_expDate
    
    p2 <- ggplot(strong_hvac_aggWeek_dt, aes(x=get)) +
      geom_point(aes(y=strong_hvac_50, color='strong_hvac_50')) +
      geom_point(aes(y=strong_hvac_60, color='strong_hvac_60')) +
      geom_point(aes(y=strong_hvac_70, color='strong_hvac_70')) +
      geom_point(aes(y=strong_hvac_80, color='strong_hvac_80')) +
      geom_point(aes(y=strong_hvac_90, color='strong_hvac_90')) +
      ggtitle(paste(plot_name, "aggWeek"))
    
    p2 = add.window.line(p2, strong_hvac_aggWeek_dt, 'strong_hvac_50', windowingWeek)
    p2 = add.window.line(p2, strong_hvac_aggWeek_dt, 'strong_hvac_60', windowingWeek)
    p2 = add.window.line(p2, strong_hvac_aggWeek_dt, 'strong_hvac_70', windowingWeek)
    p2 = add.window.line(p2, strong_hvac_aggWeek_dt, 'strong_hvac_80', windowingWeek)
    p2 = add.window.line(p2, strong_hvac_aggWeek_dt, 'strong_hvac_90', windowingWeek)
    
    p1 = add.event.vline(p1)
    p2 = add.event.vline(p2)
    
    plots <- arrangeGrob(p1, p2)
    
    ggsave(file = paste0("../plots/", plot_name, ".png"), width = 20, height = 20, dpi = 600, plots)
  }
}


### 
## 4. full_hvacON counting : 24hours 
## ==> # of "over peak*(0.5~0.8)" == 96?

for(lab in 1:1){ 
  # lab_dt & name selection 
  lab_dt = dt_list[[lab]]
  lab_name = lab_names[lab]
  
  print(lab_name)
  
  hvac_peak = quantile(lab_dt$hvac, .95, na.rm = T)
  print(hvac_peak)
  
  full_hvacON_aggDay_dt = lab_dt[, .(full_hvacON_50 = sum(hvac > (hvac_peak * 0.5), na.rm = T)==96,
                                      full_hvacON_40 = sum(hvac > (hvac_peak * 0.4), na.rm = T)==96,
                                      full_hvacON_30 = sum(hvac > (hvac_peak * 0.3), na.rm = T)==96,
                                      full_hvacON_20 = sum(hvac > (hvac_peak * 0.2), na.rm = T)==96,
                                      full_hvacON_10 = sum(hvac > (hvac_peak * 0.1), na.rm = T)==96), by=aggDay]
  
  setnames(full_hvacON_aggDay_dt,old="aggDay",new="get")
  
  full_hvacON_aggWeek_dt = full_hvacON_aggDay_dt[, .(full_hvacON_50 = sum(full_hvacON_50, na.rm = T),
                                                       full_hvacON_40 = sum(full_hvacON_40, na.rm = T),
                                                       full_hvacON_30 = sum(full_hvacON_30, na.rm = T),
                                                       full_hvacON_20 = sum(full_hvacON_20, na.rm = T),
                                                       full_hvacON_10 = sum(full_hvacON_10, na.rm = T)), by=.(aggWeek=as.Date(cut(get, breaks = "week", start.on.monday = T)))]
  
  setnames(full_hvacON_aggWeek_dt,old="aggWeek",new="get")
  
  
  rownum_expDate <- 1
  
  for (d in expDate) {
    rownum_expDate <- append(rownum_expDate, (nrow(full_hvacON_aggWeek_dt[d>full_hvacON_aggWeek_dt$get,])+1))
  }
  
  for(windowingWeek in c(4,8,12,16)) {
    
    plot_name = paste(lab_name, "full(24hrs)_hvacON count - windowing", windowingWeek,"weeks")  
    print(plot_name)
    
    p <- ggplot(full_hvacON_aggWeek_dt, aes(x=get)) +
      
      #                         geom_point(aes(y=full_hvacON_50, color='full_hvacON_50')) +
      #                         geom_smooth(aes(y=full_hvacON_50, color='full_hvacON_50'), span = s) +    
      #                         
      #                         geom_point(aes(y=full_hvacON_40, color='full_hvacON_40')) +
      #                         geom_smooth(aes(y=full_hvacON_40, color='full_hvacON_40'), span = s) +             
      
      geom_point(aes(y=full_hvacON_30, color='full_hvacON_30')) +        
      geom_point(aes(y=full_hvacON_20, color='full_hvacON_20')) +
      geom_point(aes(y=full_hvacON_10, color='full_hvacON_10')) +
      ggtitle(paste(plot_name, "aggWeek"))
    
    p = add.window.line(p, full_hvacON_aggWeek_dt, 'full_hvacON_30', windowingWeek)
    p = add.window.line(p, full_hvacON_aggWeek_dt, 'full_hvacON_20', windowingWeek)
    p = add.window.line(p, full_hvacON_aggWeek_dt, 'full_hvacON_10', windowingWeek)
    
    p = add.event.vline(p)
    
    ggsave(file = paste0("../plots/", plot_name, ".png"), width = 20, height = 10, dpi = 600, p)
  }
}


### 
## 5. consecutive_hvacON counting : 24hours 
##
temp<-dt_list[[1]]

## 
hist(temp[hvac>0.1]$hvac, 100)   # abnormal : over 1.5
temp[hvac > 1.5, ':='(hvac = 1.5)] # max : 1.4

hvac_min <- 0.1

counting.max.consecutive.hvacON <- function(usage){
  
  cons<-ifelse((usage[1] > hvac_min) & (usage[2] > hvac_min), 1, 0)
  count<-cons
  
  for(i in 2:(length(usage)-1)) {
    if((usage[i] > hvac_min) & (usage[i+1] > hvac_min)) {
      count<-count+1
    }else {
      count<-0
    }
    cons<-append(cons,count)
  }
  
  return (ifelse(max(cons)==0,0,max(cons)+1))
}

count_max_consecutive_hvacON_aggDay_dt = temp[, .(cons_hvacON = counting.max.consecutive.hvacON(na.locf(hvac))), by=aggDay]
setnames(count_max_consecutive_hvacON_aggDay_dt,old="aggDay",new="get")

rownum_expDate <- 1
for (d in expDate) {
  rownum_expDate <- append(rownum_expDate, (nrow(count_max_consecutive_hvacON_aggDay_dt[d>count_max_consecutive_hvacON_aggDay_dt$get,])+1))
}



for (windowingWeek in c(4,8,12,16)) {
  
  plot_name = paste("MARG counting max consecutive hvacON aggDay windowing",windowingWeek,"weeks")
  
  count_max_consecutive_hvacON <- ggplot(count_max_consecutive_hvacON_aggDay_dt, aes(x=get)) +
    geom_point(aes(y=cons_hvacON, color='cons_hvacON'), alpha = 0.5) +
    ggtitle(plot_name)
  
  count_max_consecutive_hvacON = add.window.line(count_max_consecutive_hvacON, count_max_consecutive_hvacON_aggDay_dt, 'cons_hvacON', windowingWeek)
  count_max_consecutive_hvacON = add.event.vline(count_max_consecutive_hvacON)
  
  ggsave(file = paste0("../plots/", plot_name, ".png"), width = 20, height = 10, dpi = 600, count_max_consecutive_hvacON)
}


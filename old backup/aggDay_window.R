library(ggplot2)
library(scales)
# library(zoo)
library(smoother)
library(data.table)


expDate<-c(as.Date("2015-10-08"),
           as.Date("2015-12-01"),
           as.Date("2016-01-11"),
           as.Date("2016-02-01"),
           as.Date("2016-05-16"),
           as.Date("2016-06-13"))

rownum_expDate <- 1

for (d in expDate) {
  print(nrow(plot_dt[d>=plot_dt$get,])+1)
  rownum_expDate <- append(rownum_expDate, (nrow(plot_dt[d>plot_dt$get,])+1))
}


##windowingWeek이 2이면 앞뒤 1주씩보고 windowing
windowingWeek <- 4

## n: the number of samples for windowing before or after the center sample 

centered = TRUE   ##centered=TRUE이면 windowingWeek은 짝수여야함


plot_dt <- return_dts[[64]]
plot_name<-names(return_dts[64])



windowingByExpDate <- function(data, yName, windowingWeek){
  if(nrow(data) <= 150) {
    n <- windowingWeek/2
  }else {
    n <- windowingWeek/2*7
  }

  y<-data[[yName]]
  windowing <- data.table(matrix(rep(0,length(y)*3),ncol=3))
  setnames(windowing,c("get","mean","sd"))
  
  for (k in 1:length(y)) {
    start<-1
    end<-1
    
    ##Collecting UX RealSense data starts on "2015-11-02"
    if((yName=='sum_of_duration' | yName=='freq') & (data$get[k] < as.Date("2015-11-02"))) {
            windowing$mean[k] <- NA
            windowing$sd[k] <- NA 
            next
    }
    
    if(k < rownum_expDate[2]){
      start<-k-n
      end<-k+n
      
      if(k-n <= 0) {
        start <- 1
      }
      if(k+n >= rownum_expDate[2]) {
        end <- rownum_expDate[2]
      }
      
      windowing$mean[k] <- ifelse(is.nan(mean(y[start:end],na.rm=T)),NA,mean(y[start:end],na.rm=T))
      windowing$sd[k] <- ifelse(is.nan(sd(y[start:end],na.rm=T)),NA,sd(y[start:end],na.rm=T))

    } else if(k < rownum_expDate[3]){
      start<-k-n
      end<-k+n
      
      if(k-n <= rownum_expDate[2]) {
        start <- rownum_expDate[2]
      }
      if(k+n >= rownum_expDate[3]) {
        end <- rownum_expDate[3]
      }
      
      windowing$mean[k] <- ifelse(is.nan(mean(y[start:end],na.rm=T)),NA,mean(y[start:end],na.rm=T))
      windowing$sd[k] <- ifelse(is.nan(sd(y[start:end],na.rm=T)),NA,sd(y[start:end],na.rm=T))
    } else if(k < rownum_expDate[4]){
      start<-k-n
      end<-k+n
      
      if(k-n <= rownum_expDate[3]) {
        start <- rownum_expDate[3]
      }
      if(k+n >= rownum_expDate[4]) {
        end <- rownum_expDate[4]
      }
      
      windowing$mean[k] <- ifelse(is.nan(mean(y[start:end],na.rm=T)),NA,mean(y[start:end],na.rm=T))
      windowing$sd[k] <- ifelse(is.nan(sd(y[start:end],na.rm=T)),NA,sd(y[start:end],na.rm=T)) 
    } else if(k < rownum_expDate[5]){
      start<-k-n
      end<-k+n
      
      if(k-n <= rownum_expDate[4]) {
        start <- rownum_expDate[4]
      }
      if(k+n >= rownum_expDate[5]) {
        end <- rownum_expDate[5]
      }
      
      windowing$mean[k] <- ifelse(is.nan(mean(y[start:end],na.rm=T)),NA,mean(y[start:end],na.rm=T))
      windowing$sd[k] <- ifelse(is.nan(sd(y[start:end],na.rm=T)),NA,sd(y[start:end],na.rm=T))
    } else if(k < rownum_expDate[6]){
      start<-k-n
      end<-k+n
      
      if(k-n <= rownum_expDate[5]) {
        start <- rownum_expDate[5]
      }
      if(k+n >= rownum_expDate[6]) {
        end <- rownum_expDate[6]
      }
      
      windowing$mean[k] <- ifelse(is.nan(mean(y[start:end],na.rm=T)),NA,mean(y[start:end],na.rm=T))
      windowing$sd[k] <- ifelse(is.nan(sd(y[start:end],na.rm=T)),NA,sd(y[start:end],na.rm=T))
    } else if(k < rownum_expDate[7]){
      start<-k-n
      end<-k+n
      
      if(k-n <= rownum_expDate[6]) {
        start <- rownum_expDate[6]
      }
      if(k+n >= rownum_expDate[7]) {
        end <- rownum_expDate[7]
      }
      
      windowing$mean[k] <- ifelse(is.nan(mean(y[start:end],na.rm=T)),NA,mean(y[start:end],na.rm=T))
      windowing$sd[k] <- ifelse(is.nan(sd(y[start:end],na.rm=T)),NA,sd(y[start:end],na.rm=T))
    } else {
      start<-k-n
      end<-k+n
      
      if(k-n <= rownum_expDate[7]) {
        start <- rownum_expDate[7]
      }
      if(k+n >= length(y)) {
        end <- length(y)
      }
      
      windowing$mean[k] <- ifelse(is.nan(mean(y[start:end],na.rm=T)),NA,mean(y[start:end],na.rm=T))
      windowing$sd[k] <- ifelse(is.nan(sd(y[start:end],na.rm=T)),NA,sd(y[start:end],na.rm=T)) 
    }
  }
  windowing$get <- data$get
  return (windowing)
}


add.event.vline <- function(plot_body){
  result = plot_body + 
    scale_x_date("Timestamp", labels = date_format("%y-%m"), breaks = date_breaks("month")) +
    
    geom_vline(aes(xintercept = as.numeric(as.Date("2015-10-08"))),color="green4") +
    geom_vline(aes(xintercept = as.numeric(as.Date("2015-12-01"))),color="green4") +
    geom_vline(aes(xintercept = as.numeric(as.Date("2016-01-11"))),color="green4") +
    geom_vline(aes(xintercept = as.numeric(as.Date("2016-02-01"))),color="green4") +
    geom_vline(aes(xintercept = as.numeric(as.Date("2016-05-16"))),color="green4") +
    geom_vline(aes(xintercept = as.numeric(as.Date("2016-06-13"))),color="green4") +         
    theme(legend.position = "bottom")
  
  return(result)
}


add.window.line <- function(plot_body, data, valueName, windowingWeek) {
  window_df = windowingByExpDate(data,valueName,windowingWeek)
  result = plot_body +
            geom_line(data=window_df, aes_string(y = "mean", color = shQuote(valueName)), size=1) +
            geom_ribbon(data=window_df,aes(ymin = mean - sd, ymax = mean + sd), alpha = 0.2) 
  
  return (result)
}


  
###############stats plot
  
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



###############RS plot

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



###############partial_lightON, lightON_duration plot


  if(grepl("light", plot_name) | grepl("total", plot_name)){
    
    partial_lightON <- ggplot(plot_dt, aes(x=get)) +
      geom_point(aes(y=threshold50, color='threshold50'), alpha = 0.5) +
      geom_point(aes(y=threshold60, color='threshold60'), alpha = 0.5) +
      geom_point(aes(y=threshold70, color='threshold70'), alpha = 0.5) +
      geom_point(aes(y=threshold80, color='threshold80'), alpha = 0.5) +
      geom_point(aes(y=threshold90, color='threshold90'), alpha = 0.5) +
      ggtitle("partial_lightON")
    
    partial_lightON = add.window.line(partial_lightON, plot_dt, 'threshold50', windowingWeek)
    partial_lightON = add.window.line(partial_lightON, plot_dt, 'threshold60', windowingWeek)
    partial_lightON = add.window.line(partial_lightON, plot_dt, 'threshold70', windowingWeek)
    partial_lightON = add.window.line(partial_lightON, plot_dt, 'threshold80', windowingWeek)
    partial_lightON = add.window.line(partial_lightON, plot_dt, 'threshold90', windowingWeek)
    
    lightON_duration <- ggplot(plot_dt, aes(x=get)) +
      geom_point(aes(y=lightON, color='lightON'), alpha = 0.5) +     
      ggtitle("lightON duration")
    
    lightON_duration = add.window.line(lightON_duration, plot_dt, 'lightON',windowingWeek)
    
    
    stats            = add.event.vline(stats)
    partial_lightON  = add.event.vline(partial_lightON)
    lightON_duration = add.event.vline(lightON_duration)
    RS_duration      = add.event.vline(RS_duration)
    RS_freq          = add.event.vline(RS_freq)
    
    plots <- arrangeGrob(stats, partial_lightON, lightON_duration, RS_duration, RS_freq, ncol=1)
    print(plots)
#     ggsave(file = paste0("plots/",plot_name, ".png"), width = 20, height = 50, dpi = 300, plots, limitsize=FALSE)
    
  } else {
    
    stats            = add.event.vline(stats)
    RS_duration      = add.event.vline(RS_duration)
    RS_freq          = add.event.vline(RS_freq)
    
    plots <- arrangeGrob(stats, RS_duration, RS_freq, ncol=1)
    print(plots)
#     ggsave(file = paste0("plots/",plot_name, ".png"), width = 20, height = 30, dpi = 300, plots)
  }



ggsave(file = paste0("../plots/custom_window/",plot_name, ".png"), width = 20, height = 50, dpi = 300, plots, limitsize=FALSE)




















###########partial light 때문에 dt 다시 생성
for(lab in 1:4){ 
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
      
      for(feeder in feeders){
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
        
        light_min = 0.01
        light_peak = quantile(lab_dt$light, .95, na.rm = T)
        light_dt = lab_dt[, .(timestamp = timestamp,
                              aggDay = aggDay,
                              aggWeek = aggWeek,
                              light = na.locf(light),
                              lightON = ifelse(na.locf(light) > light_min, 1, 0),
                              peak_50 = filter.fault.partial.lightOn(ifelse((na.locf(light) < light_peak * 0.5) & (na.locf(light) > light_min), 1, 0)),
                              peak_60 = filter.fault.partial.lightOn(ifelse((na.locf(light) < light_peak * 0.6) & (na.locf(light) > light_min), 1, 0)),
                              peak_70 = filter.fault.partial.lightOn(ifelse((na.locf(light) < light_peak * 0.7) & (na.locf(light) > light_min), 1, 0)),
                              peak_80 = filter.fault.partial.lightOn(ifelse((na.locf(light) < light_peak * 0.8) & (na.locf(light) > light_min), 1, 0)),
                              peak_90 = filter.fault.partial.lightOn(ifelse((na.locf(light) < light_peak * 0.9) & (na.locf(light) > light_min), 1, 0)))]
        
        partial_light = light_dt[, .(lightON = sum(lightON),
                                     threshold50 = ifelse(sum(lightON)==0,1,sum(peak_50)/sum(lightON)),
                                     threshold60 = ifelse(sum(lightON)==0,1,sum(peak_60)/sum(lightON)),
                                     threshold70 = ifelse(sum(lightON)==0,1,sum(peak_70)/sum(lightON)),
                                     threshold80 = ifelse(sum(lightON)==0,1,sum(peak_80)/sum(lightON)),
                                     threshold90 = ifelse(sum(lightON)==0,1,sum(peak_90)/sum(lightON))), by=get(agg_Unit)]
        
        return_dt = merge(return_dt, partial_light, by="get")
        
        assign(dt_name, return_dt)
        return_dts = append(return_dts, setNames(list(return_dt),dt_name))
      }
    }
  }
}


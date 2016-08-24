### ------------------------------------------- ###
### Plotting
windowingByExpDate <- function(data, yName, windowingWeek, rownum_expDate){
  if((data$get[2] - data$get[1]) > 3) {
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
    
    #Collecting UX RealSense data starts on "2015-11-02"
    if((yName=='sum_of_duration' | yName=='freq') & (data.frame(data)[k,1] < as.Date("2015-11-02"))) {
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
        end <- rownum_expDate[2] - 1
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
        end <- rownum_expDate[3] - 1
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
        end <- rownum_expDate[4] - 1
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
        end <- rownum_expDate[5] - 1
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
        end <- rownum_expDate[6] - 1
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
        end <- rownum_expDate[7] - 1
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
#     print(paste(data.frame(data)[k,1],":",start,end, windowing$mean[k]))
  }
  windowing$sd <- ifelse(is.na(windowing$sd),0,windowing$sd)
#   windowing$get <- data.frame(data)[,1]
  windowing$get <- data$get
  
  return (windowing)
}

add.window.line <- function(plot_body, data, valueName, windowingWeek, rownum_expDate) {
  window_df = windowingByExpDate(data, valueName, windowingWeek, rownum_expDate)
  
  result = plot_body +
    geom_line(data=window_df, aes_string(y = "mean", linetype = shQuote(valueName)), size=1) +
    geom_ribbon(data=window_df,aes(ymin = mean - sd, ymax = mean + sd), alpha = 0.2)
  
  return (result)
}

add.colorful.window.line <- function(plot_body, data, valueName, windowingWeek, colorName, rownum_expDate, ribbon=TRUE) {
  window_df = windowingByExpDate(data, valueName, windowingWeek, rownum_expDate)
  
  if(ribbon==TRUE) {
    result = plot_body +
      geom_line(data=window_df, aes_string(y = "mean", linetype = shQuote(valueName)), color=colorName, size=1) +
      geom_ribbon(data=window_df, aes(ymin = mean - sd, ymax = mean + sd), fill=colorName, alpha = 0.2)
  }else {
    result = plot_body +
      geom_line(data=window_df, aes_string(y = "mean", color = shQuote(valueName)), size=1)
  }
  
  return (result)
}

set.expDate.rownum <- function(plot_dt, expDate) {
  rownum_expDate <- 1
  
  for (d in expDate) {
    rownum_expDate <- append(rownum_expDate, ifelse((nrow(plot_dt[d > data.frame(plot_dt)[,1],])+1)>nrow(plot_dt),nrow(plot_dt),(nrow(plot_dt[d > data.frame(plot_dt)[,1],])+1)))
  }
  
  return(rownum_expDate)
}

insert_minor <- function(major_labs, n_minor) {labs <- 
                                                 c( sapply( major_labs, function(x) c(x, rep("", 4) ) ) )
                                               labs[1:(length(labs)-n_minor)]}

add.event.vline.exp1.1 <- function(plot_body){
  result = plot_body + 
    scale_x_date("Timestamp", labels = date_format("%Y-%m"), breaks = date_breaks("month")) +
#     scale_x_date("Timestamp", labels = insert_minor(date_format("%Y-%m"), 4), breaks = date_breaks("week")) +
    theme_bw()+
    geom_vline(aes(xintercept = as.numeric(as.Date("2014-11-10"))),color="gray40", linetype = "longdash") +
    geom_vline(aes(xintercept = as.numeric(as.Date("2014-11-17"))),color="gray40", linetype = "longdash")
  
  return(result)
}

add.event.vline.exp1.2 <- function(plot_body){
  result = plot_body + 
    scale_x_date("Timestamp", labels = date_format("%Y-%m"), breaks = date_breaks("month")) +
    theme_bw()+
    geom_vline(aes(xintercept = as.numeric(as.Date("2014-11-10"))),color="gray40", linetype = "longdash") +
    geom_vline(aes(xintercept = as.numeric(as.Date("2014-11-17"))),color="gray40", linetype = "longdash") +
    geom_vline(aes(xintercept = as.numeric(as.Date("2015-01-15"))),color="gray40", linetype = "longdash") +
    geom_vline(aes(xintercept = as.numeric(as.Date("2015-01-22"))),color="gray40", linetype = "longdash")
  
  return(result)
}

add.event.vline.exp2 <- function(plot_body){
  result = plot_body + 
    scale_x_date("Timestamp", labels = date_format("%Y-%m"), breaks = date_breaks("month")) +
    theme_bw()+
    geom_vline(aes(xintercept = as.numeric(as.Date("2015-10-08"))),color="gray40", linetype = "longdash") +
    geom_vline(aes(xintercept = as.numeric(as.Date("2015-12-01"))),color="gray40", linetype = "longdash") +
    geom_vline(aes(xintercept = as.numeric(as.Date("2016-01-11"))),color="gray40", linetype = "longdash") +
    geom_vline(aes(xintercept = as.numeric(as.Date("2016-02-01"))),color="gray40", linetype = "longdash") +
    geom_vline(aes(xintercept = as.numeric(as.Date("2016-05-16"))),color="gray40", linetype = "longdash") +
    geom_vline(aes(xintercept = as.numeric(as.Date("2016-06-13"))),color="gray40", linetype = "longdash")
  
  return(result)
}

set.default.theme <- function(plot_body) {
  result = plot_body + 
    theme_bw()+
    theme(axis.line = element_line(colour = "black"),
          legend.text = element_text(size=23, hjust = 1),
          plot.title = element_text(size=23),
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.key = element_rect(colour="white"),
          legend.key.size = unit(10,"cm"),
          legend.margin = unit(0, "cm"),
          axis.text = element_text(size=20),
          axis.text.x = element_text(size=15, angle = 45, hjust = 1),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size=23, vjust = 1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank())+
    guides(fill = guide_legend(keywidth = 1, keyheight = 1),
           linetype = guide_legend(keywidth = 4, keyheight = 1),
           colour = guide_legend(keywidth = 3, keyheight = 1))
  
  return(result)
}

##theme for light pattern plot; without legend box
set.colorful.theme <- function(plot_body, colorName) {
  result = plot_body + 
    theme_bw()+
    theme(axis.line = element_line(colour = "black"),
          legend.text = element_text(size=23, hjust = 1),
          plot.title = element_text(size=23),
          legend.position = "none",
          legend.title = element_blank(),
          legend.key = element_rect(colour="white"),
          legend.key.size = unit(10,"cm"),
          legend.margin = unit(0, "cm"),
          axis.text = element_text(size=20),
          axis.text.x = element_text(size=15, angle = 45, hjust = 1),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size=23, vjust = 1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank())+
    guides(fill = guide_legend(keywidth = 1, keyheight = 1),
           linetype = guide_legend(keywidth = 4, keyheight = 1),
           colour = guide_legend(keywidth = 3, keyheight = 1))+
    scale_color_manual(values=colorName)+
    scale_fill_manual(values=colorName)
  
  return(result)
}

get.expDate.1.1 <- function() {
  exp_Date<-c(as.Date("2014-11-10"),
             as.Date("2014-11-17"),
             as.Date("2016-11-16"),
             as.Date("2016-11-16"),
             as.Date("2016-11-16"),
             as.Date("2016-11-16"))
  return(exp_Date)
}

set.expDate.1.1 <- function(raw_dt) {
  cut_dt <- raw_dt[get>= "2014-10-01" & get<= "2014-12-16"]
  
  return(cut_dt)
}

get.expDate.1.2 <- function() {
  exp_Date<-c(as.Date("2014-11-10"),
             as.Date("2014-11-17"),
             as.Date("2015-01-15"),
             as.Date("2015-01-22"),
             as.Date("2016-11-16"),
             as.Date("2016-11-16"))
  return(exp_Date)
}

set.expDate.1.2 <- function(raw_dt) {
  cut_dt <- raw_dt[get>= "2014-10-01" & get<= "2015-02-28"]
  
  return(cut_dt)
}

get.expDate.2 <- function() {
  exp_Date<-c(as.Date("2015-10-08"),
             as.Date("2015-12-01"),
             as.Date("2016-01-11"),
             as.Date("2016-02-01"),
             as.Date("2016-05-16"),
             as.Date("2016-06-13"))
  return(exp_Date)
}

set.expDate.2 <- function(raw_dt) {
  cut_dt <- raw_dt[get>= "2015-03-01" & get<= "2016-07-26"]
  
  return(cut_dt)
}


get.expDate <- function() {
  exp_Date<-list(c(as.Date("2014-10-01"), as.Date("2014-10-31")),
                 c(as.Date("2014-11-10"), as.Date("2014-11-16")),
                 c(as.Date("2014-11-17"), as.Date("2014-12-16")),
                 c(as.Date("2015-01-15"), as.Date("2015-01-21")),
                 c(as.Date("2015-01-22"), as.Date("2015-02-28")),
                 c(as.Date("2015-03-01"), as.Date("2015-09-30")),
                 c(as.Date("2015-10-08"), as.Date("2015-11-30")),
                 c(as.Date("2015-12-01"), as.Date("2016-01-10")),
                 c(as.Date("2016-01-11"), as.Date("2016-01-31")),
                 c(as.Date("2016-02-01"), as.Date("2016-05-15")),
                 c(as.Date("2016-05-16"), as.Date("2016-06-12")),
                 c(as.Date("2016-06-13"), as.Date("2016-07-26")))
  return(exp_Date)
}

save.plot <- function(file, plot) {
  ggsave(file, width = 8, height = 6, dpi = 300, plot, limitsize=FALSE)
}

save.wide.plot <- function(file, plot) {
  ggsave(file, width = 10, height = 6, dpi = 300, plot, limitsize=FALSE)
}






# 마그 지난 1월 점심시간 컴퓨터 절전 대박
plot.lunch.saving <- function(lab, dt, target){    
  
  if (expDate[4]=="2016-11-16"){
    dt = dt[aggDay >= "2014-09-11" & aggDay <= "2014-12-16"]
  } else if (expDate[4]=="2015-01-21"){
    dt = dt[aggDay >= "2014-09-11" & aggDay < "2015-06-01"]
  }
  
  # * before lunch usage: 11:00 ~ 12:00 — index 17:20
  # * during lunch usage: 11:30 ~ 13:30 — index 19:26
  # *  after lunch usage: 13:00 ~ 14:00 — index 25:28
  before_lunch_index = 17:20
  during_lunch_index = 19:26
  after_lunch_index  = 25:28
  
  peak_of_target = quantile(data.frame(dt[, .(get(target))])[,1], .90)
  target_on_threshold = peak_of_target * 0.1
  
  lunch_saving_threshold_ratio = 0.8
  target_on_threshold_usage = peak_of_target * 0.1
  
  before_dt = dt[index %in% before_lunch_index, .(before_lunch = max(get(target))), by=aggDay]
  during_dt = dt[index %in% during_lunch_index, .(during_lunch = min(get(target))), by=aggDay]
  after_dt = dt[index %in%  after_lunch_index, .( after_lunch = max(get(target))), by=aggDay]
  
  lunch_dt = merge(before_dt, during_dt, by='aggDay')
  lunch_dt = merge(lunch_dt, after_dt, by='aggDay')
  
  ## Conditions of lunch saving 
  lunch_dt[(during_lunch < (before_lunch * lunch_saving_threshold_ratio)) 
           & (during_lunch < (after_lunch * lunch_saving_threshold_ratio)) 
           & (before_lunch > target_on_threshold_usage), 
           ':='(lunch_saving = 1), by=aggDay]
  
  ## aggregation: week table 'lunch_saving_per_week' 
  lunch_dt[, ':='(aggWeek=as.Date(cut(aggDay, breaks = "week", start.on.monday = T)))]
  lunch_saving_per_week = lunch_dt[, .(lunch_saving_count = sum(lunch_saving, na.rm = T)), by=aggWeek]

  setnames(lunch_saving_per_week,old="aggWeek",new="get")

  rownum_expDate <- set.expDate.rownum(lunch_saving_per_week, expDate)
  

  ## plot 
  p1 = ggplot(lunch_saving_per_week, aes(x=get)) +
    ggtitle(paste(lab, target))

  p1 = add.window.line(p1, lunch_saving_per_week, 'lunch_saving_count', windowingWeek = 4, rownum_expDate)

  if (expDate[4]=="2016-11-16"){
    p1 = add.event.vline.exp1(p1)
  } else if (expDate[4]=="2015-01-21"){
    p1 = add.event.vline.exp2(p1)
  } else{
    p1 = add.event.vline(p1)
  }
  
  p1 = set.default.theme(p1)

  print(p1)
}


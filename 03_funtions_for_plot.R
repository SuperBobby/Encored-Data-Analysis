### ------------------------------------------------------------ ###
## Functions for plotting 
##
### EJ, JY @ ADSL, SNU 
###                                   last update : 2016. 8. 30.
### ------------------------------------------------------------ ###

windowingByExpDate <- function(data, data_name, yName, windowingWeek, rownum_expDate){
  if(grepl("aggWeek", data_name)){
    if(grepl("workingday", data_name) | grepl("non_workingday", data_name)){
      n <- windowingWeek/2/2
    } else{
      n <- windowingWeek/2
    }
  } else{
    if(grepl("allDay", data_name)){
      n <- windowingWeek/2*7
    } else if(grepl("workingday", data_name)){
      n <- windowingWeek/2*5
    } else{
      n <- windowingWeek/2*2
    }
  }
  
  y<-data[[yName]]
  windowing <- data.table(matrix(rep(0,length(y)*3),ncol=3))
  setnames(windowing,c("timestamp","mean","sd"))
  
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
#   windowing$timestamp <- data.frame(data)[,1]
  windowing$timestamp <- data$timestamp
  
  return (windowing)
}

windowingByExpDate2 <- function(data, data_name, target, windowingWeek, expDate){
  #windowing before and after n days
  n <- windowingWeek/2*7
  
  windowing <- data.table(matrix(rep(0,nrow(data)*3),ncol=3))
  setnames(windowing,c("timestamp","mean","sd"))
  
  for (k in 1:nrow(data)) {
    point_date = data$timestamp[k]
    start_date = (point_date - n)
    end_date = (point_date + n)
    
    for(i in 1:length(expDate)){
      if(start_date < expDate[i]){
        if(point_date < expDate[i]){
          start_date = start_date
        } else{
          start_date = expDate[i]
        }
      } else{
        start_date = start_date
      }
      
      if(end_date > expDate[i]){
        if(point_date < expDate[i]){
          end_date = expDate[i]-1
        } else{
          end_date = end_date
        }
      } else if(end_date == expDate[i]){
        end_date = expDate[i]-1
      } else{
        end_date = end_date
      }
    }
    
    cut_data <- data[(timestamp >= start_date) & (timestamp <= end_date)]
    
    #     print(paste("start:",start_date,"->",cut_data$timestamp[1]))
    #     print(paste("end:",end_date,"->",cut_data$timestamp[nrow(cut_data)]))
    #     print(nrow(cut_data))
    
    windowing$mean[k] <- mean(cut_data[[target]])
    windowing$sd[k] <- ifelse(is.na(sd(cut_data[[target]])),0,sd(cut_data[[target]]))
  }
  
  windowing$timestamp <- data$timestamp
  
  return (windowing)
}

add.window.line <- function(plot_body, data, data_name, valueName, windowingWeek, rownum_expDate, expDate) {
#   window_df = windowingByExpDate(data, data_name, valueName, windowingWeek, rownum_expDate)
  window_df = windowingByExpDate2(data, data_name, valueName, windowingWeek, expDate)

  result = plot_body +
    geom_line(data=window_df, aes_string(y = "mean", linetype = shQuote(valueName)), size=1) +
    geom_ribbon(data=window_df,aes(ymin = mean - sd, ymax = mean + sd), alpha = 0.2)
  
  return (result)
}


add.colorful.window.line <- function(plot_body, data, data_name, valueName, windowingWeek, colorName, rownum_expDate, ribbon=TRUE) {
  window_df = windowingByExpDate(data, data_name, valueName, windowingWeek, rownum_expDate)
  
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

get.expDate.rownum <- function(plot_dt, expDate) {
  rownum_expDate <- 1
  
  for (d in expDate) {
    rownum_expDate <- append(rownum_expDate, ifelse((nrow(plot_dt[d > data.frame(plot_dt)[,1],])+1)>nrow(plot_dt),nrow(plot_dt),(nrow(plot_dt[d > data.frame(plot_dt)[,1],])+1)))
  }
  
  return(rownum_expDate)
}

insert_minor <- function(major_labs, n_minor) {labs <- 
                                                 c( sapply( major_labs, function(x) c(x, rep("", 4) ) ) )
                                               labs[1:(length(labs)-n_minor)]}



# add.event.vline.exp1.1
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

# set default theme
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


# save plot 
save.plot <- function(file, plot, width_ = 8, height_ = 6, dpi_ = 300) {
  
  ggsave(file, width = width_, height = height_, 
         dpi = dpi_, plot, limitsize=FALSE)
}



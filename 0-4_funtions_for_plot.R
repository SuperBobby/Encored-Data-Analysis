### ------------------------------------------------------------ ###
## Functions for plotting 
##
### EJ, JY @ ADSL, SNU 
###                                   last update : 2016. 8. 30.
### ------------------------------------------------------------ ###


# 플롯 수정 제안 사항 (2017.1.4)
# 
# * 플롯 구분짓는 색상 지정 
# (total, feeder-light/com/hvac, partial lighting, 24hrs lighting, lunchtime saving-light/com)
# * X, Y 축 선
# * Y축 min -- 0 

windowingByExpDate <- function(data, target, windowingWeek, expDate){
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

    if(!is.na(data[[target]][k])){
      windowing$mean[k] <- mean(cut_data[[target]], na.rm = T)
      #standard deviation of one row is NA
      windowing$sd[k] <- ifelse(is.na(sd(cut_data[[target]], na.rm = T)),0,sd(cut_data[[target]], na.rm = T))
    } else{
      windowing$mean[k] <- NA
        windowing$sd[k] <- NA
    }
  }
  
  windowing$timestamp <- data$timestamp

  return (windowing)
}

add.window.line <- function(plot_body, data, target, windowingWeek, expDate) {
#   window_df = windowingByExpDate(data, target, windowingWeek, rownum_expDate)
  window_df = windowingByExpDate(data, target, windowingWeek, expDate)

  result = plot_body +
    geom_line(data=window_df, aes_string(y = "mean", linetype = shQuote(target)), size=1) +
    geom_ribbon(data=window_df, aes(ymin = mean - sd, ymax = mean + sd), alpha = 0.2)
  
  return (result)
}


add.colorful.window.line <- function(plot_body, data, target, windowingWeek, colorName, expDate, ribbon=TRUE) {
  window_df = windowingByExpDate(data, target, windowingWeek, expDate)
  
  if(ribbon==TRUE) {
    result = plot_body +
      geom_line(data=window_df, aes_string(y = "mean", linetype = shQuote(target)), color=colorName, size=1) +
      geom_ribbon(data=window_df, aes(ymin = mean - sd, ymax = mean + sd), fill=colorName, alpha = 0.2)
  }else {
    result = plot_body +
      geom_line(data=window_df, aes_string(y = "mean", color = shQuote(target)), size=1)
  }
  
  return (result)
}


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
    # geom_vline(aes(xintercept = as.numeric(as.Date("2016-05-16"))),color="gray40", linetype = "longdash") +
    geom_vline(aes(xintercept = as.numeric(as.Date("2016-06-13"))),color="gray40", linetype = "longdash")
  
  return(result)
}

add.event.vline.all <- function(plot_body){
  result = plot_body + 
    scale_x_date("Timestamp", labels = date_format("%Y-%m"), breaks = date_breaks("month")) +
    theme_bw()+
    geom_vline(aes(xintercept = as.numeric(as.Date("2014-11-10"))),color="gray40", linetype = "longdash") +
    geom_vline(aes(xintercept = as.numeric(as.Date("2014-11-17"))),color="gray40", linetype = "longdash") +
    geom_vline(aes(xintercept = as.numeric(as.Date("2015-01-15"))),color="gray40", linetype = "longdash") +
    geom_vline(aes(xintercept = as.numeric(as.Date("2015-01-22"))),color="gray40", linetype = "longdash") +
    geom_vline(aes(xintercept = as.numeric(as.Date("2015-10-08"))),color="gray40", linetype = "longdash") +
    geom_vline(aes(xintercept = as.numeric(as.Date("2015-12-01"))),color="gray40", linetype = "longdash") +
    geom_vline(aes(xintercept = as.numeric(as.Date("2016-01-11"))),color="gray40", linetype = "longdash") +
    geom_vline(aes(xintercept = as.numeric(as.Date("2016-02-01"))),color="gray40", linetype = "longdash") +
    # geom_vline(aes(xintercept = as.numeric(as.Date("2016-05-16"))),color="gray40", linetype = "longdash") +
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
set.colorful.theme <- function(plot_body) {
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
           colour = guide_legend(keywidth = 3, keyheight = 1))
#     scale_color_manual(values=colorName)+
#     scale_fill_manual(values=colorName)
  
  return(result)
}


# save plot 
save.plot <- function(file, plot, width_ = 8, height_ = 6, dpi_ = 300) {
  
  ggsave(file, width = width_, height = height_, 
         dpi = dpi_, plot, limitsize=FALSE)
}


basic.plot <- function(data, data_name){
  p <- ggplot(data, aes(x=timestamp)) +
    ggtitle(data_name) +
    ylab("Energy use (kWh/day)")+
    geom_line(aes(y=peak, linetype = "peak"), size=1) + 
    geom_line(aes(y=avg, linetype = "avg"), size=1) + 
    geom_line(aes(y=base, linetype = "base"), size=1) + 
    scale_x_date("Timestamp", labels = date_format("%Y-%m"), breaks = date_breaks("month")) +
    scale_linetype_discrete(breaks=c("peak", "avg", "base")) + 
    theme(axis.text.x = element_text(size=15, angle = 45, hjust = 1)) +
    geom_vline(aes(xintercept = as.numeric(as.Date("2014-11-10"))),color="gray40", linetype = "longdash") +
    geom_vline(aes(xintercept = as.numeric(as.Date("2014-11-17"))),color="gray40", linetype = "longdash") +
    geom_vline(aes(xintercept = as.numeric(as.Date("2015-01-15"))),color="gray40", linetype = "longdash") +
    geom_vline(aes(xintercept = as.numeric(as.Date("2015-01-22"))),color="gray40", linetype = "longdash") +
    geom_vline(aes(xintercept = as.numeric(as.Date("2015-10-08"))),color="gray40", linetype = "longdash") +
    geom_vline(aes(xintercept = as.numeric(as.Date("2015-12-01"))),color="gray40", linetype = "longdash") +
    geom_vline(aes(xintercept = as.numeric(as.Date("2016-01-11"))),color="gray40", linetype = "longdash") +
    geom_vline(aes(xintercept = as.numeric(as.Date("2016-02-01"))),color="gray40", linetype = "longdash") +
    # geom_vline(aes(xintercept = as.numeric(as.Date("2016-05-16"))),color="gray40", linetype = "longdash") +
    geom_vline(aes(xintercept = as.numeric(as.Date("2016-06-13"))),color="gray40", linetype = "longdash")
  
  p = set.default.theme(p)
#   png(filename = paste0("../plots/validationCheck/", data_name, ".png"))
#   plot(p)
#   dev.off()  

  ggsave(filename = paste0("../validationCheck/", data_name, ".png"), plot = p)
# #   save.plot(paste0("../plots/validationCheck/", data_name, ".png"), plot)
  
}

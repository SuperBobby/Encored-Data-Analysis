library(data.table)
library(ggplot2)
library(scales)

PLOT_PATH = "../plots/waste_investigation/"
Sys.setlocale("LC_ALL", "English")

##
## NEW examples(cases) of waste investigation 
## 

##
# 1. Daily 
# 
hourly_dt = fread("../data/raw/marg_hours.csv")
hourly_dt[, ':='(aggDay = get.officehour.date(timestamp))]
daily_dt = hourly_dt[, .(computer = sum(computer),
                         light = sum(light),
                         total = sum(total)), by=(timestamp = aggDay)]
# daily_dt$timestamp = as.Date(daily_dt$timestamp, tz='ROK')
daily_dt$timestamp = as.POSIXct(daily_dt$timestamp, tz='ROK')
daily_dt$timestamp= daily_dt$timestamp - 9*60*60
# View(daily_dt)

DATE_FROM = "2014-10-21"
DATE_TO = "2014-10-24"

daily_plot_dt = daily_dt[timestamp >= DATE_FROM & timestamp <= DATE_TO]

# 1.1. Daily - total 
daily_total_plot <- ggplot(daily_plot_dt, aes(x = timestamp, y=total)) +
  # geom_bar(stat = "identity") +
  geom_line() +
  geom_point() +
  ylab("Power consumption of total (kWh/day)\n") +
  xlab("")+
  ylim(0, 75) + 
  ggtitle("daily - total \n") +
  scale_x_datetime(date_breaks = "days", date_minor_breaks = "days",
                   date_labels = "%Y/%m/%d\n")
print(daily_total_plot)
save.plot(paste0(PLOT_PATH, "daily_total_plot.png"), daily_total_plot)


# 1.2. Daily - light 
# DATE_FROM = "2014-10-20"
# DATE_TO = "2014-10-23"
# 
# daily_plot_dt = daily_dt[timestamp >= DATE_FROM & timestamp <= DATE_TO]

daily_light_plot <- ggplot(daily_plot_dt, aes(x = timestamp, y=light)) +
  # geom_bar(stat = "identity") +
  geom_line() +
  geom_point() +
  ylab("Power consumption of light (kWh/day)\n") +
  xlab("")+
  ylim(0, 25) + 
  ggtitle("daily - light \n") +
  scale_x_datetime(date_breaks = "days", date_minor_breaks = "days",
                   date_labels = "%Y/%m/%d\n")
print(daily_light_plot)
save.plot(paste0(PLOT_PATH, "daily_light_plot.png"), daily_light_plot)


# 1.3. Daily - computer 
# DATE_FROM = "2014-10-06"
# DATE_TO = "2014-10-13"
# 
# daily_plot_dt = daily_dt[timestamp >= DATE_FROM & timestamp <= DATE_TO]

daily_computer_plot <- ggplot(daily_plot_dt, aes(x = timestamp, y=computer)) +
  # geom_bar(stat = "identity") +
  geom_line() +
  geom_point() +
  ylab("Power consumption of computer (kWh/day)\n") +
  xlab("")+
  ylim(0, 45) +
  ggtitle("daily - computer \n") +
  scale_x_datetime(date_breaks = "days", date_minor_breaks = "days",
                   date_labels = "%Y/%m/%d\n")
print(daily_computer_plot)
save.plot(paste0(PLOT_PATH, "daily_computer_plot.png"), daily_computer_plot)



##
# 2. 15-min data (quarter) 
# 
quarter_dt = data.table(marg_dt)
DATE_FROM = "2014-10-19 24:00"
DATE_TO = "2014-10-22 24:00"

quarter_plot_dt = quarter_dt[timestamp >= DATE_FROM & timestamp <= DATE_TO]

# 2.1. quarter - total 
quarter_total_plot <- ggplot(quarter_plot_dt, aes(x = timestamp, y=total*4)) +
  # geom_bar(stat = "identity") +
  geom_line() +
  geom_point() +
  ylab("Power consumption of total (kW/15min)\n") +
  xlab("")+
  ylim(0, 4.1) +
  ggtitle("quarter - total \n") +
  scale_x_datetime(date_breaks = "days", date_minor_breaks = "days",
                   date_labels = "%Y/%m/%d\n")
print(quarter_total_plot)
save.plot(paste0(PLOT_PATH, "quarter_total_plot.png"), quarter_total_plot)



# 2.2. quarter - light 
# DATE_FROM = "2014-10-20"
# DATE_TO = "2014-10-23"
# quarter_plot_dt = quarter_dt[timestamp >= DATE_FROM & timestamp <= DATE_TO]

quarter_light_plot <- ggplot(quarter_plot_dt, aes(x = timestamp, y=light*4)) +
  # geom_bar(stat = "identity") +
  geom_line() +
  geom_point() +
  ylab("Power consumption of light (kW/15min)\n") +
  xlab("")+
  ylim(0, 1.5) +
  ggtitle("quarter - light \n") +
  scale_x_datetime(date_breaks = "days", date_minor_breaks = "days",
                   date_labels = "%Y/%m/%d\n")
print(quarter_light_plot)
save.plot(paste0(PLOT_PATH, "quarter_light_plot.png"), quarter_light_plot)

# 2.3. quarter - computer 
# DATE_FROM = "2014-10-19 22:00"
# DATE_TO = "2014-10-22 23:00"

# quarter_plot_dt = quarter_dt[timestamp >= DATE_FROM & timestamp <= DATE_TO]

quarter_computer_plot <- ggplot(quarter_plot_dt, aes(x = timestamp, y=computer*4)) +
  # geom_bar(stat = "identity") +
  geom_line() +
  geom_point() +
  ylab("Power consumption of computer (kW/15min)\n") +
  xlab("")+
  ylim(0, 2.5) +
  ggtitle("quarter - computer \n") +
  scale_x_datetime(date_breaks = "days", date_minor_breaks = "days",
                   date_labels = "%Y/%m/%d\n")
print(quarter_computer_plot)
save.plot(paste0(PLOT_PATH, "quarter_computer_plot.png"), quarter_computer_plot)



##
# 3. 1Hz data (sec) 
# 

load.total.sec.tidy.data = function(TARGET_DATE, lab){
  
  FILE_PATH = paste0(TIDY_DATA_DIR, lab, '_', as.character(TARGET_DATE), '(total).csv')
  
  if(file.exists(FILE_PATH)){
    dt = fread(FILE_PATH)
    
    ## valid duration check: should be 24 hours
    dt_duration = get.sec.dt.duration(dt)
    if(dt_duration >= 24){
      print(paste(FILE_PATH, 'loaded'))
      names(dt) <- c('dts', 'total') ## column naming 
      return(dt)        
      
    } else {
      print(paste("Invalid sec data duration (", dt_duration, "hours ):", FILE_PATH))
      return(NULL)
    }
    
  } else {
    print(paste("file don't exist:", FILE_PATH))
    return(NULL)
  }
}


PLOT_PATH = "../plots/waste_investigation/"

T_DATE     = "2015-09-01"
FIRST_CUT  = "2015-09-01 12:00"
SECOND_CUT = "2015-09-01 13:30"
  
# 3.1. sec - total
sec_total_dt = load.total.sec.tidy.data(T_DATE, 'marg')
sec_total_dt$dts = as.POSIXct(sec_total_dt$dts)

# sec_plot_dt = sec_total_dt
sec_plot_dt = sec_total_dt[(dts > FIRST_CUT) & (dts < SECOND_CUT)]

sec_total_plot <- ggplot(sec_plot_dt, aes(x = dts, y=total/1000/1000)) +
  # geom_bar(stat = "identity") +
  geom_line() +
  geom_point() +
  ylab("Power consumption of total (kW/sec)\n") +
  xlab("")+
  ylim(0, 10) +
  ggtitle("sec - total \n") +
  scale_x_datetime(date_breaks = "hours", date_minor_breaks = "30 mins",
                   date_labels = "%H:%M\n")
print(sec_total_plot)
save.plot(paste0(PLOT_PATH, "sec_total_plot.png"), sec_total_plot)


# 3.2. sec - light
sec_light_dt = load.light.sec.tidy.data(T_DATE, 'marg')
sec_light_dt$dts = as.POSIXct(sec_light_dt$dts)

# sec_plot_dt = sec_light_dt
sec_plot_dt = sec_light_dt[(dts > FIRST_CUT) & (dts < SECOND_CUT)]

sec_light_plot <- ggplot(sec_plot_dt, aes(x = dts, y=light/1000/1000)) +
  geom_line() +
  geom_point() +
  ylab("Power consumption of light (kW/sec)\n") +
  xlab("")+
  ylim(0, 0.8) +
  ggtitle("sec - light \n") +
  scale_x_datetime(date_breaks = "hours", date_minor_breaks = "30 mins",
                   date_labels = "%H:%M\n")
print(sec_light_plot)
save.plot(paste0(PLOT_PATH, "sec_light_plot.png"), sec_light_plot)


## zoom in --> 잠시 보류 
## 
# zoom_in_sec_plot_dt = sec_light_dt[(dts > "2015-09-01 12:48:10" ) & (dts < "2015-09-01 12:48:25")]
# sec_light_plot <- ggplot(zoom_in_sec_plot_dt, aes(x = dts, y=light/1000/1000)) +
#   geom_line() +
#   geom_point() +
#   ylab("Power consumption of light (kW/sec)\n") +
#   xlab("")+
#   ylim(0, 0.8) +
#   ggtitle("sec - light \n") +
# scale_x_datetime(date_breaks = "secs", date_minor_breaks = "secs",
#                  date_labels = "%H:%M:%S\n")
# print(sec_light_plot)
# save.plot(paste0(PLOT_PATH, "sec_light_plot.png"), sec_light_plot)



# 3.3. sec - com
sec_com_dt = load.com.sec.tidy.data(T_DATE, 'marg', 'com')
sec_com_dt$dts = as.POSIXct(sec_com_dt$dts, tz='rok')

# sec_plot_dt = sec_com_dt
sec_plot_dt = sec_com_dt[(dts > FIRST_CUT) & (dts < SECOND_CUT)]

sec_com_plot <- ggplot(sec_plot_dt, aes(x = dts, y=com1/1000/1000)) +
  # geom_bar(stat = "identity") +
  geom_line() +
  geom_point() +
  ylab("Power consumption of com (kW/sec)\n") +
  xlab("")+
  ylim(0.5, 1.3) +
  ggtitle("sec - com \n") +
  scale_x_datetime(date_breaks = "hours", date_minor_breaks = "30 mins",
                   date_labels = "%H:%M\n")
print(sec_com_plot)
save.plot(paste0(PLOT_PATH, "sec_com_plot.png"), sec_com_plot)



# 
# {
#   START_DATE = as.Date("2015-09-29")
#   END_DATE = as.Date("2016-12-06")
#   
#   TARGET_DATE = START_DATE
#   
#   repeat {
#     
#     sec_light_dt = load.light.sec.tidy.data(TARGET_DATE, 'marg')
#     sec_light_dt$dts = as.POSIXct(sec_light_dt$dts)
#     
#     sec_plot_dt = sec_light_dt
#     # sec_plot_dt = sec_light_dt[(dts > "2015-09-01 12:00") & (dts < "2015-09-01 14:00")]
#     
#     sec_light_plot <- ggplot(sec_plot_dt, aes(x = dts, y=light/1000/1000)) +
#       # geom_bar(stat = "identity") +
#       geom_line() +
#       geom_point() +
#       ylab("Power consumption of light (kW/sec)\n") +
#       xlab("")+
#       # ylim(0, 1.5) +
#       ggtitle("sec - light \n") +
#       scale_x_datetime(date_breaks = "hours", date_minor_breaks = "hours",
#                        date_labels = "%H:%M\n")
#     print(sec_light_plot)
#     save.plot(paste0(PLOT_PATH, "sec_light_plot", TARGET_DATE, ".png"), sec_light_plot)
#     
#     
#     ## Loop until the END_DATE 
#     if(TARGET_DATE == END_DATE){
#       break
#     } else {
#       TARGET_DATE = TARGET_DATE + 1
#     }
#   }
# }
# 


# 3.2. sec - light




















##
## Figure 4: An example of the waste investigation 
## 

PLOT_PATH = "../plots/"
Sys.setlocale("LC_ALL", "English")

## f1: light 
DATE_FROM = "2014-10-19 22:00"
DATE_TO = "2014-10-22 23:00"

dt = data.table(marg_dt)

plot_dt = dt[timestamp >= DATE_FROM & timestamp <= DATE_TO, 
             c("timestamp", "light"), with=F]

f1 <- ggplot(plot_dt, aes(x = timestamp, y=light*4)) +
  geom_line() +
  ylab("Power consumption of light (kW/15min)\n") +
  xlab("")+
  ggtitle("A result of waste investigation for light usage \n") +
  scale_x_datetime(date_breaks = "days", date_minor_breaks = "days",
                   date_labels = "%Y-%m-%d\n(%a)")

print(f1)

# save.plot(paste0(PLOT_PATH, "waste_investigation_light.png"), f1)

# tmp = rle(dt$light > 1/4)
# tmp2 = which(tmp$lengths > 96)
# 
# length(tmp2) # 150
# i = 150
# from = sum(tmp$lengths[1:i])
# dt$light[(from-100):(from+100)]
# dt$timestamp[(from-100):(from+100)]


## f2: computer 
DATE_FROM = "2014-10-06"
DATE_TO = "2014-10-13"

dt = fread("../data/raw/marg_hours.csv")
  
plot_dt = dt[timestamp >= DATE_FROM & timestamp <= DATE_TO, 
             c("timestamp", "computer"), with=F]

plot_dt$timestamp = as.POSIXct(plot_dt$timestamp)

f2 <- ggplot(plot_dt, aes(x = timestamp, y=computer)) +
  geom_line() +
  ylab("Power consumption of computer (kW/h)\n") +
  xlab("")+
  ylim(c(0, 2.3))+
  ggtitle("A result of waste investigation for computer usage \n") + 
  scale_x_datetime(date_breaks = "days", date_minor_breaks = "days",
                   date_labels = "%Y-%m-%d\n(%a)")

print(f2)

# save.plot(paste0(PLOT_PATH, "waste_investigation_computer.png"), f2)




## Figure 8. Changes in power consumption during a day. 
## The light usage on non-working day (left) shows that the peak, mean, and base are decreased dramatically. 
## The computer usage on a working day (right) shows that 
## the peak is maintained while the base is decreased.  
## [Make it recognizable for black/white prints?]


# Data Table #
# marg_dt_check <- as.data.table(marg_dt_check)
marg_dt_new=marg_dt[,.(timestamp,computer,light,workingday)]
marg_dt_new$date=as.Date(substr(marg_dt_new$timestamp,1,10))  
marg_dt_new$time=substr(marg_dt_new$timestamp,12,16)
marg_dt_new$month=substr(marg_dt_new$date,1,7)


marg_computer=marg_dt_new[((date>="2014-10-01" & date < "2014-11-01") | (date>="2015-02-01" & date < "2015-03-01")) & workingday==TRUE]
marg_computer=marg_computer[,.(mean_com=mean(computer),sd_com=sd(computer),computer),by=.(time,month)]
marg_computer[month == "2014-10", month := "2014-10 (before)"] ; marg_computer[month == "2015-02", month := "2015-2 (after)"]
marg_light=marg_dt_new[((date>="2014-10-01" & date < "2014-11-01") | (date>="2015-02-01" & date < "2015-03-01")) & workingday==FALSE]
marg_light=marg_light[,.(mean_light=mean(light),sd_light=sd(light),light),by=.(time,month)]
marg_light[month == "2014-10", month := "2014-10 (before)"] ; marg_light[month == "2015-02", month := "2015-2 (after)"]

# marg_computer[1:100,]
# marg_light[1:100,]

# timelabel for the order of x axis #
timelabel2 = c("07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00","16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00","00:00","01:00","02:00","03:00","04:00","05:00","06:00")
# timelabel <- factor(timelabel, levels = timelabel)

timelabel <- unique(marg_dt_new[,time])

# function for sd ribbon #
ymin = function(x){
  ymin=mean(x)-sd(x)
}
ymax = function(x){
  ymax=mean(x)+sd(x)
}

# peak and base matrix #
com_Feb_basepeak = marg_dt_new[month=="2015-2 (after)",.(peak=quantile(computer,0.9),base=quantile(computer,0.1)),by=.(date)]
com_Oct_basepeak = marg_dt_new[month=="2014-10 (before)",.(peak=quantile(computer,0.9),base=quantile(computer,0.1)),by=.(date)]
light_Feb_basepeak = marg_dt_new[month=="2015-2 (after)",.(peak=quantile(light,0.9),base=quantile(light,0.1)),by=.(date)]
light_Oct_basepeak = marg_dt_new[month=="2014-10 (before)",.(peak=quantile(light,0.9),base=quantile(light,0.1)),by=.(date)]



ynum=round(matrix(as.numeric(c(lapply(com_Feb_basepeak,mean)[c(2,3)],lapply(com_Oct_basepeak,mean)[c(2,3)],
                               lapply(light_Feb_basepeak,mean)[c(2,3)],lapply(light_Oct_basepeak,mean)[c(2,3)])),nrow=4,byrow=FALSE),digits=2)


# I cannot change legend labels even with 'scale_fill_manual' and 'scale_fill_discrete' #
### Plot for marg_computer ###
p=ggplot() +
  geom_line(data=marg_computer,aes(x=time,y=mean_com,group=factor(month),color=factor(month))) +
  stat_summary(data=marg_computer[which(marg_computer$month=="2015-2 (after)"),],aes(x=time, y=computer), fun.ymin = ymin ,fun.ymax = ymax, fill="lightseagreen", geom="ribbon",group=1,alpha=0.2) +
  stat_summary(data=marg_computer[which(marg_computer$month=="2014-10 (before)"),],aes(x=time, y=computer), fun.ymin = ymin ,fun.ymax = ymax, fill="lightcoral", geom="ribbon",group=1,alpha=0.2) +
  scale_x_discrete(limits=timelabel,breaks=timelabel2,labels=timelabel2) +
  # scale_y_continuous(breaks=c(ynum[,1],0.15,0.65)) +
  coord_cartesian(ylim = c(0.15, 0.65)) +
  ggtitle("computer power consumption in 2015-02(0.23,0.43) & 2014-10(0.31,0.46) (workingday) \n") +
  ylab("Power consumption (Watt/15min)\n") +
  # geom_hline(aes(yintercept=ynum[,1]), colour=c(rep("grey40",2),rep("grey20",2)), linetype="dashed") +
  theme(axis.text.x = element_text(hjust = 0.5,size=12),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size=20),
        legend.text = element_text(size=20, hjust = 1),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key = element_rect(colour="white"),
        legend.key.size = unit(20,"cm"),
        legend.margin = unit(0, "cm"),
        axis.text = element_text(size=20),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=20, vjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(colour = "black",fill="white")) +
  guides(fill = guide_legend(keywidth = 1, keyheight = 1),
         linetype = guide_legend(keywidth = 4, keyheight = 1),
         colour = guide_legend(keywidth = 3, keyheight = 1))

p = set.default.theme(p)
save.plot(paste0("../plots/computer_trend_in_a_day.png"), p)


# ggsave("computer power consumption in 2015Feb&2014Oct (workingday).png",width = 10, height = 8, path="/home/hanjy2/Energy_saving/")


# Plot for marg_lifht #
q=ggplot() +
  geom_line(data=marg_light,aes(x=time,y=mean_light,group=factor(month),color=factor(month))) +
  stat_summary(data=marg_light[which(marg_light$month=="2015-2 (after)"),],aes(x=time, y=light), fun.ymin = ymin ,fun.ymax = ymax, fill="lightseagreen", geom="ribbon",group=1,alpha=0.2) +
  stat_summary(data=marg_light[which(marg_light$month=="2014-10 (before)"),],aes(x=time, y=light), fun.ymin = ymin ,fun.ymax = ymax, fill="lightcoral", geom="ribbon",group=1,alpha=0.2) +
  scale_x_discrete(limits=timelabel,breaks=timelabel2,labels=timelabel2) +
  # scale_y_continuous(breaks=c(ynum[,2],0,0.35)) +
  coord_cartesian(ylim = c(0, 0.35)) +
  ggtitle("light power consumption in 2015-02(0.03,0.25) & 2014-10(0.04,0.26) (non-workingday) \n") +
  ylab("Power consumption (Watt/15min)\n") +
  # geom_hline(aes(yintercept=ynum[,2]), colour=c(rep("grey40",2),rep("grey20",2)), linetype="dashed") +
  theme(axis.text.x = element_text(hjust = 0.5,size=12),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size=20),
        legend.text = element_text(size=20, hjust = 1),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key = element_rect(colour="white"),
        legend.key.size = unit(20,"cm"),
        legend.margin = unit(0, "cm"),
        axis.text = element_text(size=20),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=20, vjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(colour = "black",fill="white")) +
  guides(fill = guide_legend(keywidth = 1, keyheight = 1),
         linetype = guide_legend(keywidth = 4, keyheight = 1),
         colour = guide_legend(keywidth = 3, keyheight = 1))

q = set.default.theme(q)
save.plot(paste0("../plots/light_trend_in_a_day.png"), q)


# ggsave("light power consumption in 2015Feb&2014Oct (non-workingday).png",width = 10, height = 8,path="/home/hanjy2/Energy_saving/")







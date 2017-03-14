library(data.table)
library(ggplot2)

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
  ylab("Power consumption of light (kW/h)\n") +
  xlab("")+
  ggtitle("A result of waste investigation for light usage \n") +
  scale_x_datetime(date_breaks = "days", date_minor_breaks = "days",
                   date_labels = "%Y-%m-%d\n(%a)")

save.plot(paste0(PLOT_PATH, "waste_investigation_light.png"), f1)




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

save.plot(paste0(PLOT_PATH, "waste_investigation_computer.png"), f2)

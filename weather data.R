source("../Encored-Data-Analysis/getSNUdata.R")
library(data.table)
library(ggplot2)

##################################################
## Day table
##################################################
day_start = "2014-09-01"
day_end   = "2016-01-09"

marg_table_day = getSNUData.feeder.day("marg", day_start, day_end)
hcc_table_day = getSNUData.feeder.day( "hcc", day_start, day_end)
ux_table_day = getSNUData.feeder.day(  "ux", day_start, day_end)

elec_dt = data.table(marg_table_day)
elec_dt$timestamp = as.Date(elec_dt$timestamp)
str(elec_dt)

########################
## Weather table
########################
weather_dt = fread("../rawData/Suwon_weather.csv")
weather_dt$date_index = as.Date(weather_dt$date_index)
str(weather_dt)


setkey(elec_dt, timestamp)
setkey(weather_dt, date_string)
elec_with_weather <- elec_dt[weather_dt, nomatch=0]

ggplot(data=elec_with_weather, aes(x=timestamp))+
        geom_line(aes(y=hvac, color="1. HVAC")) +
        geom_line(aes(y=avg_temp, color="2. avg_temp")) +
        geom_line(aes(y=max_temp, color="3. max_temp")) +
        geom_line(aes(y=min_temp, color="4. min_temp")) +
        # geom_line(aes(y=avg_cloud, color="avg_cloud")) +
        # geom_line(aes(y=precipitation, color="precipitation")) +
        scale_x_date("Timestamp", labels = date_format("%y/%m/%d"), breaks = date_breaks("week")) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        ggtitle("HVAC & Temperature")

ggplot(data=elec_with_weather, aes(x=timestamp))+
        geom_line(aes(y=light, color="1. light")) +
#         geom_line(aes(y=avg_temp, color="avg_temp")) +
#         geom_line(aes(y=max_temp, color="max_temp")) +
#         geom_line(aes(y=min_temp, color="min_temp")) +
        geom_line(aes(y=avg_cloud, color="2. avg_cloud")) +
        geom_line(aes(y=precipitation, color="3. precipitation")) +
        scale_x_date("Timestamp", labels = date_format("%y/%m/%d"), breaks = date_breaks("week")) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        ggtitle("Light & cloud, rain")

        

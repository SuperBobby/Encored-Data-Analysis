library(data.table)
library(lubridate)
library(ggplot2)

## Temp table ##
## http://sts.kma.go.kr/jsp/home/contents/climateData/obs/obsValSearch.do?MNU=MNU
## http://web.kma.go.kr/notify/press/kma_list.jsp?bid=press&mode=view&num=1192880

Suwon_temp = fread("data/Suwon_temp.csv")

Suwon_temp[, ':='(date  = substr(as.character(timestamp), 1, 10),
                 year  = substr(as.character(timestamp), 1, 4),
                 month = substr(as.character(timestamp), 6, 7),
                 day   = substr(as.character(timestamp), 9, 10),
                 hour  = substr(as.character(timestamp), 12, 16))]

temp_table_day = Suwon_temp[, .(timestamp = as.Date(paste(year, month, day, sep="-")),
                               max = max(temp, na.rm = T),
                               min = min(temp, na.rm = T),
                               avg = mean(temp, na.rm = T)), by=.(year, month, day)]

summary(temp_table_day)
setkey(temp_table_day, timestamp)


## Day table ##
day_start = "2014-09-01"
day_end   = "2016-03-04"

marg_table_day = getSNUData.feeder.day("marg", day_start, day_end)
hcc_table_day = getSNUData.feeder.day( "hcc", day_start, day_end)
ux_table_day = getSNUData.feeder.day(  "ux", day_start, day_end)

marg_table_day = data.table(marg_table_day)
hcc_table_day = data.table(hcc_table_day)
ux_table_day = data.table(ux_table_day)

marg_table_day$timestamp = as.Date(marg_table_day$timestamp)
hcc_table_day$timestamp = as.Date(hcc_table_day$timestamp)
ux_table_day$timestamp = as.Date(ux_table_day$timestamp)

setkey(marg_table_day, timestamp)
setkey(hcc_table_day, timestamp)
setkey(ux_table_day, timestamp)

marg_day_temp = marg_table_day[temp_table_day]
hcc_day_temp = hcc_table_day[temp_table_day]
ux_day_temp = ux_table_day[temp_table_day]



marg_winter_2014 = marg_day_temp[timestamp > "2014-11-30" & timestamp < "2015-03-01"]
marg_winter_2015 = marg_day_temp[timestamp > "2015-11-30" & timestamp < "2016-03-01"]



# x11()
ggplot(marg_winter_2015, aes(x=timestamp)) +
        scale_x_date("Timestamp", labels = date_format("%Y-%m-%d"), breaks = date_breaks("month")) +
        
        geom_point(aes(y= hvac, color="hvac")) +
        geom_text(aes(y= hvac, label=round(hvac,2), vjust=-1)) + 
        geom_line(aes(y = hvac, color = "hvac")) + 

        geom_line(aes(y = avg, color = "min")) +
        geom_text(aes(y = min, label = min, vjust = -1))


Suwon_temp[, .(avg_temp = mean(temp, na.rm =T), 
               var_temp = var(temp, na.rm =T)), by=.(year, month)]

marg_day_temp[, .(avg_hvac = mean(hvac, na.rm =T), 
                      var_hvac = var(hvac, na.rm =T)), by=.(year, month)]
                 


marg_day_temp[, .(avg_com  = mean(computer, na.rm =T),
                  avg_light = mean(light, na.rm =T),
                  avg_hvac = mean(hvac, na.rm =T),
                  avg_etc = mean(etc, na.rm =T)), by=.(year, month)]


# 냉난방 가동일 
sum(marg_winter_2014$hvac > 1) # 68일
sum(marg_winter_2015$hvac > 1) # 59일 

dt = data.table(marg_defalut_table_hours)

dt[timestamp > "2014-12-01" & timestamp < "2015-03-01", 
   .(hvac_on_hours = sum(hvac > 0.05)), by=.(year(timestamp),month(timestamp))]

dt[timestamp > "2015-12-01" & timestamp < "2016-03-01", 
   .(hvac_on_hours = sum(hvac > 0.05)), by=.(year(timestamp),month(timestamp))]


# 24시간 난방 - 2014 
tmp = dt[timestamp > "2014-12-01" & timestamp < "2015-03-01"]
hvac_OnOff <- ifelse(tmp$hvac > 0.05, "on", "off")
sum(rle(hvac_OnOff)$lengths > 48)

# 24시간 난방 - 2015 
tmp = dt[timestamp > "2015-12-01" & timestamp < "2016-03-01"]
hvac_OnOff <- ifelse(tmp$hvac > 0.05, "on", "off")
sum(rle(hvac_OnOff)$lengths > 48)


get.on24hours <- function(hvac, hours){
        hvac_OnOff <- ifelse(hvac > 0.06, "on", "off")
        sum(rle(hvac_OnOff)$lengths > hours)
}        

dt[timestamp > "2014-12-01" & timestamp < "2015-03-01", 
   .(on24hours = get.on24hours(hvac, 24),
     on48hours = get.on24hours(hvac, 48),
     on72hours = get.on24hours(hvac, 72),
     on96hours = get.on24hours(hvac, 96),
     on120hours = get.on24hours(hvac, 120),
     on144hours = get.on24hours(hvac, 144)), by=.(year(timestamp),month(timestamp))]

dt[timestamp > "2015-12-01" & timestamp < "2016-03-01", 
   .(on24hours = get.on24hours(hvac, 24),
     on48hours = get.on24hours(hvac, 48),
     on72hours = get.on24hours(hvac, 72),
     on96hours = get.on24hours(hvac, 96),
     on120hours = get.on24hours(hvac, 120),
     on144hours = get.on24hours(hvac, 144)), by=.(year(timestamp),month(timestamp))]




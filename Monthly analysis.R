##################################################
## Monthly ananlysis
##################################################
source("Encored-Data-Analysis/getSNUdata.R")
library(lubridate)
library(data.table)
library(ggplot2)

day_start = "2014-09-01"
day_end   = "2016-03-01"

marg_table_day = getSNUData.feeder.day("marg", day_start, day_end)
hcc_table_day = getSNUData.feeder.day( "hcc", day_start, day_end)
ux_table_day = getSNUData.feeder.day(  "ux", day_start, day_end)

marg_table_day = data.table(marg_table_day)
hcc_table_day = data.table(hcc_table_day)
ux_table_day = data.table(ux_table_day)

getYearMonthLabel <- function(timestamp){
        result = paste(year(timestamp), month(timestamp), "01 00:00:00", sep="-")
        return(as.POSIXct(result))
}


marg_table_day[, ':='(Month = getYearMonthLabel(timestamp))]
hcc_table_day[, ':='(Month = getYearMonthLabel(timestamp))]
ux_table_day[, ':='(Month = getYearMonthLabel(timestamp))]

plot.Monthly <- function(table_day, lab){
        
        title_text = paste(toupper(lab), "Avg daily usage")
        table_day$weekday = ifelse(table_day$weekday, "weekday", "weekend")
        
        print(head(table_day))
        
        monthly_table = table_day[, .(computer = mean(computer),
                                      light    = mean(light),
                                      hvac     = mean(hvac),
                                      total    = mean(total)), by=.(Month, weekday)]
        
        monthly_table$Month = as.Date(monthly_table$Month)
        
        
        plotting <- ggplot(monthly_table, aes(x=Month)) +
                theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
                scale_x_date(date_labels = "%Y-%b", date_breaks="month") +
                ylab("Avg Daily Usage per Month (kW/h)") +
                
                geom_point(aes(y=computer, color="computer")) +
                geom_text(aes(y= computer, label=round(computer,1), vjust=-1)) + 
                geom_line(aes(y=computer, color="computer")) +
                
                geom_point(aes(y=light, color="light")) +
                geom_text(aes(y= light, label=round(light,1), vjust=-1)) + 
                geom_line(aes(y=light, color="light")) +
                
                geom_point(aes(y=hvac, color="hvac")) +
                geom_text(aes(y= hvac, label=round(hvac,1), vjust=-1)) + 
                geom_line(aes(y=hvac, color="hvac")) +
                
                geom_point(aes(y=total, color="total")) +
                geom_text(aes(y= total, label=round(total,1), vjust=-1)) + 
                geom_line(aes(y=total, color="total")) +
                
                facet_grid(. ~ weekday) + 
                ggtitle(title_text)
        
        #         if(toupper(lab) == "MARG") {
        #                 plotting <- plotting + geom_point(aes(y=hvac, color="hvac")) +
        #                         geom_text(aes(y= hvac, label=round(hvac,1), vjust=-1)) + 
        #                         geom_line(aes(y=hvac, color="hvac"))
        #         }
        
        # x11()
        print(plotting)
        return(monthly_table)
}

plot.Monthly(marg_table_day, "marg")
plot.Monthly(hcc_table_day, "hcc")
plot.Monthly(ux_table_day, "ux")




ux_tmp = getSNUData.feeder.day(  "ux", "2016-01-01", "2016-02-29")
ux_tmp <- data.table(ux_tmp)
ux_tmp[timestamp < as.POSIXct("2016-1-26") & weekday == T, lapply(.SD, mean), .SDcols=c("computer", "light", "total")]
ux_tmp[timestamp > as.POSIXct("2016-1-26") & weekday == T, lapply(.SD, mean), .SDcols=c("computer", "light", "total")]

ux_tmp[timestamp < as.POSIXct("2016-1-26") & weekday == F, lapply(.SD, mean), .SDcols=c("computer", "light", "total")]
ux_tmp[timestamp > as.POSIXct("2016-1-26") & weekday == F, lapply(.SD, mean), .SDcols=c("computer", "light", "total")]



ux_tmp1 = getSNUData.feeder.day(  "ux", "2016-01-01", "2016-01-26")
ux_tmp2 = getSNUData.feeder.day(  "ux", "2016-01-26", "2016-02-29")

ux_tmp1 <- data.table(ux_tmp1)
ux_tmp2 <- data.table(ux_tmp2)

ux_tmp1[]
ux_tmp2[, lapply(.SD, mean), .SDcols=c("computer", "light", "total")]



# .SDcols=c("computer", "light")

DT[, lapply(.SD, sum), .SDcols=2:4]

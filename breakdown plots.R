library(plyr)
library(gridExtra)
library(ggplot2)
library(scales)
library(reshape2)

str(marg_defalut_table_15min)
zero_total_indexes = which(marg_defalut_table_15min$total == 0)
marg_defalut_table_15min[zero_total_indexes,]

# 요일별 분석

lastYear_winter_marg_15min <- loadSNUData(marg_defalut_table_15min, "marg", "2014-12-01", "2015-03-01", verbose = T)
lastYear_marg <- ggplot(aes(x=timestamp, y=total), data=lastYear_winter_marg_15min) +
        geom_point(aes(color=day)) + 
        facet_grid(weekday ~ .) +
        scale_x_datetime(labels = date_format("%m/%d"), breaks = date_breaks("week")) + 
        theme(legend.position = "left") + 
        ggtitle("lastYear_winter_marg_15min")

thisYear_winter_marg_15min <- loadSNUData(marg_defalut_table_15min, "marg", "2015-12-01", "2016-01-05", verbose = T)
thisYear_marg <- ggplot(aes(x=timestamp, y=total), data=thisYear_winter_marg_15min) +
        geom_point(aes(color=day)) + 
        facet_grid(weekday ~ .) +
        scale_x_datetime(labels = date_format("%m/%d"), breaks = date_breaks("week")) + 
        theme(legend.position = "left") +
        ggtitle("thisYear_winter_marg_15min")

grid.arrange(lastYear_marg, thisYear_marg)




lastYear_winter_hcc_15min <- loadSNUData(hcc_defalut_table_15min, "hcc", "2014-12-01", "2015-03-01", verbose = T)
lastYear_hcc <- ggplot(aes(x=timestamp, y=total), data=lastYear_winter_hcc_15min) +
        geom_point(aes(color=day)) + 
        facet_grid(weekday ~ .) +
        scale_x_datetime(labels = date_format("%m/%d"), breaks = date_breaks("week")) + 
        theme(legend.position = "left") + 
        ggtitle("lastYear_winter_hcc_15min")

thisYear_winter_hcc_15min <- loadSNUData(hcc_defalut_table_15min, "hcc", "2015-12-01", "2016-01-05", verbose = T)
thisYear_hcc <- ggplot(aes(x=timestamp, y=total), data=thisYear_winter_hcc_15min) +
        geom_point(aes(color=day)) + 
        facet_grid(weekday ~ .) +
        scale_x_datetime(labels = date_format("%m/%d"), breaks = date_breaks("week")) + 
        theme(legend.position = "left") +
        ggtitle("thisYear_winter_hcc_15min")

grid.arrange(lastYear_hcc, thisYear_hcc)




lastYear_winter_ux_15min <- loadSNUData(ux_defalut_table_15min, "ux", "2014-12-01", "2015-03-01", verbose = T)
lastYear_ux <- ggplot(aes(x=timestamp, y=total), data=lastYear_winter_ux_15min) +
        geom_point(aes(color=day)) + 
        facet_grid(weekday ~ .) +
        scale_x_datetime(labels = date_format("%m/%d"), breaks = date_breaks("week")) + 
        theme(legend.position = "left") + 
        ggtitle("lastYear_winter_ux_15min")

thisYear_winter_ux_15min <- loadSNUData(ux_defalut_table_15min, "ux", "2015-12-01", "2016-01-05", verbose = T)
thisYear_ux <- ggplot(aes(x=timestamp, y=total), data=thisYear_winter_ux_15min) +
        geom_point(aes(color=day)) + 
        facet_grid(weekday ~ .) +
        scale_x_datetime(labels = date_format("%m/%d"), breaks = date_breaks("week")) + 
        theme(legend.position = "left") +
        ggtitle("thisYear_winter_ux_15min")

grid.arrange(lastYear_ux, thisYear_ux)




## BreakDown
# MARG
MARG_15min <- ggplot(aes(x=timestamp), data=marg_defalut_table_15min) +
        geom_point(aes(y=computer, color="com")) + 
        geom_point(aes(y=light, color="light")) + 
        geom_point(aes(y=hvac, color="hvac")) + 
        geom_point(aes(y=etc, color="etc")) + 
        #         geom_point(aes(y=total, color="total")) + 
        facet_grid(weekday ~ .) +
        scale_x_datetime(labels = date_format("%m/%d"), breaks = date_breaks("week")) + 
        theme(legend.position = "left", axis.text.x = element_text(angle = 90, hjust = 1)) +
        ggtitle("MARG_15min")

MARG_hours <- ggplot(aes(x=timestamp), data=marg_defalut_table_hours) +
        geom_point(aes(y=computer, color="com")) + 
        geom_point(aes(y=light, color="light")) + 
        geom_point(aes(y=hvac, color="hvac")) + 
        geom_point(aes(y=etc, color="etc")) + 
        #         geom_point(aes(y=total, color="total")) + 
        facet_grid(weekday ~ .) +
        scale_x_datetime(labels = date_format("%m/%d"), breaks = date_breaks("week")) + 
        theme(legend.position = "left", axis.text.x = element_text(angle = 90, hjust = 1)) +
        ggtitle("MARG_hours")


# HCC
HCC_hours <- ggplot(aes(x=timestamp), data=hcc_defalut_table_hours) +
        geom_point(aes(y=computer, color="com")) + 
        geom_point(aes(y=light, color="light")) + 
        geom_point(aes(y=hvac, color="hvac")) + 
        geom_point(aes(y=etc, color="etc")) + 
        #         geom_point(aes(y=total, color="total")) + 
        facet_grid(weekday ~ .) +
        scale_x_datetime(labels = date_format("%m/%d"), breaks = date_breaks("week")) + 
        theme(legend.position = "left", axis.text.x = element_text(angle = 90, hjust = 1)) +
        ggtitle("HCC_hours")


HCC_15min <- ggplot(aes(x=timestamp), data=hcc_defalut_table_15min) +
        geom_point(aes(y=computer, color="com")) + 
        geom_point(aes(y=light, color="light")) + 
        geom_point(aes(y=hvac, color="hvac")) + 
        geom_point(aes(y=etc, color="etc")) + 
        #         geom_point(aes(y=total, color="total")) + 
        facet_grid(weekday ~ .) +
        scale_x_datetime(labels = date_format("%m/%d"), breaks = date_breaks("week")) + 
        theme(legend.position = "left", axis.text.x = element_text(angle = 90, hjust = 1)) +
        ggtitle("HCC_15min")


# UX
UX_hours <- ggplot(aes(x=timestamp), data=ux_defalut_table_hours) +
        geom_point(aes(y=computer, color="com")) + 
        geom_point(aes(y=light, color="light")) + 
        geom_point(aes(y=hvac, color="hvac")) + 
        geom_point(aes(y=etc, color="etc")) + 
        #         geom_point(aes(y=total, color="total")) + 
        facet_grid(weekday ~ .) +
        scale_x_datetime(labels = date_format("%m/%d"), breaks = date_breaks("week")) + 
        theme(legend.position = "left", axis.text.x = element_text(angle = 90, hjust = 1)) +
        ggtitle("UX_hours")

UX_15min <- ggplot(aes(x=timestamp), data=ux_defalut_table_15min) +
        geom_point(aes(y=computer, color="com")) + 
        geom_point(aes(y=light, color="light")) + 
        geom_point(aes(y=hvac, color="hvac")) + 
        geom_point(aes(y=etc, color="etc")) + 
        #         geom_point(aes(y=total, color="total")) + 
        facet_grid(weekday ~ .) +
        scale_x_datetime(labels = date_format("%m/%d"), breaks = date_breaks("week")) + 
        theme(legend.position = "left", axis.text.x = element_text(angle = 90, hjust = 1)) +
        ggtitle("UX_15min")


grid.arrange(MARG_hours, MARG_15min)
grid.arrange(HCC_hours, HCC_15min)
grid.arrange(UX_hours, UX_15min)


grid.arrange(MARG_15min,HCC_15min,UX_15min)
grid.arrange(MARG_hours,HCC_hours,UX_hours)

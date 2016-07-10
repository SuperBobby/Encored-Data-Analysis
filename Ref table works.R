source("Encored-Data-Analysis/getSNUdata.R")
library(lubridate)
library(data.table)
library(ggplot2)

{
# lab = "marg"
# week = "weekend"
# output_filename = paste0("data/Ref_", lab, "_", week, ".csv")
# 
# table_from_clipboard = read.table("clipboard", sep="\t", header = T)
# 
# print(table_from_clipboard)
# 
# write.csv(table_from_clipboard, output_filename, row.names = F)
}

Ref_marg_weekday = read.csv("data/Ref_marg_weekday.csv")
Ref_marg_weekend = read.csv("data/Ref_marg_weekend.csv")
Ref_hcc_weekday = read.csv("data/Ref_hcc_weekday.csv")
Ref_hcc_weekend = read.csv("data/Ref_hcc_weekend.csv")
Ref_ux_weekday = read.csv("data/Ref_ux_weekday.csv")
Ref_ux_weekend = read.csv("data/Ref_ux_weekend.csv")

day_start = "2014-09-01"
day_end   = "2016-07-10"

marg_day_df = getSNUData.feeder.day("marg", day_start, day_end)
hcc_day_df = getSNUData.feeder.day( "hcc", day_start, day_end)
ux_day_df = getSNUData.feeder.day(  "ux", day_start, day_end)


# Reference tables 
Ref_marg_weekday$date = as.Date(Ref_marg_weekday$date, tz="rok")
Ref_marg_weekend$date = as.Date(Ref_marg_weekend$date, tz="rok") + 5
Ref_hcc_weekday$date = as.Date(Ref_hcc_weekday$date, tz="rok")
Ref_hcc_weekend$date = as.Date(Ref_hcc_weekend$date, tz="rok") + 5
Ref_ux_weekday$date = as.Date(Ref_ux_weekday$date, tz="rok")
Ref_ux_weekend$date = as.Date(Ref_ux_weekend$date, tz="rok") + 5

# Add date column to each lab's day tables
marg_day_df$date = as.Date(marg_day_df$timestamp, tz='rok')
hcc_day_df$date = as.Date(hcc_day_df$timestamp, tz='rok')
ux_day_df$date = as.Date(ux_day_df$timestamp, tz='rok')

tmp = merge(marg_day_df, Ref_marg_weekday, all.x = T)


View(tmp)

marg_table_day$date = as.Date(cut(marg_table_day$timestamp, breaks = "week", start.on.monday = T))
hcc_table_day
ux_table_day



        

View(marg_table_day)

source("Encored-Data-Analysis/getSNUdata.R")
library(lubridate)
library(data.table)
library(ggplot2)
library(zoo)
library(gridExtra)

{
# lab = "ux"
# week = "weekend"
# output_filename = paste0("data/Ref_", lab, "_", week, ".csv")
# table_from_clipboard = read.table("clipboard", sep="\t", header = T)
# print(table_from_clipboard)
# write.csv(table_from_clipboard, output_filename, row.names = F)
}

Ref_marg_weekday = read.csv("data/Ref_marg_weekday.csv")
Ref_marg_weekend = read.csv("data/Ref_marg_weekend.csv")
Ref_hcc_weekday = read.csv("data/Ref_hcc_weekday.csv")
Ref_hcc_weekend = read.csv("data/Ref_hcc_weekend.csv")
Ref_ux_weekday = read.csv("data/Ref_ux_weekday.csv")
Ref_ux_weekend = read.csv("data/Ref_ux_weekend.csv")

# day_start = "2014-09-01"
# day_end   = "2016-07-11"

# marg_day_df = getSNUData.feeder.day("marg", day_start, day_end)
# hcc_day_df = getSNUData.feeder.day( "hcc", day_start, day_end)
# ux_day_df = getSNUData.feeder.day(  "ux", day_start, day_end)


# Reference tables 
Ref_marg_weekday$date = as.Date(Ref_marg_weekday$date, tz="rok")
Ref_marg_weekend$date = as.Date(Ref_marg_weekend$date, tz="rok") + 5

Ref_hcc_weekday$date = as.Date(Ref_hcc_weekday$date, tz="rok")
Ref_hcc_weekend$date = as.Date(Ref_hcc_weekend$date, tz="rok") + 5

Ref_ux_weekday$date = as.Date(Ref_ux_weekday$date, tz="rok")
Ref_ux_weekend$date = as.Date(Ref_ux_weekend$date, tz="rok") + 5

Ref_marg = rbind(Ref_marg_weekday, Ref_marg_weekend)
Ref_hcc = rbind(Ref_hcc_weekday, Ref_hcc_weekend)
Ref_ux = rbind(Ref_ux_weekday, Ref_ux_weekend)


# Add date column to each lab's day tables
marg_day_df$date = as.Date(marg_day_df$timestamp, tz='rok')
hcc_day_df$date = as.Date(hcc_day_df$timestamp, tz='rok')
ux_day_df$date = as.Date(ux_day_df$timestamp, tz='rok')

# Naming 
names(Ref_marg) <- c("date", "com_ref", "light_ref", "hvac_ref", "total_ref")
names(Ref_hcc) <- c("date", "com_ref", "light_ref", "total_ref")
names(Ref_ux) <- c("date", "com_ref", "light_ref", "total_ref")

# fill the NA values using previous ref value 
# output files are the final tidy dables

fill.na.refs <- function(df1, df2){
                
        merged_df = merge(df1, df2, all.x = T)
        
        refs = names(df2)[-1]

        for(i in 1:length(refs)){
                merged_df[1, refs[i]] = 0
                merged_df[refs[i]] = na.locf(merged_df[refs[i]])
                merged_df[merged_df[refs[i]] == 0, refs[i]] = NA
        }
        
        return(data.table(merged_df))
}

marg_day_dt = fill.na.refs(marg_day_df, Ref_marg)
hcc_day_dt = fill.na.refs(hcc_day_df, Ref_hcc)
ux_day_dt = fill.na.refs(ux_day_df, Ref_ux)

# marg_table_day$date = as.Date(cut(marg_table_day$timestamp, breaks = "week", start.on.monday = T))

# Cut the data table - use 2016 data only 
marg_day_dt = marg_day_dt[date > "2014-12-31"]
hcc_day_dt = hcc_day_dt[date > "2014-12-31"]
ux_day_dt = ux_day_dt[date > "2014-12-31"]

head(marg_day_dt)
#          date  timestamp computer  light   hvac   etc   total day weekday com_ref light_ref hvac_ref
# 1: 2016-01-01 2016-01-01   45.496  9.178 34.058 8.204  96.962 Fri    TRUE      NA        NA       NA
# 2: 2016-01-02 2016-01-02   43.300  5.722 23.692 8.192  80.932 Sat   FALSE      NA        NA       NA
# 3: 2016-01-03 2016-01-03   38.911  7.824 47.292 8.076 102.129 Sun   FALSE      NA        NA       NA
# 4: 2016-01-04 2016-01-04   53.411 18.097 23.299 8.742 103.578 Mon    TRUE      NA        NA       NA
# 5: 2016-01-05 2016-01-05   56.529 18.306 26.229 9.089 110.182 Tue    TRUE      NA        NA       NA
# 6: 2016-01-06 2016-01-06   59.154 17.734 25.190 8.870 110.977 Wed    TRUE      NA        NA       NA
# total_ref
# 1:        NA
# 2:        NA
# 3:        NA
# 4:        NA
# 5:        NA
# 6:        NA

#          date  timestamp computer  light   hvac   etc   total day weekday com_ref light_ref hvac_ref
# 1: 2016-01-01 2016-01-01   45.496  9.178 34.058 8.204  96.962 Fri    TRUE      NA        NA       NA
# 2: 2016-01-02 2016-01-02   43.300  5.722 23.692 8.192  80.932 Sat   FALSE      NA        NA       NA
# 3: 2016-01-03 2016-01-03   38.911  7.824 47.292 8.076 102.129 Sun   FALSE      NA        NA       NA
# 4: 2016-01-04 2016-01-04   53.411 18.097 23.299 8.742 103.578 Mon    TRUE      NA        NA       NA
# 5: 2016-01-05 2016-01-05   56.529 18.306 26.229 9.089 110.182 Tue    TRUE      NA        NA       NA
# 6: 2016-01-06 2016-01-06   59.154 17.734 25.190 8.870 110.977 Wed    TRUE      NA        NA       NA
# total_ref
# 1:        NA
# 2:        NA
# 3:        NA
# 4:        NA
# 5:        NA
# 6:        NA

day_dt_list = list(marg_day_dt, hcc_day_dt, ux_day_dt)
labs = c("MARG", "HCC", "UX")

add.event.vline.ref <- function(plotbody) {
        plotbody = plotbody + 
                scale_x_date("Timestamp", labels = date_format("%y-%m-%d"), 
                             breaks = date_breaks("week")) +
                theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
                
                
                geom_vline(aes(xintercept = as.numeric(as.Date("2015-10-08"))),color="green4") +
                geom_vline(aes(xintercept = as.numeric(as.Date("2015-12-01"))),color="green4") +
                geom_vline(aes(xintercept = as.numeric(as.Date("2016-01-11"))),color="green4") +
                geom_vline(aes(xintercept = as.numeric(as.Date("2016-02-01"))),color="green4") +
                
                geom_vline(aes(xintercept = as.numeric(as.Date("2016-02-29"))),color="red") +
                
                geom_vline(aes(xintercept = as.numeric(as.Date("2016-05-16"))),color="green4") +
                geom_vline(aes(xintercept = as.numeric(as.Date("2016-06-13"))),color="green4")
        
        return(plotbody)
}


for(i in 1:3){
        
        target_dt = day_dt_list[[i]]
        lab = labs[i]
        
        print(lab)
        
        ylim_com = round(max(target_dt$computer)) * 1.05
        ylim_light = round(max(target_dt$light)) * 1.05
        
        ref_com_weekday <- ggplot(target_dt[weekday == T], aes(x = date)) + 
                
                geom_bar(aes(y=computer), fill="steelblue", stat = 'identity') + 
                geom_line(aes(y=com_ref), color="steelblue4", size=1) +
                ylab("Usage (kW/h per day)") + 
                ylim(c(0,ylim_com)) + 
                ggtitle(paste0(lab, " Computer Usage with Reference - WeekDay"))
        
        ref_com_weekend <- ggplot(target_dt[weekday == F], aes(x = date)) + 
                
                geom_bar(aes(y=computer), fill="steelblue", stat = 'identity') + 
                geom_line(aes(y=com_ref), color="steelblue4", size=1) +
                ylab("Usage (kW/h per day)") + 
                ylim(c(0,ylim_com)) + 
                ggtitle(paste0(lab, " Computer Usage with Reference - WeekEnd"))
        
        
        
        ref_light_weekday <- ggplot(target_dt[weekday == T], aes(x = date)) + 
                
                geom_bar(aes(y=light), fill="orange", stat = 'identity') + 
                geom_line(aes(y=light_ref), color="orange4", size=1) + 
                ylab("Usage (kW/h per day)") + 
                ylim(c(0,ylim_light)) + 
                ggtitle(paste0(lab, " Light Usage with Reference - WeekDay"))
        
        ref_light_weekend <- ggplot(target_dt[weekday == F], aes(x = date)) + 
                
                geom_bar(aes(y=light), fill="orange", stat = 'identity') + 
                geom_line(aes(y=light_ref), color="orange4", size=1) + 
                ylab("Usage (kW/h per day)") + 
                ylim(c(0,ylim_light)) + 
                ggtitle(paste0(lab, " Light Usage with Reference - WeekEnd"))
        
        
        ref_com_weekday = add.event.vline.ref(ref_com_weekday)
        ref_com_weekend = add.event.vline.ref(ref_com_weekend)
        ref_light_weekday = add.event.vline.ref(ref_light_weekday)
        ref_light_weekend = add.event.vline.ref(ref_light_weekend)
        
        if(lab == "MARG"){
                
                ylim_hvac = round(max(target_dt$hvac)) * 1.05
                
                ref_hvac_weekday <- ggplot(target_dt[weekday == T], aes(x = date)) + 
                        
                        geom_bar(aes(y=hvac), fill="mediumpurple", stat = 'identity') + 
                        geom_line(aes(y=hvac_ref), color="mediumpurple4", size=1) + 
                        ylab("Usage (kW/h per day)") + 
                        ylim(c(0,ylim_hvac)) + 
                        ggtitle(paste0(lab, " HVAC Usage with Reference - WeekDay"))
                
                ref_hvac_weekend <- ggplot(target_dt[weekday == F], aes(x = date)) + 
                        
                        geom_bar(aes(y=hvac), fill="mediumpurple", stat = 'identity') + 
                        geom_line(aes(y=hvac_ref), color="mediumpurple4", size=1) + 
                        ylab("Usage (kW/h per day)") + 
                        ylim(c(0,ylim_hvac)) + 
                        ggtitle(paste0(lab, " HVAC Usage with Reference - WeekEnd"))
                
                ref_hvac_weekday = add.event.vline.ref(ref_hvac_weekday)
                ref_hvac_weekend = add.event.vline.ref(ref_hvac_weekend)
                
                plots <- arrangeGrob(ref_com_weekday, ref_com_weekend, 
                                     ref_light_weekday, ref_light_weekend,
                                     ref_hvac_weekday, ref_hvac_weekend, ncol=2)
                
                ggsave(file = paste0("plots/",lab, " Usage with Reference.png"), width = 40, height = 30, dpi = 100, limitsize=F, plots)
                
        } else {
                
                plots <- arrangeGrob(ref_com_weekday, ref_com_weekend, 
                                     ref_light_weekday, ref_light_weekend, ncol=2)
                ggsave(file = paste0("plots/",lab, " Usage with Reference.png"), width = 40, height = 20, dpi = 100, limitsize=F, plots)
        }
}



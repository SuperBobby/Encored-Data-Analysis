###############################
#### daily bar & box plot function
#### 2016.01.12 Han.J.Y.
###############################
source("Encored-Data-Analysis/getSNUdata.R")

library(gridExtra)
library(ggplot2)
library(scales)

bar.box.plot <- function(data, lab, tg_fedr, srt, end) {
                
        revised_data = reviseSNUData(data, lab, srt, end)
        
        N_of_Days = nrow(revised_data)/24
        tmst      = revised_data[seq(1, nrow(revised_data), 24),"timestamp"]
        tmst      = rep(tmst, each=24)
        
        revised_data[,"timestamp"] = as.factor(tmst)
        
        title_text = paste(lab, tg_fedr, srt, "~", end)
        
        bar <- ggplot(data= revised_data, aes_string("timestamp", tg_fedr)) +
                stat_summary(aes(fill = factor(weekday)), fun.y=sum, geom="bar") +
                stat_summary(aes(label=round(..y..,2)), fun.y=sum, geom="text", size=3, vjust = -0.5) + 
                theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
                ggtitle(title_text)
        
        box <- ggplot(revised_data, aes_string("timestamp", tg_fedr)) + 
                geom_boxplot(aes(fill = factor(weekday))) +
                theme(axis.text.x = element_text(angle = 90, hjust = 1))

        grid.arrange(bar, box)
}

day_start = "2015-11-02"
day_end = "2016-02-14"
bar.box.plot(marg_defalut_table_hours, "marg", "total", day_start, day_end)
bar.box.plot(hcc_defalut_table_hours, "hcc", "total", day_start, day_end)
bar.box.plot(ux_defalut_table_hours, "ux", "total", day_start, day_end)


bar.box.plot(marg_defalut_table_hours, "marg", "computer", day_start, day_end)
bar.box.plot(hcc_defalut_table_hours, "hcc", "computer", day_start, day_end)
bar.box.plot(ux_defalut_table_hours, "ux", "computer", day_start, day_end)


bar.box.plot(marg_defalut_table_hours, "marg", "light", day_start, day_end)
bar.box.plot(hcc_defalut_table_hours, "hcc", "light", day_start, day_end)
bar.box.plot(ux_defalut_table_hours, "ux", "light", day_start, day_end)







bar.box.plot(marg_defalut_table_hours, "hcc", "computer", "2015-09-01", "2016-01-12")


bar.box.plot(marg_defalut_table_hours, "marg", "computer", "2015-11-01", day_end)
bar.box.plot(marg_defalut_table_hours, "marg", "hvac", "2015-11-01", day_end)

bar.box.plot(hcc_defalut_table_hours, "marg", "total", "2015-11-01", day_end)
bar.box.plot(ux_defalut_table_hours, "marg", "total", "2015-11-01", day_end)


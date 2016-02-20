make.quarter.label = function(input){
        h = floor(((input-1) * 15)/60)
        m = (input-1)*15 - h*60
        l = paste0(h,":",m)
        return(l)
}




heatmap.RealSense.data <- function(lab, data, date_cut, threshold) {
                      
        dt = data.table(data)
        names(dt) <- c("quarter_index", "freq", "sum_of_duration")
        
        dt = dt[sum_of_duration > threshold]
        dt = dt[quarter_index > date_cut]
        
        start_ts = as.numeric(dt$quarter_index[1])
        end_ts   = as.numeric(dt$quarter_index[nrow(dt)])
        
        full_quarter_index = data.table(quarter_index = as.POSIXct(seq(from=start_ts, to=end_ts, by = 15*60), 
                                                                   origin="1970-01-01", tz="ROK"))
        
        setkey(full_quarter_index, quarter_index)
        setkey(dt, quarter_index)
        
        # tables();
        
        dt = merge(full_quarter_index, dt, all.x=T)
        dt = cbind(dt, date_index = as.Date(dt$quarter_index, tz="ROK"))
        dt[, ':='(quarter = ((hour(quarter_index) * 60 + minute(quarter_index)) / 15) + 1), by=.(date_index)]
        
        # View(dt)
        
        plotting <- ggplot(dt, aes(x=date_index, y=quarter, fill=sum_of_duration)) +
                scale_x_date(date_labels = "%y-%b-%d", breaks = date_breaks("days")) +
                scale_y_continuous(breaks=1:96, labels=make.quarter.label(1:96)) + 
                theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                geom_raster() +
                ggtitle(paste(lab, "RealSense heatmap ( threshold:", threshold, ")" ))
        
        print(plotting)
}


#####################
# raw data loading
# RS_adsl_raw = read.csv("realsense/adsl.csv")
RS_marg_raw = read.csv("realsense/marg.csv")
RS_hcc_raw = read.csv("realsense/hcc.csv")
RS_ux_raw = read.csv("realsense/ux.csv")

RS_marg_table = make.RealSense.Table(RS_marg_raw, 0)
RS_hcc_table  = make.RealSense.Table(RS_hcc_raw, 2000, 6760700)
RS_ux_table   = make.RealSense.Table(RS_ux_raw, 0)


heatmap.RealSense.data("MARG", RS_marg_table, date_cut = "2015-10-07", threshold = 1)
heatmap.RealSense.data("HCC", RS_hcc_table, date_cut = "2015-10-07", threshold = 0)
heatmap.RealSense.data("UX", RS_ux_table, date_cut = "2015-10-07", threshold = 1)


RS_hcc_table  = make.RealSense.Table(RS_hcc_raw, 0, 86400000)
heatmap.RealSense.data("HCC", RS_hcc_table, date_cut = "2015-10-07", threshold = 0)





####################

RS_hcc_raw2 = read.csv("realsense/hcc.csv")

diff = RS_hcc_raw2$leaved - RS_hcc_raw2$joined

boxplot(diff)

range(as.numeric(diff))



RS_hcc_raw2$joined <- time2string(RS_hcc_raw2$joined)
RS_hcc_raw2$leaved <- time2string(RS_hcc_raw2$leaved)

RS_hcc_raw2 <- cbind(RS_hcc_raw2, diff)

RS_hcc_raw2 <- data.table(RS_hcc_raw2)

RS_hcc_raw2 = RS_hcc_raw2[diff > 0.5]

plot(RS_hcc_raw2$diff)

View(RS_hcc_raw2)



RS_hcc_table2 = make.RealSense.Table(RS_hcc_raw2)
hcc <- plot.RealSense.data("HCC", RS_hcc_table2, ylim = 60000, "2015-10-28", "duration_sum")




RS_hcc_raw2 = cbind(RS_hcc_raw2, diff)
range(RS_hcc_raw2$diff)
RS_hcc_raw2 = RS_hcc_raw2[RS_hcc_raw2$diff > 1,]

RS_hcc_raw2 = RS_hcc_raw2[,-4]



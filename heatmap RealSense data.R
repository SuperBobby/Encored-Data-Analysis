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

heatmap.RealSense.data("MARG", RS_marg_table, date_cut = "2015-10-07", threshold = 1)
heatmap.RealSense.data("HCC", RS_hcc_table, date_cut = "2015-10-07", threshold = 5)
heatmap.RealSense.data("UX", RS_ux_table, date_cut = "2015-10-07", threshold = 1)


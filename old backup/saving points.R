source("Encored-Data-Analysis/getSNUdata.R")
library(data.table)
library(ggplot2)

get.theDate.spline.ref <- function(data, date, week_span, set_df, verbose=F) {
        
        target_data = data; target_date = date
        
        target_ts   = as.numeric(as.POSIXct(target_date, origin = "1970-01-01", tz = "ROK"))
        
        dt = data.table(target_data)
        dt[, ':='(unix_ts       = as.numeric(dt$timestamp), 
                  week_index    = rep(1:(round(nrow(dt)/(96*7))), each=96*7, len=nrow(dt)),
                  quarter_index = rep(1:96, len=nrow(dt)))]
        
        this_week_index = dt[unix_ts == target_ts]$week_index
        yearAgo_week_index = this_week_index - 52  # A year == 52 weeks
        
        if(verbose) {
                print(paste("Input date :", target_date, "(", isWeekday(target_date), ")"))
                print(paste("This week index :", this_week_index))
                print(paste("A year ago week index :", yearAgo_week_index))
                print(paste("Subset span :", (yearAgo_week_index-week_span), "~", (yearAgo_week_index+week_span), "( week span:", week_span, ")"))
        }
        
        # yearAgo_week_index = dt[unix_ts == yearAgo_ts]$week_index
        # dt[week_index == yearAgo_week_index]
        
        subset_dt = dt[week_index >= (yearAgo_week_index-week_span) & week_index <= (yearAgo_week_index+week_span)]
        
        weekday_mean_table = subset_dt[weekday == isWeekday(target_date), 
                                       .(computer = mean(computer),
                                         light    = mean(light),
                                         hvac     = mean(hvac),
                                         total = mean(total)), by=.(week_index, quarter_index)]
        # print(weekday_mean_table)
        feeders = c("computer", "light", "hvac", "total")
        spline_returns = numeric(0)
        
        for(feeder in feeders){
                feeder_dt = data.table(matrix(unlist(weekday_mean_table[,feeder, with=F]), nrow=96))
                feeder_dt = feeder_dt[,lapply(.SD, cumsum)]
                
                tmp_df = data.frame(index=1:(week_span*2+1), value=as.numeric(feeder_dt[96]))
                fit = smooth.spline(x=tmp_df$index, y=tmp_df$value, df=set_df)
                fit_y = fit$y[week_span+1]
                spline_returns = c(spline_returns, ifelse(fit_y>0, fit_y, 0))
                
        }
        names(spline_returns) <- c(feeders)
        return(spline_returns)
}

plot.daily.points <- function(lab = c("marg", "hcc", "ux"), data, start, end, week_span, set_df, plot_type, verbose=F) {
        
        start_timestamp = as.numeric(as.POSIXct(start, format="%Y-%m-%d"))
        end_timestamp = as.numeric(as.POSIXct(end, format="%Y-%m-%d"))
        
        loop_timestamp = start_timestamp
        
        timestamp   = numeric(0)
        point_com   = numeric(0)
        point_light = numeric(0)
        point_hvac  = numeric(0)
        point_etc   = numeric(0)
        point_total = numeric(0)
        
        timestamp_gap = 60 * 60 * 24
        
        while(loop_timestamp < end_timestamp){
                
                com   = 0; light = 0; hvac  = 0; etc   = 0; total   = 0
                
                query_timestamp = as.POSIXct(loop_timestamp, format="%Y-%m-%d", origin='1970-01-01', tz="ROK")
                # print(query_timestamp)
                query = paste("http://localhost:3000/api/labs/", lab, "/energy/daily.json?day_from=", query_timestamp, sep="")
                
                #print(query)
                
                rd = readLines(query, warn="F")
                dat <- fromJSON(rd)
                
                if(length(dat)==0){
                        com   = 0
                        light = 0
                        hvac  = 0
                        etc   = 0
                        total   = 0  
                } else {
                        
                        for(i in 1:length(dat[[1]]$feeders)){
                                if(dat[[1]]$feeders[[i]]$description == "computer"){
                                        com = com + dat[[1]]$feeders[[i]]$value
                                } else if(dat[[1]]$feeders[[i]]$description == "light"){
                                        light = light + dat[[1]]$feeders[[i]]$value
                                } else if(dat[[1]]$feeders[[i]]$description == "hvac"){
                                        hvac = hvac + dat[[1]]$feeders[[i]]$value
                                } else if(dat[[1]]$feeders[[i]]$description == "unclassified"){
                                        etc = etc + dat[[1]]$feeders[[i]]$value
                                } else {
                                        print("The feeder data is not classfied appropriatly")
                                        return()
                                }
                        }        
                        total  = dat[[1]]$sum
                }
                
                ## Extract the reference 
                input_date = as.character(query_timestamp)
                ref = get.theDate.spline.ref(data, input_date, week_span, set_df, verbose)
                # print(ref)
                
                timestamp   = c(timestamp, query_timestamp)
                point_com   = c(point_com, round(ref[1] - com))
                point_light = c(point_light, round(ref[2] - light))
                point_hvac  = c(point_hvac, round(ref[3] - hvac))
                point_total = c(point_total, round((ref[1]-com)+(ref[2]-light)+(ref[3]-hvac)))
                
                print(paste(query_timestamp, ":", round(ref[1] - com), round(ref[2] - light), round(ref[3] - hvac), round((ref[1]-com)+(ref[2]-light)+(ref[3]-hvac))))
                
                loop_timestamp = loop_timestamp + timestamp_gap
        }
        
        timestamp = as.POSIXct(timestamp, format="%Y-%m-%d", origin='1970-01-01', tz="ROK")        
        day = weekdays(timestamp, abbreviate = T)
        weekday = isWeekday(timestamp + 9*60*60)
        
        return_table = data.frame(date_index = as.Date(timestamp, tz="ROK"), 
                                  computer   = point_com, 
                                  light      = point_light, 
                                  hvac       = point_hvac, 
                                  total      = point_total, 
                                  day        = day, 
                                  weekday    = weekday)
        
        #         names(return_table)[2] = paste(lab, "_", resolution, sep="")
        #         print(summary(return_table))
        row.names(return_table) = NULL
        
        plot_title = paste(toupper(lab), "total point", start, "~", end, "(week_span:", week_span, ", df:", set_df, ")")
        
        return_table = cbind(return_table, week_index = rep(1:(nrow(return_table)/7), each=7, length=nrow(return_table)))
        
        dt = data.table(return_table)
        
        week_df = cbind(date_index = dt[day=="월"]$date_index,
                        dt[,.(total=sum(total)), by=.(week_index)])
        
        if(plot_type == "weeks") {
                plot_df = week_df
        } else if(plot_type == "days") {
                plot_df = return_table
        } else {
                print("plot_type : weeks or days")
                return()
        }
        
        plotting <- ggplot(plot_df, aes(x=date_index, y=total, label=total)) +
                geom_point(aes(color=total)) +
                geom_text(vjust = 0, nudge_y = 1) +
                # scale_colour_gradient2(low="red", high="blue") + 
                geom_line() +
                theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                ggtitle(plot_title) +
                scale_x_date(date_labels = "%y-%b-%d", breaks = date_breaks(plot_type))
        
        print(plotting)
        print(plot_df)
        return(plot_df)
}

# day_start = "2015-10-12"
# day_start = "2016-02-01"
day_start = "2016-01-01"
day_end   = "2016-02-15"

marg_point_table = plot.daily.points("marg", marg_defalut_table_15min, day_start, day_end, 
                                     week_span=8, set_df = 3, plot_type="days", verbose=F)
hcc_point_table = plot.daily.points("hcc", hcc_defalut_table_15min, day_start, day_end, 
                                    week_span=6, set_df = 3, plot_type="weeks", verbose=F)
ux_point_table = plot.daily.points("ux", ux_defalut_table_15min, day_start, day_end, 
                                   week_span=6, set_df = 3, plot_type="weeks", verbose=F)






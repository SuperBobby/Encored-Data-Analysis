# MUST restart the R session! (cause of toJSON function...)

library(jsonlite)
library(data.table)
library(splines)
library(ggplot2)
library(scales)
library(timeDate)

get.spline.ref <- function(data, date, weekday_, week_span=6, set_df = 3) {
        # week_span = 6 # +- this from target week
        # set_df = 3
        
        target_data = data; target_date = date
        
        target_ts   = as.numeric(as.POSIXct(target_date, origin = "1970-01-01", tz = "ROK"))
        
        dt = data.table(target_data)
        dt[, ':='(unix_ts       = as.numeric(dt$timestamp), 
                  week_index    = rep(1:(round(nrow(dt)/(96*7))), each=96*7, len=nrow(dt)),
                  quarter_index = rep(1:96, len=nrow(dt)))]
        # View(dt)
        # print(dt)
        
        # yearAgo_ts = target_ts - 365*24*60*60
        
                # print((dt[unix_ts == target_ts]$week_index))
                # print((dt[unix_ts == yearAgo_ts]$week_index))
        
        target_week_index = dt[unix_ts == target_ts]$week_index
        print(dt[unix_ts == target_ts])
        # yearAgo_week_index = this_week_index - 52  # A year == 52 weeks
        
        print(paste("Input date :", target_date))
        # print(paste("This week index :", this_week_index))
        print(paste("A year ago week index :", target_week_index))
        print(paste("Subset span :", (target_week_index-week_span), "~", (target_week_index+week_span), "( week span:", week_span, ")"))
        
        # yearAgo_week_index = dt[unix_ts == yearAgo_ts]$week_index
        # dt[week_index == yearAgo_week_index]
        
        subset_dt = dt[week_index >= (target_week_index-week_span) & week_index <= (target_week_index+week_span)]
        
        weekday_mean_table = subset_dt[weekday == weekday_, .(computer = mean(computer),
                                                              light    = mean(light),
                                                              hvac     = mean(hvac),
                                                              total = mean(total)), by=.(week_index, quarter_index)]
        # print(weekday_mean_table)
        
        result_df = data.frame(index=1:96)
        feeders = c("computer", "light", "hvac", "total")
        
        par(mfrow=c(2,2))
        for(feeder in feeders){
                feeder_dt = data.table(matrix(unlist(weekday_mean_table[,feeder, with=F]), nrow=96))
                feeder_dt = feeder_dt[,lapply(.SD, cumsum)]
                
                spline_returns = numeric(0)
                for(i in 1:96){
                        tmp_df = data.frame(index=1:(week_span*2+1), value=as.numeric(feeder_dt[i]))
                        fit = smooth.spline(x=tmp_df$index, y=tmp_df$value, df=set_df)
                        fit_y = fit$y[week_span+1]
                        spline_returns = c(spline_returns, ifelse(fit_y>0, fit_y, 0))
                        # print(spline_returns)
                        if(i == 96) {
                                # fit2 = smooth.spline(x=tmp_df$index, y=tmp_df$value, cv=T)
                                
                                plot(tmp_df, cex=.5, col="black")
                                title(paste("Smoothing Spline (", ifelse(test = weekday_, "weekday", "weekend"), feeder, ")"))
                                lines(fit, col="red", lwd=2)
                                # lines(fit2, col="blue", lwd=2)
                                # legend("topright", legend = paste("df:", round(fit$df)), col="red", lty=1, lwd=2, cex=.8)
                                # legend("topright", legend = c(paste("df", fit$df), paste("df",fit2$df,"(CV)")), col=c("red", "blue"), lty=1, lwd=2, cex=.8)
                        }
                }
                result_df = cbind(result_df, spline_returns)
        }
        par(mfrow=c(1,1))
        names(result_df) <- c("index",feeders)
        
        # print(paste("ref date:",date))
        print(toJSON(result_df))
        return(result_df)
}

trace.spline.ref <- function(lab, data, date_from, weekday_, week_long, week_span = 6, set_df = 3, with_temp = F, with_cloud = F){
        
        weather_dt = fread("data/Suwon_weather.csv")
        weather_dt$date_index = as.Date(weather_dt$date_index)
        
        target_data = data; target_date = date_from
        target_ts   = as.numeric(as.POSIXct(target_date, origin = "1970-01-01", tz = "ROK"))
        
        elec_dt = data.table(target_data)
        elec_dt[, ':='(unix_ts       = as.numeric(elec_dt$timestamp), 
                       week_index    = rep(1:(round(nrow(elec_dt)/(96*7))), each=96*7, len=nrow(elec_dt)),
                       quarter_index = rep(1:96, len=nrow(elec_dt)))]
        
        weather_dt[, ':='(unix_ts       = as.numeric(weather_dt$date_index), 
                          week_index    = rep(1:(round(nrow(weather_dt)/7)), each=7, len=nrow(weather_dt)),
                          weekday       = isWeekday(weather_dt$date_index))]
        # data.table check
        # print(elec_dt)
        # print(weather_dt)
        
        weather_mean_table = weather_dt[weekday == weekday_, 
                                        .(avg_temp  = mean(avg_temp),
                                          avg_cloud = mean(avg_cloud)), by=.(week_index)]
        print(weather_mean_table)
        
        ref_df = data.frame(index=1:week_long)
        avg_df = data.frame(index=1:week_long)
        
        feeders = c("computer", "light", "hvac", "total")
        for(feeder in feeders){
                
                spline_returns = numeric(0)
                fit_y_data     = numeric(0)
                
                ref_date = character(0)
                week_index = numeric(0)
                
                for(step in 1:week_long){
                        
                        target_week_index = elec_dt[unix_ts == target_ts]$week_index + 1 + (step-1)
                        # print(target_week_index)
                        
                        subset_elec_dt    =    elec_dt[week_index >= (target_week_index-week_span) & week_index <= (target_week_index+week_span)]
                        # print(subset_elec_dt)
                        
                        elec_mean_table = subset_elec_dt[weekday == weekday_, 
                                                         .(computer = mean(computer),
                                                           light    = mean(light),
                                                           hvac     = mean(hvac),
                                                           total    = mean(total)), by=.(week_index, quarter_index)]
                        # print(elec_mean_table)
                        
                        feeder_dt = data.table(matrix(unlist(elec_mean_table[,feeder, with=F]), nrow=96))
                        feeder_dt = feeder_dt[,lapply(.SD, cumsum)]
                        
                        tmp_df = data.frame(index=1:(week_span*2+1), value=as.numeric(feeder_dt[96]))
                        fit = smooth.spline(x=tmp_df$index, y=tmp_df$value, df=set_df)
                        fit_y = fit$y[week_span+1]
                        
                        spline_returns = c(spline_returns, ifelse(fit_y>0, fit_y, 0))
                        fit_y_data     = c(fit_y_data, fit$data$y[week_span+1])
                        
                        theDate = subset_elec_dt[week_index==target_week_index & weekday == weekday_]$timestamp[1]
                        
                        ref_date   = c(ref_date, theDate)
                        week_index = c(week_index, target_week_index)
                        #console.log
                        print(paste(target_week_index, theDate, feeder, fit$y[week_span+1], fit$data$y[week_span+1]))
                }
                ref_df = cbind(ref_df, spline_returns)
                avg_df = cbind(avg_df, fit_y_data)
        }
        
        # ref_df = cbind(as.POSIXct(as.numeric(ref_date), origin="1970-01-01",tz="ROK"), ref_df)
        ref_df = cbind(ref_date, ref_df)
        ref_df = cbind(week_index, ref_df)
        ref_df = ref_df[,-3]
        names(ref_df) <- c("week_index", "date_index", paste0(feeders, "_ref"))
        # print(ref_df)
        
        # ref_df = cbind(as.POSIXct(as.numeric(ref_date), origin="1970-01-01",tz="ROK"), ref_df)
        avg_df = cbind(ref_date, avg_df)
        avg_df = avg_df[,-2]
        names(avg_df) <- c("date_index", paste0(feeders, "_avg"))
        
        result_df = merge(ref_df, avg_df)
        result_df$date_index = as.Date(as.POSIXct(as.numeric(as.character(result_df$date_index)), origin='1970-01-01', tz='ROK'), tz='ROK')
        
        result_df = merge(result_df, weather_mean_table)
        # print(result_df)
        
        plot_title = paste(lab, ifelse(weekday_, "weekDay", "weekEnd"), date_from, "~ (", week_long, "weeks, spline df:", set_df, ")")
        plotting <- ggplot(result_df, aes(x=date_index)) + 
                geom_line(aes(y=computer_ref, color="computer_ref")) + 
                geom_point(aes(y=computer_avg, color="computer_ref", shape="com")) + 
                
                geom_line(aes(y=light_ref, color="light_ref")) +
                geom_point(aes(y=light_avg, color="light_ref", shape="light")) + 
                
                
                geom_line(aes(y=hvac_ref, color="hvac_ref")) +
                geom_point(aes(y=hvac_avg, color="hvac_ref", shape="hvac")) + 
                
                geom_line(aes(y=total_ref, color="total_ref")) +
                geom_point(aes(y=total_avg, color="total_ref", shape="total")) + 
                
                scale_x_date("date", labels = date_format("%y-%m-%d"), breaks = date_breaks("week")) +
                theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                ylab("Usage (kW/h)") + 
                
                ggtitle(plot_title)
        
        if(with_temp  == T) plotting <- plotting + geom_line(aes(y=avg_temp, color="avg_temp"),linetype = 2)
        if(with_cloud == T) plotting <- plotting + geom_line(aes(y=avg_cloud, color="avg_cloud"), linetype = 2)
        
        print(plotting)
        return(result_df)
}

###########################################
## get reference
target_date = "2015-2-23"
marg_ref_weekday <- get.spline.ref(marg_defalut_table_15min, target_date, weekday_=T)
marg_ref_weekend <- get.spline.ref(marg_defalut_table_15min, target_date, weekday_=F)

get.spline.ref(hcc_defalut_table_15min, target_date, weekday_=T)
get.spline.ref(hcc_defalut_table_15min, target_date, weekday_=F)

get.spline.ref(ux_defalut_table_15min, target_date, weekday_=T)
get.spline.ref(ux_defalut_table_15min, target_date, weekday_=F)


###########################################
## reference trace
from = "2014-11-3"
long = 60
trace.spline.ref("MARG", marg_defalut_table_15min, from, 
                 weekday_ = T, week_long = long, set_df = 3, with_temp = T, with_cloud = F)
trace.spline.ref("MARG", marg_defalut_table_15min, from, 
                 weekday_ = F, week_long = long, set_df = 3, with_temp = T, with_cloud = F)

trace.spline.ref("HCC", hcc_defalut_table_15min, from, 
                 weekday_ = T, week_long = long, set_df = 3, with_temp = F, with_cloud = F)
trace.spline.ref("HCC", hcc_defalut_table_15min, from, 
                 weekday_ = F, week_long = long, set_df = 3, with_temp = F, with_cloud = F)

trace.spline.ref("UX", ux_defalut_table_15min, from, 
                 weekday_ = T, week_long = long, set_df = 5, with_temp = F, with_cloud = F)
trace.spline.ref("UX", ux_defalut_table_15min, from, 
                 weekday_ = F, week_long = long, set_df = 3, with_temp = F, with_cloud = F)





{# 
# #### sample plotting ####
# 
# tmp = matrix(weekday_mean_table$hvac, nrow=96)
# tmp_df = data.frame(index=1:13, value=colSums(tmp))
# 
# fit = smooth.spline(x=tmp_df$index, y=tmp_df$value, df=set_df)
# fit2 = smooth.spline(x=tmp_df$index, y=tmp_df$value, cv=T)
# 
# fit$y[7]
# fit2$y[7]
# fit$df
# fit2$df
# 
# plot(tmp_df, cex=.5, col="gray")
# title("Smoothing Spline of MARG (HVAC)")
# lines(fit, col="red", lwd=2)
# lines(fit2, col="blue", lwd=2)
# legend("topright", legend = c(paste("df", fit$df), paste("df",fit2$df,"(CV)")), col=c("red", "blue"), lty=1, lwd=2, cex=.8)
# 
# ################################################################
}


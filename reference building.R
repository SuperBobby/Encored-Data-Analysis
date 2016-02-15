library(data.table)
library(splines)
library(jsonlite)

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
        
        yearAgo_ts = target_ts - 365*24*60*60
        
        yearAgo_week_index = dt[unix_ts == yearAgo_ts]$week_index + 1 
        # dt[week_index == yearAgo_week_index]
        
        subset_dt = dt[week_index >= (yearAgo_week_index-week_span) & week_index <= (yearAgo_week_index+week_span)]

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
                                legend("topright", legend = paste("df:", round(fit$df)), col="red", lty=1, lwd=2, cex=.8)
                                # legend("topright", legend = c(paste("df", fit$df), paste("df",fit2$df,"(CV)")), col=c("red", "blue"), lty=1, lwd=2, cex=.8)
                        }
                }
                result_df = cbind(result_df, spline_returns)
        }
        par(mfrow=c(1,1))
        names(result_df) <- c("index",feeders)

        print(paste("ref date:",date))
        return(toJSON(result_df))
}

# marg_defalut_table_15min 
# hcc_defalut_table_15min 
# ux_defalut_table_15min 

ref_date = "2016-2-8"
get.spline.ref(marg_defalut_table_15min, ref_date, weekday_=T)
get.spline.ref(marg_defalut_table_15min, ref_date, weekday_=F)

get.spline.ref(hcc_defalut_table_15min, ref_date, weekday_=T)
get.spline.ref(hcc_defalut_table_15min, ref_date, weekday_=F)

get.spline.ref(ux_defalut_table_15min, ref_date, weekday_=T)
get.spline.ref(ux_defalut_table_15min, ref_date, weekday_=F)





#### plotting ####

tmp = matrix(weekday_mean_table$hvac, nrow=96)
tmp_df = data.frame(index=1:13, value=colSums(tmp))

fit = smooth.spline(x=tmp_df$index, y=tmp_df$value, df=set_df)
fit2 = smooth.spline(x=tmp_df$index, y=tmp_df$value, cv=T)

fit$y[7]
fit2$y[7]
fit$df
fit2$df

plot(tmp_df, cex=.5, col="gray")
title("Smoothing Spline of MARG (HVAC)")
lines(fit, col="red", lwd=2)
lines(fit2, col="blue", lwd=2)
legend("topright", legend = c(paste("df", fit$df), paste("df",fit2$df,"(CV)")), col=c("red", "blue"), lty=1, lwd=2, cex=.8)

################################################################





as.POSIXct(yearAgo_ts, origin = "1970-01-01", tz = "ROK")

dt = cbind(dt, new_index = rep(1:96, nrow(dt)))

dt[weekday==T, lapply(.(computer, light, hvac), mean), by=.(new_index, weekday)]
dt[,':='(timestamp = NULL, day = NULL, new_index = NULL)]

dt[weekday==F, lapply(.SD, mean), by=.(new_index)]

###############################
#### showing pattern functions
#### barplot with errors with SNU API
#### 2015.10.30 Han.J.Y.
###############################
source("Encored-Data-Analysis/getSNUdata.R")

library(plyr)
library(stringr)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(scales)
library(timeDate)

show.pattern = function(raw_data, target, label, start_date, weeks, ymax, return_data = F){
        
        barplot.withError = function(data, main_title, y_range){
                
                means  = sapply(data, mean, na.rm=T)
                stDevs = sapply(data, sd, na.rm=T)
                
                peak = quantile(data, 0.95, na.rm = T)
                base = quantile(data, 0.05, na.rm = T)
                print(paste("peak(95th percentiles):", peak))
                print(paste("base( 5th percentiles):", base))
                
                n = length(means)
                avg = mean(stack(data)$values, na.rm=T) * 96 
                mp = barplot(means, 
                             col=rep(topo.colors(n/4), each = 4),
                             main=main_title,
                             ylim=y_range,
                             xlab=paste("average usage/day:", round(avg,2)),
                             ylab="Usage (kW/15min)")
                box()
                
                segments(mp, means - stDevs, mp, means + stDevs, lwd=2)
                segments(mp - 0.1, means - stDevs, mp + 0.1, means - stDevs, lwd=2)
                segments(mp - 0.1, means + stDevs, mp + 0.1, means + stDevs, lwd=2)
                
                lines(c(-4,116), c(base,base), col="blue")
                text(x = -2, y = base*1.05, labels = round(base,2), lty = 2, offset=0.3)
                
                lines(c(-4,116), c(peak,peak), col="red")
                text(x = -2, y = peak*1.05, labels = round(peak,2), lty = 2, offset=0.3)
        }
        
        data = raw_data
        unit = 96
        
        # indexing
        start = which(data[,1] == start_date)
        indexes = seq(from = start, by = unit*7, length.out = weeks)
        
        ##  preparation for loop
        par(mfrow=c(weeks,2))
        
        sub_data = data[-1]
        result = matrix(rep(0, 7*unit), 7, unit)
        
        #for the return set
        return_row_names = character(0)
        return_result = matrix(0, 0, unit)
        
        ## auto y range ... estimation from the whole... need to adjust manually after the first try
        if(missing(ymax)){
                print("ymax missing... auto y_range")
                
                print(max(raw_data[,target]))
                y_range = c(0, max(raw_data[,target]) * 1.2)
                print(y_range)
        } 
        else {
                y_range = c(0, ymax)
        }
        
        for (start_index in indexes){
                
                row_names = character(0)
                col_names = character(0)
                
                # for a week -- 7days
                result = matrix(rep(0, 7*unit), 7, unit)
                
                for (i in 1:7){ 
                        from = (start_index+unit*(i-1))
                        to = (start_index-1+unit*(i))    
                        
                        result[i,] = as.numeric(sub_data[from:to,target])
                        
                        row_names = c(row_names, strtrim(data[from,1], 10))
                        return_row_names = c(return_row_names, strtrim(data[from,1], 10))
                }
                
                col_names = substr(as.character(data[from:to,1]), 12,16)
                
                # return set
                return_result = rbind(return_result, result)
                
                # local loop set
                result = as.data.frame(x = result, row.names = row_names)
                names(result) = col_names
                
                plot_title = paste(label, target, row_names[1], "~")
                
                if(!return_data) {
                        barplot.withError(result[isWeekday(row_names),], main_title = paste(plot_title, "weekday"), y_range) 
                        barplot.withError(result[isWeekend(row_names),], main_title = paste(plot_title, "weekend"), y_range) 
                }
        }
        
        return_result = as.data.frame(return_result)
        return_result = cbind(return_row_names , return_result)
        names(return_result) = c("timestamp", col_names)
        
        par(mfrow=c(1,1))
        
        if(return_data) {    
                return(return_result)
        }
}


##################
### analysis  ####
##################
from_date = "2015-11-02"
plot_weeks = 10

show.pattern(marg_defalut_table_15min, target="total", label = "MARG", from_date, weeks = 4, return_data=F, ymax=2)
show.pattern(marg_defalut_table_15min, target="total", label = "MARG", "2015-10-05", weeks = 6, return_data=F, ymax=2)
show.pattern(marg_defalut_table_15min, target="total", label = "MARG", "2015-11-02", weeks = 5, return_data=F, ymax=2)

show.pattern( hcc_defalut_table_15min, target="total", label = "HCC",  from_date, weeks = 4, return_data=F, ymax=1)
show.pattern( hcc_defalut_table_15min, target="total", label = "HCC",  "2015-10-05", weeks = 4, return_data=F, ymax=1)
show.pattern( hcc_defalut_table_15min, target="total", label = "HCC",  "2015-11-02", weeks = 3, return_data=F, ymax=1)

show.pattern(  ux_defalut_table_15min, target="total", label = "UX",   from_date, weeks = 4, return_data=F, ymax=0.8)
show.pattern(  ux_defalut_table_15min, target="total", label = "UX",   "2015-10-05", weeks = 5, return_data=F, ymax=0.8)
show.pattern(  ux_defalut_table_15min, target="total", label = "UX",   "2015-11-02", weeks = 3, return_data=F, ymax=0.8)


# install.packages("lubridate")
library(lubridate)
library(plyr)

# raw data loading
RS_raw_marg = read.csv("realsense/marg.csv")
RS_raw_hcc = read.csv("realsense/hcc.csv")


getRealSenseTable = function(data){
                
        # convert data format
        joined = as.POSIXct(data$joined/1000, format="%Y-%m-%d", origin='1970-01-01', tz="ROK")
        leaved = as.POSIXct(data$leaved/1000, format="%Y-%m-%d", origin='1970-01-01', tz="ROK")
        duration = as.numeric(data$leaved - data$joined)
        
        basic_table = data.frame(joined, duration)
        
        basic_table = basic_table[order(joined, leaved),]
        
        # duration pre-processing ###############################
        duration[duration > 600000] = 0
        
        
        # second 
        second(basic_table$joined) = 0
        
        # minite 
        minute(basic_table$joined)[minute(basic_table$joined) < 15] = 0
        minute(basic_table$joined)[minute(basic_table$joined) >= 15 & minute(basic_table$joined) < 30] = 15
        minute(basic_table$joined)[minute(basic_table$joined) >= 30 & minute(basic_table$joined) < 45] = 30
        minute(basic_table$joined)[minute(basic_table$joined) >= 45] = 45
        
        
        # aggregation
        # counting = rle(as.numeric(basic_table$joined))$lengths
        # timestamp = joined[cumsum(counting)]
        
        basic_table$joined = as.factor(basic_table$joined)
        # timestamp = unique(as.character(basic_table$joined))
        counting = count(basic_table, "joined")
        avg_duration = aggregate(duration ~ joined, data = basic_table, mean)
        
        return_table = merge(counting, avg_duration)
        
        return(return_table)
}

tmp_marg = getRealSenseTable(RS_raw_hcc)

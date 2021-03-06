#install.packages("rjson")
# source("Encored-Data-Analysis/Encored public functions.R")

require("rjson")
# library("scales")
# library(reshape2)
# library(timeDate)

## Functions #########################################################
getSNUData.total <- function(lab = c("marg", "hcc", "ux"), 
                           resolution = c("quarters", "hours"),
                           start, cut, verbose=T) {
        
        start_timestamp = as.numeric(as.POSIXct(start, format="%Y-%m-%d"))
        end_timestamp = as.numeric(as.POSIXct(cut, format="%Y-%m-%d"))
        
        query_timestamp = start_timestamp
        usage = 0;
        
        timestamp = numeric(0)
        return_usage = numeric(0)
        
        c("quarters", "hours")
        
        if(resolution == "quarters") {
                timestamp_gap = 60 * 15
        } else if (resolution == "hours") {
                timestamp_gap = 60 * 60
        }
        
        while(query_timestamp < end_timestamp){
                
                query = paste("http://adsl.snu.ac.kr:3300/api/labs/", lab, "/energy/", resolution, 
                              ".json?base_time=", query_timestamp, "000&limit=1", sep="")
                #                 print(query)
                
                rd = readLines(query, warn="F")
                dat <- fromJSON(rd)
                
                if(length(dat)==0){
                        usage  = 0
                } else {
                        usage  = dat[[1]]$sum
                }
                
                timestamp = c(timestamp, as.POSIXct(query_timestamp, format="%Y-%m-%d", origin='1970-01-01', tz="ROK"))
                return_usage = c(return_usage, usage)
                
                if(verbose) {
                        print(paste(as.POSIXct(query_timestamp, format="%Y-%m-%d", origin='1970-01-01', tz="ROK"), 
                                    ":", usage, "kW/h"))
                }
                
                query_timestamp = query_timestamp + timestamp_gap
                
        }
        timestamp = as.POSIXct(timestamp, format="%Y-%m-%d", origin='1970-01-01', tz="ROK")
        
        return_table = data.frame(timestamp, return_usage)
        names(return_table)[2] = paste(lab, "_", resolution, sep="")
        
        print(summary(return_table))
        
        return(return_table)
}


getSNUData.feeder <- function(lab = c("marg", "hcc", "ux"), 
                              resolution = c("quarters", "hours"),
                              start, cut, verbose=T) {
        
        start_timestamp = as.numeric(as.POSIXct(start, format="%Y-%m-%d"))
        end_timestamp = as.numeric(as.POSIXct(cut, format="%Y-%m-%d"))
        
        query_timestamp = start_timestamp
        
        timestamp   = numeric(0)
        usage_com   = numeric(0)
        usage_light = numeric(0)
        usage_hvac  = numeric(0)
        usage_etc   = numeric(0)
        usage_total   = numeric(0)
        
        if(resolution == "quarters") {
                timestamp_gap = 60 * 15
        } else if (resolution == "hours") {
                timestamp_gap = 60 * 60
        }
        
        while(query_timestamp < end_timestamp){
                
                com   = 0
                light = 0
                hvac  = 0
                etc   = 0
                total   = 0
                
                query = paste("http://adsl.snu.ac.kr:3300/api/labs/", lab, "/energy/", resolution, 
                              ".json?base_time=", query_timestamp, "000&limit=1", sep="")
                
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
                
                timestamp   = c(timestamp, query_timestamp)
                usage_com   = c(usage_com, com)
                usage_light = c(usage_light, light)
                usage_hvac  = c(usage_hvac, hvac)
                usage_etc   = c(usage_etc, etc)
                usage_total   = c(usage_total, total)
                
                if(verbose) {
                        print(paste(lab, "-", as.POSIXct(query_timestamp, format="%Y-%m-%d", origin='1970-01-01', tz="ROK"), 
                                    ":", total, "kW/h"))
                }
                
                query_timestamp = query_timestamp + timestamp_gap
                
        }
        
        timestamp = as.POSIXct(timestamp, format="%Y-%m-%d", origin='1970-01-01', tz="ROK")        
        day = weekdays(timestamp, abbreviate = T)
        weekday = isWeekday(timestamp + 9*60*60)
        
        return_table = data.frame(timestamp, 
                                  computer = usage_com, 
                                  light    = usage_light, 
                                  hvac     = usage_hvac, 
                                  etc      = usage_etc, 
                                  total      = usage_total, 
                                  day      = day, 
                                  weekday  = weekday)
        
        row.names(return_table) = NULL
        return(return_table)
}


getSNUData.feeder.day <- function(lab = c("marg", "hcc", "ux"), 
                                  start, cut, verbose=T) {
  
  start_timestamp = as.numeric(as.POSIXct(start, format="%Y-%m-%d"))
  end_timestamp = as.numeric(as.POSIXct(cut, format="%Y-%m-%d"))
  
  loop_timestamp = start_timestamp
  
  timestamp   = numeric(0)
  usage_com   = numeric(0)
  usage_light = numeric(0)
  usage_hvac  = numeric(0)
  usage_etc   = numeric(0)
  usage_total   = numeric(0)
  
  timestamp_gap = 60 * 60 * 24
  
  while(loop_timestamp < end_timestamp){
    
    com   = 0
    light = 0
    hvac  = 0
    etc   = 0
    total   = 0
    
    query_timestamp = as.POSIXct(loop_timestamp, format="%Y-%m-%d", origin='1970-01-01', tz="ROK")
    query = paste("http://adsl.snu.ac.kr:3300/api/labs/", lab, "/energy/daily.json?day_from=", query_timestamp, sep="")
    
    # print(query)
    
    rd = readLines(query, warn="F")
    dat <- fromJSON(rd)
    
    # print(dat)
    
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
        }
      }        
      total  = dat[[1]]$sum
    }
    
    #                 print(query)                
    #                 print(query_timestamp)
    #                 print(com)
    #                 print(light)
    #                 print(hvac)
    #                 print(etc)
    #                 print(total)       
    
    timestamp   = c(timestamp, query_timestamp)
    usage_com   = c(usage_com, com)
    usage_light = c(usage_light, light)
    usage_hvac  = c(usage_hvac, hvac)
    usage_etc   = c(usage_etc, etc)
    usage_total   = c(usage_total, total)
    
    if(verbose) {
      print(paste(as.POSIXct(query_timestamp, format="%Y-%m-%d", origin='1970-01-01', tz="ROK"), 
                  ":", total, "kW/h"))
    }
    
    
    # print(loop_timestamp)
    
    loop_timestamp = loop_timestamp + timestamp_gap 
    
  }
  
  timestamp = as.POSIXct(timestamp, format="%Y-%m-%d", origin='1970-01-01', tz="ROK")        
  day = weekdays(timestamp, abbreviate = T)
  weekday = isWeekday(timestamp + 9*60*60)
  
  return_table = data.frame(timestamp, 
                            computer = usage_com, 
                            light    = usage_light, 
                            hvac     = usage_hvac, 
                            etc      = usage_etc, 
                            total      = usage_total, 
                            day      = day, 
                            weekday  = weekday)
  
  #         names(return_table)[2] = paste(lab, "_", resolution, sep="")
  #         print(summary(return_table))
  row.names(return_table) = NULL
  return(return_table)
}


reviseSNUData <- function(default_data, 
                        lab = c("marg", "hcc", "ux"),
                        start, cut, verbose=T) {
        
        start_timestamp = as.numeric(as.POSIXct(start, format="%Y-%m-%d"))
        end_timestamp = as.numeric(as.POSIXct(cut, format="%Y-%m-%d"))
        
        timestamp_gap = as.numeric(default_data$timestamp[2]) - as.numeric(default_data$timestamp[1])
        
        print(timestamp_gap)
        
        if(timestamp_gap == 60 * 15) {
                resolution = "quarters"
        } else if (timestamp_gap == 60 * 60) {
                resolution = "hours"
        }
        
        ## Header        
        default_start_timestamp = as.numeric(default_data$timestamp[1])
        
        if(default_start_timestamp == start_timestamp) {
                cat("nothing to do for header\n")
                header = NULL
        } else if(default_start_timestamp < start_timestamp) { # need to cut 
                cat("default data cutted @head\n")
                default_data = subset(default_data, subset=default_data$timestamp >= start)
                header = NULL
        } else { # need to get more data
                cat("get more data for header\n")
                header = getSNUData.feeder(lab, resolution, start, 
                                           as.POSIXct(default_start_timestamp, format="%Y-%m-%d", origin='1970-01-01', tz="ROK"), verbose)
        }
        
        ## Footer        
        default_end_timestamp = as.numeric(default_data$timestamp[length(default_data$timestamp)]) + timestamp_gap
        
        #         print(default_end_timestamp)
        #         print(end_timestamp)
        
        if(default_end_timestamp == end_timestamp) {
                cat("nothing to do for footer\n")
                footer = NULL
        } else if(default_end_timestamp > end_timestamp) { # need to cut 
                cat("default data cutted @foot\n")
                default_data = subset(default_data, subset=default_data$timestamp < cut)
                footer = NULL
        } else { # need to get more data
                cat("get more data for footer\n")
                footer = getSNUData.feeder(lab, resolution, 
                                           as.POSIXct(default_end_timestamp, format="%Y-%m-%d", origin='1970-01-01', tz="ROK"), cut, verbose)
        }
        
        #         print(header)
        #         print(footer)
        
        # make the return table
        return_data = rbind(header, default_data, footer)
        
        return(return_data)
}


cutTail <- function(data, n){
        len = nrow(data)
        index = 1:(len-n)
        return(data[index,])
}

df.viewer = function(df, n=20){
        rows_for_a_page = n
        n_pages = round(nrow(df) / n) + 1
        
        i = 1
        
        while(i < n_pages){
                if ((rows_for_a_page*(i)) < nrow(df)){
                        end_row = (rows_for_a_page*(i))
                } else {
                        end_row = nrow(df)
                }
                
                print(df[(rows_for_a_page*(i-1)+1):end_row,])
                
                tmp = readline("press enter to continue... ('q' for break )")
                if(tmp=='q'){ 
                        break() 
                }
                i = i + 1
        }
}



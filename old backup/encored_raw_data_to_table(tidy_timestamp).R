###############################
## data loading
## setting the period of datasets which should be tabulated -- 2015.7.8. J.Y.Han
###############################

#dataname = "data/logs_site73_2015-03-30_2015-03-31.csv/site73-2015-03-30.csv"
dataname = "data/raw/logs_site73_2015-04-24_2015-04-25.csv"
dir = "data/"
date = "2015-04-24 "

realtime_data <- read.table(dataname, header=T, sep=',')
# summary(realtime_data)
# head(realtime_data)

realtime_data$feeder_id <- as.factor(realtime_data$feeder_id)
realtime_data$device_id <- as.factor(realtime_data$device_id)

# summary(realtime_data)
# str(realtime_data)

# sum check (converting into kWh)
sum(as.numeric(realtime_data$active_power/3600)) / 1000000

###############################
## generation time index
###############################

hour <- c("00", "01", "02", "03", "04", "05", "06", "07", "08", "09", 
          "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", 
          "20", "21", "22", "23")
min <- c("00", "01", "02", "03", "04", "05", "06", "07", "08", "09", 
         "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", 
         "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", 
         "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", 
         "40", "41", "42", "43", "44", "45", "46", "47", "48", "49",
         "50", "51", "52", "53", "54", "55", "56", "57", "58", "59")
sec <- min

time_index <- paste(date, 
                    paste(rep(hour, rep(3600, 24)), ":", 
                          paste(rep(min, rep(60,60)), ":", 
                                sec, sep=""), sep=""), sep="")

###############################  
## Feeder seperated dataframe
## duplicated rows will be removed
###############################

data_feeder_sec <-as.data.frame(time_index)

for(device in levels(realtime_data$device_id)) {
        for(feeder in levels(realtime_data$feeder_id)) {
                
                index <- as.data.frame(time_index)
                names(index) <- "timestamp"
                
                tmp <- subset(realtime_data, 
                              subset = realtime_data$device_id == device
                              & realtime_data$feeder_id == feeder, 
                              select = c("timestamp", "active_power"))
                
                names(tmp)[2] <- paste("v", device, "_", feeder, sep="")
                index <- merge(index, tmp, by.x = "timestamp", by.y = "timestamp", all.x = T)
                
                du <- nrow(index[duplicated(index$timestamp),])
                index <- index[!duplicated(index$timestamp),] # remove duplicated rows
                print(paste(device, "_", feeder," - ", du, " duplicated rows", sep=""))   
                
                data_feeder_sec <- cbind(data_feeder_sec, index[2])
                
                #rm(tmp)
        }
}

# NA to 0
for(col_name in names(data_feeder_sec)[-1]) {
        #         print(col_name)
        data_feeder_sec[col_name][is.na(data_feeder_sec[col_name])] <- 0
}

#save(data_feeder_sec, file="data_feeder_sec(2015-03-30).Rda")
write.csv(data_feeder_sec, paste(dir, date, "-tidy.csv", sep=""))

#check sum
sum(realtime_data$active_power/3600/1000000)
sum(data_feeder_sec[-1]/3600/1000000)
sum(realtime_data$active_power/3600/1000000) - sum(data_feeder_sec[-1]/3600/1000000)


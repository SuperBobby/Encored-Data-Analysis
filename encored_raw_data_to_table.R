source("Encored public functions.R")
library(stringr)
####################################################
## Encored UNIX timestamp raw data loader by JY.Han
## last update 2015. 4. 29.
####################################################

start_timeindex = Sys.time()

# target raw files setting: 411 --> 2015/04/11
file_list = paste("0", as.character(seq(411, 411, 1)), sep="")
raw_file_list = paste("data/raw/logs-2015", file_list, ".csv", sep="")
date_list = paste("2015-", substring(file_list, 1, 2), 
                      "-", substring(file_list, 3, 4), sep="")

# file output dir & office hour setting
tmp_dir = "data/tmp/"
dir = "data/"
day_start_hour = "09:00:00"

# diffs table for collecting diff btw raw_data & new_table (checksum)
diffs = numeric(0)

################################################################
## raw-data preprocessing - remove ',' at the end of the lines
################################################################
for (i in 1:length(raw_file_list)){
        
        # initial variables        
        raw_file = raw_file_list[i]
        date = date_list[i]

        # check if the pre-processed file is or not
        output_file_name = paste(tmp_dir, "tmp-", date, ".csv", sep="")
        if (file.exists(output_file_name)){
                cat("pre-processed file already exist!\n\n")
        } else {
                cat(paste("Pre-processed file is not exist...\nEditing each end of row...removing ',' from raw file:", raw_file, "\n"))
                input_file  = file(raw_file, open = "r")
                input_lines = readLines(input_file, n = -1)
                
                for (i in 2:length(input_lines)){
                        input_lines[i] = str_sub(input_lines[i], 1, str_length(input_lines[i])-1)
                        if (i %% 10000 == 0) cat(".")
                        if (i %% 100000 == 0) cat(paste(i/40000, "% ", sep=""))
                }
                cat("100%\n") # file editing is done
                
                cat("preprocessed file exporting...\n")
                output_file = file(output_file_name)
                writeLines(input_lines, output_file, sep = "\n")
                
                close(input_file)
                close(output_file)
        }

        # loading pre-processed file
        cat(paste("loading pre-processed file...", output_file_name, "\n"))
        realtime_data = read.table(output_file_name, header=T, sep=',')
        
        realtime_data$feeder_id = as.factor(realtime_data$feeder_id)
        realtime_data$device_id = as.factor(realtime_data$device_id)
        
        options(digits.secs = 0)
        realtime_data$timestamp = round(realtime_data$timestamp/1000)
        #realtime_data$timestamp = as.POSIXct(realtime_data$timestamp, origin = "1970-01-01")
        #realtime_data$timestamp = as.character(realtime_data$timestamp)
        
        # sum check (converting into kWh)
        cat("Daily total summation(kW/h)\n")
        print(sum(as.numeric(realtime_data$active_power/3600)) / 1000000)
        
        ###############################
        ## generation timestamp index
        ###############################
        cat("Generating time index\n")      
        target_date = paste(date, day_start_hour)
        time_index_start = as.numeric(as.POSIXct(strptime(target_date, "%Y-%m-%d %H:%M:%S")))
          
        time_index = seq(time_index_start, time_index_start+86399, 1)
        time_index_string = as.POSIXct(time_index, origin = "1970-01-01")
        
        ###############################  
        ## Feeder seperated dataframe
        ## duplicated rows will be removed
        ###############################

        cat("Building the BIG table\n")   
        
        BIG_table = data.frame(time_index_string, timestamp = time_index)
        
        for(device in levels(realtime_data$device_id)) {
                for(feeder in levels(realtime_data$feeder_id)) {
                        
                        index = as.data.frame(time_index)
                        names(index) = "timestamp"
                        
                        tmp = subset(realtime_data, 
                                     subset = realtime_data$device_id == device
                                     & realtime_data$feeder_id == feeder, 
                                     select = c("timestamp", "active_power"))
                        
                        names(tmp)[2] = paste("v", device, "_", feeder, sep="")
                        index = merge(index, tmp, by.x = "timestamp", by.y = "timestamp", all.x = T)
                        
                        du = nrow(index[duplicated(index$timestamp),])
                        index = index[!duplicated(index$timestamp),] # remove duplicated rows
                        cat(paste(device, "_", feeder," - ", du, " duplicated rows\n", sep=""))   
                        
                        BIG_table = cbind(BIG_table, index[2])
                        
                        # debuging
                        #print(head(BIG_table, n=2))
                        rm(tmp)
                }
        }

        # NA to 0, and remove zero_sum column
        BIG_table[is.na(BIG_table)] <- 0
        non_zero_sum = as.logical(colSums(BIG_table[,c(-1,-2)]))
        BIG_table = BIG_table[, c(T, T, non_zero_sum)]
        
        # check sum
        cat("check sum\n")
        raw_file_sum = sum(realtime_data$active_power/3600/1000000)
        new_table_sum = sum(BIG_table[,c(-1,-2)])/3600/1000000
        diff = raw_file_sum - new_table_sum
        
        print(raw_file_sum)
        print(new_table_sum)
        cat(paste(date, "\ndiff is...", diff,"\n"))
        
        diffs = c(diffs, diff)
        
        ## export the BIG table
        cat("Writing file...\n")
        write.csv(BIG_table, paste(dir, date, "-tidy.csv", sep=""), row.names = F)
        
        cat(paste("process complete :", raw_file))
        cat("\n-----------------------------------------------\n\n\n")
}       

names(diffs) <- date_list
cat("diff summary\n")
print(diffs)

finish_timeindex = Sys.time()
print(finish_timeindex - start_timeindex)


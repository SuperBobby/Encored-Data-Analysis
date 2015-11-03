########################
### public functions ###
########################
resolution.processor = function(data, jump=0, merge, timestamp_string = "X"){
        
        for(name in names(data)){
                
                merge_index = 1+jump   ##starting point
                
                if(name == timestamp_string){
                        
                        timestamp = character(0)
                        while((merge_index+merge-1) < nrow(data)){
                                
                                new_ts = as.character(data[timestamp_string][merge_index,1])
                                timestamp = c(timestamp, new_ts)
                                
                                merge_index = merge_index + merge
                        }
                        
                        merged_data = as.data.frame(timestamp)
                        print("timestamp done")
                        
                } else {
                        
                        values = numeric(0)
                        while((merge_index+merge-1) < nrow(data)){
                                
                                cat(paste(merge_index, "\n"))
                                
                                value = sum(data[name][merge_index:(merge_index+merge-1) ,1])
                                
                                values = c(values, value)    
                                merge_index = merge_index + merge 
                        }
                        
                        merged_data = cbind(merged_data, values)
                        print(paste(name, " merged"))
                }
        }
        
        names(merged_data) = names(data)
        return(merged_data)
}

add.daylabel = function(data, start){
        
        #if(names(data) doesn't have 'day' column) ...
        for(name in names(data)){
                if(name == "day"){
                        print("this dataset already has a day column")
                        return()
                }
        }
        
        total_length = nrow(data)
        default_daylabel  = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
        default_daytype = c("weekend","weekday","weekday","weekday","weekday","weekday","weekend")
        start_index = which(default_daylabel == start)
        
        daylabel = character(0)
        typelabel = character(0)
        
        for(i in start_index:(start_index+6)){
                
                if (i <= 7){ 
                        day  = default_daylabel[i]
                        type = default_daytype[i]
                        print(day)
                        print(type)            
                } else {
                        day  = default_daylabel[i-7]
                        type = default_daytype[i-7]
                        print(day)
                        print(type)
                }
                
                daylabel = c(daylabel, day)
                typelabel = c(typelabel, type)
        }
        
        i = total_length/7
        i = ceiling(i)
        
        day = rep(daylabel,i)
        day = day[1:total_length]
        
        type = rep(typelabel,i)
        type = type[1:total_length]
        
        data = cbind(data, day)
        data = cbind(data, type)
        
        print(head(data[,c(1,length(data)-1,length(data))], n=8))
        print(tail(data[,c(1,length(data)-1,length(data))], n=8))
        
        return(data)
}

tidy.missing.days = function(ts){
        
        # before 5am or not? check
        if (as.numeric(substr(ts,start = 12, stop = 13)) < 5){
                if(as.numeric(substr(ts,start = 9, stop = 10))-1 > 0){
                        if(as.numeric(substr(ts,start = 9, stop = 10))-1 < 10){
                                date = paste("0",as.character(as.numeric(substr(ts,start = 9, stop = 10))-1),sep="")
                                date = paste(strtrim(ts, 8), date, sep="")
                                return(date)
                        } else {
                                date = as.character(as.numeric(substr(ts,start = 9, stop = 10))-1)
                                date = paste(strtrim(ts, 8), date, sep="")
                                return(date)
                        }
                } else {
                        print("the date became '00'. please add the date manually. the error date is...")
                        print(ts)
                        return(NA)
                }
        }
        else{
                return(strtrim(ts, 10))
        }
}

windowing = function(data, size=7, timestamp_string = "X"){
        
        for(name in names(data)){
                if(name == "day"){
                        # remove character column
                        data = data[,-which(names(data) == "day")]
                        data = data[,-which(names(data) == "type")]
                }
        }
        
        if(size %% 2 !=0 ){
                width = (size-1)/2
        } else {
                width = size/2
        }
        
        print(paste("width: ", width))
        
        for(name in names(data)){
                
                if(name == timestamp_string){
                        
                        center = width
                        timestamp = character(0)
                        
                        while(center+width < nrow(data)){
                                
                                center = center + 1
                                if(center > nrow(data)) break
                                
                                new_ts = as.character(data[timestamp_string][center,1])
                                new_ts = strtrim(new_ts,10)
                                timestamp = c(timestamp, new_ts)
                        }
                        
                        merged_data = as.data.frame(timestamp)
                        print("timestamp done")
                        #print(merged_data)
                        
                } else {
                        
                        center = width
                        values = numeric(0)
                        
                        while(center+width < nrow(data)){
                                
                                center = center + 1
                                if(center > nrow(data)) break
                                
                                
                                value = mean(data[name][(center-width):(center+width) ,1])
                                print(paste(name, center-width, center+width, value))
                                
                                values = c(values, value)    
                        }
                        
                        merged_data = cbind(merged_data, values)
                        print(paste(name, " merged"))
                }
        }
        
        names(merged_data) = names(data)
        return(merged_data)
}

date.omit = function(data, omit_list){
        
        result = data
        for(target in omit_list){
                print(target)
                result = result[result$X != target,]
        }
        return(result)
}

data.extraction = function(data, feeder_name, day_naming= T){
        result      = data[[feeder_name]]
        naming_tag  = as.character(data$day)
        names(result) <- naming_tag
        return(result)
}

p.value.test = function(obs, data, feeder, hist_title="histogam"){
        
        par(mfrow=c(1,length(obs)))
        
        for(o in obs){
                
                d = names(obs)[obs==o]
                
                #print(paste(feeder, ", ", d))
                sub = subset(data, subset = data$day == d, select = feeder)[[1]]
                
                df_ = length(sub)-1
                SE = sd(sub) 
                T_ = (o - mean(sub)) / SE
                p = pt(q = T_, df = df_, lower.tail = T)
                
                print(paste(names(obs)[obs==o], " : ", round(o, digits = 2), 
                            ", T=", round(T_, digits = 2), ", df=", df_, 
                            ", p-value(one-tail)=", round(p, digits=3), sep=""))
                
                hist_title = paste(feeder, " (", d, "), n=", length(sub), sep="")
                hist(sub, breaks=length(sub), main = hist_title, xlab=paste("mean=",round(mean(sub),2)))
        }
        par(mfrow=c(1,1))
}

data.dumper = function(data, timestamp1, timestamp2){
        
        index1 = which(marg_day_5am_tidy$X  == timestamp1)
        index2 = which(marg_day_5am_tidy$X  == timestamp2)
        print(paste(index1, index2))
        
        target_data = data[index1:index2,]
        
        return(target_data)
}

missing.to.NA = function(data, indicator, threshold) {
        
        missing = which(data[indicator] < threshold)
        
        data[missing,-1] = NA
        
        return(data)
}

######################
### MARG functions ###
######################

marg.add.summation = function(data){
        #com_feeder = c("D406.3" "D406.5", "D406.6")
        
        marg_com_sum = as.matrix(data[-1]) %*% matrix(c(0,0,0,1,0,1,1,0,0,0,0,0,0,0,0))
        marg_total = as.matrix(data[-1]) %*% matrix(c(1,0,1,1,1,1,1,1,1,1,1,1,1,1,1))
        
        data = cbind(data, marg_com_sum)
        data = cbind(data, marg_total)
        
        return(data)
}

marg.mean.printer = function(data, timestamp1, timestamp2){
        
        index1 = which(marg_day_5am_tidy$X  == timestamp1)
        index2 = which(marg_day_5am_tidy$X  == timestamp2)
        
        target_data = data[index1:index2,]
        subset1 = subset(target_data, subset=target_data$type=="weekday")
        subset2 = subset(target_data, subset=target_data$type=="weekend")
        
        print(target_data$X)
        print(paste(index1, index2, "::", index2-index1+1, "days"))
        print(paste("weekdays:", nrow(subset1), "weekends:", nrow(subset2)))
        
        feeders = c("marg_total", "D406.light", "marg_com_sum")
        
        print("--all--")
        for(feeder in feeders){
                mean_value = mean(target_data[[feeder]])
                print(paste(feeder, ":", round(mean_value,1) ,sep=" "))
        }
        print("--weekday--")
        for(feeder in feeders){
                mean_value = mean(subset1[[feeder]])
                print(paste(feeder, ":", round(mean_value,1) ,sep=" "))
        }
        print("--weekend--")
        for(feeder in feeders){
                mean_value = mean(subset2[[feeder]])
                print(paste(feeder, ":", round(mean_value,1) ,sep=" "))
        }
}


######################
### HCC functions ###
######################

hcc.add.summation = function(data){
        hcc_com_sum = as.matrix(data[-1]) %*% matrix(c(0,0,0,0,1,0,0,1,0,0,0,0,0))
        hcc_total = as.matrix(data[-1]) %*% matrix(c(0,1,1,1,1,0,1,1,0,1,1,1,0))
        
        data = cbind(data, hcc_com_sum)
        data = cbind(data, hcc_total)
        
        return(data)
}

hcc.mean.printer = function(data, timestamp1, timestamp2){
        
        index1 = which(hcc_day_5am_tidy$X  == timestamp1)
        index2 = which(hcc_day_5am_tidy$X  == timestamp2)
        
        target_data = data[index1:index2,]
        subset1 = subset(target_data, subset=target_data$type=="weekday")
        subset2 = subset(target_data, subset=target_data$type=="weekend")
        
        print(target_data$X)
        print(paste(index1, index2, "::", index2-index1+1, "days"))
        print(paste("weekdays:", nrow(subset1), "weekends:", nrow(subset2)))
        
        feeders = c("hcc_total", "D410.light", "hcc_com_sum")
        
        print("--all--")
        for(feeder in feeders){
                mean_value = mean(target_data[[feeder]])
                print(paste(feeder, ":", round(mean_value,1) ,sep=" "))
        }
        print("--weekday--")
        for(feeder in feeders){
                mean_value = mean(subset1[[feeder]])
                print(paste(feeder, ":", round(mean_value,1) ,sep=" "))
        }
        print("--weekend--")
        for(feeder in feeders){
                mean_value = mean(subset2[[feeder]])
                print(paste(feeder, ":", round(mean_value,1) ,sep=" "))
        }
}



######################
###  UX  functions ###
######################

ux.add.summation = function(data){
        ux_com_sum = as.matrix(data[-1]) %*% matrix(c(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
        ux_total = as.matrix(data[-1]) %*% matrix(c(1,1,1,1,1,1,1,1,1,1,1,1,1,0,1))
        
        data = cbind(data, ux_com_sum)
        data = cbind(data, ux_total)
        
        return(data)
}

ux.mean.printer = function(data, timestamp1, timestamp2){
        
        index1 = which(ux_day_5am_tidy$X  == timestamp1)
        index2 = which(ux_day_5am_tidy$X  == timestamp2)
        
        target_data = data[index1:index2,]
        subset1 = subset(target_data, subset=target_data$type=="weekday")
        subset2 = subset(target_data, subset=target_data$type=="weekend")
        
        print(target_data$X)
        print(paste(index1, index2, "::", index2-index1+1, "days"))
        print(paste("weekdays:", nrow(subset1), "weekends:", nrow(subset2)))
        
        feeders = c("ux_total", "D409.light", "ux_com_sum")
        
        print("--all--")
        for(feeder in feeders){
                mean_value = mean(target_data[[feeder]])
                print(paste(feeder, ":", round(mean_value,1) ,sep=" "))
        }
        print("--weekday--")
        for(feeder in feeders){
                mean_value = mean(subset1[[feeder]])
                print(paste(feeder, ":", round(mean_value,1) ,sep=" "))
        }
        print("--weekend--")
        for(feeder in feeders){
                mean_value = mean(subset2[[feeder]])
                print(paste(feeder, ":", round(mean_value,1) ,sep=" "))
        }
}

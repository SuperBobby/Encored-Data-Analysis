
data = RS_hcc
allowance = 2

class(RS_marg$joined)


# data$joined = as.numeric(data$joined)
# data$leaved = as.numeric(data$leaved)

data = cbind(data, used = rep(0,nrow(data)))

updated_joined = numeric(0)
updated_leaved = numeric(0)

merged_raw = numeric(0)

for(i in 1:(nrow(data)-1)){
# for(i in 1:100){
                
        print(paste(i, "/", nrow(data)))
#         print(data$leaved[i])
#         print(data$joined[i+1])

        
        if(data$used[i] == 1){
                next
        } else {
                data$used[i] = 1
                tmp_joined = data$joined[i]
                tmp_leaved = data$leaved[i]
                
                j = i + 1
                
                for(j in (i+1):nrow(data)){
                        # cat(paste(j, " "))
                        
                        if(data$used[j] == 1){
                                next
                        } else {
                                gap = difftime(data$joined[j], tmp_leaved, units = "secs")
                                
#                                 gap = data$joined[j] - tmp_leaved

                                if(gap > 0 & gap < allowance){
                                        print(paste("merged gap:", gap))
                                        print(paste(tmp_leaved, "as", data$leaved[j], "@", i))
                                        
                                        tmp_leaved = data$leaved[j]
                                        merged_raw = c(merged_raw, i)
                                        data$used[j] = 1
                                        
                                }
                        }
                }
                updated_joined = c(updated_joined, tmp_joined)
                updated_leaved = c(updated_leaved, tmp_leaved)
                cat("\n")
        }
}



result_dt = data.table(joined = as.POSIXct(updated_joined, origin = "1970-01-01", tz = "ROK"),
                       leaved = as.POSIXct(updated_leaved, origin = "1970-01-01", tz = "ROK"),
                       id = 1:nrow(result_dt),
                       duration = difftime(as.POSIXct(updated_leaved, origin = "1970-01-01", tz = "ROK"),
                                           as.POSIXct(updated_joined, origin = "1970-01-01", tz = "ROK"), units = "secs"))

length(merged_raw) / nrow(data)



# day segment plot
library(data.table)
library(ggplot2)
library(lubridate)

# target_date = "2016-02-24"
date_from = "2016-02-15"
date_to = "2016-02-21"


# RS_ux[, ':='(daily_id = NULL)]

convert2secs <- function(joined){
        result = (hour(joined) * 60 * 60) + (minute(joined) * 60) + second(joined)
        return(result)
}


RS_ux[, ':='(daily_id = 1:nrow(.SD)), by=as.Date(joined, tz="ROK")]
RS_ux[, ':='(secs = convert2secs(joined))]






dt = RS_ux

sub_dt = dt[as.Date(joined, tz="ROK") >= as.Date(date_from, tz="ROK") & as.Date(joined, tz="ROK") <= as.Date(date_to, tz="ROK")]
# sub_dt = dt[as.Date(joined, tz="ROK") == as.Date(target_date, tz="ROK")]


# boxplot(sub_dt)

x11()
ggplot(sub_dt) + 
        geom_segment(aes(x=joined, y=daily_id, xend=leaved, yend=daily_id), color="blue", size=5) +
        geom_text(aes(x=joined, y=daily_id, label=paste(daily_id,":",round(duration,1)), color = ifelse(duration < 1, TRUE, FALSE)), vjust=0) +
        scale_color_manual(values = c("red", "black")) +
        scale_y_reverse() + 
        ggtitle(paste("UX :", date_from, date_to))






# 
# data = RS_marg
# gaps = numeric(0)
# 
# for(i in 1:(nrow(data)-1)){
#         print(paste(i, "/", nrow(data)))
#         #         print(data$leaved[i])
#         #         print(data$joined[i+1])
#         j = i + 1
#         gap = difftime(data$joined[j], data$leaved[i], units = "secs")
#         gaps = c(gaps, gap)
# }
# 

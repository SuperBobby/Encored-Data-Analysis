# installing stringr
# install.packages("stringr")
# load stringr
library(stringr)

buildTimestamp <- function(string){
        
        month_label = c("Jan", "Feb", "Mar")
        
        splited = unlist(strsplit(string, " "))
        
        year  = splited[3]
        month = which(month_label == splited[1])
        day   = unlist(strsplit(splited[2], ","))
        
        result = paste(paste(year, month, day, sep="-"), splited[4])
        
        return(as.POSIXct(result))
}

fileName <- "data/WOL packets.txt"

timestamp = character(0)
mac_addr  = character(0)

conn <- file(fileName,open="r")
linn <-readLines(conn)
for (i in 1:length(linn)){
#         arrival_raw = 
#         wol_raw = grep(pattern = "Wake On LAN", linn[i], value = TRUE)
        
        if(regexpr(pattern = "Arrival", linn[i]) != -1){
                txtp = regexpr(pattern = ":", linn[i])
                time_string = substring(linn[i], txtp[1]+2, txtp[1]+22)
                timestamp = c(timestamp, buildTimestamp(time_string))
        }
        
        if(regexpr(pattern = "Wake On LAN", linn[i]) != -1){
                txtp = regexpr(pattern = "\\(", linn[i])
                mac = substring(linn[i], txtp[1]+1, txtp[1]+17)
                mac_addr = c(mac_addr, mac)
        }
}
close(conn)

wol_table = unique(data.frame(timestamp, mac_addr, stringsAsFactors = F))
wol_table$timestamp = as.POSIXct(as.numeric(wol_table$timestamp), origin="1970-01-01")

wol_table


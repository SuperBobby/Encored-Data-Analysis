#install.packages("rjson")
require("rjson")
source("Encored-Data-Analysis/getSNUdata.R")
library(ggplot2)
library(reshape2)

## Functions #########################################################

accumulateUsage <- function(x, unit=24, verbose=T) {
        timestamp = x[,1]
        data = x[,2]
        accUsage = numeric(0)
        accSum = 0
        i = 1
        
        for(i in 1:length(data)){
                accSum = accSum + data[i]
                accUsage = c(accUsage, accSum)
                
                if(i %% unit == 0) {
                        accSum = 0
                }
                if(i == length(data)){
                        break
                }
                if(verbose){
                        print(paste(i, timestamp[i]))
                }
        }
        return_table = data.frame(timestamp, accUsage)
        return(return_table)
}


getSavingRate <- function(x, shift, verbose=T) {
        last = x[1:(nrow(x)-shift),]
        this = x[(1+shift):nrow(x),]
        
        timestamp = this[,1]
        SavingRate = this[,2]/last[,2]
        
        return_table = data.frame(timestamp, SavingRate)
        print(summary(return_table))
        
        return(return_table)
}


getSectionCount <- function(x, unit=5, firstCut, secondCut, verbose=F) {
        
        xAxis = seq(from=0, to=1.2, by=(unit/100))
        for(i in 2:length(xAxis)){
                
                if(verbose) {
                        cat(paste(xAxis[i], ":", sum(x[,2] > xAxis[i-1] & x[,2] <= xAxis[i]), "\n"))
                        sum(x[,2] < xAxis[i] & x[,2] >= xAxis[i-1])
                }
                
                
                if(i == length(xAxis)){
                        if(verbose) {
                                cat(paste(xAxis[i], ":", sum(x[,2] > xAxis[i]), "\n"))
                        }
                }
        }
        if(missing(firstCut)){
                cat(paste("firstCut is missing. used defalut 0.95\n"))
                firstCut = 0.95
        }
        if(missing(secondCut)){
                cat(paste("secondCut is missing. used defalut 1.05\n"))
                secondCut = 1.05
        }
        
        firstSection  = sum(x[,2] > 0 & x[,2] <= firstCut)
        secondSection = sum(x[,2] > firstCut & x[,2] <= secondCut)
        thirdSection  = sum(x[,2] > secondCut)
        
        firstSectionRate  = round(firstSection/length(x[,2]),2)
        secondSectionRate = round(secondSection/length(x[,2]),2)
        thirdSectionRate  = round(thirdSection/length(x[,2]),2)
        
        cat(paste("0 ~", firstCut,          ":", firstSection, "(", firstSectionRate*100, "%)\n"))
        cat(paste(firstCut, "~", secondCut, ":", secondSection, "(", secondSectionRate*100, "%)\n"))
        cat(paste(secondCut,              "~ :", thirdSection,  "(", thirdSectionRate*100, "%)\n"))
        
        return(c(firstSection, secondSection,thirdSection))
}


plotTrafficLightFlow <- function(x, unit=24, lab, firstCut, secondCut, verbose=F){
        
        collectedData = numeric(0)
        
        t_span = paste(as.Date(x$timestamp[1], tz="ROK"), "~", as.Date(x$timestamp[length(x$timestamp)], tz="ROK"))
        
        for(i in 1:unit){
                index = seq(from=i, to=nrow(x), by=unit)
                print(i)
                collectedData = c(collectedData, getSectionCount(x[index,], firstCut = firstCut, secondCut = secondCut, verbose=F))
        }
        return_table = matrix(collectedData, nrow = unit, byrow = T)
        return_table = data.frame(factor(paste(1:24, 'h', sep=""), levels=paste(1:24, 'h', sep="")), return_table)
        
        names(return_table) = c("hour", "green", "yellow", "red")
        
        df_melted = melt(return_table, id.var="hour")        
        cols <- c("green", "yellow", "red")
        pt <- ggplot(df_melted, aes(x=hour, y=value, fill = variable)) +
                geom_bar(stat="identity") + 
                scale_fill_manual(values = cols) + 
                labs(y = "count") +
                ggtitle(paste(lab, ": cut", firstCut, "/", secondCut, "(", t_span, ")"))
        print(pt)        
}


## Implements ########################################################
lab_set = c("marg", "hcc", "ux")
resolution_set = c("quarters", "hours")

# lab = lab_set[1]
# resolution = resolution_set[2]

start = "2015-10-19"
cut = "2015-11-30"


# data loading from mongodb
marg_hours = getSNUData.sum("marg", "hours", start, cut, verbose = T)
hcc_hours = getSNUData.sum("hcc", "hours", start, cut, verbose = T)
ux_hours = getSNUData.sum("ux", "hours", start, cut, verbose = T)

marg_hours_acc = accumulateUsage(marg_hours, 24)
hcc_hours_acc = accumulateUsage(hcc_hours, 24)
ux_hours_acc = accumulateUsage(ux_hours, 24)

marg_hours_saving_rate = getSavingRate(marg_hours_acc, 24*7) # 24*7 : compare with 1 week before usage
hcc_hours_saving_rate = getSavingRate(hcc_hours_acc, 24*7)
ux_hours_saving_rate = getSavingRate(ux_hours_acc, 24*7)

fst = 1.0
snd = 1.20
getSectionCount(marg_hours_saving_rate, firstCut = fst, secondCut = snd)
getSectionCount(hcc_hours_saving_rate, firstCut = fst, secondCut = snd)
getSectionCount(ux_hours_saving_rate, firstCut = fst, secondCut = snd)

plotTrafficLightFlow(marg_hours_saving_rate, lab='marg', unit=24, firstCut = fst, secondCut = snd)
plotTrafficLightFlow(hcc_hours_saving_rate, lab='hcc', unit=24, firstCut = fst, secondCut = snd)
plotTrafficLightFlow(ux_hours_saving_rate, lab='ux', unit=24, firstCut = fst, secondCut = snd)

# 
# 
# write.csv(marg_forPlot, file="marg.csv", row.names = F)
# write.csv(marg_hours_saving_rate, file="margRate.csv", row.names = F)

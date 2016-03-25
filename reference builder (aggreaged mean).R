source("Encored-Data-Analysis/getSNUdata.R")
library(data.table)
library(splines)
library(ggplot2)
library(scales)
library(timeDate)

## last Season
last_season_start = "2015-12-28"
last_season_end = "2016-03-21"

## realtime ref
last_12weeks_marg_hours <- reviseSNUData(marg_defalut_table_hours, "marg", last_season_start, last_season_end, verbose = T)
last_12weeks_hcc_hours <- reviseSNUData(hcc_defalut_table_hours, "hcc", last_season_start, last_season_end, verbose = T)
last_12weeks_ux_hours <- reviseSNUData(ux_defalut_table_hours, "ux", last_season_start, last_season_end, verbose = T)

quantile(last_12weeks_marg_hours$total, .90) * 1.1   # 5.9114
quantile(last_12weeks_hcc_hours$total, .90) * 1.1    # 2.9348
quantile(last_12weeks_ux_hours$total, .90) * 1.1     # 2.64165


marg_lastSeason_15min <- reviseSNUData(marg_defalut_table_15min, "marg", last_season_start, last_season_end, verbose = T)
 hcc_lastSeason_15min <- reviseSNUData( hcc_defalut_table_15min, "hcc",  last_season_start, last_season_end, verbose = T)
  ux_lastSeason_15min <- reviseSNUData(  ux_defalut_table_15min, "ux",   last_season_start, last_season_end, verbose = T)


## MARG
data = marg_lastSeason_15min
head(data)
tail(data)

weekDAY = subset(data, subset=data$weekday == T)
weekEND = subset(data, subset=data$weekday == F)

new_index_weekDAY = rep(1:96, nrow(weekDAY)/96)
new_index_weekEND = rep(1:96, nrow(weekEND)/96)

weekDAY = cbind(weekDAY, new_index_weekDAY)
weekEND = cbind(weekEND, new_index_weekEND)

weekDAY_aggregated = aggregate(. ~ new_index_weekDAY,  weekDAY[,c(2,3,4,5,6,9)], mean)
weekDAY_aggregated = cumsum(weekDAY_aggregated)

weekEND_aggregated = aggregate(. ~ new_index_weekEND,  weekEND[,c(2,3,4,5,6,9)], mean)
weekEND_aggregated = cumsum(weekEND_aggregated)

target_date = "2015-3-23"
marg_ref_weekday <- get.spline.ref(marg_defalut_table_15min, target_date, weekday_=T)
marg_ref_weekend <- get.spline.ref(marg_defalut_table_15min, target_date, weekday_=F)

weekDAY_aggregated$hvac = marg_ref_weekday$hvac
weekEND_aggregated$hvac = marg_ref_weekend$hvac

weekDAY_aggregated$total = rowSums(weekDAY_aggregated[,2:5])
weekEND_aggregated$total = rowSums(weekEND_aggregated[,2:5])

weekDAY_aggregated
weekEND_aggregated

library(jsonlite)
toJSON(weekDAY_aggregated)
toJSON(weekEND_aggregated)








## HCC
data = hcc_lastSeason_15min
head(data)
tail(data)

weekDAY = subset(data, subset=data$weekday == T)
weekEND = subset(data, subset=data$weekday == F)

new_index_weekDAY = rep(1:96, nrow(weekDAY)/96)
new_index_weekEND = rep(1:96, nrow(weekEND)/96)

weekDAY = cbind(weekDAY, new_index_weekDAY)
weekEND = cbind(weekEND, new_index_weekEND)

weekDAY_aggregated = aggregate(. ~ new_index_weekDAY,  weekDAY[,c(2,3,4,5,6,9)], mean)
weekDAY_aggregated = cumsum(weekDAY_aggregated)

weekEND_aggregated = aggregate(. ~ new_index_weekEND,  weekEND[,c(2,3,4,5,6,9)], mean)
weekEND_aggregated = cumsum(weekEND_aggregated)


weekDAY_aggregated
weekEND_aggregated

toJSON(weekDAY_aggregated)
toJSON(weekEND_aggregated)




## UX
data = ux_lastSeason_15min
head(data)
tail(data)

weekDAY = subset(data, subset=data$weekday == T)
weekEND = subset(data, subset=data$weekday == F)

new_index_weekDAY = rep(1:96, nrow(weekDAY)/96)
new_index_weekEND = rep(1:96, nrow(weekEND)/96)

weekDAY = cbind(weekDAY, new_index_weekDAY)
weekEND = cbind(weekEND, new_index_weekEND)

weekDAY_aggregated = aggregate(. ~ new_index_weekDAY,  weekDAY[,c(2,3,4,5,6,9)], mean)
weekDAY_aggregated = cumsum(weekDAY_aggregated)

weekEND_aggregated = aggregate(. ~ new_index_weekEND,  weekEND[,c(2,3,4,5,6,9)], mean)
weekEND_aggregated = cumsum(weekEND_aggregated)


weekDAY_aggregated
weekEND_aggregated

toJSON(weekDAY_aggregated)
toJSON(weekEND_aggregated)



















# sum(weekDAY_aggregated$total)
# sum(weekEND_aggregated$total)
# 
# sum(weekDAY_aggregated$computer)
# sum(weekDAY_aggregated$light)
# sum(weekDAY_aggregated$hvac)
# 
# sum(weekEND_aggregated$computer)
# sum(weekEND_aggregated$light)
# sum(weekEND_aggregated$hvac)

# ## data.frame to JSON
# # install.packages("df2json")
# library(df2json)
# df2json(weekDAY_aggregated)


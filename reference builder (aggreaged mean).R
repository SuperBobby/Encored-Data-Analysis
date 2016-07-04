source("Encored-Data-Analysis/getSNUdata.R")
library(data.table)
library(splines)
library(ggplot2)
library(scales)
library(timeDate)
library(stringr)

        ## last Season
last_season_start = "2016-3-21"
last_season_end = "2016-6-13"

# spline target date
target_date = "2015-6-15"
                
## realtime ref
last_12weeks_marg_hours <- reviseSNUData(marg_defalut_table_hours, "marg", last_season_start, last_season_end, verbose = T)
last_12weeks_hcc_hours <- reviseSNUData(hcc_defalut_table_hours, "hcc", last_season_start, last_season_end, verbose = T)
last_12weeks_ux_hours <- reviseSNUData(ux_defalut_table_hours, "ux", last_season_start, last_season_end, verbose = T)

marg_lastSeason_15min <- reviseSNUData(marg_defalut_table_15min, "marg", last_season_start, last_season_end, verbose = T)
 hcc_lastSeason_15min <- reviseSNUData( hcc_defalut_table_15min, "hcc",  last_season_start, last_season_end, verbose = T)
  ux_lastSeason_15min <- reviseSNUData(  ux_defalut_table_15min, "ux",   last_season_start, last_season_end, verbose = T)


## MARG
# data = marg_lastSeason_15min
head(marg_lastSeason_15min)
tail(marg_lastSeason_15min)

marg_weekDAY = subset(marg_lastSeason_15min, subset=marg_lastSeason_15min$weekday == T)
marg_weekEND = subset(marg_lastSeason_15min, subset=marg_lastSeason_15min$weekday == F)

marg_new_index_weekDAY = rep(1:96, nrow(marg_weekDAY)/96)
marg_new_index_weekEND = rep(1:96, nrow(marg_weekEND)/96)

marg_weekDAY = cbind(marg_weekDAY, marg_new_index_weekDAY)
marg_weekEND = cbind(marg_weekEND, marg_new_index_weekEND)

marg_weekDAY_aggregated = aggregate(. ~ marg_new_index_weekDAY,  marg_weekDAY[,c(2,3,4,5,6,9)], mean)
marg_weekDAY_aggregated = cumsum(marg_weekDAY_aggregated)

marg_weekEND_aggregated = aggregate(. ~ marg_new_index_weekEND,  marg_weekEND[,c(2,3,4,5,6,9)], mean)
marg_weekEND_aggregated = cumsum(marg_weekEND_aggregated)


marg_marg_ref_weekday <- get.spline.ref(marg_defalut_table_15min, target_date, weekday_=T)
marg_marg_ref_weekend <- get.spline.ref(marg_defalut_table_15min, target_date, weekday_=F)

marg_weekDAY_aggregated$hvac = marg_ref_weekday$hvac
marg_weekEND_aggregated$hvac = marg_ref_weekend$hvac

marg_weekDAY_aggregated$total = rowSums(marg_weekDAY_aggregated[,2:5])
marg_weekEND_aggregated$total = rowSums(marg_weekEND_aggregated[,2:5])


## HCC
# data = hcc_lastSeason_15min
head(hcc_lastSeason_15min)
tail(hcc_lastSeason_15min)

hcc_weekDAY = subset(hcc_lastSeason_15min, subset=hcc_lastSeason_15min$weekday == T)
hcc_weekEND = subset(hcc_lastSeason_15min, subset=hcc_lastSeason_15min$weekday == F)

hcc_new_index_weekDAY = rep(1:96, nrow(hcc_weekDAY)/96)
hcc_new_index_weekEND = rep(1:96, nrow(hcc_weekEND)/96)

hcc_weekDAY = cbind(hcc_weekDAY, hcc_new_index_weekDAY)
hcc_weekEND = cbind(hcc_weekEND, hcc_new_index_weekEND)

hcc_weekDAY_aggregated = aggregate(. ~ hcc_new_index_weekDAY, hcc_weekDAY[,c(2,3,4,5,6,9)], mean)
hcc_weekDAY_aggregated = cumsum(hcc_weekDAY_aggregated)

hcc_weekEND_aggregated = aggregate(. ~ hcc_new_index_weekEND, hcc_weekEND[,c(2,3,4,5,6,9)], mean)
hcc_weekEND_aggregated = cumsum(hcc_weekEND_aggregated)



## UX
# data = ux_lastSeason_15min
head(ux_lastSeason_15min)
tail(ux_lastSeason_15min)

ux_weekDAY = subset(ux_lastSeason_15min, subset=ux_lastSeason_15min$weekday == T)
ux_weekEND = subset(ux_lastSeason_15min, subset=ux_lastSeason_15min$weekday == F)

ux_new_index_weekDAY = rep(1:96, nrow(ux_weekDAY)/96)
ux_new_index_weekEND = rep(1:96, nrow(ux_weekEND)/96)

ux_weekDAY = cbind(ux_weekDAY, ux_new_index_weekDAY)
ux_weekEND = cbind(ux_weekEND, ux_new_index_weekEND)

ux_weekDAY_aggregated = aggregate(. ~ ux_new_index_weekDAY,  ux_weekDAY[,c(2,3,4,5,6,9)], mean)
ux_weekDAY_aggregated = cumsum(ux_weekDAY_aggregated)

ux_weekEND_aggregated = aggregate(. ~ ux_new_index_weekEND,  ux_weekEND[,c(2,3,4,5,6,9)], mean)
ux_weekEND_aggregated = cumsum(ux_weekEND_aggregated)




# to console 

quantile(last_12weeks_marg_hours$total, .90) * 1.1 
quantile(last_12weeks_hcc_hours$total, .90) * 1.1  
quantile(last_12weeks_ux_hours$total, .90) * 1.1   

collection = numeric(0)

collection = c(collection, marg_weekDAY_aggregated[96,"computer"])
collection = c(collection, marg_weekDAY_aggregated[96,"light"])
collection = c(collection, marg_weekDAY_aggregated[96,"hvac"])
collection = c(collection, marg_weekDAY_aggregated[96,"total"])

collection = c(collection, marg_weekEND_aggregated[96,"computer"])
collection = c(collection, marg_weekEND_aggregated[96,"light"])
collection = c(collection, marg_weekEND_aggregated[96,"hvac"])
collection = c(collection, marg_weekEND_aggregated[96,"total"])

collection = c(collection, hcc_weekDAY_aggregated[96,"computer"])
collection = c(collection, hcc_weekDAY_aggregated[96,"light"])
collection = c(collection, hcc_weekDAY_aggregated[96,"total"])

collection = c(collection, hcc_weekEND_aggregated[96,"computer"])
collection = c(collection, hcc_weekEND_aggregated[96,"light"])
collection = c(collection, hcc_weekEND_aggregated[96,"total"])

collection = c(collection, ux_weekDAY_aggregated[96,"computer"])
collection = c(collection, ux_weekDAY_aggregated[96,"light"])
collection = c(collection, ux_weekDAY_aggregated[96,"total"])

collection = c(collection, ux_weekEND_aggregated[96,"computer"])
collection = c(collection, ux_weekEND_aggregated[96,"light"])
collection = c(collection, ux_weekEND_aggregated[96,"total"])

collection

library(jsonlite)

toJSON(marg_weekDAY_aggregated)
toJSON(marg_weekEND_aggregated)

toJSON(hcc_weekDAY_aggregated)
toJSON(hcc_weekEND_aggregated)

toJSON(ux_weekDAY_aggregated)
toJSON(ux_weekEND_aggregated)



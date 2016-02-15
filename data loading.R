

###########################################
## Default data tables (15min & hours)
###########################################
start = "2014-09-01"
end = "2016-01-15"
marg_defalut_table_15min <- getSNUData.feeder("marg", "quarters", start, end, verbose = T)
 hcc_defalut_table_15min <- getSNUData.feeder("hcc",  "quarters", start, end, verbose = T)
  ux_defalut_table_15min <- getSNUData.feeder("ux",   "quarters", start, end, verbose = T)

marg_defalut_table_hours <- getSNUData.feeder("marg", "hours", start, end, verbose = T)
 hcc_defalut_table_hours <- getSNUData.feeder("hcc",  "hours", start, end, verbose = T)
  ux_defalut_table_hours <- getSNUData.feeder("ux",   "hours", start, end, verbose = T)

##################################################
## Update the default data tables (15min & hours)
##################################################
source("Encored-Data-Analysis/getSNUdata.R")
update_start = "2014-09-01"
update_end = "2016-02-14"

marg_defalut_table_15min <- reviseSNUData(marg_defalut_table_15min, "marg", update_start, update_end, verbose = T)
 hcc_defalut_table_15min <- reviseSNUData( hcc_defalut_table_15min, "hcc",  update_start, update_end, verbose = T)
  ux_defalut_table_15min <- reviseSNUData(  ux_defalut_table_15min, "ux",   update_start, update_end, verbose = T)

marg_defalut_table_hours <- reviseSNUData(marg_defalut_table_hours, "marg", update_start, update_end, verbose = T)
 hcc_defalut_table_hours <- reviseSNUData( hcc_defalut_table_hours, "hcc",  update_start, update_end, verbose = T)
  ux_defalut_table_hours <- reviseSNUData(  ux_defalut_table_hours, "ux",   update_start, update_end, verbose = T)

###########################################
## save $ load the data tables
###########################################
save(marg_defalut_table_15min, file ="data/marg_15min.RData")
save( hcc_defalut_table_15min, file ="data/hcc_15min.RData")
save(  ux_defalut_table_15min, file ="data/ux_15min.RData")

save(marg_defalut_table_hours, file ="data/marg_hours.RData")
save( hcc_defalut_table_hours, file ="data/hcc_hours.RData")
save(  ux_defalut_table_hours, file ="data/ux_hours.RData")

write.csv(marg_defalut_table_15min, file ="data/marg_15min.csv")
write.csv( hcc_defalut_table_15min, file ="data/hcc_15min.csv")
write.csv(  ux_defalut_table_15min, file ="data/ux_15min.csv")

write.csv(marg_defalut_table_hours, file ="data/marg_hours.csv")
write.csv( hcc_defalut_table_hours, file ="data/hcc_hours.csv")
write.csv(  ux_defalut_table_hours, file ="data/ux_hours.csv")


load("data/marg_15min.RData")
load("data/hcc_15min.RData")
load("data/ux_15min.RData")

load("data/marg_hours.RData")
load("data/hcc_hours.RData")
load("data/ux_hours.RData")

# write.csv(marg_defalut_table_15min, "data/marg_15min.csv")
# write.csv(hcc_defalut_table_15min, "data/hcc_15min.csv")
# write.csv(ux_defalut_table_15min, "data/ux_15min.csv")

# write.csv(marg_defalut_table_hours, "data/marg_hour.csv")
# write.csv(hcc_defalut_table_hours, "data/hcc_hour.csv")
# write.csv(ux_defalut_table_hours, "data/ux_hour.csv")

###########################################
## preprocessing
###########################################


## to-do...


##################################################
## Day table
##################################################
day_start = "2014-09-01"
day_end   = "2016-01-09"

marg_table_day = getSNUData.feeder.day("marg", day_start, day_end)
hcc_table_day = getSNUData.feeder.day( "hcc", day_start, day_end)
ux_table_day = getSNUData.feeder.day(  "ux", day_start, day_end)



# ## Implements ###########################
lab_set = c("marg", "hcc", "ux")
resolution_set = c("quarters", "hours")
# 
# # lab = lab_set[1]
# # resolution = resolution_set[2]
start = "2015-10-01"
end = "2015-10-02"
# 
# # data loading from mongodb
marg_hours = getSNUData.sum("marg", resolution_set[2], start, end, verbose = T)
# hcc_hours = getSNUData.sum("hcc", "hours", start, end, verbose = T)
# ux_hours = getSNUData.sum("ux", "hours", start, end, verbose = T)




tmp <- getSNUData.feeder("marg", "quarters", "2015-07-01", "2015-07-02", verbose = T)

tmp <- getSNUData.feeder("marg", "hours", "2015-07-01", "2015-07-02", verbose = T)


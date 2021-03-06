source("getSNUdata.R")

# ###########################################
# ## Default data tables (15min & hours)
# ###########################################
# start = "2014-09-01"
# end = "2016-10-20"
# marg_defalut_table_15min <- getSNUData.feeder("marg", "quarters", start, end, verbose = T)
#  hcc_defalut_table_15min <- getSNUData.feeder("hcc",  "quarters", start, end, verbose = T)
#   ux_defalut_table_15min <- getSNUData.feeder("ux",   "quarters", start, end, verbose = T)
# 
# marg_defalut_table_hours <- getSNUData.feeder("marg", "hours", start, end, verbose = T)
#  hcc_defalut_table_hours <- getSNUData.feeder("hcc",  "hours", start, end, verbose = T)
#   ux_defalut_table_hours <- getSNUData.feeder("ux",   "hours", start, end, verbose = T)

##################################################
## Update the default data tables (15min & hours)
##################################################
source("getSNUdata.R")
update_start = "2014-09-01"
update_end = "2016-10-20"

marg_defalut_table_15min <- reviseSNUData(marg_defalut_table_15min, "marg", update_start, update_end, verbose = T)
 hcc_defalut_table_15min <- reviseSNUData( hcc_defalut_table_15min, "hcc",  update_start, update_end, verbose = T)
  ux_defalut_table_15min <- reviseSNUData(  ux_defalut_table_15min, "ux",   update_start, update_end, verbose = T)

marg_defalut_table_hours <- reviseSNUData(marg_defalut_table_hours, "marg", update_start, update_end, verbose = T)
 hcc_defalut_table_hours <- reviseSNUData( hcc_defalut_table_hours, "hcc",  update_start, update_end, verbose = T)
  ux_defalut_table_hours <- reviseSNUData(  ux_defalut_table_hours, "ux",   update_start, update_end, verbose = T)

###########################################
## save $ load the data tables
###########################################
save(marg_defalut_table_15min, file ="../data/raw/marg_15min.RData")
save( hcc_defalut_table_15min, file ="../data/raw/hcc_15min.RData")
save(  ux_defalut_table_15min, file ="../data/raw/ux_15min.RData")

save(marg_defalut_table_hours, file ="../data/raw/marg_hours.RData")
save( hcc_defalut_table_hours, file ="../data/raw/hcc_hours.RData")
save(  ux_defalut_table_hours, file ="../data/raw/ux_hours.RData")

write.csv(marg_defalut_table_15min, file ="../data/raw/marg_15min.csv")
write.csv( hcc_defalut_table_15min, file ="../data/raw/hcc_15min.csv")
write.csv(  ux_defalut_table_15min, file ="../data/raw/ux_15min.csv")

write.csv(marg_defalut_table_hours, file ="../data/raw/marg_hours.csv")
write.csv( hcc_defalut_table_hours, file ="../data/raw/hcc_hours.csv")
write.csv(  ux_defalut_table_hours, file ="../data/raw/ux_hours.csv")

load("../data/raw/marg_15min.RData")
load("../data/raw/hcc_15min.RData")
load("../data/raw/ux_15min.RData")

load("../data/raw/marg_hours.RData")
load("../data/raw/hcc_hours.RData")
load("../data/raw/ux_hours.RData")

# write.csv(marg_defalut_table_15min, "../data/raw/marg_15min.csv")
# write.csv(hcc_defalut_table_15min, "../data/raw/hcc_15min.csv")
# write.csv(ux_defalut_table_15min, "../data/raw/ux_15min.csv")

# write.csv(marg_defalut_table_hours, "../data/raw/marg_hour.csv")
# write.csv(hcc_defalut_table_hours, "../data/raw/hcc_hour.csv")
# write.csv(ux_defalut_table_hours, "../data/raw/ux_hour.csv")

###########################################
## preprocessing
###########################################


## to-do...






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


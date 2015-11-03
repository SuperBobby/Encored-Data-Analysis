source("codes/getSNUdata.R")

###########################################
## Default data tables 
###########################################
start = "2014-09-01"
end = "2015-10-30"
# start = "2015-09-01"
# end = "2015-10-01"
marg_defalut_table_15min <- getSNUData.feeder("marg", "quarters", start, end, verbose = T)
 hcc_defalut_table_15min <- getSNUData.feeder("hcc",  "quarters", start, end, verbose = T)
  ux_defalut_table_15min <- getSNUData.feeder("ux",   "quarters", start, end, verbose = T)

marg_defalut_table_hours <- getSNUData.feeder("marg", "hours", start, end, verbose = T)
hcc_defalut_table_hours <- getSNUData.feeder("hcc",  "hours", start, end, verbose = T)
ux_defalut_table_hours <- getSNUData.feeder("ux",   "hours", start, end, verbose = T)

###########################################
## Update the default data tables
###########################################
update_start = "2014-09-01"
update_end = "2015-11-03"
marg_defalut_table_15min <- loadSNUData(marg_defalut_table_15min, "marg", update_start, update_end, verbose = T)
 hcc_defalut_table_15min <- loadSNUData( hcc_defalut_table_15min, "hcc",  update_start, update_end, verbose = T)
  ux_defalut_table_15min <- loadSNUData(  ux_defalut_table_15min, "ux",   update_start, update_end, verbose = T)

write.csv(marg_defalut_table_15min, "data/marg_15min.csv")
write.csv(hcc_defalut_table_15min, "data/hcc_15min.csv")
write.csv(ux_defalut_table_15min, "data/ux_15min.csv")


# write.csv(marg_defalut_table_hours, "data/marg_hour.csv")
# write.csv(hcc_defalut_table_hours, "data/hcc_hour.csv")
# write.csv(ux_defalut_table_hours, "data/ux_hour.csv")

###########################################
## preprocessing
###########################################






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

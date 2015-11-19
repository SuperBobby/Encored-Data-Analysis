### average usage per weekday

target_start
target_end

# marg
marg_avg_target <- loadSNUData(marg_defalut_table_hours, "marg", target_start, target_end, verbose = T)

marg_day = aggregate(sum ~ date, 
                     data = cbind(marg_defalut_table_hours, date = as.character(as.Date(marg_defalut_table_hours$timestamp))), 
                     sum)
marg_day = cbind(marg_day, day = weekdays(as.Date(as.character(marg_day$date))))

marg_avg_dayUsage = aggregate(sum ~ day, 
                              data = marg_day, mean)


# hcc
hcc_defalut_table_hours <- loadSNUData(hcc_defalut_table_hours, "hcc",  target_start, target_end, verbose = T)

hcc_day = aggregate(sum ~ date, 
                    data = cbind(hcc_defalut_table_hours, date = as.character(as.Date(hcc_defalut_table_hours$timestamp))), 
                    sum)
hcc_day = cbind(hcc_day, day = weekdays(as.Date(as.character(hcc_day$date))))

hcc_avg_dayUsage = aggregate(sum ~ day, 
                             data = hcc_day, mean)


# ux
ux_defalut_table_hours <- loadSNUData(ux_defalut_table_hours, "ux",   target_start, target_end, verbose = T)

ux_day = aggregate(sum ~ date, 
                    data = cbind(ux_defalut_table_hours, date = as.character(as.Date(ux_defalut_table_hours$timestamp))), 
                    sum)
ux_day = cbind(ux_day, day = weekdays(as.Date(as.character(ux_day$date))))

ux_avg_dayUsage = aggregate(sum ~ day, 
                             data = ux_day, mean)
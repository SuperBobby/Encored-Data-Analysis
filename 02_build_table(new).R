### --------------------------- ###
### Build tables and functions for extraction & aggregation
### return_dts : peak, base, average, lightON(count), threshold80(ratio;(# of 15min blocks over 80% of peak / # of lightON 15min blocks))
### MARG_return_dts : the same as 'return_dts', but for MARG exp1,2 

## four stat : peak, base, avg, mid --> c(1,2,3,4)
get.four.stats <- function(usage, type){
  peak = quantile(usage, .90, na.rm = T)
  base = quantile(usage, .10, na.rm = T)
  avg  = mean(usage, na.rm = T)
  med  = median(usage, na.rm = T)
  
  result=c(peak, base, avg, med)
  return(result[type])
}

filter.fault.partial.lightOn <- function(input){
  light = na.locf(input)
  
  for(i in 2:(length(light)-1)){
    
    if(sum(light[(i-1):(i+1)]) == 1){
      light[i] = 0
    }
  }
  return(light)
}
# 
# # 1. lab
# lab_names = c("MARG", "HCC", "UX", "All Labs")
# 
# # 2. aggregation unit 
# agg_Units = c("aggWeek", "aggDay")
# 
# # 3. day selection
# day_selections = c("allDay", "weekDay", "weekEnd")
# 
# # 4. feeders
# feeders = c("total", "computer", "light", "hvac")
# # feeders = c("hvac")
# 
# 
# return_dts = list(0)
# 
# for(lab in 1:4){ 
#   # for(lab in 1){
#   #   lab_dt & name selection 
#   lab_dt = dt_list[[lab]]
#   #   lab_dt = marg_dt
#   lab_name = lab_names[lab]
#   
#   for(agg_Unit in agg_Units){
#     # aggregation unit selection
#     # agg_Units = c("aggWeek", "aggDay")
#     # get(agg_Unit)
#     
#     for(day_selection in day_selections){
#       
#       # day selection
#       # print(day_selection)
#       
#       for(feeder in feeders){
#         # feeder selection
#         # get(feeder)
#         
#         dt_name = paste(lab_name, agg_Unit, day_selection, feeder, sep="_")
#         print(dt_name)
#         
#         if(day_selection == "allDay"){
#           
#           return_dt = lab_dt[, .(peak = get.four.stats(get(feeder), 1),
#                                  base = get.four.stats(get(feeder), 2),
#                                  avg  = get.four.stats(get(feeder), 3)),
#                              by=get(agg_Unit)]
#           
#         } else if(day_selection == "weekDay") {
#           
#           return_dt = lab_dt[weekday == T, .(peak = get.four.stats(get(feeder), 1),
#                                              base = get.four.stats(get(feeder), 2),
#                                              avg  = get.four.stats(get(feeder), 3)), 
#                              by=get(agg_Unit)]
#           
#         } else if(day_selection == "weekEnd") {
#           
#           return_dt = lab_dt[weekday == F, .(peak = get.four.stats(get(feeder), 1),
#                                              base = get.four.stats(get(feeder), 2),
#                                              avg  = get.four.stats(get(feeder), 3)),
#                              by=get(agg_Unit)]
#         }
#         
#         light_min = 0.01
#         light_peak = quantile(lab_dt$light, .90, na.rm = T)
#         light_dt = lab_dt[, .(timestamp = timestamp,
#                               aggDay = aggDay,
#                               aggWeek = aggWeek,
#                               light = na.locf(light),
#                               lightON = ifelse(na.locf(light) > light_min, 1, 0),
#                               peak_80 = filter.fault.partial.lightOn(ifelse((na.locf(light) < light_peak * 0.8) & (na.locf(light) > light_min), 1, 0)))]
#         
#         partial_light = light_dt[, .(lightON = sum(lightON),
#                                      threshold80 = ifelse(sum(lightON)==0,1,sum(peak_80)/sum(lightON))), by=get(agg_Unit)]
#         
#         if(agg_Unit == "aggWeek"){
#           partial_light = partial_light[, .(get = get,
#                                             lightON = lightON*15.0/60.0/7.0,
#                                             threshold80 = threshold80*100)]
#         } else{
#           partial_light = partial_light[, .(get = get,
#                                             lightON = lightON*15.0/60.0,
#                                             threshold80 = threshold80*100)]
#         }
#         
#         return_dt = merge(return_dt, partial_light, by="get")
#         
#         assign(dt_name, return_dt)
#         return_dts = append(return_dts, setNames(list(return_dt),dt_name))
#       }
#     }
#   }
# }
# 
# 
# ## making MARG_return_dts for exp1,2
# MARG_return_dts = list(0)
# 
# for(lab in 1){ 
#   lab_dt = dt_list[[lab]]
#   lab_dt = lab_dt[aggDay >= "2014-09-11" & aggDay < "2015-06-01"]
#   lab_dt[, ':='(total = total-hvac)]
#   lab_name = lab_names[lab]
#   
#   for(agg_Unit in agg_Units){
#     # aggregation unit selection
#     # agg_Units = c("aggWeek", "aggDay")
#     # get(agg_Unit)
#     
#     for(day_selection in day_selections){
#       
#       # day selection
#       # print(day_selection)
#       
#       for(feeder in feeders){
#         # feeder selection
#         # get(feeder)
#         
#         dt_name = paste(lab_name, agg_Unit, day_selection, feeder, sep="_")
#         print(dt_name)
#         
#         if(day_selection == "allDay"){
#           
#           return_dt = lab_dt[, .(peak = get.four.stats(get(feeder), 1),
#                                  base = get.four.stats(get(feeder), 2),
#                                  avg  = get.four.stats(get(feeder), 3)),
#                              by=get(agg_Unit)]
#           
#         } else if(day_selection == "weekDay") {
#           
#           return_dt = lab_dt[weekday == T, .(peak = get.four.stats(get(feeder), 1),
#                                              base = get.four.stats(get(feeder), 2),
#                                              avg  = get.four.stats(get(feeder), 3)), 
#                              by=get(agg_Unit)]
#           
#         } else if(day_selection == "weekEnd") {
#           
#           return_dt = lab_dt[weekday == F, .(peak = get.four.stats(get(feeder), 1),
#                                              base = get.four.stats(get(feeder), 2),
#                                              avg  = get.four.stats(get(feeder), 3)),
#                              by=get(agg_Unit)]
#         }
#         
#         light_min = 0.01
#         light_peak = quantile(lab_dt$light, .90, na.rm = T)
#         light_dt = lab_dt[, .(timestamp = timestamp,
#                               aggDay = aggDay,
#                               aggWeek = aggWeek,
#                               light = na.locf(light),
#                               lightON = ifelse(na.locf(light) > light_min, 1, 0),
#                               peak_80 = filter.fault.partial.lightOn(ifelse((na.locf(light) < light_peak * 0.8) & (na.locf(light) > light_min), 1, 0)))]
#         
#         partial_light = light_dt[, .(lightON = sum(lightON),
#                                      threshold80 = ifelse(sum(lightON)==0,1,sum(peak_80)/sum(lightON))), by=get(agg_Unit)]
#         
#         if(agg_Unit == "aggWeek"){
#           partial_light = partial_light[, .(get = get,
#                                             lightON = lightON*15.0/60.0/7.0,
#                                             threshold80 = threshold80*100)]
#         } else{
#           partial_light = partial_light[, .(get = get,
#                                             lightON = lightON*15.0/60.0,
#                                             threshold80 = threshold80*100)]
#         }
#         
#         return_dt = merge(return_dt, partial_light, by="get")
#         
#         assign(dt_name, return_dt)
#         MARG_return_dts = append(MARG_return_dts, setNames(list(return_dt),dt_name))
#       }
#     }
#   }
# }
# 
# 
# ########################
# ## RealSense table
# ########################
# 
# return_rs_dts = list(0)
# 
# # for(lab in 1:4){ 
# for(lab in 1:4){
#   # lab_dt & name selection 
#   lab_dt = dt_list[[lab]]
#   lab_name = lab_names[lab]
#   
#   for(agg_Unit in agg_Units){
#     # aggregation unit selection
#     # agg_Units = c("aggWeek", "aggDay")
#     # get(agg_Unit)
#     
#     for(day_selection in day_selections){
#       
#       # day selection
#       # print(day_selection)
#       
#       dt_name = paste(lab_name, agg_Unit, day_selection, sep="_")
#       print(dt_name)
#       
#       if(day_selection == "allDay"){
#         
#         return_rs_dt = lab_dt[, .(sum_of_duration = sum(sum_of_duration, na.rm=T),
#                                   freq = sum(freq, na.rm=T))
#                               , by=get(agg_Unit)]
#         
#       } else if(day_selection == "weekDay") {
#         
#         return_rs_dt = lab_dt[weekday == T, .(sum_of_duration = sum(sum_of_duration, na.rm=T), 
#                                               freq = sum(freq, na.rm=T))
#                               , by=get(agg_Unit)]
#         
#       } else if(day_selection == "weekEnd") {
#         
#         return_rs_dt = lab_dt[weekday == F, .(sum_of_duration = sum(sum_of_duration, na.rm=T), 
#                                               freq = sum(freq, na.rm=T))
#                               , by=get(agg_Unit)]
#       }
#       
#       assign(dt_name, return_rs_dt)
#       return_rs_dts = append(return_rs_dts, setNames(list(return_rs_dt),dt_name))
#     }
#   }
# }
# 
# 
# 
# ########################
# ## Weather table
# ########################
# weather_dt = fread("../rawData/Suwon_weather.csv")
# weather_dt$date_index = as.Date(weather_dt$date_index)
# setnames(weather_dt,old="date_index",new="get")
# str(weather_dt)
# 
# weather_dt <- weather_dt[, ':='(aggDay = get,
#                                 aggWeek = as.Date(cut(get, breaks = "week", start.on.monday = T)))]
# aggWeek_weather_dt <- weather_dt[, .(get = aggWeek,
#                                      avg_temp = mean(avg_temp),
#                                      max_temp = mean(max_temp),
#                                      min_temp = mean(min_temp)), by=aggWeek] 
# aggWeek_weather_dt <- aggWeek_weather_dt[, aggWeek:=NULL]
# 
# 
# 
# 

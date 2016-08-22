load("../rawData/marg_hours.RData")
load("../rawData/hcc_hours.RData")
load("../rawData/ux_hours.RData")

marg_defalut_table_hours[1:2,]
marg_defalut_table_hours[1,]
sum(marg_defalut_table_15min[1:4,'light'])


dt = marg_dt

plot.lunch.saving(marg_dt)
plot.lunch.saving(hcc_dt)
plot.lunch.saving(ux_dt)

target='light'



plot.lunch.saving('marg', marg_dt, 'light')
plot.lunch.saving('hcc', hcc_dt, 'light')
plot.lunch.saving('ux', ux_dt, 'light')

plot.lunch.saving('marg', marg_dt, 'computer')
plot.lunch.saving('hcc', hcc_dt, 'computer')
plot.lunch.saving('ux', ux_dt, 'computer')


# 마그 지난 1월 점심시간 컴퓨터 절전 대박

plot.lunch.saving <- function(lab, dt, target){    
  
  # * before lunch usage: 11:00 ~ 12:00 — index 17:20
  # * during lunch usage: 11:30 ~ 13:30 — index 19:26
  # *  after lunch usage: 13:00 ~ 14:00 — index 25:28
  before_lunch_index = 17:20
  during_lunch_index = 19:26
  after_lunch_index  = 25:28
  
  peak_of_target = quantile(data.frame(dt[, .(get(target))])[,1], .90)
  target_on_threshold = peak_of_target * 0.1
  
  lunch_saving_threshold_ratio = 0.8
  target_on_threshold_usage = peak_of_target * 0.1
  
  before_dt = dt[index %in% before_lunch_index, .(before_lunch = max(get(target))), by=aggDay]
  during_dt = dt[index %in% during_lunch_index, .(during_lunch = min(get(target))), by=aggDay]
  after_dt = dt[index %in%  after_lunch_index, .( after_lunch = max(get(target))), by=aggDay]
  
  lunch_dt = merge(before_dt, during_dt, by='aggDay')
  lunch_dt = merge(lunch_dt, after_dt, by='aggDay')
  
  ## Conditions of lunch saving 
  lunch_dt[(during_lunch < (before_lunch * lunch_saving_threshold_ratio)) 
           & (during_lunch < (after_lunch * lunch_saving_threshold_ratio)) 
           & (before_lunch > light_on_threshold_usage), 
           ':='(lunch_saving = 1), by=aggDay]
  
  ## aggregation: week table 'lunch_saving_per_week' 
  lunch_dt[, ':='(aggWeek=as.Date(cut(aggDay, breaks = "week", start.on.monday = T)))]
  lunch_saving_per_week = lunch_dt[, .(lunch_saving_count = sum(lunch_saving, na.rm = T)), by=aggWeek]
  
  ## plot 
  p1 = ggplot(lunch_saving_per_week, aes(x=aggWeek)) +
    geom_point(aes(y=lunch_saving_count, color='count')) +
#     geom_jitter(aes(y=lunch_saving_count, color='count')) +
    geom_smooth(aes(y=lunch_saving_count, color='count'), method='auto', span = 0.5) +
    ggtitle(paste(lab, target))
  
  p1 = add.event.vline(p1)
  print(p1)
}

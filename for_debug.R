tmp = summary_list$MARG_allDay_total_avg$`1-2`

data.frame(tmp)[,2]

names(summary_list)


representation_func = mean



for(category in names(summary_list)[1:1]){
  
  one_category = summary_list[[category]]
  
  for(exp_label in names(one_category)){
  
    exp_dt = one_category[[exp_label]]
  
    exp_data = data.frame(exp_dt)[,2]
    
    representative_value = representation_func(exp_data)
    
    
    print(paste(category, exp_label))
    print(exp_dt)
    print(exp_data)
    print(representative_value)
  }
}
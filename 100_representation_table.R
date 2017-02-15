### -------------------------------- ###
### representation_table
### -------------------------------- ### 

## initialize representation_table 
representation_table = data.frame(exp_label = names(get.labeled.expDate.all()))
# representation_func  = mean

for(category in names(summary_list)){
  
  one_category = summary_list[[category]]
  
  category_values = character(0)
  
  for(exp_label in names(one_category)){
    
    exp_dt = one_category[[exp_label]]
    
    exp_data = data.frame(exp_dt)[,2]
    
    # representative_value = paste0(round(mean(exp_data),1), '(', round(sd(exp_data),1), ') : ', length(exp_data))
    # representative_value = paste0(round(mean(exp_data),2), ' (', round(sd(exp_data),2), ')')
    representative_value = round(mean(exp_data),2)
    
    category_values = c(category_values, representative_value)
    # names(category_values)[length(category_values)] = exp_label
    
    print(paste(category, exp_label))
    # print(exp_dt)
    # print(exp_data)
    # print(representative_value)
    
  }
  print(category_values)
  representation_table = cbind(representation_table, category_values)
  names(representation_table)[length(representation_table)] = category
}


View(t(representation_table))

write.csv(t(representation_table), "../data/energy_saving_exp_summary.csv")

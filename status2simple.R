

PLOTTING = F

LOADING_NROW = 600


load.status.data = function(TARGET_DATE, lab, LOADING_NROW){
  
  FILE_PATH = paste0(TIDY_DATA_DIR, lab, '_', as.character(TARGET_DATE), '(', feeder, ').csv')
  
  dt = fread(FILE_PATH, nrows = LOADING_NROW)
  print(paste(FILE_PATH, 'loaded'))
  
  return(dt)
}



get.simple.status.dt <- function(status_dt, CONSECUTIVE_CHANGE_SEC_THER){
  status_dt = na.omit(status_dt)
  rle_return = rle(status_dt$status)
  
  status_change_indexes = c(0, cumsum(rle_return$lengths)) + 1
  
  ## diff > CONSECUTIVE_CHANGE_SEC_THER ???
  
  status_change_indexes = status_change_indexes[-length(status_change_indexes)] 
  
  return(status_dt[status_change_indexes,])
}



dt_for_plot
unit

## plotting
if(PLOTTING){
  dt_for_plot = one_com_feeder_dt
  
  unit = COMPARING_LENGTH * 4 + 1
  loop_max = nrow(dt_for_plot) / unit
  
  com_feeder_name = paste0(lab, '_com', target_feeder)
  
  max_value = range(com_usage)[2]
  min_value = range(com_usage)[1] * 0.9
  
  for(i in 1:loop_max){
    sub_dt = dt_for_plot[(unit*(i-1)+1):(unit*(i))]
    plot_name = paste(lab, sub_dt$dts[1])
    print(paste('plot:', i, plot_name))
    
    p <- ggplot(sub_dt) +
      geom_point(aes(x=dts, y=usage, color=factor(status)), size=1) +
      theme(axis.text.x = element_text(size=5, angle = 90, hjust = 1)) +
      ylim(min_value, max_value) +
      ggtitle(plot_name) + 
      theme(legend.position = "bottom")
    
    ggsave(filename = paste0(PLOT_PATH, lab, target_feeder,'-', i, ".png"), plot = p, width = 50, height = 10, units='cm')
  }
}

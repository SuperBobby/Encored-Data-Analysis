

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

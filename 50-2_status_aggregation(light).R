library(data.table)
library(ggplot2)
library(zoo)

STATUS_DT_SAVE_PATH = "../data/status/"
STATUS_AGG_PATH = "../data/status/aggregated/"

LAB_LABLES = c('marg', 'hcc', 'ux')

START_DATE = as.Date("2015-09-01")
END_DATE = as.Date("2016-12-06")

# START_DATE = as.Date("2015-09-01")
# END_DATE = START_DATE + 5

CONSECUTIVE_CHANGE_SEC_THER = 0

## -----------------------------
## Functions 
##
load.light.status.data <- function(TARGET_DATE, lab){
  
  FILE_PATH = paste0(STATUS_DT_SAVE_PATH, lab, '_', TARGET_DATE, '_status_dt(light).csv')
  # print(FILE_PATH)
  
  if(file.exists(FILE_PATH)){
    dt = fread(FILE_PATH)
    print(paste("file loaded:", FILE_PATH))
    return(dt)
  } else {
    print(paste("No file:", FILE_PATH))
    return(NULL)
  }
}

get.aggregated.light.status.dt <- function(status_dt, CONSECUTIVE_CHANGE_SEC_THER){
  
  rle_return = rle(status_dt$status)
  
  status_change_index = c(0, cumsum(rle_return$lengths)) + 1
  
  status_duration_dt = cbind(status_dt[status_change_index[-length(status_change_index)]], duration = rle_return$lengths)
  
  return(status_duration_dt[duration > CONSECUTIVE_CHANGE_SEC_THER])
}


## ------------------------------------------- ##
## LooooooP~!
##
LOOP_START = Sys.time()
LOOP_END = Sys.time()

## loop for each lab
for(lab in LAB_LABLES){
  
  ## for measuring time cost (each lab)  
  LOOP_START = c(LOOP_START, Sys.time())
  
  print(lab)
  
  ## Date loop 
  TARGET_DATE = START_DATE
  
  ## initialize aggregated_light_status_dt
  aggregated_light_status_dt = data.frame(matrix(data=NA, nrow=1, ncol=4))
  names(aggregated_light_status_dt) <-c("dts", "light_usage", "status", "duration")
  
  repeat {
    
    ## load status data
    light_status_dt = load.light.status.data(TARGET_DATE, lab)
    # tmp = load.light.status.data("2015-09-02", 'marg')
    
    if(!is.null(light_status_dt)){
      agg_dt = get.aggregated.light.status.dt(light_status_dt, 0)
      aggregated_light_status_dt = rbind(aggregated_light_status_dt, agg_dt)
      
    } else {
      print(paste0(TARGET_DATE, " is passed(no existing or invalid file)"))
    }

    ## Loop until the END_DATE 
    if(TARGET_DATE == END_DATE){
      break
    } else {
      TARGET_DATE = TARGET_DATE + 1
    }
  }
  
  aggregated_light_status_dt = na.omit(aggregated_light_status_dt)
  
  ## saving aggregated_light_status_dt
  output_file_name = paste0(STATUS_AGG_PATH, lab, '_light_aggregated_status_dt', '.csv')

  if(!file.exists(output_file_name)){
  
    write.csv(aggregated_light_status_dt, output_file_name, row.names = F)
    print(paste(output_file_name, "is saved"))
    
  } else {
    
    existing_file = fread(output_file_name)
    if(max(aggregated_light_status_dt$dts) > max(existing_file$dts)){
      write.csv(aggregated_light_status_dt, output_file_name, row.names = F)
      print(paste(output_file_name, "is over written - ", max(aggregated_light_status_dt$dts)))
      
    } else {
      print(paste(output_file_name, "is already exist. No file saved"))
      
    }
  }
  
  LOOP_END = c(LOOP_END, Sys.time())
}

print((LOOP_END - LOOP_START)[-1]/60/60)

# get.aggregated.light.status.dt(light_status_dt,0)
# plot(light_dt$light)


# Time differences in hours
# [1] 1.723522 1.700900 1.698932



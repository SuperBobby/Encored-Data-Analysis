library(data.table)
library(ggplot2)
library(zoo)

DEFAULT_STATUS = 'stay'
RISING_STATUS  = 'up'
FALLING_STATUS = 'down'

# TIDY_DATA_DIR = "data/sec/"
# STATUS_DT_SAVE_PATH = 'data/status/'
# PLOT_PATH = "plots/"

TIDY_DATA_DIR = "../data/sec_tidy/"
STATUS_DT_SAVE_PATH = "../data/status/"
PLOT_PATH = "../plots/milli/"

LAB_LABLES = c('marg', 'hcc', 'ux')

START_DATE = as.Date("2015-09-01")
# END_DATE = as.Date("2015-09-02")
END_DATE = as.Date("2016-12-06")


CONSECUTIVE_CHANGE_SEC_THER = 3

PLOTTING = F

load.status.data = function(TARGET_DATE, lab){
  TARGET_DATE = as.character(as.Date(TARGET_DATE))
  FILE_PATH = paste0(STATUS_DT_SAVE_PATH, lab, '_', TARGET_DATE, '_', 'status_dt.csv')
  
  # print(file.exists(FILE_PATH))
  
  if(file.exists(FILE_PATH)){
    dt = fread(FILE_PATH)
    print(paste(FILE_PATH, 'loaded'))
  }else {
    return(NULL)
  }
  return(dt)
}

get.simple.status.dt <- function(status_dt, CONSECUTIVE_CHANGE_SEC_THER){
  rle_return = rle(status_dt$status)

  valid_status_values = rle_return$values[rle_return$lengths > CONSECUTIVE_CHANGE_SEC_THER]
  
  rising_status = sum(valid_status_values == RISING_STATUS)
  default_status = sum(valid_status_values == DEFAULT_STATUS)
  falling_status = sum(valid_status_values == FALLING_STATUS)
  
  return(c(rising_status, default_status, falling_status))
}


## ------------------------------------------- ##
## LooooooP~!
##
LOOP_START = Sys.time()
LOOP_END = Sys.time()

## loop for each lab
for(lab in LAB_LABLES){
  
  ## initialize simple_status_dt
  simple_status_dt = data.frame(matrix(data=NA, nrow=1, ncol=5))
  names(simple_status_dt) <-c("lab", "dts", "up", "stay", "down")
  
  ## for measuring time cost (each lab)  
  LOOP_START = c(LOOP_START, Sys.time())
  
  ## Date loop 
  TARGET_DATE = START_DATE
  
  repeat {
    
    ## Where are we? 
    print(paste(lab, ":", TARGET_DATE))
    
    ## load status data
    status_dt = load.status.data(TARGET_DATE, lab)
    
    if(is.null(status_dt)){
      TARGET_DATE = TARGET_DATE + 1
      print(paste0(TARGET_DATE, " is passed(file doesn't exist)"))
      next
    }
    
    # adding new row to simple_status_dt
    adding_row = c(lab, as.character(TARGET_DATE), get.simple.status.dt(status_dt, CONSECUTIVE_CHANGE_SEC_THER))
    simple_status_dt = rbind(simple_status_dt, adding_row)
    # print(simple_status_dt)
    
    ## saving simple_status_dt
    simple_status_dt = na.omit(simple_status_dt)
    write.csv(simple_status_dt, paste0(STATUS_DT_SAVE_PATH, lab, '_simple_status_dt.csv'), row.names = F)
    
    ## Loop until the END_DATE 
    if(TARGET_DATE == END_DATE){
      break
    } else {
      TARGET_DATE = TARGET_DATE + 1
    }
  }
  
  LOOP_END = c(LOOP_END, Sys.time())
}

print((LOOP_END - LOOP_START)[-1])









# 
# ## plotting
# if(PLOTTING){
#   dt_for_plot = one_com_feeder_dt
#   
#   unit = COMPARING_LENGTH * 4 + 1
#   loop_max = nrow(dt_for_plot) / unit
#   
#   com_feeder_name = paste0(lab, '_com', target_feeder)
#   
#   max_value = range(com_usage)[2]
#   min_value = range(com_usage)[1] * 0.9
#   
#   for(i in 1:loop_max){
#     sub_dt = dt_for_plot[(unit*(i-1)+1):(unit*(i))]
#     plot_name = paste(lab, sub_dt$dts[1])
#     print(paste('plot:', i, plot_name))
#     
#     p <- ggplot(sub_dt) +
#       geom_point(aes(x=dts, y=usage, color=factor(status)), size=1) +
#       theme(axis.text.x = element_text(size=5, angle = 90, hjust = 1)) +
#       ylim(min_value, max_value) +
#       ggtitle(plot_name) + 
#       theme(legend.position = "bottom")
#     
#     ggsave(filename = paste0(PLOT_PATH, lab, target_feeder,'-', i, ".png"), plot = p, width = 50, height = 10, units='cm')
#   }
# }

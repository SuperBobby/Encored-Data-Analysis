library(data.table)
library(ggplot2)
library(zoo)

REF_LENGTH = 30
COMPARING_LENGTH = 90
PRE_POST_GAP_THRE = 50 # Watts

CONSECUTIVE_CHANGE_SEC_THER = 5

DEFAULT_STATUS = 'stay'
RISING_STATUS  = 'up'
FALLING_STATUS = 'down'

SEC_DT_PATH = "data/sec/"
PLOT_PATH = "plots/"
DT_SAVE_PATH = 'data/'

LOADING_NROW = -1

LAB_LABLES = c('marg', 'hcc', 'ux')

RAW_DATA_DIR = "data/raw/snu/"
TIDY_DATA_DIR = "data/sec/"
STATUS_DT_SAVE_PATH = 'data/status/'

# RAW_DATA_DIR = "../data/raw/sec_raw/"
# TIDY_DATA_DIR = "../data/sec_tidy/"
# STATUS_DT_SAVE_PATH = '../data/status/'

START_DATE = as.Date("2015-09-01")
END_DATE = as.Date("2016-12-06")

## -----------------------------
## Functions 
##

load.sec.data = function(TARGET_DATE, lab, feeder = 'com', LOADING_NROW){
  
  FILE_PATH = paste0(TIDY_DATA_DIR, lab, '_', as.character(TARGET_DATE), '(', feeder, ').csv')
  
  dt = fread(FILE_PATH, nrows = LOADING_NROW)
  print(paste(FILE_PATH, 'loaded'))
  
  return(dt)
}

get.com.status = function(sub_com_usage, PRE_POST_GAP_THRE){
  
  indexed_values = sub_com_usage[1:(REF_LENGTH+1)]
  comparing_values = sub_com_usage[(REF_LENGTH+1):length(sub_com_usage)]
  
  before_med = median(indexed_values)
  after_med  = median(comparing_values)
  
  if(before_med - after_med > PRE_POST_GAP_THRE){
    status = FALLING_STATUS
  } else if(after_med - before_med > PRE_POST_GAP_THRE) {
    status = RISING_STATUS
  } else {
    status = DEFAULT_STATUS
  }
  return(status)
}

get.simple.status.dt <- function(status_dt, CONSECUTIVE_CHANGE_SEC_THER){
  status_dt = na.omit(status_dt)
  rle_return = rle(status_dt$status)
  
  status_change_indexes = c(0, cumsum(rle_return$lengths)) + 1
  
  ## diff > CONSECUTIVE_CHANGE_SEC_THER ???
  
  status_change_indexes = status_change_indexes[-length(status_change_indexes)] 
  
  return(status_dt[status_change_indexes,])
}



## ------------------------------------------- ##
## LooooooP~!
##
LOOP_START = Sys.time()
LOOP_END = Sys.time()

## loop for each lab
for(target_lab in LAB_LABLES){
  
  ## for measuring time cost  
  LOOP_START = c(LOOP_START, Sys.time())
  
  print(target_lab)
  
  ## initialize status_dt
  status_dt = data.frame(matrix(data=NA, nrow=1, ncol=4))
  names(status_dt) <-c("lab", "target_feeder", "dts", "status")
  
  # Date loop 
  TARGET_DATE = START_DATE
  repeat {
    
    print(TARGET_DATE)
    
    ## load sec data 
    com_dt = load.sec.data(TARGET_DATE, target_lab, 'com', LOADING_NROW)
    
    if(nrow(com_dt) == 0){
      TARGET_DATE = TARGET_DATE + 1
      next
    }
    
    
    ## Loop for each computer feeder
    N_of_feeder = length(com_dt) - 1
    for(target_feeder in 1:N_of_feeder){
      
      ## initialize feeder_status_dt ... empty data.table for the tatget feeder only 
      feeder_status_dt = data.frame(matrix(data=NA, nrow=1, ncol=4))
      names(feeder_status_dt) <-c("lab", "target_feeder", "dts", "status")
      
      print(target_feeder)
      
      one_com_feeder_dt = com_dt[, c(1,target_feeder+1),with=F]
      one_com_feeder_dt = cbind(one_com_feeder_dt, status = DEFAULT_STATUS)
      
      com_usage = unlist(one_com_feeder_dt[,2, with=F]) / 1000
      com_usage = na.locf(com_usage)
      
      com_usage = round(com_usage)
      
      STARTING_INDEX = REF_LENGTH+1
      FINISHING_INDEX = nrow(one_com_feeder_dt) - COMPARING_LENGTH - 1
      
      for(index in STARTING_INDEX:(FINISHING_INDEX)){
        
        sub_com_usage = com_usage[(index-REF_LENGTH):(index+COMPARING_LENGTH)]
        
        status = get.com.status(sub_com_usage, PRE_POST_GAP_THRE)
        feeder_status_dt = rbind(feeder_status_dt, c(target_lab, target_feeder, one_com_feeder_dt[index]$dts,
                                                     status)) # 3 + 1 columns 
        
        one_com_feeder_dt[index]$status = status
        
        # show the current status change at console  
        if(index %% 600 == 0) {
          print(one_com_feeder_dt[index])
        }
      }
      
      feeder_status_dt = get.simple.status.dt(feeder_status_dt)
      print(feeder_status_dt)
      status_dt = rbind(status_dt, feeder_status_dt)
    }
    
    ## Loop until the END_DATE 
    if(TARGET_DATE == END_DATE){
      break
    } else {
      TARGET_DATE = TARGET_DATE + 1
    }
  }
  
  # View(status_dt)
  status_dt = na.omit(status_dt)
  write.csv(status_dt, paste0(STATUS_DT_SAVE_PATH, 'simple_status_', target_lab, '.csv'), row.names = F)
  
  LOOP_END = c(LOOP_END, Sys.time())
}

print((LOOP_END - LOOP_START)[-1])

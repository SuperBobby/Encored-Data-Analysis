library(data.table)
library(ggplot2)
library(zoo)

REF_LENGTH = 20
COMPARING_LENGTH = 60
PRE_POST_GAP_THRE = 40 # Watts

FUMBLING_LENGTH = 10

DEFAULT_STATUS = 'stay'
RISING_STATUS  = 'up'
FALLING_STATUS = 'down'

# TIDY_DATA_DIR = "data/sec/"
# STATUS_DT_SAVE_PATH = 'data/status/'
# PLOT_PATH = "plots/"

TIDY_DATA_DIR = "../data/sec_tidy/"
STATUS_DT_SAVE_PATH = "../data/status/"
PLOT_PATH = "../plots/milli/"

PLOTTING = F

LOADING_NROW = -1

# LAB_LABLES = c('marg', 'hcc', 'ux')
LAB_LABLES = c('marg')

# START_DATE = as.Date("2015-09-01")
# END_DATE = as.Date("2015-09-02")

START_DATE = as.Date("2016-12-06")
END_DATE = as.Date("2016-12-06")

## -----------------------------
## Functions 
##

load.sec.data = function(TARGET_DATE, lab, feeder = 'com', LOADING_NROW){
  
  FILE_PATH = paste0(TIDY_DATA_DIR, lab, '_', as.character(TARGET_DATE), '(', feeder, ').csv')
  
  if(file.exists(FILE_PATH)){
    dt = fread(FILE_PATH, nrows = LOADING_NROW)
    print(paste(FILE_PATH, 'loaded'))
  } else {
    dt = data.table()
  }
  return(dt)
}

get.com.status = function(sub_com_usage, PRE_POST_GAP_THRE){
  
  indexed_values = sub_com_usage[1:(REF_LENGTH)]
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
  
  repeat {
    ## initialize status_dt
    status_dt = data.frame(matrix(data=NA, nrow=1, ncol=5))
    names(status_dt) <-c("lab", "target_feeder", "dts", "usage", "status")
  
    print(TARGET_DATE)
    
    ## load sec data
    com_dt = load.sec.data(TARGET_DATE, lab, 'com', LOADING_NROW)
    
    if(nrow(com_dt) == 0){
      TARGET_DATE = TARGET_DATE + 1
      print(paste0(TARGET_DATE, " is passed(file doesn't exist)"))
      next
    }
    
    
    ## Loop for each computer feeder
    N_of_feeder = length(com_dt) - 1
    for(target_feeder in 1:N_of_feeder){
      
      print(target_feeder)
      
      ## initialize feeder_status_dt ... empty data.table for the tatget feeder only 
      feeder_status_dt = data.frame(matrix(data=NA, nrow=1, ncol=4))
      names(feeder_status_dt) <-c("lab", "target_feeder", "dts", "status")
      
      ## subset the target feeder
      one_com_feeder_dt = com_dt[, c(1,target_feeder+1),with=F]
      one_com_feeder_dt = cbind(one_com_feeder_dt, status = DEFAULT_STATUS)
      
      com_usage = unlist(one_com_feeder_dt[,2, with=F]) / 1000
      com_usage = round(na.locf(com_usage))

      ## build the index for com_usage subsetting 
      STARTING_INDEX = REF_LENGTH+1
      FINISHING_INDEX = nrow(one_com_feeder_dt) - COMPARING_LENGTH
      
      index = STARTING_INDEX
      while(index <= FINISHING_INDEX){
        
        ## Fumbling for skipping 
        pre     = median(com_usage[(index-FUMBLING_LENGTH):(index)], na.rm = T)
        post    = median(com_usage[(index):(index+FUMBLING_LENGTH)], na.rm = T)
        btw_var = var(com_usage[(index-FUMBLING_LENGTH):(index+FUMBLING_LENGTH)], na.rm = T)
        
        if(abs(pre - post) < PRE_POST_GAP_THRE*0.3 | (btw_var > 5000)){
          index = index + FUMBLING_LENGTH
          next

        } else {
          
          sub_com_usage = com_usage[(index-REF_LENGTH):(index+COMPARING_LENGTH)]
          # print(paste((index-REF_LENGTH),':',(index+COMPARING_LENGTH)))
        
          status = get.com.status(sub_com_usage, PRE_POST_GAP_THRE)
          feeder_status_dt = rbind(feeder_status_dt, c(lab, target_feeder, one_com_feeder_dt[index]$dts,
                                                       status)) # 3 + 1 columns
          one_com_feeder_dt[index]$status = status
          index = index + 1
        }
        
        # show the current status change at console  
        if(index %% 600 == 0) {
          print(one_com_feeder_dt[index])
        }
      }
      
      ## plotting
      if(PLOTTING){
        dt_for_plot = one_com_feeder_dt
        
        # unit = COMPARING_LENGTH * 4 + 1
        unit = 600
        loop_max = nrow(dt_for_plot) / unit
        
        com_feeder_name = paste0(lab, '_com', target_feeder)
        
        max_value = range(com_usage)[2]
        min_value = range(com_usage)[1] * 0.9
        
        for(i in 1:loop_max){
          sub_dt = dt_for_plot[(unit*(i-1)+1):(unit*(i))]
          plot_name = paste(lab, sub_dt$dts[1])
          print(paste('plot:', target_feeder, '-', i, plot_name))
          
          p <- ggplot(sub_dt) +
            geom_point(aes(x=dts, y=get(com_feeder_name)/1000, color=factor(status)), size=1) +
            theme(axis.text.x = element_text(size=5, angle = 90, hjust = 1)) +
            ylim(min_value, max_value) +
            ggtitle(plot_name) + 
            theme(legend.position = "bottom")
          
          ggsave(filename = paste0(PLOT_PATH, lab, target_feeder,'-', i, ".png"), plot = p, width = 50, height = 10, units='cm')
        }
      }

      ## add new row to status_dt
      names(one_com_feeder_dt) <- c('dts', 'usage', 'status')
      one_com_feeder_dt$usage = one_com_feeder_dt$usage / 1000
      status_dt = rbind(status_dt, cbind(lab, target_feeder, one_com_feeder_dt), fill=T)
    }
    
    ## saving status_dt
    status_dt = na.omit(status_dt)
    write.csv(status_dt, paste0(STATUS_DT_SAVE_PATH, lab, '_', TARGET_DATE, '_status_dt', '.csv'), row.names = F)
    
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

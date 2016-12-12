library(data.table)
library(ggplot2)
library(zoo)

# PRE_POST_WIDTH = 30 # secs
# HIGHLIGHT_WIDTH = 5 # secs 
# 
# PRE_POST_HEIGHT_THRE = 30 # Watts
# 
# ON_OFF_GAP_THRE   = 80 # Watts
# PRE_POST_GAP_THRE = 50 # Watts

# MOVING_AVG_WIDTH = 61

INDEX_LENGTH = 30
CHANGE_THRE = 170
COMPARING_LENGTH = 180

DEFAULT_STATUS = 'stay'

SEC_DT_PATH = "data/sec/"
PLOT_PATH = "plots/"
DT_SAVE_PATH = 'data/'

LOADING_NROW = -1

get.moving.avg <- function(x,n=5){filter(x,rep(1/n,n), sides=2)}

get.com.status = function(sub_com_usage, CHANGE_THRE){
  indexed_value = median(sub_com_usage[1:INDEX_LENGTH])
  comparing_values = sub_com_usage[(INDEX_LENGTH+1):length(sub_com_usage)]
  
  #         indexed_value = sub_com_usage[1]
  #         comparing_values = sub_com_usage[-1]
  #         
  change_sign = sign(median(comparing_values) - indexed_value)
  
  #         print(sub_com_usage)
  #         print(change_sign)
  #         print(sum(indexed_value <= comparing_values))
  
  if((change_sign >= 0) & (sum(indexed_value <= comparing_values) > CHANGE_THRE)){
    status = 'up'
  } else if((change_sign < 0) & (sum(indexed_value >= comparing_values) > CHANGE_THRE)) {
    status = 'down'
  } else {
    status = DEFAULT_STATUS
  }
  
  #         if(change_sign >= 0){
  #                 change_magnitude = abs(sum(indexed_value <= comparing_values))        
  #         } else {
  #                 change_magnitude = abs(sum(indexed_value >= comparing_values))
  #         }
  # 
  #         if(change_magnitude >= CHANGE_THRE) {
  #                 change_magnitude = floor(change_magnitude / 10)*10 
  #         } else {
  #                 change_magnitude = DEFAULT_STATUS
  #         }
  #         status = change_magnitude*change_sign
  return(status)
}

## ------------------------------------------- ##
## LoooooooooooooooooooooooP~!
##

LOOP_START = numeric(0)
LOOP_END = numeric(0)

# lab_labels = c('marg', 'hcc', 'ux')
lab_labels = c('marg', 'hcc', 'ux')

for(lab in lab_labels){
  
  ## initialize status_dt
  status_dt = data.frame(matrix(data=NA, nrow=1, ncol=4))
  names(status_dt) <-c("lab", "target_feeder", "dts", "status")
  
  ## load input file 
  input_file_name = paste0(SEC_DT_PATH, lab, "_2015-09-01(com).csv")  
  com_dt = fread(input_file_name, nrows = LOADING_NROW)
  
  N_of_feeder = length(com_dt) - 1
  
  ## loop for each computer feeder
  for(target_feeder in 1:N_of_feeder){
    
    LOOP_START = c(LOOP_START, Sys.time())
    
    one_com_feeder_dt = com_dt[, c(1,target_feeder+1),with=F]
    one_com_feeder_dt = cbind(one_com_feeder_dt, status = DEFAULT_STATUS)
    
    com_usage = unlist(one_com_feeder_dt[,2, with=F]) / 1000
    com_usage = na.locf(com_usage)
    
    # com_usage = get.moving.avg(com_usage, MOVING_AVG_WIDTH) 
    com_usage = round(com_usage)
    # com_usage = floor(com_usage/10)*10
    
    #                 STARTING_INDEX = (MOVING_AVG_WIDTH - 1) / 2 + 1
    #                 FINISHING_INDEX = (nrow(one_com_feeder_dt)-STARTING_INDEX) - COMPARING_LENGTH - 1
    
    STARTING_INDEX = INDEX_LENGTH
    FINISHING_INDEX = nrow(one_com_feeder_dt) - COMPARING_LENGTH - 1 - INDEX_LENGTH
    
    for(index in STARTING_INDEX:(FINISHING_INDEX)){
      
      sub_com_usage = com_usage[(index-INDEX_LENGTH+1):(index+COMPARING_LENGTH)]
      
      status = get.com.status(sub_com_usage, CHANGE_THRE)
      status_dt = rbind(status_dt, c(lab, target_feeder, one_com_feeder_dt[index]$dts,
                                     status)) # 3 + 1 columns 
      
      one_com_feeder_dt[index]$status = status
      
      # show the current status change at console  
      if(index %% 60 == 0| one_com_feeder_dt[index]$status != 0) {
        print(status)
        print(one_com_feeder_dt[index])
      }
    }
    
    ##
    ## plotting 
    dt_for_plot = one_com_feeder_dt
    
    unit = COMPARING_LENGTH * 2 + 1
    loop_max = nrow(dt_for_plot) / unit
    
    com_feeder_name = paste0(lab, '_com', target_feeder)
    
    max_value = range(com_usage)[2]
    min_value = range(com_usage)[1] * 0.9
    
    for(i in 1:loop_max){
      sub_dt = dt_for_plot[(unit*(i-1)+1):(unit*(i))]
      plot_name = paste(lab, sub_dt$dts[1])
      print(paste('plot:', plot_name))
      
      p <- ggplot(sub_dt) +
        geom_point(aes(x=dts, y=get(com_feeder_name)/1000, color=factor(status)), size=1) +
        theme(axis.text.x = element_text(size=5, angle = 90, hjust = 1)) +
        ylim(min_value, max_value) +
        ggtitle(plot_name) + 
        theme(legend.position = "bottom")
      
      # print(p)
      ggsave(filename = paste0(PLOT_PATH, lab, target_feeder,'-', i, ".png"), plot = p, width = 60, height = 20, units='cm')
    }
    
    write.csv(status_dt, paste0(DT_SAVE_PATH, '(', lab, '_com', target_feeder, ').csv'), row.names = F)
    LOOP_END = c(LOOP_END, Sys.time())    
  }
}

print((LOOP_END - LOOP_START)/60/60)
# > print((LOOP_END - LOOP_START)/60/60)
# [1] 0.520502 1.277545 2.017589

# (LOOP_END - LOOP_START) / 60 / 60
# [1] 0.5013724 1.2374102 1.8722608 0.4998197 1.2156891 0.5112391

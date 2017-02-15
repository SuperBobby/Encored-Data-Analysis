library(data.table)
library(ggplot2)
library(zoo)
library(scales)

# two events in a minute
REF_LENGTH = 15
COMPARING_LENGTH = 15 

COM_PRE_POST_GAP_THRE = 40 # Watts for event thresholder
VALID_COM_VAR_THRE = 120^2 # usage variance thresholder for filtering out noisy values  

DEFAULT_STATUS = 'stay'
RISING_STATUS  = 'up'
FALLING_STATUS = 'down'

TIDY_DATA_DIR = "../data/sec_tidy/"
STATUS_DT_SAVE_PATH = "../data/status/"
PLOT_PATH = "../plots/milli/status_check/"

STATUS_OUTPUT_FILE_UPDATE = T

STATUS_CHECK_PLOTTING = F
TARGET_FEEDER_FOR_PLOT = 0  # 0 means all 

# LAB_LABLES = c('marg')
LAB_LABLES = c('marg', 'hcc', 'ux')

START_DATE = as.Date("2015-09-01")
END_DATE = as.Date("2016-12-06")
# 
# START_DATE = as.Date("2015-09-01")
# END_DATE = START_DATE + 1

## -----------------------------
## Functions 
##

get.sec.dt.duration <- function(dt) {
  from = as.POSIXct(dt$dts[1])
  to   = as.POSIXct(dt$dts[nrow(dt)])
  duration = round(as.numeric(difftime(to, from, units = 'hours')))
  
  return(duration)        
}

load.com.sec.tidy.data = function(TARGET_DATE, lab, feeder){ ### should be unified with function "load.light.sec.tidy.data"
  
  FILE_PATH = paste0(TIDY_DATA_DIR, lab, '_', as.character(TARGET_DATE), '(', feeder, ').csv')
  
  if(lab == 'marg') {
    col_names = c('dts', paste0('com', 1:3))
  } else if(lab == 'hcc') {
    col_names = c('dts', paste0('com', 1:2))
  } else if(lab == 'ux') {
    col_names = c('dts', paste0('com', 1:1))
  }
  
  if(file.exists(FILE_PATH)){
    dt = fread(FILE_PATH)
    
    ## valid duration check: should be 24 hours
    dt_duration = get.sec.dt.duration(dt)
    if(dt_duration == 24){
      print(paste(FILE_PATH, 'loaded'))
      names(dt) <- col_names
      return(dt)        
      
    } else {
      print(paste("Invalid sec data duration (", dt_duration, "hours ):", FILE_PATH))
      return(NULL)
    }
    
  } else {
    print(paste("file don't exist:", FILE_PATH))
    return(NULL)
  }
}

get.com.status = function(sub_com_usage, com_gap_thre, var_thre){
  
  index_usage = sub_com_usage[REF_LENGTH+1]
  if(index_usage > 1200) {
    return(DEFAULT_STATUS)
  }
  
  indexed_values = sub_com_usage[1:(REF_LENGTH)]
  comparing_values = sub_com_usage[(REF_LENGTH+1):length(sub_com_usage)]
  
  before_med = median(indexed_values)
  after_med  = median(comparing_values)

  usage_var = var(sub_com_usage)

  if(usage_var > var_thre) {
    return(DEFAULT_STATUS)
    
  } else if(before_med - after_med > com_gap_thre){
    return(FALLING_STATUS)
    
  } else if(after_med - before_med > com_gap_thre) {
    return(RISING_STATUS)
    
  } else {
    return(DEFAULT_STATUS)
  }
}

# load.com.sec.tidy.data("2015-11-28", 'marg', 'com')

## ------------------------------------------- ##
## LooooooP~!
##
LOOP_START = Sys.time()
LOOP_END = Sys.time()

## loop for each lab
for(lab in LAB_LABLES){
  ## for measuring time cost (each lab)  
  LOOP_START = c(LOOP_START, Sys.time())
  
  ## Date loop 
  TARGET_DATE = START_DATE
  
  repeat {
    
    print(TARGET_DATE)
    
    # check if the com_status_dt is already exist
    output_status_file = paste0(STATUS_DT_SAVE_PATH, lab, '_', TARGET_DATE, '_status_dt(com).csv')
    
    if(!file.exists(output_status_file) | STATUS_OUTPUT_FILE_UPDATE){
      
      ## initialize com_status_dt
      ##         
      com_status_dt = data.frame(matrix(data=NA, nrow=1, ncol=4))
      names(com_status_dt) <-c("dts", "com", "feeder", "status")
      
      ## load sec data
      com_dt = load.com.sec.tidy.data(TARGET_DATE, lab, 'com')
      
      if(is.null(com_dt)){
        
        print(paste0(TARGET_DATE, " is passed(file doesn't exist or invalid file)"))
        if(TARGET_DATE == END_DATE){
          break
        } else {
          TARGET_DATE = TARGET_DATE + 1
          next
        }
      } 
      
      ## Loop for each computer feeder
      N_of_feeder = length(com_dt) - 1
      for(target_feeder in 1:N_of_feeder){
        
        print(paste("com feeder:",target_feeder))
        
        com_feeder_name = paste0('com', target_feeder)
        
        ## subset the target feeder
        one_com_feeder_dt = com_dt[, c('dts', com_feeder_name),with=F]
        
        # change unit to W/h 
        names(one_com_feeder_dt) <- c('dts', 'com')
        one_com_feeder_dt$com = one_com_feeder_dt$com / 1000
        
        com_usage = one_com_feeder_dt$com
        com_usage = na.locf(com_usage)
        status = rep('none', length(com_usage))
        
        ## build the index for com_usage subsetting 
        START_INDEX = REF_LENGTH+1
        END_INDEX   = nrow(one_com_feeder_dt) - COMPARING_LENGTH
        
        index = START_INDEX
        while(index <= END_INDEX){
          
          com_usage_subset = com_usage[(index-REF_LENGTH):(index+COMPARING_LENGTH)]
          
          status[index] = get.com.status(com_usage_subset, COM_PRE_POST_GAP_THRE, VALID_COM_VAR_THRE)
          
          index = index + 1
          
          # show the current status change at console  
          if(index %% (6*60*60) == 0) {
            print(cbind(lab, one_com_feeder_dt[index]))
          }
        }
        
        ## rbind to build the aggregated_status_dt
        one_com_feeder_dt = cbind(one_com_feeder_dt, feeder=target_feeder, status = status)
        com_status_dt = rbind(com_status_dt, one_com_feeder_dt)
        
      }
      
      ## saving com_status_dt
      com_status_dt = na.omit(com_status_dt)
      write.csv(com_status_dt, output_status_file, row.names = F)
      print(paste0("status file saved: ", output_status_file))
  
    } else {
      print(paste('status_dt file is already exist:', output_status_file))
      com_status_dt = fread(output_status_file)
    }
    
    ## STATUS_CHECK_PLOTTING
    if(STATUS_CHECK_PLOTTING){
      
      if(TARGET_FEEDER_FOR_PLOT == 0){
        feeder_range = 1:max(com_status_dt$feeder)
        
      } else {
        feeder_range = TARGET_FEEDER_FOR_PLOT
      }
      for(target_feeder in feeder_range){
        
        dt_for_plot = com_status_dt[feeder==target_feeder]
        dt_for_plot$dts = as.POSIXct(dt_for_plot$dts)
        
        # unit = COMPARING_LENGTH * 4 + 1
        unit = 6000
        loop_max = nrow(dt_for_plot) / unit
        
        com_feeder_name = paste0('com')
        
        max_value = max(dt_for_plot$com, na.rm = T)
        min_value = min(dt_for_plot$com, na.rm = T) * 0.9
        
        print(paste("ylim:", min_value, '-', max_value))
        
        for(i in 1:loop_max){
          sub_dt = dt_for_plot[(unit*(i-1)+1):(unit*(i))]
          
          plot_name = paste(lab, sub_dt$dts[1], 
                            '/ REF(secs):', REF_LENGTH, 
                            '/ COMPARING(secs):', COMPARING_LENGTH, 
                            '/ THRE(Watt):',  COM_PRE_POST_GAP_THRE, 
                            '/ VALID_COM_VAR:', VALID_COM_VAR_THRE)
          print(paste('plot: computer', target_feeder, '-', i, plot_name))
          
          p <- ggplot(sub_dt) +
            geom_point(aes(x=dts, y=get(com_feeder_name), color=factor(status)), size=1) +
            theme(axis.text.x = element_text(size=5, angle = 90, hjust = 1)) +
            ylim(min_value, max_value) +
            ggtitle(plot_name) + 
            theme(legend.position = "bottom") + 
            scale_x_datetime(minor_breaks = date_breaks("1 hour"))
          
          ggsave(filename = paste0(PLOT_PATH, lab, TARGET_DATE,'_(com-', target_feeder, '-', i, ").png"), 
                 plot = p, width = 50, height = 10, units='cm')
          
        }
      }
      
    }
    
    ## Loop until the END_DATE 
    if(TARGET_DATE == END_DATE){
      break
    } else {
      TARGET_DATE = TARGET_DATE + 1
    }
  }
  
  LOOP_END = c(LOOP_END, Sys.time())
}

print((LOOP_END - LOOP_START)[-1]/60/60)

# Time differences in secs
# [1] 2.2266725 1.5413171 0.7854194


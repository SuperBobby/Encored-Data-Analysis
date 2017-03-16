library(data.table)
library(ggplot2)
library(zoo)
library(scales)

# two events in a minute
REF_LENGTH = 15
COMPARING_LENGTH = 15 

TOTAL_PRE_POST_GAP_THRE = 40 # Watts for event thresholder
VALID_TOTAL_VAR_THRE = 120^2 # usage variance thresholder for filtering out noisy values  

DEFAULT_STATUS = 'stay'
RISING_STATUS  = 'up'
FALLING_STATUS = 'down'

TIDY_DATA_DIR = "../data/sec_tidy/"
STATUS_DT_SAVE_PATH = "../data/status/"
PLOT_PATH = "../plots/milli/status_check/takealook/"

STATUS_OUTPUT_FILE_UPDATE = T

STATUS_CHECK_PLOTTING = T
TARGET_FEEDER_FOR_PLOT = 0  # 0 means all 

LAB_LABLES = c('marg')
# LAB_LABLES = c('marg', 'hcc', 'ux')

START_DATE = as.Date("2015-09-01")
END_DATE = as.Date("2015-12-30")
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

load.total.sec.tidy.data = function(TARGET_DATE, lab, feeder){ ### should be unified with function "load.light.sec.tidy.data"
  
  FILE_PATH = paste0(TIDY_DATA_DIR, lab, '_', as.character(TARGET_DATE), '(', feeder, ').csv')
  
  # col_names = c('dts', paste0('total', 1:3))
  
  if(file.exists(FILE_PATH)){
    dt = fread(FILE_PATH)
    
    ## valid duration check: should be 24 hours
    dt_duration = get.sec.dt.duration(dt)
    if(dt_duration >= 24){
      print(paste(FILE_PATH, 'loaded'))
      # names(dt) <- col_names
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

get.total.status = function(sub_total_usage, total_gap_thre, var_thre){
  
  index_usage = sub_total_usage[REF_LENGTH+1]
  if(index_usage > 1200) {
    return(DEFAULT_STATUS)
  }
  
  indexed_values = sub_total_usage[1:(REF_LENGTH)]
  totalparing_values = sub_total_usage[(REF_LENGTH+1):length(sub_total_usage)]
  
  before_med = median(indexed_values)
  after_med  = median(totalparing_values)
  
  usage_var = var(sub_total_usage)
  
  if(usage_var > var_thre) {
    return(DEFAULT_STATUS)
    
  } else if(before_med - after_med > total_gap_thre){
    return(FALLING_STATUS)
    
  } else if(after_med - before_med > total_gap_thre) {
    return(RISING_STATUS)
    
  } else {
    return(DEFAULT_STATUS)
  }
}

# load.total.sec.tidy.data("2015-11-28", 'marg', 'total')

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
    
    # check if the total_status_dt is already exist
    output_status_file = paste0(STATUS_DT_SAVE_PATH, lab, '_', TARGET_DATE, '_status_dt(total).csv')
    
    if(!file.exists(output_status_file) | STATUS_OUTPUT_FILE_UPDATE){
      
      ## initialize total_status_dt
      ##         
      total_status_dt = data.frame(matrix(data=NA, nrow=1, ncol=4))
      names(total_status_dt) <-c("dts", "total", "feeder", "status")
      
      ## load sec data
      total_dt = load.total.sec.tidy.data(TARGET_DATE, lab, 'total')
      
      if(is.null(total_dt)){
        
        print(paste0(TARGET_DATE, " is passed(file doesn't exist or invalid file)"))
        if(TARGET_DATE == END_DATE){
          break
        } else {
          TARGET_DATE = TARGET_DATE + 1
          next
        }
      } 
      
      ## Loop for each total feeder
      # N_of_feeder = length(total_dt) - 1
      for(target_feeder in 1:N_of_feeder){
        
        # print(paste("total feeder:",target_feeder))
        
        # total_feeder_name = paste0('total', target_feeder)
        
        ## subset the target feeder
        one_total_feeder_dt = total_dt
        # one_total_feeder_dt = total_dt[, c('dts', total_feeder_name),with=F]
        
        # change unit to W/h 
        names(one_total_feeder_dt) <- c('dts', 'total')
        one_total_feeder_dt$total = one_total_feeder_dt$total / 1000
        
        total_usage = one_total_feeder_dt$total
        total_usage = na.locf(total_usage)
        status = rep('none', length(total_usage))
        
        ## build the index for total_usage subsetting 
        START_INDEX = REF_LENGTH+1
        END_INDEX   = nrow(one_total_feeder_dt) - COMPARING_LENGTH
        
        index = START_INDEX
        while(index <= END_INDEX){
          
          total_usage_subset = total_usage[(index-REF_LENGTH):(index+COMPARING_LENGTH)]
          
          status[index] = get.total.status(total_usage_subset, TOTAL_PRE_POST_GAP_THRE, VALID_TOTAL_VAR_THRE)
          
          index = index + 1
          
          # show the current status change at console  
          if(index %% (6*60*60) == 0) {
            print(cbind(lab, one_total_feeder_dt[index]))
          }
        }
        
        ## rbind to build the aggregated_status_dt
        one_total_feeder_dt = cbind(one_total_feeder_dt, feeder=target_feeder, status = status)
        total_status_dt = rbind(total_status_dt, one_total_feeder_dt)
        
      }
      
      ## saving total_status_dt
      total_status_dt = na.omit(total_status_dt)
      write.csv(total_status_dt, output_status_file, row.names = F)
      print(paste0("status file saved: ", output_status_file))
      
    } else {
      print(paste('status_dt file is already exist:', output_status_file))
      total_status_dt = fread(output_status_file)
    }
    
    ## STATUS_CHECK_PLOTTING
    if(STATUS_CHECK_PLOTTING){
      
      if(TARGET_FEEDER_FOR_PLOT == 0){
        feeder_range = 1:max(total_status_dt$feeder)
        
      } else {
        feeder_range = TARGET_FEEDER_FOR_PLOT
      }
      for(target_feeder in feeder_range){
        
        dt_for_plot = total_status_dt[feeder==target_feeder]
        dt_for_plot$dts = as.POSIXct(dt_for_plot$dts)
        
        # unit = COMPARING_LENGTH * 4 + 1
        unit = 6000
        loop_max = nrow(dt_for_plot) / unit
        
        total_feeder_name = paste0('total')
        
        max_value = max(dt_for_plot$total, na.rm = T)
        min_value = min(dt_for_plot$total, na.rm = T) * 0.9
        
        print(paste("ylim:", min_value, '-', max_value))
        
        for(i in 1:loop_max){
          sub_dt = dt_for_plot[(unit*(i-1)+1):(unit*(i))]
          
          plot_name = paste(lab, sub_dt$dts[1], 
                            '/ REF(secs):', REF_LENGTH, 
                            '/ COMPARING(secs):', COMPARING_LENGTH, 
                            '/ THRE(Watt):',  TOTAL_PRE_POST_GAP_THRE, 
                            '/ VALID_TOTAL_VAR:', VALID_TOTAL_VAR_THRE)
          print(paste('plot: total', target_feeder, '-', i, plot_name))
          
          p <- ggplot(sub_dt) +
            geom_point(aes(x=dts, y=get(total_feeder_name), color=factor(status)), size=1) +
            theme(axis.text.x = element_text(size=5, angle = 90, hjust = 1)) +
            ylim(min_value, max_value) +
            ggtitle(plot_name) + 
            theme(legend.position = "bottom") + 
            scale_x_datetime(minor_breaks = date_breaks("1 hour"))
          
          ggsave(filename = paste0(PLOT_PATH, lab, TARGET_DATE,'_(total-', target_feeder, '-', i, ").png"), 
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
# [1] 2.366261 2.332963 1.013373


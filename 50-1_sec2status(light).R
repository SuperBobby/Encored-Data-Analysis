library(data.table)
library(ggplot2)
library(zoo)

REF_LENGTH = 10
COMPARING_LENGTH = 10
LIGHT_PRE_POST_GAP_THRE = 100 # Watts
CONSECUTIVE_CHANGE_SEC_THER = 0

DEFAULT_STATUS = 'stay'
RISING_STATUS  = 'up'
FALLING_STATUS = 'down'

TIDY_DATA_DIR = "../data/sec_tidy/"
STATUS_DT_SAVE_PATH = "../data/status/"
PLOT_PATH = "../plots/milli/status_check/"

STATUS_CHECK_PLOTTING = F

LAB_LABLES = c('marg', 'hcc', 'ux')

START_DATE = as.Date("2015-09-01")
END_DATE = as.Date("2016-12-06")

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

load.light.sec.tidy.data = function(TARGET_DATE, lab){
  
  FILE_PATH = paste0(TIDY_DATA_DIR, lab, '_', as.character(TARGET_DATE), '(light).csv')
  
  if(file.exists(FILE_PATH)){
    dt = fread(FILE_PATH)
    
    ## valid duration check: should be 24 hours
    dt_duration = get.sec.dt.duration(dt)
    if(dt_duration == 24){
      print(paste(FILE_PATH, 'loaded'))
      names(dt) <- c('dts', 'light') ## column naming 
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

get.light.status = function(light_usage_subset, light_gap_thre){
  
  indexed_values = light_usage_subset[1:(REF_LENGTH)]
  comparing_values = light_usage_subset[(REF_LENGTH+1):length(light_usage_subset)]
  
  before_med = median(indexed_values, na.rm = T)
  after_med  = median(comparing_values, na.rm = T)
  
  if(before_med - after_med > light_gap_thre){
    status = FALLING_STATUS
    
  } else if(after_med - before_med > light_gap_thre) {
    
    status = RISING_STATUS
    
  } else {
    status = DEFAULT_STATUS
  }
  
  # print(paste(before_med, after_med, status))
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
    print(TARGET_DATE)
    
    # check if the status_dt is already exist
    output_status_file = paste0(STATUS_DT_SAVE_PATH, lab, '_', TARGET_DATE, '_status_dt(light).csv')
    if(file.exists(output_status_file)){
      
      print(paste('status_dt file is already exist:', output_status_file))
      light_status_dt = fread(output_status_file)
      
    } else {
      
      ## load sec data
      light_dt = load.light.sec.tidy.data(TARGET_DATE, lab)
      
      if(is.null(light_dt)){
        
        print(paste0(TARGET_DATE, " is passed(not existing or invalid file)"))
        if(TARGET_DATE == END_DATE){
          break
        } else {
          TARGET_DATE = TARGET_DATE + 1
          next
        }
      }
      
      # change unit to W/h 
      light_dt$light = light_dt$light/1000
      light_usage = na.locf(light_dt$light)
      status = rep("stay", length(light_usage))
      
      ## build the index for light_usage subsetting 
      START_INDEX = REF_LENGTH+1
      END_INDEX = nrow(light_dt) - COMPARING_LENGTH
      
      index = START_INDEX
      while(index <= END_INDEX){
        
        light_usage_subset = light_usage[(index-REF_LENGTH):(index+COMPARING_LENGTH)]
        status[index] = get.light.status(light_usage_subset, LIGHT_PRE_POST_GAP_THRE)
        
        index = index + 1
        
        # show the current status to the console  
        if(index %% 2*60*60 == 0) {
          print(light_dt[index])
        }
      }
      
      ## rbind to build the aggregated_status_dt
      light_status_dt = cbind(light_dt, status=status)
      
      ## saving light_status_dt
      light_status_dt = na.omit(light_status_dt)
      write.csv(light_status_dt, output_status_file, row.names = F)
      print(paste0("status file saved: ", output_status_file))
      
    }

    ## STATUS_CHECK_PLOTTING
    if(STATUS_CHECK_PLOTTING){
      dt_for_plot = light_status_dt
      
      unit = 6000
      loop_max = nrow(dt_for_plot) / unit
      
      light_feeder_name = 'light'
      
      max_value = max(light_status_dt$light, na.rm = T)
      min_value = min(light_status_dt$light, na.rm = T) * 0.9
      
      print(paste("ylim:", min_value, '-', max_value))
      
      for(i in 1:loop_max){
        sub_dt = dt_for_plot[(unit*(i-1)+1):(unit*(i))]
        plot_name = paste(lab, sub_dt$dts[1])
        print(paste('plot: light', '-', i, plot_name))
        
        p <- ggplot(sub_dt) +
          geom_point(aes(x=dts, y=get(light_feeder_name), color=factor(status)), size=1) +
          theme(axis.text.x = element_text(size=5, angle = 90, hjust = 1)) +
          ylim(min_value, max_value) +
          ggtitle(plot_name) + 
          theme(legend.position = "bottom")
        
        ggsave(filename = paste0(PLOT_PATH, lab, TARGET_DATE,'_(light-', i, ").png"), plot = p, width = 50, height = 10, units='cm')
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

# get.aggregated.light.status.dt(light_status_dt,0)
# plot(light_dt$light)


# Time differences in hours
# [1] 1.723522 1.700900 1.698932



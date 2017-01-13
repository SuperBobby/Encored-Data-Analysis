library(data.table)
library(ggplot2)
library(zoo)

CONSECUTIVE_CHANGE_SEC_THER = 3

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

get.daily.status.dt <- function(status_dt, CONSECUTIVE_CHANGE_SEC_THER){
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
  
  ## initialize daily_status_dt
  daily_status_dt = data.frame(matrix(data=NA, nrow=1, ncol=5))
  names(daily_status_dt) <-c("lab", "timestamp", RISING_STATUS, DEFAULT_STATUS, FALLING_STATUS)
  
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
    
    # adding new row to daily_status_dt
    adding_row = c(lab, as.character(TARGET_DATE), get.daily.status.dt(status_dt, CONSECUTIVE_CHANGE_SEC_THER))
    daily_status_dt = rbind(daily_status_dt, adding_row)
    # print(daily_status_dt)
    
    ## saving daily_status_dt
    daily_status_dt = na.omit(daily_status_dt)
    write.csv(daily_status_dt, paste0(STATUS_DT_SAVE_PATH, lab, '_daily_status_dt.csv'), row.names = F)
    
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




plot.status <- function(lab, dt, expDate, PLOT_PATH) {
  plot_dt = dt
  
  if(expDate[length(expDate)] == "2014-11-17"){
    #exp1-1
    plot_dt = cut.expDate.1.1(plot_dt)
    plot_name = paste('exp1-1', lab, sep="_")
  } else if(expDate[length(expDate)] == "2015-01-22"){
    #exp1-2
    plot_dt = cut.expDate.1.2(plot_dt)
    plot_name = paste('exp1-2', lab, sep="_")
  } else{
    #exp3
    plot_dt = cut.expDate.2(plot_dt)
    plot_name = paste('exp2', lab, sep="_")
  }
  
  windowingWeek = 4
  
  status <- ggplot(plot_dt, aes(x=timestamp)) +
    ggtitle(plot_name) +
    ylab("Count (per day)")+
    scale_linetype_discrete(breaks=c(RISING_STATUS, FALLING_STATUS))
  # scale_linetype_discrete(breaks=c(RISING_STATUS, DEFAULT_STATUS, FALLING_STATUS))
  
  status = add.window.line(status, plot_dt, plot_name, RISING_STATUS, windowingWeek, expDate)
  status = add.window.line(status, plot_dt, plot_name, FALLING_STATUS, windowingWeek, expDate)
  # status = add.window.line(status, plot_dt, plot_name, DEFAULT_STATUS, windowingWeek, expDate)
  
  if(expDate[length(expDate)] == "2014-11-17"){
    #exp1-1
    status = add.event.vline.exp1.1(status)
  } else if(expDate[length(expDate)] == "2015-01-22"){
    #exp1-2
    status = add.event.vline.exp1.2(status)
  } else{
    #exp2
    status = add.event.vline.exp2(status)
  }
  
  status = set.default.theme(status)
  
  save.plot(paste0(PLOT_PATH, plot_name, "_on_stay_off.png"), status)
  
  print(paste("plot:", plot_name))
  return(status)
}

if(PLOTTING){
  for(lab in tolower(LABS)){
    print(paste("plot:", lab))
    
    daily_dt = fread(paste0("../data/status/", lab, "_daily_status_dt.csv"))
    daily_dt$timestamp = as.Date(daily_dt$timestamp)
    daily_dt$stay = as.numeric(daily_dt$stay)
    daily_dt$on = as.numeric(daily_dt$on)
    daily_dt$off = as.numeric(daily_dt$off)
    plot.status(toupper(lab), daily_dt, get.expDate.2(), PLOT_PATH)
    
  }
}

# 
# 
dt = fread("../data/status/marg_daily_status_dt.csv")
dt$timestamp = as.Date(dt$timestamp)
dt$stay = as.numeric(dt$stay)
dt$on = as.numeric(dt$on)
dt$off = as.numeric(dt$off)
# names(dt)[2] = 'timestamp'
plot.status('MARG', dt, get.expDate.2(), PLOT_PATH)
# 
# 
# 
# 
# dt = fread("../data/status/hcc_daily_status_dt.csv")
# dt$timestamp = as.Date(dt$timestamp)
# dt$stay = as.numeric(dt$stay)
# dt$up = as.numeric(dt$up)
# dt$down = as.numeric(dt$down)
# names(dt)[2] = 'timestamp'
# plot.status('HCC', dt, get.expDate.2(), PLOT_PATH)
# 
# 
# 
# dt = fread("../data/status/ux_daily_status_dt.csv")
# dt$timestamp = as.Date(dt$timestamp)
# dt$stay = as.numeric(dt$stay)
# dt$up = as.numeric(dt$up)
# dt$down = as.numeric(dt$down)
# names(dt)[2] = 'timestamp'
# plot.status('UX', dt, get.expDate.2(), PLOT_PATH)



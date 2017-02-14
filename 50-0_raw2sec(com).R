## ------------------------------------------------------- ##
## Encored second-interval raw data preprocessing
## 1sec raw data --> 1sec computer-feeder-only table for each lab
##                                        2016-12-9 JY.Han 
##

library(data.table)
library(bit64)

did_MARG = 1171
did_HCC  = 1168
did_UX   = 1169

fid_MARG = c(7,8,14)
fid_HCC = c(5,12)
fid_UX = c(14)

LAB_LABLES = c('marg', 'hcc', 'ux')

RAW_DATA_DIR = "../data/raw/sec_raw/"
TIDY_DATA_DIR = "../data/sec_tidy/"

# RAW_DATA_DIR = "R/"
# TIDY_DATA_DIR = "R/sec/"

START_DATE = as.Date("2015-09-01")
END_DATE = START_DATE
  
  # as.Date("2016-12-06")

## -----------------------------
## Functions 
##

load.sec.raw.data = function(TARGET_DATE){
  
  HEADER = c('dts',  'did',  paste0('watt_', 0:23))
  
  FILE_PATH = paste0(RAW_DATA_DIR, as.character(TARGET_DATE), ".csv")
  
  dt = fread(FILE_PATH, skip=2)
  
  if(nrow(dt) == 0){
    print(paste(TARGET_DATE, 'is NOT a valid raw data'))
  } else {
    names(dt) <- HEADER
  }
  return(dt)
}

get.dt.lab.tidy = function(raw_dt, TARGET_DATE, did_lab){
  
  dt_lab = raw_dt[did == did_lab]
  dt_lab$dts = as.POSIXct(round(dt_lab$dts/1000), origin = "1970-01-01")
  
  # View(dt_lab)
  
  start_dts = dt_lab$dts[1]
  duration = as.numeric(dt_lab$dts[nrow(dt_lab)]) - as.numeric(start_dts)
  
  # tidy timestamp 
  t=as.numeric(as.POSIXct(start_dts, origin = "1970-01-01"))
  full_dts = data.table(dts = as.POSIXct(seq(t, t+duration, 1), origin = "1970-01-01"))
  
  ## Merged data.table with tidy timestamp
  setkey(dt_lab, dts)
  setkey(full_dts, dts)
  dt_lab_tidy = merge(full_dts, dt_lab, all.x = T, all.y = F)
  # dt_lab_tidy = full_dts[dt_lab]
  
  setorder(dt_lab_tidy, dts)
  
  return(unique(dt_lab_tidy))
}


## -----------------------------
## raw --> sec table
## 

TARGET_DATE = START_DATE
repeat {
  
  print(TARGET_DATE)
  
  ## initialize a list for computer-feeder-only tables of each lab
  com_list = list()
  
  ## load raw data 
  raw_dt = load.sec.raw.data(TARGET_DATE)
  
  if(nrow(raw_dt) == 0){
    TARGET_DATE = TARGET_DATE + 1
    next
  }
  
  ## loop for each lab
  for(target_lab in LAB_LABLES){
    
    # print(target_lab)
    
    if(target_lab == 'marg') {
      did_lab = did_MARG
      fid_lab = fid_MARG
      col_names = c('dts', paste0('com', 1:3))
      
    } else if(target_lab == 'hcc') {
      did_lab = did_HCC
      fid_lab = fid_HCC
      col_names = c('dts', paste0('com', 1:2))
      
    } else if(target_lab == 'ux') {
      did_lab = did_UX
      fid_lab = fid_UX
      col_names = c('dts', 'com1')
      
    }
    
    ## subset of the target lab
    lab_dt = get.dt.lab.tidy(raw_dt, TARGET_DATE, did_lab)
    
    ## computer-feeder-only tables of each lab   
    com_feeder_cols = paste0('watt_', fid_lab)
    lab_dt_com = lab_dt[, c('dts', com_feeder_cols), with=F]
    
    names(lab_dt_com) = col_names
    
    output_file_name = paste0(TIDY_DATA_DIR, target_lab, '_', TARGET_DATE, '(com).csv')
    write.csv(lab_dt_com, output_file_name, row.names = F)
    print(paste(output_file_name, 'saved'))
    
  }
  
  ## Loop until the END_DATE 
  if(TARGET_DATE == END_DATE){
    break
  } else {
    TARGET_DATE = TARGET_DATE + 1
  }
  
}

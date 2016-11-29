### ------------------------------------------------------------ ###
## Functions for building a table  
##
### JY, EJ @ ADSL, SNU 
###                                   last update : 2016. 8. 30.
### ------------------------------------------------------------ ###

## get Experiment date 
get.expDate.1.1 <- function() {
  exp_Date<-c(as.Date("2014-11-10"),
              as.Date("2014-11-17"))
#               as.Date("2016-11-16"),
#               as.Date("2016-11-16"),
#               as.Date("2016-11-16"),
#               as.Date("2016-11-16"))
  return(exp_Date)
}

get.expDate.1.2 <- function() {
  exp_Date<-c(as.Date("2014-11-10"),
              as.Date("2014-11-17"),
              as.Date("2015-01-15"),
              as.Date("2015-01-22"))
#               as.Date("2016-11-16"),
#               as.Date("2016-11-16"))
  return(exp_Date)
}

get.expDate.2 <- function() {
  exp_Date<-c(as.Date("2015-10-08"),
              as.Date("2015-12-01"),
              as.Date("2016-01-11"),
              as.Date("2016-02-01"),
              as.Date("2016-05-16"),
              as.Date("2016-06-13"))
  return(exp_Date)
}


get.expDate.all <- function() {
  exp_Date = list(c(as.Date("2014-10-01"), as.Date("2014-10-31")), # pre 1
                  c(as.Date("2014-11-10"), as.Date("2014-11-16")), # 1-1
                  c(as.Date("2014-11-17"), as.Date("2014-12-16")), # post 1-1
                  c(as.Date("2015-01-15"), as.Date("2015-01-21")), # 1-2
                  c(as.Date("2015-01-22"), as.Date("2015-04-30")), # post 1-2
                  c(as.Date("2015-03-01"), as.Date("2015-09-30")), # pre 2
                  c(as.Date("2015-10-08"), as.Date("2015-11-30")), # 2-1
                  c(as.Date("2015-12-01"), as.Date("2016-01-10")), # 2-2
                  c(as.Date("2016-01-11"), as.Date("2016-01-31")), # 2-3
                  c(as.Date("2016-02-01"), as.Date("2016-05-15")), # 2-4
                  c(as.Date("2016-05-16"), as.Date("2016-06-12")), # 2-5
                  c(as.Date("2016-06-13"), as.Date("2016-08-28"))) # post 2

  exp_Names = c('pre 1', '1-1', 'post 1-1', '1-2', 'post 1-2', 'pre 2', '2-1', '2-2', '2-3', '2-4', '2-5', 'post 2')
  exp_Date = setNames(exp_Date, exp_Names)

  return(exp_Date)
}

# 
# get.expDate.all <- function() {
#   exp_Date = list(c(as.Date("2014-10-01"), as.Date("2014-10-31")), # 14y-10
#                   c(as.Date("2014-11-01"), as.Date("2014-11-30")), # 14y-11
#                   c(as.Date("2014-12-01"), as.Date("2014-12-31")), # 14y-12
#                   c(as.Date("2015-01-01"), as.Date("2015-01-31")), # 15y-01
#                   c(as.Date("2015-02-01"), as.Date("2015-02-28")), # 15y-02
#                   c(as.Date("2015-03-01"), as.Date("2015-03-31")), # 15y-03  
#                   c(as.Date("2015-04-01"), as.Date("2015-04-30")), # 15y-04
#                   c(as.Date("2015-05-01"), as.Date("2015-05-31")), # 15y-05
#                   c(as.Date("2015-06-01"), as.Date("2015-06-30")), # 15y-06
#                   c(as.Date("2015-07-01"), as.Date("2015-07-31")), # 15y-07
#                   c(as.Date("2015-08-01"), as.Date("2015-08-30")), # 15y-08
#                   c(as.Date("2015-09-01"), as.Date("2015-08-30")), # 15y-09
#                   c(as.Date("2015-10-01"), as.Date("2015-10-31")), # 15y-10
#                   c(as.Date("2015-11-01"), as.Date("2015-11-30")), # 15y-11
#                   c(as.Date("2015-12-01"), as.Date("2015-12-31")), # 15y-12
#                   c(as.Date("2016-01-01"), as.Date("2016-01-31")), # 16y-01
#                   c(as.Date("2016-02-01"), as.Date("2016-02-28")), # 16y-02
#                   c(as.Date("2016-03-01"), as.Date("2016-03-31")), # 16y-03  
#                   c(as.Date("2016-04-01"), as.Date("2016-04-30")), # 16y-04
#                   c(as.Date("2016-05-01"), as.Date("2016-05-31")), # 16y-05
#                   c(as.Date("2016-06-01"), as.Date("2016-06-30")), # 16y-06
#                   c(as.Date("2016-07-01"), as.Date("2016-07-31")), # 16y-07
#                   c(as.Date("2018-08-01"), as.Date("2016-08-28"))) # 16y-08 
#   
#   exp_Names = c('pre 1', '1-1', 'post 1-1', '1-2', 'post 1-2', 'pre 2', '2-1', '2-2', '2-3', '2-4', '2-5', 'post 2')
#   exp_Date = setNames(exp_Date, exp_Names)
#   
#   return(exp_Date)
# }


# Cut the data.table depending on experiment date 
cut.expDate.1.1 <- function(raw_dt) {
  cut_dt <- raw_dt[timestamp>= "2014-10-01" & timestamp<= "2014-12-16"]
  
  return(cut_dt)
}

cut.expDate.1.2 <- function(raw_dt) {
  cut_dt <- raw_dt[timestamp>= "2014-10-01" & timestamp < "2015-05-01"]
  
  return(cut_dt)
}

cut.expDate.2 <- function(raw_dt) {
  cut_dt <- raw_dt[timestamp>= "2015-03-01" & timestamp<= "2016-08-28"]
  
  return(cut_dt)
}



split.table.by.expDate <- function(input_table, expDate){
  
  split_list = list()
  exp_label    = names(expDate)
  
  for(i in 1:length(all_expDate)){
    from = all_expDate[[i]][1]
    to   = all_expDate[[i]][2]
    name = exp_label[i]
    
    piece_of_dt = input_table[timestamp >= from & timestamp <= to]
    
    split_list = append(split_list, setNames(list(piece_of_dt),name))
  }
  return(split_list)
}



get.day.type.subset = function(input_dt, day_type){
  if(day_type == "allDay"){
    return_dt = input_dt
    
  } else if(day_type == "workingday") {
    return_dt = input_dt[workingday == T]
    
  } else if(day_type == "non_workingday") {
    return_dt = input_dt[workingday == F]
    
  } else {
    return_dt = NULL
  }
  
  return(return_dt)
}


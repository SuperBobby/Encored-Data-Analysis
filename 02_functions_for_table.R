### ------------------------------------------------------------ ###
## Functions for building a table  
##
### JY, EJ @ ADSL, SNU 
###                                   last update : 2016. 8. 30.
### ------------------------------------------------------------ ###

## get Experiment date 
get.expDate.1.1 <- function() {
  exp_Date<-c(as.Date("2014-11-10"),
              as.Date("2014-11-17"),
              as.Date("2016-11-16"),
              as.Date("2016-11-16"),
              as.Date("2016-11-16"),
              as.Date("2016-11-16"))
  return(exp_Date)
}

get.expDate.1.2 <- function() {
  exp_Date<-c(as.Date("2014-11-10"),
              as.Date("2014-11-17"),
              as.Date("2015-01-15"),
              as.Date("2015-01-22"),
              as.Date("2016-11-16"),
              as.Date("2016-11-16"))
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




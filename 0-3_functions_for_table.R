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
              # as.Date("2016-05-16"),
              as.Date("2016-06-13"))
  return(exp_Date)
}

get.expDate.all <- function() {
  exp_Date <-     c(as.Date("2014-11-10"),
                    as.Date("2014-11-17"),
                    as.Date("2015-01-15"),
                    as.Date("2015-01-22"),
                    as.Date("2015-10-08"),
                    as.Date("2015-12-01"),
                    as.Date("2016-01-11"),
                    as.Date("2016-02-01"),
                    as.Date("2016-06-13"))
  return(exp_Date)
}

get.labeled.expDate.all <- function() {
  exp_Date = list(c(as.Date("2014-09-01"), as.Date("2014-09-30")), # Sep 2014 
                  c(as.Date("2014-10-01"), as.Date("2014-10-31")), # Oct 2014
                  c(as.Date("2014-11-10"), as.Date("2014-11-16")), # int 1-1
                  c(as.Date("2014-11-17"), as.Date("2014-12-16")), # btw 1-1 & 1-2
                  c(as.Date("2015-01-15"), as.Date("2015-01-21")), # int 1-2
                  c(as.Date("2015-01-22"), as.Date("2015-02-28")), # Feb 2015 
                  c(as.Date("2015-03-01"), as.Date("2015-03-31")), # Mar 2015 
                  c(as.Date("2015-04-01"), as.Date("2015-04-30")), # Apr 2015 
                  
                  c(as.Date("2015-08-01"), as.Date("2015-08-31")), # AUG 2015
                  c(as.Date("2015-09-01"), as.Date("2015-09-30")), # Sep 2015
                  c(as.Date("2015-10-08"), as.Date("2015-11-30")), # int 2-1
                  c(as.Date("2015-12-01"), as.Date("2016-01-10")), # int 2-2
                  c(as.Date("2016-01-11"), as.Date("2016-01-31")), # int 2-3
                  c(as.Date("2016-02-01"), as.Date("2016-06-12")), # int 2-4
                  # c(as.Date("2016-02-01"), as.Date("2016-05-15")), # int 2-4
                  # c(as.Date("2016-05-16"), as.Date("2016-06-12")), # int 2-5
                  c(as.Date("2016-06-13"), as.Date("2016-07-31")),   # Jul 2016
                  c(as.Date("2016-08-01"), as.Date("2016-08-31")),    # Aug 2016
                  c(as.Date("2016-09-01"), as.Date("2016-09-30")),    # Sep 2016
                  c(as.Date("2016-10-01"), as.Date("2016-10-31")),    # Oct 2016
                  ) 

  exp_Names = c('Sep 2014', 'Oct 2014', 'int 1-1', 'btw 1-1 & 1-2', 'int 1-2', 'Feb 2015', 'Mar 2015', 'Apr 2015',
                'Aug 2015', 'Sep 2015', 'int 2-1', 'int 2-2', 'int 2-3', 'int 2-4', 'Jul 2016', 'Aug 2016', 'Sep 2016', 'Oct 2016')
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
  cut_dt <- raw_dt[timestamp>= "2015-08-01" & timestamp<= "2016-11-01"]
  
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


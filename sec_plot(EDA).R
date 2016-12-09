library(data.table)
library(ggplot2)
library(zoo)


PRE_POST_WIDTH = 30 # secs
HIGHLIGHT_WIDTH = 5 # secs 

PRE_POST_HEIGHT_THRE = 30 # Watts

ON_OFF_GAP_THRE   = 80 # Watts
PRE_POST_GAP_THRE = 50 # Watts

get.com.status = function(pre, highlight, post){
  
  pre_height = quantile(pre, 0.9) - quantile(pre, 0.1)
  pre_med   = median(pre)
  
  on_off_gap = range(highlight)[2] - range(highlight)[1]
  
  post_height = quantile(post, 0.9) - quantile(post, 0.1)
  post_med   = median(post)
  
  pre_post_gap = post_med - pre_med
  
  ## Thresholding 
  if(abs(on_off_gap)   >= ON_OFF_GAP_THRE & 
     abs(pre_post_gap) >= PRE_POST_GAP_THRE  &
     pre_height  <= PRE_POST_HEIGHT_THRE & 
     post_height <= PRE_POST_HEIGHT_THRE) {
    
    if(PRE_POST_GAP_THRE > 0){ return_status = 1} 
    else if(PRE_POST_GAP_THRE < 0) { return_status = -1 }
    else { return_status = 2 }
    
    print(paste(round(on_off_gap), round(pre_post_gap), 
                "(", round(pre_height), round(pre_med), "|", round(post_height), round(post_med), ")"))
    
  } else {
    return_status = 0
  }
  return(c(on_off_gap=on_off_gap, 
           pre_post_gap=pre_post_gap, 
           pre_height=pre_height, 
           pre_med=pre_med, 
           post_height=post_height, 
           post_med=post_med, 
           return_status=return_status)) # 7 returns
}


## ------------------------------------------- ##
## LoooooooooooooooooooooooP~!
##

LOOP_START = numeric(0)
LOOP_END = numeric(0)

lab_labels = c('marg', 'hcc', 'ux')

for(lab in lab_labels){
 
  ## initialize status_dt
  status_dt = data.frame(matrix(data=NA, nrow=1, ncol=10))
  names(status_dt) <-c("lab", "target_feeder", "dts", "on_off_gap", "pre_post_gap", "pre_height", "pre_med", "post_height", "post_med", "return_status")
   
  ## load input file 
  input_file_name = paste0("../data/sec_tidy/", lab, "_2015-09-01(com).csv")  
  com_dt = fread(input_file_name, nrows = -1)
  
  N_of_feeder = length(com_dt) - 1
  
  ## loop for each computer feeder
  for(target_feeder in 1:N_of_feeder){
    
    LOOP_START = c(LOOP_START, Sys.time())
    
    one_com_feeder_dt = com_dt[, c(1,target_feeder+1),with=F]
    one_com_feeder_dt = cbind(one_com_feeder_dt, status = 0)
    
    STARTING_INDEX = PRE_POST_WIDTH + HIGHLIGHT_WIDTH + 1
    FINISHING_INDEX = nrow(one_com_feeder_dt) - PRE_POST_WIDTH - HIGHLIGHT_WIDTH
    
    for(index in STARTING_INDEX:(FINISHING_INDEX)){
      com_usage = unlist(one_com_feeder_dt[,2, with=F]) / 1000
      com_usage = na.locf(com_usage)
      
      pre       = com_usage[(index-PRE_POST_WIDTH-HIGHLIGHT_WIDTH):(index-HIGHLIGHT_WIDTH-1)]
      highlight = com_usage[(index-HIGHLIGHT_WIDTH):(index+HIGHLIGHT_WIDTH)]
      post      = com_usage[(index+HIGHLIGHT_WIDTH+1):(index+PRE_POST_WIDTH+HIGHLIGHT_WIDTH)]
      
      status_vector = get.com.status(pre, highlight, post)
      status_dt = rbind(status_dt, c(lab, target_feeder, one_com_feeder_dt[index]$dts,
                                     status_vector)) # 3 + 7 columns 
      
      one_com_feeder_dt[index]$status = status_vector[length(status_vector)]
      
      # show the current status change at console  
      if(index %% 60 == 0| one_com_feeder_dt[index]$status != 0) {
        print(status_vector)
        print(one_com_feeder_dt[index])
      }
    }
    
    ##
    ## plotting 
    dt_for_plot = one_com_feeder_dt
    
    unit = 200
    loop_max = nrow(dt_for_plot) / unit
    
    com_feeder_name = paste0(lab, '_com', target_feeder)
    
    max_value = range(com_usage)[2] * 0.8 
    min_value = range(com_usage)[1]
    
    for(i in 1:loop_max){
      sub_dt = dt_for_plot[(unit*(i-1)+1):(unit*(i))]
      plot_name = paste(lab, sub_dt$dts[1])
      print(plot_name)
      
      p <- ggplot(sub_dt) +
        geom_point(aes(x=dts, y=get(com_feeder_name)/1000, color=factor(status)), size=1) +
        theme(axis.text.x = element_text(size=5, angle = 90, hjust = 1)) +
        ylim(min_value, max_value) +
        ggtitle(plot_name)
      
      # print(p)
      ggsave(filename = paste0("../plots/milli/", lab, target_feeder,'-', i, ".png"), plot = p, width = 50, height = 10, units='cm')
    }

    LOOP_END = c(LOOP_END, Sys.time())    
  }
  
  write.csv(status_dt, paste0('../data/status_dt(', lab,').csv'), row.names = F)
  
}

print(LOOP_END - LOOP_START)








# 
# #####################################################################################
# # raw_dt = fread("../data/sec_data/logs-20150411.csv")
# # milli_dt = fread("../data/sec_data/site73-2015-03-30.csv")
# 
# 
# milli_dt = fread("../data/sec_tidy/marg_2015-09-01(com).csv")
# 
# # head(raw_dt)
# # head(milli_dt)
# 
# unit = 200
# denom = 1000
# loop_max = nrow(milli_dt) / unit
# 
# max_value = range(milli_dt$marg_com1, na.rm = T)[2] * 0.8 / denom
# min_value = range(milli_dt$marg_com1, na.rm = T)[1] / denom
# 
# for(i in 1:loop_max){
#   sub_dt = milli_dt[(unit*(i-1)+1):(unit*(i))]
#   plot_name = sub_dt$dts[1]
#   print(plot_name)
#   
#   p <- ggplot(sub_dt) +
#     geom_point(aes(x=dts, y=marg_com1/denom), size=1) +
#     theme(axis.text.x = element_text(size=5, angle = 90, hjust = 1)) +
#     ylim(min_value, max_value) +
#     ggtitle(plot_name)
#   
#   # print(p)
#   ggsave(filename = paste0("../plots/milli/marg1-", i, ".png"), plot = p, width = 50, height = 10, units='cm')
# }
# 
# 
# 
# max_value = range(milli_dt$marg_com2, na.rm = T)[2] / denom
# for(i in 1:loop_max){
#   sub_dt = milli_dt[(unit*(i-1)+1):(unit*(i))]
#   plot_name = sub_dt$dts[1]
#   print(plot_name)
#   
#   p <- ggplot(sub_dt) +
#     geom_point(aes(x=dts, y=marg_com2/denom), size=1) +
#     theme(axis.text.x = element_text(size=5, angle = 90, hjust = 1)) +
#     ylim(min_value, max_value) +
#     ggtitle(plot_name)
#   
#   # print(p)
#   ggsave(filename = paste0("../plots/milli/marg2-", i, ".png"), plot = p, width = 50, height = 10, units='cm')
# }
# 
# 
# max_value = range(milli_dt$marg_com3, na.rm = T)[2] / denom
# for(i in 1:loop_max){
#   sub_dt = milli_dt[(unit*(i-1)+1):(unit*(i))]
#   plot_name = sub_dt$dts[1]
#   print(plot_name)
#   
#   p <- ggplot(sub_dt) +
#     geom_point(aes(x=dts, y=marg_com3/denom), size=1) +
#     theme(axis.text.x = element_text(size=5, angle = 90, hjust = 1)) +
#     ylim(min_value, max_value) +
#     ggtitle(plot_name)
#   
#   # print(p)
#   ggsave(filename = paste0("../plots/milli/marg3-", i, ".png"), plot = p, width = 50, height = 10, units='cm')
# }
# 
# 
#   

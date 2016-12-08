library(data.table)
library(ggplot2)

# raw_dt = fread("../data/sec_data/logs-20150411.csv")
# milli_dt = fread("../data/sec_data/site73-2015-03-30.csv")


milli_dt = fread("../data/sec_tidy/marg_2015-09-01(com).csv")

# head(raw_dt)
# head(milli_dt)

unit = 600
denom = 1000
loop_max = nrow(milli_dt) / unit

max_value = range(milli_dt$marg_com1, na.rm = T)[2] / denom

for(i in 1:loop_max){
  sub_dt = milli_dt[(unit*(i-1)+1):(unit*(i))]
  plot_name = sub_dt$dts[1]
  print(plot_name)

  p <- ggplot(sub_dt) +
    geom_point(aes(x=dts, y=marg_com1/denom), size=1) +
    theme(axis.text.x = element_text(size=5, angle = 90, hjust = 1)) +
    ylim(0, max_value) +
    ggtitle(plot_name)
  
  # print(p)
  ggsave(filename = paste0("../plots/milli/marg1-", i, ".png"), plot = p)
}

# 
# for(i in 1:loop_max){
#   sub_dt = milli_dt[(unit*(i-1)+1):(unit*(i))]
#   plot_name = sub_dt$time_index[1]
#   print(plot_name)
#   
#   p <- ggplot(sub_dt) +
#     geom_point(aes(x=time_index, y=v1168_12/denom), size=1) +
#     theme(axis.text.x = element_text(size=5, angle = 90, hjust = 1)) +
#     ylim(0, max_value) +
#     ggtitle(plot_name)
#   
#   # print(p)
#   ggsave(filename = paste0("../plots/milli/hcc2-", i, ".png"), plot = p)
# }
# 


  
  

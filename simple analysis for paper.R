library(data.table)

get.prop.mainfeeders <- function(dt){

  sub_dt = dt[timestamp < "2014-10-1"]
  
  # print(summary(sub_dt))
  
  sums = colSums(sub_dt[,c('computer', 'light', 'hvac', 'etc', 'total'),with=F])
  
  print(round(sums,2))
  print(sum(sums[1:4]))
  
  print(round(sums[1:4] / sums[5], 2))
  
  print(sum(round(sums[1:4] / sums[5], 2)))
  
}

get.prop.mainfeeders(marg_dt)
get.prop.mainfeeders(hcc_dt)
get.prop.mainfeeders(ux_dt)



tmp <- reviseSNUData(marg_defalut_table_15min, "marg", "2014-08-01", "2014-10-01", verbose = T)





RS_raw_marg = read.csv("realsense/marg.csv")

# tail(RS_raw_marg)

joined = as.POSIXct(RS_raw_marg$joined/1000, format="%Y-%m-%d", origin='1970-01-01', tz="ROK")
leaved = as.POSIXct(RS_raw_marg$leaved/1000, format="%Y-%m-%d", origin='1970-01-01', tz="ROK")
duration = as.numeric(RS_raw_marg$leaved - RS_raw_marg$joined)

start = joined[1]

basic_table = data.frame(joined, leaved, duration)


# boxplot(duration)

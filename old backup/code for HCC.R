
############################################
### data loading and basic preprocessing ###
############################################
#marg_15min = read.csv("data/MARG_15min.csv")
marg_15min = read.csv("data/marg_15min_replacement.csv")
marg_15min = na.omit(marg_15min)

marg_hour = resolution.processor(data = marg_15min, jump = 0, 
                                 merge = 4)
marg.day =  resolution.processor(data = marg_hour, jump = 0, merge = 24)

marg_hour[-1] = marg_hour[-1]/1000000
marg.day[-1] = marg.day[-1]/1000000

marg_hour = marg.add.summation(marg_hour)
marg.day = marg.add.summation(marg.day)
#write.csv(marg_hour, file="data/marg_hour.csv")

marg.day.5am = resolution.processor(data = marg_hour, jump = 5, merge = 24)
# marg.week.Thur = resolution.processor(data = marg.day.5am, jump = 2, merge = 7)
# marg_15min.raw = read.csv("data/marg_15min_raw.csv")

# tidy dataset!
marg_day_5am_tidy = add.daylabel(marg.day.5am, start ="Tue")
marg_day_5am_tidy = na.omit(marg_day_5am_tidy)

#########################
### missing detection ###
### omit dates        ###
#########################
missing_days = as.character(marg_15min$X[marg_15min$D406.3 < 0.1])

missing_days = sapply(missing_days, tidy.missing.days)
missing_days = na.omit(missing_days)

print("data missing event dates")
missing_days = unique(missing_days)
print(missing_days)

# Manually, missing day add : LED issue, MT, holidays
# manually = c(   "2015-01-23", "2015-01-24", 
#                 "2015-02-18", "2015-02-19", "2015-02-20", "2015-02-21", "2015-02-22")
manually = c(   "2014-11-12", "2014-11-13", "2014-11-14", "2014-11-15", "2014-11-16",
                "2015-01-23", "2015-01-24", 
                "2015-02-18", "2015-02-19", "2015-02-20", "2015-02-21", "2015-02-22")
missing_days = c(missing_days, manually)
missing_days = unique(missing_days)
missing_days = sort(missing_days)

print(paste("Total missing days...", length(missing_days), " days", sep=""))
print(missing_days)

marg_day_5am_tidy$X = strtrim(marg_day_5am_tidy$X,10)
marg_day_5am_tidy = date.omit(marg_day_5am_tidy, missing_days)

write.csv(marg_day_5am_tidy, file="data/marg_day_5am_tidy.csv")
#write.csv(marg_day_5am_tidy, file="data/marg_day_5am_tidy_for_com_.csv")


#######################
###### windowing ######
#######################
marg_windowing_7days = windowing(marg_day_5am_tidy)
write.csv(marg_windowing_7days, file="data/marg_windowing_7days.csv")
#write.csv(marg_windowing_7days, file="data/marg_windowing_7days_for_com.csv")

#windowing plot... using R function/library


#########################
### marg mean print   ###
#########################

marg.mean.printer(marg_day_5am_tidy, "2014-12-04", "2014-12-10")
marg.mean.printer(marg_day_5am_tidy, "2014-12-11", "2014-08-31")
marg.mean.printer(marg_day_5am_tidy, "2014-08-05", "2014-08-31")
marg.mean.printer(marg_day_5am_tidy, "2014-08-05", "2014-08-31")
marg.mean.printer(marg_day_5am_tidy, "2014-08-05", "2014-08-31")
marg.mean.printer(marg_day_5am_tidy, "2014-08-05", "2014-08-31")




# Aug
marg.mean.printer(marg_day_5am_tidy, "2014-08-05", "2014-08-31")

# Sep
marg.mean.printer(marg_day_5am_tidy, "2014-09-01", "2014-09-30")

# Oct
marg.mean.printer(marg_day_5am_tidy, "2014-10-01", "2014-10-31")


# before 1st Exp.
marg.mean.printer(marg_day_5am_tidy, "2014-10-01", "2014-11-09")


# 1st Exp.
marg.mean.printer(marg_day_5am_tidy, "2014-11-10", "2014-11-16")
marg.mean.printer(marg_day_5am_tidy, "2014-11-10", "2014-11-11")

# Btw Exp.s
marg.mean.printer(marg_day_5am_tidy, "2014-11-17", "2015-01-14")

# 2nd Exp.
marg.mean.printer(marg_day_5am_tidy, "2015-01-15", "2015-01-21")

# After 2nd Exp.
marg.mean.printer(marg_day_5am_tidy, "2015-01-22", "2015-02-23")

# Dec
marg.mean.printer(marg_day_5am_tidy, "2014-12-01", "2014-12-31")

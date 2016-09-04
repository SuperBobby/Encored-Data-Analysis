source("Encored-Data-Analysis/getSNUdata.R")

##################################################
## Day table
##################################################
last_start = "2014-12-01"
last_end   = "2015-03-01"

this_start = "2016-01-01"
this_end   = "2016-01-30"

marg_lastSeason_day = getSNUData.feeder.day("marg", last_start, last_end)
 hcc_lastSeason_day = getSNUData.feeder.day( "hcc", last_start, last_end)
  ux_lastSeason_day = getSNUData.feeder.day(  "ux", last_start, last_end)

marg_thisSeason_day = getSNUData.feeder.day("marg", this_start, this_end)
 hcc_thisSeason_day = getSNUData.feeder.day( "hcc", this_start, this_end)
  ux_thisSeason_day = getSNUData.feeder.day(  "ux", this_start, this_end)

#total 
mean(c(marg_thisSeason_day$total, hcc_thisSeason_day$total, ux_thisSeason_day$total)) / mean(c(marg_lastSeason_day$total, hcc_lastSeason_day$total, ux_lastSeason_day$total))


#com
mean(c(marg_thisSeason_day$computer, hcc_thisSeason_day$computer, ux_thisSeason_day$computer)) / mean(c(marg_lastSeason_day$computer, hcc_lastSeason_day$computer, ux_lastSeason_day$computer))
#light
mean(c(marg_thisSeason_day$light, hcc_thisSeason_day$light, ux_thisSeason_day$light)) / mean(c(marg_lastSeason_day$light, hcc_lastSeason_day$light, ux_lastSeason_day$light))
# hvac
mean(c(marg_thisSeason_day$hvac, hcc_thisSeason_day$hvac, ux_thisSeason_day$hvac)) / mean(c(marg_lastSeason_day$hvac, hcc_lastSeason_day$hvac, ux_lastSeason_day$hvac))



mean(marg_thisSeason_day$total) / mean(marg_lastSeason_day$total)
mean(hcc_thisSeason_day$total) / mean(hcc_lastSeason_day$total)
mean(ux_thisSeason_day$total) / mean(ux_lastSeason_day$total)




mean(c(marg_thisSeason_day$total, hcc_thisSeason_day$total, ux_thisSeason_day$total)) / mean(c(marg_lastSeason_day$total, hcc_lastSeason_day$total, ux_lastSeason_day$total))

mean(sum(marg_lastSeason_day$total) + sum(hcc_lastSeason_day$total) + sum(ux_lastSeason_day$total))

sum(marg_thisSeason_day$total) + sum(hcc_thisSeason_day$total) + sum(ux_thisSeason_day$total)



marg_lastWinter = getSNUData.feeder.day("marg", "2014-12-01", "2015-03-01")
marg_lastSpring = getSNUData.feeder.day("marg", "2015-03-01", "2015-06-01")
marg_lastSummer = getSNUData.feeder.day("marg", "2015-06-01", "2015-09-01")
marg_lastFall   = getSNUData.feeder.day("marg", "2015-09-01", "2015-12-01")
marg_thisWinter = getSNUData.feeder.day("marg", "2015-12-01", "2016-01-11")

mean(marg_lastWinter$computer) # 34.37975
mean(marg_lastSpring$computer) # 36.3652
mean(marg_lastSummer$computer) # 38.10604
mean(marg_lastFall$computer)   # 42.83563

mean(marg_lastWinter$light) # 13.1629
mean(marg_lastSpring$light) # 13.77645
mean(marg_lastSummer$light) # 15.67102
mean(marg_lastFall$light)   # 15.51649

mean(marg_lastWinter$hvac) # 23.32711
mean(marg_lastSpring$hvac) # 6.733921
mean(marg_lastSummer$hvac) # 32.71356
mean(marg_lastFall$hvac)   # 12.55509


hcc_lastWinter = getSNUData.feeder.day("hcc", "2014-12-01", "2015-03-01")
hcc_lastSpring = getSNUData.feeder.day("hcc", "2015-03-01", "2015-06-01")
hcc_lastSummer = getSNUData.feeder.day("hcc", "2015-06-01", "2015-09-01")
hcc_lastFall   = getSNUData.feeder.day("hcc", "2015-09-01", "2015-12-01")

mean(hcc_lastWinter$computer) # 17.49733
mean(hcc_lastSpring$computer) # 19.71125
mean(hcc_lastSummer$computer) # 21.58572
mean(hcc_lastFall$computer)   # 23.00989

mean(hcc_lastWinter$light) # 17.79548
mean(hcc_lastSpring$light) # 19.61408
mean(hcc_lastSummer$light) # 17.3995
mean(hcc_lastFall$light)   # 18.58847



ux_lastWinter = getSNUData.feeder.day("ux", "2014-12-01", "2015-03-01")
ux_lastSpring = getSNUData.feeder.day("ux", "2015-03-01", "2015-06-01")
ux_lastSummer = getSNUData.feeder.day("ux", "2015-06-01", "2015-09-01")
ux_lastFall   = getSNUData.feeder.day("ux", "2015-09-01", "2015-12-01")

mean(ux_lastWinter$computer) # 9.632707
mean(ux_lastSpring$computer) # 11.37642
mean(ux_lastSummer$computer) # 11.38148
mean(ux_lastFall$computer)   # 10.80335

mean(ux_lastWinter$light) # 12.51643
mean(ux_lastSpring$light) # 16.03355
mean(ux_lastSummer$light) # 14.1691
mean(ux_lastFall$light)   # 13.41814






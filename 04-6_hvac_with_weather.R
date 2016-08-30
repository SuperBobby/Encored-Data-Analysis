### 
## 04-6. MARG HVAC + weahter(max, avg, min temperature info) 
## 

build.table.weather <- function(){
  return_dts = list()
  
  weather_dt = fread("../rawData/Suwon_weather.csv")
  weather_dt$date_index = as.Date(weather_dt$date_index)
  setnames(weather_dt,old="date_index",new="get")
  str(weather_dt)
  
  weather_dt <- weather_dt[, ':='(get = get,
                                  aggWeek = as.Date(cut(get, breaks = "week", start.on.monday = T)))]
  
  aggWeek_weather_dt <- weather_dt[, .(get = aggWeek,
                                       avg_temp = mean(avg_temp),
                                       max_temp = mean(max_temp),
                                       min_temp = mean(min_temp)), by=aggWeek] 
  aggWeek_weather_dt <- aggWeek_weather_dt[, aggWeek:=NULL]
  
  weather_dt <- weather_dt[get >= "2014-10-01" & get <= "2016-07-26"]
  aggWeek_weather_dt <- aggWeek_weather_dt[get >= "2014-09-29" & get <= "2016-07-26"]
  
  assign("weather_aggDay", weather_dt)
  return_dts = append(return_dts, setNames(list(weather_dt),"weather_aggDay"))
  
  assign("weather_aggWeek", aggWeek_weather_dt)
  return_dts = append(return_dts, setNames(list(aggWeek_weather_dt),"weather_aggWeek"))
  
  return(return_dts)
}

plot.hvac.with.weather <- function(dt, weather_dt, expDate){
  plot_name = names(dt)
  
  if((strsplit(plot_name,"_")[[1]][4] != "hvac") | (strsplit(plot_name,"_")[[1]][1] != "MARG")){
    return()
  }

  plot_dt = dt[[1]]
  
  if(expDate[4] == "2016-11-17"){
    #exp1-1
    plot_dt = set.expDate.1.1(plot_dt)
    plot_name = paste('exp1-1', names(dt), sep="_")
  } else if(expDate[4] == "2015-01-22"){
    #exp1-2
    plot_dt = set.expDate.1.2(plot_dt)
    plot_name = paste('exp1-2', names(dt), sep="_")
  } else{
    #exp3
    plot_dt = set.expDate.2(plot_dt)
    plot_name = paste('exp2', names(dt), sep="_")
  }
  
  if(grepl("aggDay", plot_name)){
    temp_weather_dt <- weather_dt[[1]]
    weather_name <- "aggDay"
  } else{
    temp_weather_dt <- weather_dt[[2]]
    weather_name <- "aggWeek"
  }
  
  rownum_expDate <- set.expDate.rownum(temp_weather_dt, expDate)
  
  windowingWeek <- 4
  
  grid.newpage()
  plot_name = paste(plot_name, "+ Temperature")  
  print(plot_name)
  
  #     print(strsplit(plot_name,"_")[[1]][2])
  #     print(rownum_expDate)
  #     print(head(temp_weather_dt))
  #     print(head(w_max_tmp))
  #     print(nrow(w_max_tmp))
  
  p2 <- ggplot(temp_weather_dt, aes(x=get)) +
    ylab("temperature(°C)")+
    scale_x_date(labels = date_format("%Y-%m"), breaks = date_breaks("month"), limits = c(as.Date("2014-10-01"), as.Date("2016-07-26"))) +
    ylim(-20,40)+
    scale_color_manual(values=c("dodgerblue2", "skyblue", "midnightblue"), 
                       breaks=c("max_temp", "avg_temp", "min_temp"))
  p2 = add.colorful.window.line(p2, temp_weather_dt, weather_name, 'max_temp', windowingWeek, "dodgerblue2", rownum_expDate, ribbon=FALSE)
  p2 = add.colorful.window.line(p2, temp_weather_dt, weather_name, 'avg_temp', windowingWeek, "skyblue", rownum_expDate, ribbon=FALSE)
  p2 = add.colorful.window.line(p2, temp_weather_dt, weather_name, 'min_temp', windowingWeek, "midnightblue", rownum_expDate, ribbon=FALSE)
  
  p2 = p2 + 
    theme_bw()+
    theme(axis.line = element_line(colour = "black"),
          legend.text = element_text(size=23, hjust = 1),
          plot.title = element_text(size=23),
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.key = element_rect(colour="white"),
          legend.key.size = unit(10,"cm"),
          legend.margin = unit(0, "cm"),
          legend.key.height = unit(2,"cm"),
          legend.key.width = unit(2,"cm"),
          axis.text = element_text(size=20),
          axis.text.x = element_text(size=15, angle = 45, hjust = 1),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size=23, vjust = 1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(colour = "black"),
          panel.background = element_rect(fill = NA))+
    guides(fill = guide_legend(keywidth = 1, keyheight = 1),
           colour = guide_legend(keywidth = 3, keyheight = 1))
  p2 <- p2 +
    guides(colour=guide_legend(override.aes = list(size=2)))
  #         p2            = add.event.vline.exp2(p2)
  
  rownum_expDate <- set.expDate.rownum(plot_dt, expDate)

  stats <- ggplot(plot_dt, aes(x=get)) +
    #       geom_point(aes(y=peak, color='peak')) +
    ylab("electricity usage(kWh)")+
    ggtitle(plot_name)
  stats = add.window.line(stats, plot_dt, plot_name, "peak", windowingWeek, rownum_expDate)
  
  stats = stats + 
    scale_linetype_discrete(breaks=c("peak"), labels=c("90% of peak"))+
    scale_linetype_manual(values=c("dashed"))
  
  if(expDate[4] == "2016-11-16"){
    #exp1-1
    stats = add.event.vline.exp1.1(stats)
  } else if(expDate[4] == "2015-01-22"){
    #exp1-2
    stats = add.event.vline.exp1.2(stats)
  } else{
    #exp3
    stats = add.event.vline.exp2(stats)
  }

  stats = set.default.theme(stats)
  stats <- stats +
    theme(plot.margin= unit(c(0,2,0,0),"lines"),
          legend.key.height = unit(2,"cm"),
          legend.key.width = unit(2,"cm"))+
    guides(fill = guide_legend(keywidth = 1, keyheight = 1),
           linetype=guide_legend(override.aes = list(size=1.5)),
           colour=guide_legend(keywidth = 3, keyheight = 1))
  
  g1<-ggplot_gtable(ggplot_build(stats))
  g2<-ggplot_gtable(ggplot_build(p2))
  
  pp<-c(subset(g1$layout, name == "panel", se = t:r))
  g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, pp$l, pp$b, pp$l)
  
  #get the y-axis of p2(temperature plot) 
  ia<-which(g2$layout$name=="axis-l")
  ga <- g2$grobs[[ia]]
  ax <- ga$children[[2]]
  ax$widths <- rev(ax$widths)
  ax$grobs <- rev(ax$grobs)
  ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(0, "npc") + unit(0, "cm")
  g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
  g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
  g <- gtable_add_grob(g, g2$grobs[[7]], pp$t, length(g$widths), pp$b)
  
  leg1 <- g1$grobs[[which(g1$layout$name == "guide-box")]]
  leg2 <- g2$grobs[[which(g2$layout$name == "guide-box")]]
  g$grobs[[which(g$layout$name == "guide-box")]] <- gtable:::cbind_gtable(leg1, leg2, "first")
  
  #     il <- which(g2$layout$name == "ylab")
  #     gl <- g2$grobs[[il]]
  #     gl$x <- gl$x - unit(1, "npc") + unit(0.15, "cm")
  #     g <- gtable_add_cols(g, g2$widths[g2$layout[il, ]$l], length(g$widths) - 1)
  #     g <- gtable_add_grob(g, gl, pp$t, length(g$widths) - 1, pp$b)
  #     
  #     grid.arrange(g, ncol=1, heights=c(10, 1),widths =c(1) ,as.table =TRUE)
  
  png(file = paste0("../plots/weather_",plot_name, ".png"), width = 800, height = 600)
  grid.draw(g)
  dev.off()
  
  return(g)
}

# 
# expDate <- get.expDate.3()
# 
# start.time <- Sys.time()
# for(i in 2:length(return_dts)){ 
#   plot_dt   = return_dts[[i]]
#   
#   plot_name = names(return_dts[i])
#   
#   if((strsplit(plot_name,"_")[[1]][4] != "hvac") | (strsplit(plot_name,"_")[[1]][1] != "MARG")){
#     next
#   }
#   
#   if(strsplit(plot_name,"_")[[1]][2] == "aggWeek") {
#     temp_weather_dt = aggWeek_weather_dt[get >= "2014-10-01" & get <= "2016-07-26"]
#   }else {
#     temp_weather_dt = weather_dt[get >= "2014-10-01" & get <= "2016-07-26"]
#   }
#   
#   ##windowingWeek must be even
#   for(windowingWeek in c(4)) {
#     
#     rownum_expDate <- set.expDate.rownum(temp_weather_dt, expDate)
#     
#     grid.newpage()
#     plot_name = names(return_dts[i])
#     plot_name = paste(plot_name, "+ Temperature - windowing ", windowingWeek,"weeks")  
#     print(plot_name)
#     
#     #     print(strsplit(plot_name,"_")[[1]][2])
#     #     print(rownum_expDate)
#     #     print(head(temp_weather_dt))
#     #     print(head(w_max_tmp))
#     #     print(nrow(w_max_tmp))
#     
#     p2 <- ggplot(temp_weather_dt, aes(x=get)) +
#       ylab("temperature(°C)")+
#       scale_x_date(labels = date_format("%Y-%m"), breaks = date_breaks("month"), limits = c(as.Date("2014-10-01"), as.Date("2016-07-26"))) +
#       ylim(-20,40)+
#       scale_color_manual(values=c("dodgerblue2", "skyblue", "midnightblue"), 
#                          breaks=c("max_temp", "avg_temp", "min_temp"))
#     p2 = add.colorful.window.line(p2, temp_weather_dt, 'max_temp', windowingWeek, "dodgerblue2", rownum_expDate, ribbon=FALSE)
#     p2 = add.colorful.window.line(p2, temp_weather_dt, 'avg_temp', windowingWeek, "skyblue", rownum_expDate, ribbon=FALSE)
#     p2 = add.colorful.window.line(p2, temp_weather_dt, 'min_temp', windowingWeek, "midnightblue", rownum_expDate, ribbon=FALSE)
#     
#     p2 = p2 + 
#       theme_bw()+
#       theme(axis.line = element_line(colour = "black"),
#             legend.text = element_text(size=23, hjust = 1),
#             plot.title = element_text(size=23),
#             legend.position = "bottom",
#             legend.title = element_blank(),
#             legend.key = element_rect(colour="white"),
#             legend.key.size = unit(10,"cm"),
#             legend.margin = unit(0, "cm"),
#             legend.key.height = unit(2,"cm"),
#             legend.key.width = unit(2,"cm"),
#             axis.text = element_text(size=20),
#             axis.text.x = element_text(size=15, angle = 45, hjust = 1),
#             axis.title.x = element_blank(),
#             axis.title.y = element_text(size=23, vjust = 1),
#             panel.grid.major = element_blank(),
#             panel.grid.minor = element_blank(),
#             panel.border = element_rect(colour = "black"),
#             panel.background = element_rect(fill = NA))+
#       guides(fill = guide_legend(keywidth = 1, keyheight = 1),
#              colour = guide_legend(keywidth = 3, keyheight = 1))
#     p2 <- p2 +
#       guides(colour=guide_legend(override.aes = list(size=2)))
#     #         p2            = add.event.vline.exp3(p2)
#     
#     rownum_expDate <- set.expDate.rownum(plot_dt, expDate)
#     
#     #     print("plot_dt")
#     #     print(rownum_expDate)
#     
#     stats <- ggplot(plot_dt, aes(x=get)) +
#       #       geom_point(aes(y=peak, color='peak')) +
#       ylab("electricity usage(kWh)")+
#       ggtitle(plot_name)
#     stats = add.window.line(stats, plot_dt, "peak", windowingWeek, rownum_expDate)
#     
#     stats = stats + 
#       scale_linetype_discrete(breaks=c("peak"), labels=c("90% of peak"))+
#       scale_linetype_manual(values=c("dashed"))
#     stats            = add.event.vline.exp3(stats)
#     
#     stats = set.default.theme(stats)
#     stats <- stats +
#       theme(plot.margin= unit(c(0,2,0,0),"lines"),
#             legend.key.height = unit(2,"cm"),
#             legend.key.width = unit(2,"cm"))+
#       guides(fill = guide_legend(keywidth = 1, keyheight = 1),
#              linetype=guide_legend(override.aes = list(size=1.5)),
#              colour=guide_legend(keywidth = 3, keyheight = 1))
#     
#     g1<-ggplot_gtable(ggplot_build(stats))
#     g2<-ggplot_gtable(ggplot_build(p2))
#     
#     pp<-c(subset(g1$layout, name == "panel", se = t:r))
#     g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, pp$l, pp$b, pp$l)
#     
#     #get the y-axis of p2(temperature plot) 
#     ia<-which(g2$layout$name=="axis-l")
#     ga <- g2$grobs[[ia]]
#     ax <- ga$children[[2]]
#     ax$widths <- rev(ax$widths)
#     ax$grobs <- rev(ax$grobs)
#     ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(0, "npc") + unit(0, "cm")
#     g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
#     g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
#     g <- gtable_add_grob(g, g2$grobs[[7]], pp$t, length(g$widths), pp$b)
#     
#     leg1 <- g1$grobs[[which(g1$layout$name == "guide-box")]]
#     leg2 <- g2$grobs[[which(g2$layout$name == "guide-box")]]
#     g$grobs[[which(g$layout$name == "guide-box")]] <- gtable:::cbind_gtable(leg1, leg2, "first")
#     
#     #     il <- which(g2$layout$name == "ylab")
#     #     gl <- g2$grobs[[il]]
#     #     gl$x <- gl$x - unit(1, "npc") + unit(0.15, "cm")
#     #     g <- gtable_add_cols(g, g2$widths[g2$layout[il, ]$l], length(g$widths) - 1)
#     #     g <- gtable_add_grob(g, gl, pp$t, length(g$widths) - 1, pp$b)
#     #     
#     #     grid.arrange(g, ncol=1, heights=c(10, 1),widths =c(1) ,as.table =TRUE)
#     
#     png(file = paste0("../plots/weather_",plot_name, ".png"), width = 800, height = 600)
#     grid.draw(g)
#     dev.off()
#     
#     
#   }
# }
# end.time <- Sys.time()
# end.time-start.time


#build table
table_weather <- build.table.weather()

#plot
for(i in 1:length(table_stats)){
  plot_hvac_with_weather <- plot.hvac.with.weather(table_stats[i], table_weather, get.expDate.2())
}



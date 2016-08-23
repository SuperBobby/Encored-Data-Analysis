
partial_lightON_color = "orange2"
lightON_duration_color = "darkolivegreen"
full_lightON_color = "violetred4"
strong_light_color = "darkblue"

## 1. Four stats plots  +  partial_lightON & lightON duration plots 
expDate <- get.expDate.3()

start.time <- Sys.time()
for(i in 2:length(return_dts)){ 
  plot_dt   = return_dts[[i]]
  
  rownum_expDate <- set.expDate.rownum(plot_dt, expDate)

  ##windowingWeek must be even
  for(windowingWeek in c(4)) {
    
    plot_name = paste(names(return_dts[i]), "- windowing ", windowingWeek,"weeks")  
    print(plot_name)
    
    stats <- ggplot(plot_dt, aes(x=get)) +
      ggtitle(plot_name) +
      ylab("Energy use (kWh/day)")+
      scale_linetype_discrete(breaks=c("peak", "avg", "base"))
    stats = add.window.line(stats, plot_dt, "peak", windowingWeek, rownum_expDate)
    stats = add.window.line(stats, plot_dt, "base", windowingWeek, rownum_expDate)
    stats = add.window.line(stats, plot_dt, "avg", windowingWeek, rownum_expDate)
    
    
    if(grepl("light", plot_name)){
      partial_lightON <- ggplot(plot_dt, aes(x=get)) +
        ggtitle(plot_name)+
        ylab("Partial light-ON (%)")
      partial_lightON = add.colorful.window.line(partial_lightON, plot_dt, 'threshold80', windowingWeek, partial_lightON_color)
      
      lightON_duration <- ggplot(plot_dt, aes(x=get)) +
        ggtitle(plot_name)+
        ylim(0,24)+
        ylab("Light-ON duration (hours)")+
        scale_color_discrete(breaks = c("lightON"), labels = c("number of lightON blocks(15min)"))
      lightON_duration = add.colorful.window.line(lightON_duration, plot_dt, 'lightON',windowingWeek, lightON_duration_color)
      
      stats            = add.event.vline(stats)
      partial_lightON  = add.event.vline(partial_lightON)
      lightON_duration = add.event.vline(lightON_duration)
      stats            = set.default.theme(stats)
      partial_lightON  = set.colorful.theme(partial_lightON, partial_lightON_color)
      lightON_duration = set.colorful.theme(lightON_duration, lightON_duration_color)
      
      save.plot(paste0("../plots/",plot_name, "_1.png"), stats)
      save.plot(paste0("../plots/",plot_name, "_2.png"), partial_lightON)
      save.plot(paste0("../plots/",plot_name, "_3.png"), lightON_duration)

    } else {
      
      stats            = add.event.vline(stats)
      stats            = set.default.theme(stats)
      
      save.plot(paste0("../plots/",plot_name, ".png"), stats)
    }
  }
}
end.time <- Sys.time()
end.time-start.time


###
## 1-1. MARG exp1
expDate <- get.expDate.1()


start.time <- Sys.time()
for(i in 2:length(MARG_return_dts)){ 
  plot_dt   = MARG_return_dts[[i]]
  
  #Exp1
  plot_dt = plot_dt[get >= "2014-09-11" & get <= "2014-12-16"]
  
  rownum_expDate <- set.expDate.rownum(plot_dt, expDate)
  
  ##windowingWeek must be even
  for(windowingWeek in c(4,8)) {
    
    plot_name = names(MARG_return_dts[i])
    
    if((strsplit(plot_name,"_")[[1]][4] != "computer") & (strsplit(plot_name,"_")[[1]][4] != "light") & (strsplit(plot_name,"_")[[1]][4] != "total")){
      next
    }
    
    plot_name = paste(names(MARG_return_dts[i]), "- windowing ", windowingWeek,"weeks")  
    
    print(plot_name)
    
    stats <- ggplot(plot_dt, aes(x=get)) +
      ggtitle(plot_name) +
      ylab("Energy use (kWh/day)")+
      scale_linetype_discrete(breaks=c("peak", "avg", "base"))
    stats = add.window.line(stats, plot_dt, "peak", windowingWeek, rownum_expDate)
    stats = add.window.line(stats, plot_dt, "base", windowingWeek, rownum_expDate)
    stats = add.window.line(stats, plot_dt, "avg", windowingWeek, rownum_expDate)
    
    if(grepl("light", plot_name)){
      partial_lightON <- ggplot(plot_dt, aes(x=get)) +
        ggtitle(plot_name)+
        ylab("Partial light-ON (%)")
      partial_lightON = add.colorful.window.line(partial_lightON, plot_dt, 'threshold80', windowingWeek, partial_lightON_color)
      
      lightON_duration <- ggplot(plot_dt, aes(x=get)) +
        ggtitle(plot_name)+
        ylim(0,24)+
        ylab("Light-ON duration (hours)")+
        scale_color_discrete(breaks = c("lightON"), labels = c("number of lightON blocks(15min)"))
      lightON_duration = add.colorful.window.line(lightON_duration, plot_dt, 'lightON',windowingWeek, lightON_duration_color)
      
      stats            = add.event.vline.exp1(stats)
      partial_lightON  = add.event.vline.exp1(partial_lightON)
      lightON_duration = add.event.vline.exp1(lightON_duration)
      stats            = set.default.theme(stats)
      partial_lightON  = set.colorful.theme(partial_lightON, partial_lightON_color)
      lightON_duration = set.colorful.theme(lightON_duration, lightON_duration_color)
      
      save.plot(paste0("../plots/Exp2_",plot_name, "_1.png"), stats)
      save.plot(paste0("../plots/Exp2_",plot_name, "_2.png"), partial_lightON)
      save.plot(paste0("../plots/Exp2_",plot_name, "_3.png"), lightON_duration)
      
    } else if(grepl("total", plot_name)){
      #       save.wide.plot(paste0("../plots/Exp1_",plot_name, ".png"), stats)
      save.wide.plot(paste0("../plots/Exp2_",plot_name, ".png"), stats)
    } else{
      
      stats            = add.event.vline.exp1(stats)
      stats            = set.default.theme(stats)
      
      save.plot(paste0("../plots/Exp2_",plot_name, ".png"), stats)
    }
  }
}
end.time <- Sys.time()
end.time-start.time

###
## 1-2. MARG exp2
expDate <- get.expDate.2()

start.time <- Sys.time()
for(i in 2:length(MARG_return_dts)){ 
  plot_dt   = MARG_return_dts[[i]]
  
  #Exp2
  plot_dt = plot_dt[get >= "2014-09-11" & get < "2015-06-01"]
  
  rownum_expDate <- set.expDate.rownum(plot_dt, expDate)
  
  ##windowingWeek must be even
  for(windowingWeek in c(4,8)) {
    
    plot_name = names(MARG_return_dts[i])
    
    if((strsplit(plot_name,"_")[[1]][4] != "computer") & (strsplit(plot_name,"_")[[1]][4] != "light") & (strsplit(plot_name,"_")[[1]][4] != "total")){
      next
    }
    
    plot_name = paste(names(MARG_return_dts[i]), "- windowing ", windowingWeek,"weeks")  
    
    print(plot_name)
    
    stats <- ggplot(plot_dt, aes(x=get)) +
      ggtitle(plot_name) +
      ylab("Energy use (kWh/day)")+
      scale_linetype_discrete(breaks=c("peak", "avg", "base"))
    stats = add.window.line(stats, plot_dt, "peak", windowingWeek, rownum_expDate)
    stats = add.window.line(stats, plot_dt, "base", windowingWeek, rownum_expDate)
    stats = add.window.line(stats, plot_dt, "avg", windowingWeek, rownum_expDate)
    
    #Exp2
    stats = stats + 
      scale_x_date("Timestamp", labels = date_format("%Y-%m"), breaks = date_breaks("month")) +
      theme_bw()+
      geom_vline(aes(xintercept = as.numeric(as.Date("2014-11-10"))),color="gray40", linetype = "longdash") +
      geom_vline(aes(xintercept = as.numeric(as.Date("2014-11-16"))),color="gray40", linetype = "longdash") +
      geom_vline(aes(xintercept = as.numeric(as.Date("2015-01-15"))),color="gray40", linetype = "longdash") +
      geom_vline(aes(xintercept = as.numeric(as.Date("2015-01-21"))),color="gray40", linetype = "longdash")
    stats            = set.default.theme(stats)
    
    if((strsplit(plot_name,"_")[[1]][4] == "total")){
      #       save.wide.plot(paste0("../plots/Exp1_",plot_name, ".png"), stats)
      save.wide.plot(paste0("../plots/Exp2_",plot_name, ".png"), stats)
    }else{
      #       save.plot(paste0("../plots/Exp1_",plot_name, ".png"), stats)
      save.plot(paste0("../plots/Exp2_",plot_name, ".png"), stats)
    }
  }
}
end.time <- Sys.time()
end.time-start.time

###
## 2. lunch light OFF plots --> 상단 1번 과정 안에 통합되어야 함 

get.lunch.lightOFF <- function(dt, threshold){
  # print(dt)
  
  before_lunch = max(dt$light[17:20], na.rm = T) # 11:00 ~ 12:00
  during_lunch = min(dt$light[19:26], na.rm = T) # 11:30 ~ 13:30 
  after_luhch = max(dt$light[25:28], na.rm = T) # 13:00 ~ 14:00 
  
  # print(paste(before_lunch, during_lunch, after_luhch))
  
  if(during_lunch < before_lunch * threshold & 
       during_lunch <  after_luhch * threshold){
    return(1)
  } else {
    return(0)
  }
}

for(lab in 1:4){ 
  # lab_dt & name selection 
  lab_dt = dt_list[[lab]]
  lab_name = lab_names[lab]
  
  lab_dt$light <- na.locf(lab_dt$light)
  
  rownum_expDate <- 1
  
  lunch_dt_aggDay <- lab_dt[, .(aggWeek=as.Date(cut(aggDay, breaks = "week", start.on.monday = T)),
                                lunch_lightOFF_80 = get.lunch.lightOFF(.SD, 0.8)), by=aggDay]
  
  setnames(lunch_dt_aggDay,old="aggDay",new="get")
  
  lunch_dt_aggWeek <- lunch_dt_aggDay[, .(lunch_lightOFF_80 = sum(lunch_lightOFF_80)), by=aggWeek]
  
  setnames(lunch_dt_aggWeek,old="aggWeek",new="get")
  
  aggDay_rownum_expDate <- set.expDate.rownum(lunch_dt_aggDay, expDate)
  
  aggWeek_rownum_expDate <- set.expDate.rownum(lunch_dt_aggWeek, expDate)
  
  for(windowingWeek in c(4,8)) {
    
    plot_name = paste(lab_name, "lunch light OFF count - windowing", windowingWeek,"weeks")  
    print(plot_name)
    
    rownum_expDate <- aggDay_rownum_expDate
    
    p1 <- ggplot(lunch_dt_aggDay, aes(x=get)) +
      ggtitle(paste(plot_name, "aggDay")) +
      ylab("lunch light On = 0; Off = 1")
    
    p1 = add.window.line(p1, lunch_dt_aggDay, 'lunch_lightOFF_80', windowingWeek, rownum_expDate)
    
    rownum_expDate <- aggWeek_rownum_expDate
    
    p2 <- ggplot(lunch_dt_aggWeek, aes(x=get)) +           
      ggtitle(paste(plot_name, "aggWeek"))+
      ylab("number of lunch_lightOFF days")
    
    p2 = add.window.line(p2, lunch_dt_aggWeek, 'lunch_lightOFF_80', windowingWeek, rownum_expDate)
    
    p1 = add.event.vline(p1)
    p2 = add.event.vline(p2)
    p1 = set.default.theme(p1)
    p2 = set.default.theme(p2)
    
    #     plots <- arrangeGrob(p1, p2)
    
    save.plot(paste0("../plots/",plot_name, "_1.png"), p1)
    save.plot(paste0("../plots/",plot_name, "_2.png"), p2)
  }
}

### 
## 3. strong_light counting light 
## ==> # of over "peak * (0.5~0.8)" in 15mins 

for(lab in 1:4){ 
  # lab_dt & name selection 
  lab_dt = dt_list[[lab]]
  lab_name = lab_names[lab]
  
  print(lab_name)
  
  #   lab_dt$light <- na.locf(lab_dt$light)
  
  light_peak = quantile(lab_dt$light, .90, na.rm = T)
  print(light_peak)
  
  strong_light_aggDay_dt = lab_dt[, .(strong_light_80 = sum(light > (light_peak * 0.8), na.rm = T)*15.0/60.0), by=aggDay]
  setnames(strong_light_aggDay_dt,old="aggDay",new="get")
  
  strong_light_aggWeek_dt = lab_dt[, .(strong_light_80 = sum(light > (light_peak * 0.8), na.rm = T)*15.0/60.0), by=aggWeek]
  setnames(strong_light_aggWeek_dt,old="aggWeek",new="get")
  
  strong_light_aggWeek_dt$strong_light_80 <- strong_light_aggWeek_dt$strong_light_80/7.0
  
  aggDay_rownum_expDate <- 1
  
  for (d in expDate) {
    aggDay_rownum_expDate <- append(aggDay_rownum_expDate, (nrow(lunch_dt_aggDay[d>lunch_dt_aggDay$get,])+1))
  }
  
  aggWeek_rownum_expDate <- 1
  
  for (d in expDate) {
    aggWeek_rownum_expDate <- append(aggWeek_rownum_expDate, (nrow(lunch_dt_aggWeek[d>lunch_dt_aggWeek$get,])+1))
  }
  
  for(windowingWeek in c(4,8)) {
    
    plot_name = paste(lab_name, "strong_light count - windowing", windowingWeek,"weeks")  
    print(plot_name)
    
    rownum_expDate <- aggDay_rownum_expDate
    
    p1 <- ggplot(strong_light_aggDay_dt, aes(x=get)) +         
      ggtitle(paste(plot_name, "aggDay"))+
      ylab("strong light-ON duration (hrs/day)")
    
    p1 = add.colorful.window.line(p1, strong_light_aggDay_dt, 'strong_light_80', windowingWeek, strong_light_color)
    
    rownum_expDate <- aggWeek_rownum_expDate
    
    p2 <- ggplot(strong_light_aggWeek_dt, aes(x=get)) +
      ggtitle(paste(plot_name, "aggWeek"))+
      ylab("strong light-ON average duration (hrs/day)")
    
    p2 = add.colorful.window.line(p2, strong_light_aggWeek_dt, 'strong_light_80', windowingWeek, strong_light_color)
    
    p1 = add.event.vline(p1)
    p2 = add.event.vline(p2)
    p1 = set.colorful.theme(p1, strong_light_color)
    p2 = set.colorful.theme(p2, strong_light_color)
    
    #     plots <- arrangeGrob(p1, p2)
    
    save.plot(paste0("../plots/",plot_name, "_1.png"), p1)
    save.plot(paste0("../plots/",plot_name, "_2.png"), p2)
    #     ggsave(file = paste0("../plots/", plot_name, ".png"), width = 8, height = 12, dpi = 300, plots)
  }
}


### 
## 4. full_lightON counting : 24hours 
## ==> # of "over peak*(0.5~0.8)" == 96?

for(lab in 1:4){ 
  # lab_dt & name selection 
  lab_dt = dt_list[[lab]]
  lab_name = lab_names[lab]
  
  print(lab_name)
  
  #   lab_dt$light <- na.locf(lab_dt$light)
  
  light_peak = quantile(lab_dt$light, .90, na.rm = T)
  print(light_peak)
  
  full_lightON_aggDay_dt = lab_dt[, .(full_lightON_10 = sum(light > (light_peak * 0.1), na.rm = T)==96), by=aggDay]
  setnames(full_lightON_aggDay_dt,old="aggDay",new="get")
  
  full_lightON_aggWeek_dt = full_lightON_aggDay_dt[, .(full_lightON_10 = sum(full_lightON_10, na.rm = T)), by=.(aggWeek=as.Date(cut(get, breaks = "week", start.on.monday = T)))]
  setnames(full_lightON_aggWeek_dt,old="aggWeek",new="get")
  
  rownum_expDate <- 1
  
  for (d in expDate) {
    rownum_expDate <- append(rownum_expDate, (nrow(lunch_dt_aggWeek[d>lunch_dt_aggWeek$get,])+1))
  }
  
  for(windowingWeek in c(4,8)) {
    
    plot_name = paste(lab_name, "full(24hrs)_lightON count - windowing", windowingWeek,"weeks")  
    print(plot_name)
    
    p <- ggplot(full_lightON_aggWeek_dt, aes(x=get)) +
      #       
      #       #                         geom_point(aes(y=full_lightON_50, color='full_lightON_50')) +
      #       #                         geom_smooth(aes(y=full_lightON_50, color='full_lightON_50'), span = s) +    
      #       #                         
      #       #                         geom_point(aes(y=full_lightON_40, color='full_lightON_40')) +
      #       #                         geom_smooth(aes(y=full_lightON_40, color='full_lightON_40'), span = s) +             
      #       
      #       geom_point(aes(y=full_lightON_30, color='full_lightON_30')) +        
      #       geom_point(aes(y=full_lightON_20, color='full_lightON_20')) +
      geom_point(aes(y=full_lightON_10), colour='gray70') +
      ggtitle(plot_name)+
      ylab("24hrs light-ON day (count/week)")
    # +
    #       theme(legend.text = element_text(size=20),
    #             plot.title = element_text(size=20),
    #             legend.title = element_blank(),
    #             axis.text = element_text(size=15),
    #             axis.title = element_text(size=15))+
    #       guides(fill = guide_legend(keywidth = 1, keyheight = 1),
    #              linetype=guide_legend(keywidth = 3, keyheight = 2),
    #              colour=guide_legend(keywidth = 3, keyheight = 2))
    
    #     p = add.window.line(p, full_lightON_aggWeek_dt, 'full_lightON_30', windowingWeek, rownum_expDate)
    #     p = add.window.line(p, full_lightON_aggWeek_dt, 'full_lightON_20', windowingWeek, rownum_expDate)
    p = add.colorful.window.line(p, full_lightON_aggWeek_dt, 'full_lightON_10', windowingWeek, full_lightON_color)
    p = add.event.vline(p)
    p = set.colorful.theme(p, full_lightON_color)
    
    save.plot(paste0("../plots/",plot_name, ".png"), p)
    #     ggsave(file = paste0("../plots/", plot_name, ".png"), width = 20, height = 10, dpi = 200, p)
  }
}


### 
## 5. MARG HVAC + weahter(max, avg, min temperature info) 
## 
expDate <- get.expDate.3()

start.time <- Sys.time()
for(i in 2:length(return_dts)){ 
  plot_dt   = return_dts[[i]]
  
  plot_name = names(return_dts[i])
  
  if((strsplit(plot_name,"_")[[1]][4] != "hvac") | (strsplit(plot_name,"_")[[1]][1] != "MARG")){
    next
  }
  
  if(strsplit(plot_name,"_")[[1]][2] == "aggWeek") {
    temp_weather_dt = aggWeek_weather_dt[get >= plot_dt$get[1] & get <= plot_dt$get[nrow(plot_dt)]]
  }else {
    temp_weather_dt = weather_dt[get >= plot_dt$get[1] & get <= plot_dt$get[nrow(plot_dt)]]  
  }
  
  ##windowingWeek must be even
  for(windowingWeek in c(4,8)) {
    
    rownum_expDate <- 1
    for (d in expDate) {
      rownum_expDate <- append(rownum_expDate, (nrow(temp_weather_dt[d>temp_weather_dt$get,])+1))
    }
    
    grid.newpage()
    plot_name = names(return_dts[i])
    plot_name = paste(plot_name, "+ Temperature - windowing ", windowingWeek,"weeks")  
    print(plot_name)
    
    print(strsplit(plot_name,"_")[[1]][2])
    print(rownum_expDate)
    print(head(temp_weather_dt))
    print(head(w_max_tmp))
    print(nrow(w_max_tmp))
    
    p2 <- ggplot(temp_weather_dt, aes(x=get)) +
      ylab("temperature(°C)")+
      ylim(-20,40)+
      scale_color_manual(values=c("dodgerblue2", "skyblue", "midnightblue"), 
                         breaks=c("max_temp", "avg_temp", "min_temp"))
    p2 = add.colorful.window.line(p2, temp_weather_dt, 'max_temp', windowingWeek, "dodgerblue2", ribbon=FALSE)
    p2 = add.colorful.window.line(p2, temp_weather_dt, 'avg_temp', windowingWeek, "skyblue", ribbon=FALSE)
    p2 = add.colorful.window.line(p2, temp_weather_dt, 'min_temp', windowingWeek, "midnightblue", ribbon=FALSE)
    
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
    #     p2            = add.event.vline(p2)
    
    rownum_expDate <- 1
    for (d in expDate) {
      rownum_expDate <- append(rownum_expDate, (nrow(plot_dt[d>plot_dt$get,])+1))
    }
    
    print("plot_dt")
    print(rownum_expDate)
    
    stats <- ggplot(plot_dt, aes(x=get)) +
      #       geom_point(aes(y=peak, color='peak')) +
      ylab("electricity usage(kWh)")+
      ggtitle(plot_name)
    stats = add.window.line(stats, plot_dt, "peak", windowingWeek, rownum_expDate)
    
    stats = stats + 
      scale_linetype_discrete(breaks=c("peak"), labels=c("90% of peak"))+
      scale_linetype_manual(values=c("dashed"))
    stats            = add.event.vline(stats)
    
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
    
    ia<-which(g2$layout$name=="axis-l")
    ga <- g2$grobs[[ia]]
    ax <- ga$children[[2]]
    ax$widths <- rev(ax$widths)
    ax$grobs <- rev(ax$grobs)
    ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(0, "npc") + unit(0, "cm")
    g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
    g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
    #     g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths))
    #     g <- gtable_add_grob(g, ax, pp$t, length(g$widths), pp$b)
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
    
  }
}
end.time <- Sys.time()
end.time-start.time



###
## 6. RealSense plot
##

start.time <- Sys.time()
expDate <- get.expDate.3()

for(i in 2:length(return_rs_dts)){ 
  plot_dt   = return_rs_dts[[i]]
  
  plot_dt = plot_dt[get >= "2015-10-08" & get <= "2016-07-18"]
  
  rownum_expDate <- set.expDate.rownum(plot_dt, expDate)
  
  ##windowingWeek must be even, 
  ###add.window.line function will window from the point before (windowingWeek/2) weeks and the point after (windowingWeek/2) weeks
  for(windowingWeek in c(4,8)) {
    
    plot_name = paste(names(return_rs_dts[i]), "- windowing ", windowingWeek,"weeks")  
    print(plot_name)
    
    if(strsplit(plot_name,"_")[[1]][2] == "aggWeek"){
      ylim_RS_duration <- 2000
      ylim_RS_freq <- 1000
    } else{
      ylim_RS_duration <- 600
      ylim_RS_freq <- 300
    }    
    
    RS_duration <- ggplot(plot_dt, aes(x=get, ymax=ylim_RS_duration))+
      #       geom_point(aes(y=sum_of_duration, color='sum_of_duration')) +
      #       geom_text(aes(y=sum_of_duration, label=round(sum_of_duration, 0)), position=position_dodge(width=0.9), vjust=-0.25, colour="grey50") + 
      scale_y_continuous(limits=c(0,ylim_RS_duration), oob=rescale_none) +
      ggtitle(paste("RS", plot_name))+
      theme_bw()+
      ylab("total duration(seconds)")
    
    RS_freq <- ggplot(plot_dt, aes(x=get, ymax=ylim_RS_freq))+
      #       geom_point(aes(y=freq, color='freq')) +
      #       geom_text(aes(y=freq, label=round(freq, 0)), position=position_dodge(width=0.9), vjust=-0.25, colour="grey50") + 
      scale_y_continuous(limits=c(0,ylim_RS_freq), oob=rescale_none) +
      ggtitle(paste("RS", plot_name))+
      theme_bw()+
      ylab("(counts / day)")
    
    RS_duration      = add.window.line(RS_duration, plot_dt, "sum_of_duration", windowingWeek, rownum_expDate)
    RS_freq          = add.window.line(RS_freq, plot_dt, "freq", windowingWeek, rownum_expDate)
    
    RS_duration      = add.event.vline(RS_duration)
    RS_freq          = add.event.vline(RS_freq)
    
    RS_duration      = set.colorful.theme(RS_duration, "black")
    RS_freq          = set.colorful.theme(RS_freq, "black")    
    
    save.plot(paste0("../plots/RealSense_",plot_name, "_1.png"),RS_duration)
    save.plot(paste0("../plots/RealSense_",plot_name, "_2.png"),RS_freq)
    #     plots <- arrangeGrob(RS_duration, RS_freq, ncol=1)
    #     ggsave(file = paste0("../plots/RealSense_",plot_name, ".png"), width = 10, height = 10, dpi = 300, plots, limitsize=FALSE)
  }
}
end.time <- Sys.time()
end.time-start.time


###
## 7. lunch saving
##
expDate <- get.expDate.1()
expDate <- get.expDate.2()
expDate <- get.expDate.3()


plot.lunch.saving('marg', marg_dt, 'light')
plot.lunch.saving('hcc', hcc_dt, 'light')
plot.lunch.saving('ux', ux_dt, 'light')

plot.lunch.saving('marg', marg_dt, 'computer')
plot.lunch.saving('hcc', hcc_dt, 'computer')
plot.lunch.saving('ux', ux_dt, 'computer')

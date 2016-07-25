


Suwon_weather = fread("../rawData/Suwon_weather.csv")

plot(Suwon_weather$date_index, Suwon_weather$)


weather <- ggplot(Suwon_weather, aes(x=date_index)) +
            geom_line(aes(avg_temp, color='avg_temp')) + 
            geom_line(aes(max_temp, color='max_temp')) + 
            geom_line(aes(min_temp, color='min_temp'))
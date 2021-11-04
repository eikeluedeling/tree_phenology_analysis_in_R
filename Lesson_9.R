library(chillR)

### Today we want to download weather data from a database
### The Global Summary of the Day
?handle_gsod

### The German Weather Service
?handle_dwd


### Get a list of stations close to a location of interest
station_list <- handle_gsod(action="list_stations",
                            location=c(7.10,50.73),
                            time_interval=c(1990,2020))

station_list

station_list_dwd <- handle_dwd(action="list_stations",
                          location=c(7.10,50.73),
                          time_interval=c(1990,2020))

station_list_dwd <- handle_dwd(action="list_stations",
                             location=c(7.10,50.73),
                             c(19160101, Date2YEARMODA(Sys.Date()))
                             )

station_list_dwd

### download weather data for one of these stations

weather <- handle_gsod(action="download_weather",
                     location=station_list$chillR_code[4],
                     time_interval=c(1990,2020))


weather_dwd <- handle_dwd(action = "download_weather",
                     location = station_list_dwd[1 : 3, "Station_ID"],
                     time_interval = c(20000101, 20210601),
                     stations_to_choose_from = 25,
                     station_list = station_list_dwd,
                     drop_most = TRUE, 
                     add.DATE = FALSE,
                     quiet = TRUE,
                     add_station_name = FALSE)

weather

weather_dwd

### "clean" weather data

weather_clean <- handle_gsod(weather)

weather_dwd_clean <- handle_dwd(weather_dwd)

### We can now save this for future use

dir.create("data")

write.csv(station_list,"data/station_list.csv",row.names=FALSE)

write.csv(station_list_dwd,"data/station_list_dwd.csv",row.names=FALSE)

write.csv(weather_clean$weather,"data/Bonn_chillR_weather.csv",
          row.names=FALSE)

write.csv(weather_dwd_clean$Aachen,"data/weather_aachen.csv",row.names=FALSE)






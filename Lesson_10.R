library(chillR)
weather<-make_all_day_table(KA_weather)
Tmin_int<-interpolate_gaps(KA_weather[,"Tmin"])
weather[,"Tmin"]<-Tmin_int$interp
weather[,"Tmin_interpolated"]<-Tmin_int$missing

Tmax_int<-interpolate_gaps(KA_weather[,"Tmax"])
weather[,"Tmax"]<-Tmax_int$interp
weather[,"Tmax_interpolated"]<-Tmax_int$missing

# add an extra day to the KA_weather dataset that is not connected to the days that are already there.
# this creates a large gap, which we can then interpolate
KA_weather_gap<-rbind(KA_weather,c(Year=2011,Month=3,Day=3,Tmax=26,Tmin=14)) 

# fill in the gaps between Julian date 300 (late October) and 100 (early April), only returning data between 2000 and 2011
fixed_winter_days<-fix_weather(KA_weather_gap, start_year=2000, end_year=2011, start_date=300, end_date=100)

# fill in all gaps
fixed_all_days<-fix_weather(KA_weather_gap)

fixed_all_days$weather

fixed_all_days$QC


gap_weather<-KA_weather[200:305,]
gap_weather[,"Tmin_observed"]<-gap_weather$Tmin
gap_weather$Tmin[c(2,4:5,7:9,11:14,16:20,22:27,29:35,37:44,46:54,56:65,67:77,79:90,92:104)]<-NA
fixed_gaps<-fix_weather(gap_weather)$weather

library(ggplot2)

ggplot(data=fixed_gaps,aes(DATE,Tmin_observed)) +
  geom_line(lwd=1.3) +
  xlab("Date") +
  ylab("Daily minimum temperature (°C)") +
  geom_line(data=fixed_gaps,aes(DATE,Tmin),col="red",lwd=1.3)


fixed_gaps[,"error"]<-abs(fixed_gaps$Tmin-fixed_gaps$Tmin_observed)

ggplot(data=fixed_gaps,aes(DATE,error)) +
  geom_line(lwd=1.3) +
  xlab("Date") +
  ylab("Error introduced by interpolation (°C)") +
  geom_point(data=fixed_gaps[which(!fixed_gaps$no_Tmin),],aes(DATE,error),col="red",cex=3)

### Filling long gaps in daily records

Bonn<-read.csv("data/Bonn_chillR_weather.csv")

Bonn_QC<-fix_weather(Bonn)$QC

Bonn_QC

station_list<-handle_gsod(action="list_stations",location=c(7.10,50.73),time_interval=c(1990,2020))

station_list

positions_in_station_list<-c(2,3,6)

patch_weather<-list()
for(i in 1:length(positions_in_station_list))
{patch_weather[[i]]<-handle_gsod(handle_gsod(action="download_weather",
                                             location=station_list$chillR_code[
                                               positions_in_station_list[i]],
                                             time_interval=c(1990,2020)))$weather
names(patch_weather)[i]<-station_list$STATION.NAME[
  positions_in_station_list[i]]
}

patched<-patch_daily_temperatures(weather = Bonn,
                                  patch_weather = patch_weather)

patched$statistics

patched<-patch_daily_temperatures(weather = Bonn,
                                  patch_weather = patch_weather,
                                  max_mean_bias = 1,
                                  max_stdev_bias = 2)

patched$statistics

post_patch_stats<-fix_weather(patched)$QC

post_patch_stats

Bonn_weather<-fix_weather(patched)

write.csv(Bonn_weather$weather,"data/Bonn_wheather.csv")



### Filling gaps in hourly records

stations<-handle_cimis("list_stations",location=c(-122,38.5))
downloaded_winters<-handle_cimis("download_weather",stations$chillR_code[2],
                                 time_interval = c(2008,2008))
winters_daily<-handle_cimis(downloaded_winters)$weather

to_interp<-Winters_hours_gaps
to_interp[,"Temp_recorded"]<-to_interp[,"Temp"]
to_interp[,"Temp"]<-to_interp[,"Temp_gaps"]
interp<-interpolate_gaps_hourly(hourtemps=to_interp,latitude=38.5,
                                daily_temps=list(Winters=winters_daily))


interp$daily_patch_report

interp$weather

require(stats)
y<-rnorm(100)
IQ<-quantile(y)[4]-quantile(2)[2]

inter<-interp$weather
inter[,"DATE"]<-ISOdate(inter$Year,inter$Month,inter$Day,inter$Hour)

orchard_extremes<-make_all_day_table(inter,timestep="day",
                                     input_timestep = "hour")

orchard_extremes

winters_hours<-stack_hourly_temps(fix_weather(winters_daily),latitude=38)$hourtemps
start_hour_winters<-which(winters_hours$Year==inter$Year[1]&
                            winters_hours$Month==inter$Month[1]&
                            winters_hours$Day==inter$Day[1]&
                            winters_hours$Hour==inter$Hour[1])
end_hour_winters<-which(winters_hours$Year==inter$Year[nrow(inter)]&
                          winters_hours$Month==inter$Month[nrow(inter)]&
                          winters_hours$Day==inter$Day[nrow(inter)]&
                          winters_hours$Hour==inter$Hour[nrow(inter)])

orchard_hours<-stack_hourly_temps(orchard_extremes,latitude=38)$hourtemps
start_hour_orchard<-which(orchard_hours$Year==inter$Year[1]&
                            orchard_hours$Month==inter$Month[1]&
                            orchard_hours$Day==inter$Day[1]&
                            orchard_hours$Hour==inter$Hour[1])
end_hour_orchard<-which(orchard_hours$Year==inter$Year[nrow(inter)]&
                          orchard_hours$Month==inter$Month[nrow(inter)]&
                          orchard_hours$Day==inter$Day[nrow(inter)]&
                          orchard_hours$Hour==inter$Hour[nrow(inter)])

observed<-inter$Temp_recorded
option1<-winters_hours$Temp[start_hour_winters:end_hour_winters]
option2<-orchard_hours$Temp[start_hour_orchard:end_hour_orchard]
option3<-interpolate_gaps(inter$Temp_gaps)$interp
option4<-inter$Temp

eval_table<-eval_table_gaps<-data.frame(Option=1:4,
                                        Input_data=c("daily","daily","hourly","hourly"),
                                        Interpolation_method=c("from proxy","local extremes",
                                                               "linear","hourly interpolation"),
                                        RMSEP=NA,RPIQ=NA)

observed_gaps<-observed[which(is.na(inter$Temp_gaps))]
option1_gaps<-option1[which(is.na(inter$Temp_gaps))]
option2_gaps<-option2[which(is.na(inter$Temp_gaps))]
option3_gaps<-option3[which(is.na(inter$Temp_gaps))]
option4_gaps<-option4[which(is.na(inter$Temp_gaps))]

eval_table_gaps[,"RMSEP"]<-round(c(RMSEP(option1_gaps,observed_gaps),
                                   RMSEP(option2_gaps,observed_gaps),
                                   RMSEP(option3_gaps,observed_gaps),
                                   RMSEP(option4_gaps,observed_gaps)),1)

eval_table_gaps[,"RPIQ"]<-round(c(RPIQ(option1_gaps,observed_gaps),
                                  RPIQ(option2_gaps,observed_gaps),
                                  RPIQ(option3_gaps,observed_gaps),
                                  RPIQ(option4_gaps,observed_gaps)),1)

eval_table_gaps

eval_table<-data.frame(Option=1:4,
                       Input_data=c("daily","daily","hourly","hourly"),
                       Interpolation_method=c("from proxy","local extremes",
                                              "linear","hourly interpolation"),
                       RMSEP=NA,RPIQ=NA)

eval_table[,"RMSEP"]<-round(c(RMSEP(option1,observed),RMSEP(option2,observed),
                              RMSEP(option3,observed),RMSEP(option4,observed)),1)

eval_table[,"RPIQ"]<-round(c(RPIQ(option1,observed),RPIQ(option2,observed),
                             RPIQ(option3,observed),RPIQ(option4,observed)),1)

eval_table

### Computing agroclimatic metrics

all_chill<-data.frame(DATE=inter$DATE,"Obs" = Dynamic_Model(observed),
                      "Opt1" = Dynamic_Model(option1),
                      "Opt2" = Dynamic_Model(option2),
                      "Opt3" = Dynamic_Model(option3),
                      "Opt4" = Dynamic_Model(option4))

library(reshape2)

all_chill<-melt(all_chill, id.vars=1)

all_chill[which(all_chill$variable=="Obs"),"Method"]<-"Observed temperatures"
all_chill[which(all_chill$variable=="Opt1"),"Method"]<-"Option 1 - idealized record from proxy data"
all_chill[which(all_chill$variable=="Opt2"),"Method"]<-"Option 2 - idealized record from daily orchard data"
all_chill[which(all_chill$variable=="Opt3"),"Method"]<-"Option 3 - linear interpolation of hourly data"
all_chill[which(all_chill$variable=="Opt4"),"Method"]<-"Option 4 - use of interpolate_gaps_hourly"


ggplot(data=all_chill, aes(DATE,value, colour=Method)) + geom_line(lwd=1.3) +
  ylab("Chill accumulation (Chill Portions)") + xlab("Date") + theme_bw(base_size = 15) +
  theme(legend.position = c(0.4, 0.85))


### same for heat

all_heat<-data.frame(DATE=inter$DATE,"Obs" = GDH(observed),
                     "Opt1" = GDH(option1),
                     "Opt2" = GDH(option2),
                     "Opt3" = GDH(option3),
                     "Opt4" = GDH(option4))
all_heat<-melt(all_heat, id.vars=1)

all_heat[which(all_heat$variable=="Obs"),"Method"]<-"Observed temperatures"
all_heat[which(all_heat$variable=="Opt1"),"Method"]<-"Option 1 - idealized record from proxy data"
all_heat[which(all_heat$variable=="Opt2"),"Method"]<-"Option 2 - idealized record from daily orchard data"
all_heat[which(all_heat$variable=="Opt3"),"Method"]<-"Option 3 - linear interpolation of hourly data"
all_heat[which(all_heat$variable=="Opt4"),"Method"]<-"Option 4 - use of interpolate_gaps_hourly"

ggplot(data=all_heat, aes(DATE,value, colour=Method)) + geom_line(lwd=1.3) +
  ylab("Heat accumulation (Growing Degree Hours)") + xlab("Date") + theme_bw(base_size = 15) +
  theme(legend.position = c(0.4, 0.85))

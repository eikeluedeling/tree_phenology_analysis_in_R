require(chillR)

### calculate daylength etc.

daylength

Days<-daylength(latitude=-33,JDay=1:365)

str(Days)

Days_df<-data.frame(JDay=1:365,
                    Sunrise=Days$Sunrise,
                    Sunset=Days$Sunset,
                    Daylength=Days$Daylength)

plot(x=Days_df$JDay, y=Days_df$Sunrise)








### using the ggplot function
require(ggplot2)
?ggplot2
### find the cheat sheet

### reorganizing the data.frame
require(reshape2)
Days_df<-melt(Days_df, id=c("JDay")) 

Days_df

### seeing how ggplot works

ggplot(Days_df, aes(JDay, value))

ggplot(Days_df, aes(JDay, value)) + geom_line(lwd=1.5)

ggplot(Days_df, aes(JDay, value)) + geom_line(lwd=1.5) + facet_grid(cols=vars(variable))

ggplot(Days_df, aes(JDay, value)) + geom_line(lwd=1.5) + facet_grid(cols=vars(variable)) +
  ylab("Time of Day ; Daylength (Hours)")

ggplot(Days_df, aes(JDay, value)) + geom_line(lwd=1.5) + facet_grid(cols=vars(variable)) +
  ylab("Time of Day ; Daylength (Hours)") + theme_bw(base_size = 12)

g <- ggplot(Days_df, aes(JDay, value))

g

g <- g + geom_line(lwd=1.5) + facet_grid(cols=vars(variable)) +
  ylab("Time of Day ; Daylength (Hours)") + theme_bw(base_size = 12)

g


### the KA_weather dataset

KA_weather

### making hourly temperatures

temps <- stack_hourly_temps(KA_weather, latitude=50.4)

str(temps)

hourtemps <- temps$hourtemps

hourtemps[,"DateTime"] <- ISOdate(year = hourtemps$Year,
                                  month = hourtemps$Month,
                                  day = hourtemps$Day,
                                  hour = hourtemps$Hour)

ggplot(hourtemps, aes(DateTime,Temp)) + geom_line()

ggplot(hourtemps[1000:1300,], aes(DateTime,Temp)) + geom_line()




### Now let's see how we can derive empirical temperature curves from hourly data

empi_curve <- Empirical_daily_temperature_curve(Winters_hours_gaps)

empi_curve[1:48,]

g <- ggplot(data=empi_curve[1:96,], aes(Hour,Prediction_coefficient)) +
  geom_line(lwd=1.3, col="red")

g <- g + facet_grid(rows=vars(Month))

g <- g + xlab("Hour of the day") + ylab("Prediction coefficient")

g + theme_bw(base_size=12)

### Now let's generate temperatures from this information

?Empirical_hourly_temperatures

coeffs <- Empirical_daily_temperature_curve(Winters_hours_gaps)

Winters_daily <- make_all_day_table(Winters_hours_gaps, input_timestep="hour")

Winters_hours <- Empirical_hourly_temperatures(Winters_daily,coeffs)

Winters_hours[,"DateTime"] <- ISOdate(year = Winters_hours$Year,
                                  month = Winters_hours$Month,
                                  day = Winters_hours$Day,
                                  hour = Winters_hours$Hour)


ggplot(Winters_hours[100:300,], aes(DateTime,Temp)) + geom_line()


### comparing different hourly temperature methods

require(reshape2)

Winters_hours <- Winters_hours[,c("Year","Month","Day","Hour","Temp")]

colnames(Winters_hours)[ncol(Winters_hours)] <- "Temp_empirical"

Winters_ideal <- stack_hourly_temps(Winters_daily, latitude=38.5)$hourtemps

Winters_ideal <- Winters_ideal[,c("Year","Month","Day","Hour","Temp")]

colnames(Winters_ideal)[ncol(Winters_ideal)] <- "Temp_ideal"

### making the triangular temperature curve

Winters_triangle <- Winters_daily
Winters_triangle[,"Hour"] <- 0
Winters_triangle$Hour[nrow(Winters_triangle)] <- 23
Winters_triangle[,"Temp"] <- 0
Winters_triangle <- make_all_day_table(Winters_triangle,timestep="hour")
colnames(Winters_triangle)[ncol(Winters_triangle)] <- "Temp_triangular"

for(i in 2:nrow(Winters_triangle))
  {if(is.na(Winters_triangle$Tmin[i]))
    Winters_triangle$Tmin[i] <- Winters_triangle$Tmin[i-1]
  if(is.na(Winters_triangle$Tmax[i]))
    Winters_triangle$Tmax[i] <- Winters_triangle$Tmax[i-1]
}

Winters_triangle$Temp_triangular <- NA

Winters_triangle$Temp_triangular[which(Winters_triangle$Hour==6)] <-
  Winters_triangle$Tmin[which(Winters_triangle$Hour==6)] 

Winters_triangle$Temp_triangular[which(Winters_triangle$Hour==18)] <-
  Winters_triangle$Tmax[which(Winters_triangle$Hour==18)] 

Winters_triangle$Temp_triangular <-
  interpolate_gaps(Winters_triangle$Temp_triangular)$interp

Winters_triangle<-Winters_triangle[,c("Year","Month","Day","Hour","Temp_triangular")]

### merging data.frames

Winters_temps <- merge(Winters_hours_gaps,Winters_hours, by=c("Year","Month","Day","Hour"))

Winters_temps <- merge(Winters_temps,Winters_triangle, by=c("Year","Month","Day","Hour"))

Winters_temps <- merge(Winters_temps,Winters_ideal, by=c("Year","Month","Day","Hour"))

### adding date and plotting

Winters_temps[,"DATE"] <- ISOdate(Winters_temps$Year,
                                  Winters_temps$Month,
                                  Winters_temps$Day,
                                  Winters_temps$Hour)


Winters_temps_to_plot <- Winters_temps[,c("DATE","Temp","Temp_empirical","Temp_triangular","Temp_ideal")]

Winters_temps_to_plot <- Winters_temps_to_plot[100:200,]

Winters_temps_to_plot <- melt(Winters_temps_to_plot, id=c("DATE")) 

colnames(Winters_temps_to_plot) <- c("DATE","Method","Temperature")


ggplot(data=Winters_temps_to_plot, aes(DATE,Temperature, colour=Method)) +
  geom_line(lwd=1.3) + ylab("Temperature (°C)") + xlab("Date")


### Testing the fits

RMSEP(Winters_temps$Temp_triangular,Winters_temps$Temp)

RMSEP(Winters_temps$Temp_ideal,Winters_temps$Temp)

RMSEP(Winters_temps$Temp_empirical,Winters_temps$Temp)



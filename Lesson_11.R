library(chillR)

## we first run the weather generator for the sample record from Klein-Altendorf,
## calibrated with temperatures from 1998-2005,
## to generate 100 years of synthetic data (indexed 2001-2100)

Temp<-temperature_generation(KA_weather,
                             years=c(1998,2005),
                             sim_years = c(2001,2100))

str(Temp)

Temp[[1]][1:5,]

# now we want to see what happened
# for this we first make a data.frame containing the synthetic and historic data

#  historic first
Temperatures<-cbind(KA_weather[
  which(KA_weather$Year %in% 1998:2005),] ,Data_source="observed")

Temperatures[1,]

# future data
generated_temps<-cbind(Temp[[1]][,c("Year","Month","Day","Tmin","Tmax")],
                       Data_source="simulated")

generated_temps[1:5,]


Temperatures<-rbind(Temperatures,generated_temps)

Temperatures[,"Date"]<-as.Date(ISOdate(2000,
                                       Temperatures$Month,
                                       Temperatures$Day))

Temperatures[1:5,]

## now we can plot these data with ggplot2

library(ggplot2)

# first for the minimum temperature
g <- ggplot(data=Temperatures, aes(Date,Tmin))

g <- g + facet_wrap(vars(Data_source))
  
g + geom_line(aes(colour = factor(Year)))

g <- g + geom_smooth(aes(colour = factor(Year)))

g <- g + theme(legend.position = "none")

g <- g + scale_x_date(date_labels = "%b")

g

# now (a bit condensed) for the maximum temperature

g <- ggplot(data=Temperatures, aes(Date,Tmax)) +
  facet_wrap(vars(Data_source)) +
  geom_smooth(aes(colour = factor(Year))) +
  theme(legend.position = "none") +
  scale_x_date(date_labels = "%b")
g


## now we can calculate chill for each of the seasons in the datasets. We'll
## do this separately for observed and simulated years and then put the
## datasets together

chill_observed <- chilling(
  stack_hourly_temps(
    Temperatures[which(Temperatures$Data_source=="observed"),],
    latitude = 50.4),
  Start_JDay = 305,
  End_JDay = 59)


chill_simulated<-chilling(
  stack_hourly_temps(
    Temperatures[which(Temperatures$Data_source=="simulated"),],
    latitude = 50.4),
  Start_JDay = 305,
  End_JDay = 59)


chill_comparison<-cbind(chill_observed ,Data_source="observed")
chill_comparison<-rbind(chill_comparison,
                        cbind(chill_simulated ,Data_source="simulated"))

chill_comparison[1:5,]

# we have to remove some incomplete years (the last of the historic and
# simulated records, where the seasons aren't complete)

chill_comparison_full_seasons<-chill_comparison[
  which(chill_comparison$Perc_complete==100),]



g <- ggplot(chill_comparison_full_seasons, aes(x=Chill_portions))

g <- g + geom_histogram(binwidth=1,aes(fill = factor(Data_source)))

g <- g + facet_wrap(vars(Data_source))

g <- g + xlab("Chill accumulation (Chill Portions)") +
         ylab("Frequency")  +
         theme(legend.position = "none")

## now we have a histogram of the plausible chill distribution between 1998 and
## 2005, which we can use to estimate the risk of chill shortfalls

## maybe we want to display the same information differently, e.g. as an
## empirical cumulative distribution - then we can use a dedicated ggplot function

chill_simulations<-chill_comparison_full_seasons[
  which(chill_comparison_full_seasons$Data_source=="simulated"),]

g <- ggplot(chill_simulations, aes(x=Chill_portions))

g <- g + stat_ecdf(geom = "step",lwd=1.5,col="blue")

g <- g + ylab("Cumulative probability") +
         xlab("Chill accumulation (in Chill Portions)") 

## We can now also calculate Safe Winter Chill

quantile(chill_simulations$Chill_portions, 0.1)

## or other distribution features such as the interquartile range

quantile(chill_simulations$Chill_portions, c(0.25,0.75))


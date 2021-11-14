## this lesson is about saving and loading data

require(chillR)


# Here's the call from the earlier lesson. We don't have to run this again.
Temp<-temperature_generation(KA_weather,years=c(1998,2005), sim_years = c(2001,2100))

# Now we make a temperature scenario that raises all temperatures by 2°C

change_scenario<-data.frame(Tmin=rep(2,12),
                            Tmax=rep(2,12))

change_scenario


# We can use this to modify the temperature generation

Temp_2<-temperature_generation(KA_weather,years=c(1998,2005),
                               sim_years = c(2001,2100),
                               temperature_scenario = change_scenario)

# As before, we'll make a data.frame that contains all
# our data, so we can take a look at it.

Temperature_scenarios<-cbind(
  KA_weather[which(KA_weather$Year %in% 1998:2005),],
  Data_source="observed")

Temperature_scenarios<-rbind(
  Temperature_scenarios,
  cbind(Temp[[1]][,c("Year","Month","Day","Tmin","Tmax")],
        Data_source="simulated"))

Temperature_scenarios<-rbind(
  Temperature_scenarios,
  cbind(Temp_2[[1]][,c("Year","Month","Day","Tmin","Tmax")],
        Data_source="Warming_2C"))

Temperature_scenarios[,"Date"]<-as.Date(ISOdate(2000,
                                                Temperature_scenarios$Month,
                                                Temperature_scenarios$Day))

# plot it with ggplot
library(ggplot2)

ggplot(data=Temperature_scenarios, aes(Date,Tmin)) +
  geom_smooth(aes(colour = factor(Year))) +
  facet_wrap(vars(Data_source)) +
 # theme_bw(base_size = 20) +
  theme(legend.position = "none") +
  scale_x_date(date_labels = "%b")

ggplot(data=Temperature_scenarios, aes(Date,Tmax)) +
  geom_smooth(aes(colour = factor(Year))) +
  facet_wrap(vars(Data_source)) +
 # theme_bw(base_size = 20) +
  theme(legend.position = "none") +
  scale_x_date(date_labels = "%b")


# Now we make historic temperature scenarios

# We'll first get a long-term temperature record for the Cologne-Bonn airport

# download weather station list for the vicinity of Bonn
station_list<-handle_gsod(action="list_stations",location=c(7.1,50.8))
# download weather data for Cologne/Bonn airport
Bonn_weather_raw<-handle_gsod(action="download_weather",
                              location=station_list$chillR_code[1],
                              time_interval = c(1973,2019),
                              station_list = station_list)
# convert weather data to chillR format
Bonn_weather<-handle_gsod(Bonn_weather_raw)
# check record for missing data
fix_weather(Bonn_weather)$QC
# (incidentally almost all gaps are for years covered by the KA_weather dataset)
Bonn_patched<-patch_daily_temperatures(
  weather=Bonn_weather$weather,
  patch_weather=list(KA_weather))
# There are still 26 days missing here, out of 47 years -
# let's simply interpolate these gaps now
Bonn<-fix_weather(Bonn_patched)
Bonn_temps<-Bonn$weather


# Now we can make a temperature scenario that represents conditions in 1980
# This uses a running mean function to determine mean monthly min and max
# temperatures of that year.
scenario_1980<-temperature_scenario_from_records(weather=Bonn_temps,year=1980)

scenario_1980


# when we first ran our weather generator, we got the following warnings:

# * scenario doesn’t contain named elements - consider using the following element names:
#     ‘data,’ ‘reference_year,’‘scenario_type,’‘labels’
# * setting scenario_type to ‘relative’
# * Reference year missing - can’t check if relative temperature scenario is valid

# We can now run the weather generator for 1980

temps_1980<-temperature_generation(weather=Bonn_temps, years=c(1973,2019),
                                   sim_years=c(2001,2100),
                                   temperature_scenario = scenario_1980)

# We still get a warning
# * Absolute temperature scenario specified - calibration weather record only
#    used for simulating temperature variation, but not for the means

# This is no problem for now - but let's now make this into a relative
# temperature scenario (i.e. relative change compared to a baseline)

# The central year of the dataset we downloaded is 1996, so we make a 1996 scenario
scenario_1996<-temperature_scenario_from_records(weather=Bonn_temps,year=1996)

# Now we can use these two scenarios to convert the 1980 scenario into a scenario
# of relative change compared to the 1996 baseline

relative_scenario<-temperature_scenario_baseline_adjustment(
  baseline=scenario_1996,
  temperature_scenario = scenario_1980)

relative_scenario

# Now we make a 1980 scenario with this new relative scenario
temps_1980<-temperature_generation(weather=Bonn_temps, years=c(1973,2019),
                                   sim_years=c(2001,2100),
                                   temperature_scenario = relative_scenario)

# We may want to make multiple historic scenarios. That's fairly easy because
# most of the functions accept vectors and lists.

all_past_scenarios<-temperature_scenario_from_records(
  weather=Bonn_temps,
  year=c(1980,1990,2000,2010))

adjusted_scenarios<-temperature_scenario_baseline_adjustment(
  baseline=scenario_1996,
  temperature_scenario = all_past_scenarios)

all_past_scenario_temps<-temperature_generation(
  weather=Bonn_temps,
  years=c(1973,2019),
  sim_years=c(2001,2100),
  temperature_scenario = adjusted_scenarios)

# Now we have a list containing 100 years of daily data for four past scenario
# years.

# We can calculate temperature-based metrics for all these records with a single
# command. This already includes the generation of hourly temperatures, so we
# have to specify the latitude. The default temperature model collection is the
# usual list of chill and heat metrics we've encountered earlier.

chill_hist_scenario_list<-tempResponse_daily_list(all_past_scenario_temps,
                                                  latitude=50.9,
                                                  Start_JDay = 305,
                                                  End_JDay = 59)

chill_hist_scenario_list

# now we prepare for plotting all this

scenarios<-names(chill_hist_scenario_list)[1:4]

all_scenarios<-chill_hist_scenario_list[[scenarios[1]]]
all_scenarios[,"scenario"]<-as.numeric(scenarios[1])


for (sc in scenarios[2:4])
  all_scenarios<-rbind(all_scenarios,
                       cbind(chill_hist_scenario_list[[sc]],scenario=as.numeric(sc)))

all_scenarios<-all_scenarios[which(all_scenarios$Perc_complete==100),]

# Let's compute the actual 'observed' chill for comparison
actual_chill<-tempResponse_daily_list(Bonn_temps,latitude=50.9,
                                      Start_JDay = 305,
                                      End_JDay = 59)[[1]]
actual_chill<-actual_chill[which(actual_chill$Perc_complete==100),]

# now we're ready for plotting
g <- ggplot(data=all_scenarios,aes(scenario,Chill_Portions,
                              fill=factor(scenario)))

g <- g +  geom_violin()

g <- g + ylab("Chill accumulation (Chill Portions)") +
         xlab("Scenario year") +
         theme_bw(base_size=15)

g <- g + ylim(c(0,90))

g <- g + geom_point(data=actual_chill,
                    aes(End_year,Chill_Portions,fill="blue"),
                    col="blue",show.legend = FALSE)
g <- g + scale_fill_discrete(name="Scenario",
                             breaks = unique(all_scenarios$scenario)) 

# For making annual scenarios, we can also use linear regression instead of the
# running mean. Let's look at the difference.

temperature_means<-data.frame(Year=min(Bonn_temps$Year):max(Bonn_temps$Year),
                              Tmin=aggregate(Bonn_temps$Tmin,FUN="mean",
                                             by=list(Bonn_temps$Year))[,2],
                              Tmax=aggregate(Bonn_temps$Tmax,FUN="mean",
                                             by=list(Bonn_temps$Year))[,2])
temperature_means[,"runn_mean_Tmin"]<-runn_mean(temperature_means$Tmin,15)
temperature_means[,"runn_mean_Tmax"]<-runn_mean(temperature_means$Tmax,15)

Tmin_regression<-lm(Tmin~Year, temperature_means)
temperature_means[,"regression_Tmin"]<-Tmin_regression$coefficients[1]+
  Tmin_regression$coefficients[2]*temperature_means$Year

Tmax_regression<-lm(Tmax~Year, temperature_means)
temperature_means[,"regression_Tmax"]<-Tmax_regression$coefficients[1]+
  Tmax_regression$coefficients[2]*temperature_means$Year


ggplot(temperature_means,aes(Year, Tmin)) + 
  geom_point() + 
  geom_line(data=temperature_means,aes(Year, runn_mean_Tmin),lwd=2,col="blue") + 
  geom_line(data=temperature_means,aes(Year, regression_Tmin),lwd=2,col="red") +
  theme_bw(base_size=15) +
  ylab("Mean monthly minimum temperature (°C)")

ggplot(temperature_means,aes(Year, Tmax)) + 
  geom_point() + 
  geom_line(data=temperature_means,aes(Year, runn_mean_Tmax),lwd=2,col="blue") + 
  geom_line(data=temperature_means,aes(Year, regression_Tmax),lwd=2,col="red") +
  theme_bw(base_size=15) +
  ylab("Mean monthly maximum temperature (°C)")


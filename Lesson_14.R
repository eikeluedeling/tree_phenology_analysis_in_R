library(chillR)

# first, I realized that I had forgotton to save the Bonn_temps dataset,
# so we'll have to generate it again (and this time I remember saving it)

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

write.csv(Bonn_temps, file="data/Bonn_temps.csv", row.names = FALSE)

Bonn_temps<-read_tab("data/Bonn_temps.csv")

# We can get future climate data from the ClimateWizard database
# (only for RCP4.5 and RCP8.5 of the AR5 scenarios, so this is going to be
# outdated soon)

getClimateWizardData(coordinates=c(longitude=10.61,latitude=34.93),
                     scenario="rcp45", start_year=2020, end_year=2050,
                     metric=c("CD18","R02"), GCMs=c("bcc-csm1-1","BNU-ESM"))

getClimateWizardData(coordinates=c(longitude=10.61,latitude=34.93),
                     scenario="rcp45", start_year=2020, end_year=2050,
                     metric=c("monthly_min_max_temps"), GCMs=c("bcc-csm1-1"),
                     temperature_generation_scenarios = TRUE)


# chillR contains a convenient way to download data for all the GCMs for
# a particular station

RCPs<-c("rcp45","rcp85")
Times<-c(2050,2085)

### not demonstrated in real time

for(RCP in RCPs)
  for(Time in Times)
  {start_year <- Time-15
  end_year <- Time+15
  clim_scen <-getClimateWizardData(
    c(longitude = 7.143,latitude = 50.866),
    RCP,
    start_year,
    end_year,
    temperature_generation_scenarios = TRUE,
    baseline =c(1975, 2005),
    metric = "monthly_min_max_temps",
    GCMs = "all")
  save_temperature_scenarios(clim_scen,
                             "data/ClimateWizard",
                             paste0("Bonn_futures_",Time,"_",RCP))}

# The ClimateWizard data is (by default) given relative to a 1990 baseline,
# so we have to make adjustment scenarios (the baseline of our observed data
# is 1996, not 1990)

scenario_1990<-temperature_scenario_from_records(Bonn_temps,1990)
scenario_1996<-temperature_scenario_from_records(Bonn_temps,1996)
adjustment_scenario<-temperature_scenario_baseline_adjustment(scenario_1996,
                                                              scenario_1990)

adjustment_scenario

# Now we can use this to adjust all the downloaded climate scenarios.
# We can directly add the temperature generation step (which we already
# encountered in lesson 13)

######## We can't run this live #########

RCPs<-c("rcp45","rcp85")
Times<-c(2050,2085)

for(RCP in RCPs)
  for(Time in Times)
  {
    clim_scen<-load_ClimateWizard_scenarios(
      "data/climateWizard",
      paste0("Bonn_futures_",Time,"_",RCP))
    clim_scen_adjusted<-
      temperature_scenario_baseline_adjustment(
        baseline_temperature_scenario=adjustment_scenario,
        temperature_scenario=clim_scen)
    Temps<-temperature_generation(
      weather=Bonn_temps, 
      years=c(1973,2019),
      sim_years=c(2001,2101),
      temperature_scenario = clim_scen_adjusted)
    
    save_temperature_scenarios(
      Temps,
      "data/Weather",
      paste0("Bonn_",Time,"_",RCP))
  }

# We'll add some historic scenarios as well

all_past_scenarios<-temperature_scenario_from_records(
  weather=Bonn_temps,
  year=c(1980,1990,2000,2010))

adjusted_scenarios<-temperature_scenario_baseline_adjustment(
  baseline=scenario_1996,
  temperature_scenario = all_past_scenarios)

######## We can't run this live #########


all_past_scenario_temps <- temperature_generation(
  weather = Bonn_temps,
  years = c(1973, 2019),
  sim_years = c(2001, 2101),
  temperature_scenario = adjusted_scenarios
)

save_temperature_scenarios(all_past_scenario_temps,
                           "data/Weather",
                           "Bonn_historic")
# Now temperature data for all scenarios are saved. Let's now make temperature
# response functions to apply to these scenarios

frost_model <- function(x)
  step_model(x,
             data.frame(
               lower = c(-1000, 0),
               upper = c(0, 1000),
               weight = c(1, 0)
             ))

models <- list(Chill_CP = Dynamic_Model,
               Heat_GDH = GDH,
               Frost_H = frost_model)

# Now we first apply this to the historic scenarios and the observed temperature
# data

Temps <- load_temperature_scenarios("data/Weather", "Bonn_historic")
chill_past_scenarios <- tempResponse_daily_list(
  Temps,
  latitude = 50.866,
  Start_JDay = 305,
  End_JDay = 59,
  models = models,
  misstolerance = 10
)
chill_observed <- tempResponse_daily_list(
  Bonn_temps,
  latitude = 50.866,
  Start_JDay = 305,
  End_JDay = 59,
  models = models,
  misstolerance = 10
)

save_temperature_scenarios(chill_past_scenarios,
                           "data/chill",
                           "Bonn_historic")
save_temperature_scenarios(chill_observed,
                           "data/chill",
                           "Bonn_observed")

# Let's plot the historic and observed data. chillR has a convenient function
# for that

chill_past_scenarios<-load_temperature_scenarios(
  "data/chill",
  "Bonn_historic")
chill_observed<-load_temperature_scenarios(
  "data/chill",
  "Bonn_observed")

chills <- make_climate_scenario(
  chill_past_scenarios,
  caption = "Historic",
  historic_data = chill_observed,
  time_series = TRUE
)

plot_climate_scenarios(
  climate_scenario_list=chills,
  metric="Chill_CP",
  metric_label="Chill (Chill Portions)")

# Now we can use the same procedure on the future data

for(RCP in RCPs)
  for(Time in Times)
  {
    Temps<-load_temperature_scenarios(
      "data/Weather",
      paste0("Bonn_",Time,"_",RCP))
    chill<-tempResponse_daily_list(
      Temps,
      latitude=50.866,
      Start_JDay = 305,
      End_JDay = 59,
      models=models,
      misstolerance = 10)
    save_temperature_scenarios(
      chill,
      "data/chill",
      paste0("Bonn_",Time,"_",RCP))
  }

# To plot the results, we should make scenarios out of them

for(RCP in RCPs)
  for(Time in Times)
  {
    chill<-load_temperature_scenarios(
      "data/chill",
      paste0("Bonn_",Time,"_",RCP))
    if(RCP=="rcp45") RCPcaption <- "RCP4.5"
    if(RCP=="rcp85") RCPcaption <- "RCP8.5"
    if(Time=="2050") Time_caption <- "2050"
    if(Time=="2085") Time_caption <- "2085"
    chills <-make_climate_scenario(
      chill,
      caption =c(RCPcaption, Time_caption),
      add_to = chills)
  }

# And here's our plot of all of this
# First for the chill model

plot_climate_scenarios(
  climate_scenario_list=chills,
  metric="Chill_CP",
  metric_label="Chill (Chill Portions)",
  texcex=1.5)

# Now for heat

plot_climate_scenarios(
  climate_scenario_list=chills,
  metric="Heat_GDH",
  metric_label="Heat (Growing Degree Hours)",
  texcex=1.5)

# Now for frost

plot_climate_scenarios(
  climate_scenario_list=chills,
  metric="Frost_H",
  metric_label="Frost hours",
  texcex=1.5)






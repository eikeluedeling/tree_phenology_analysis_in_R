library(chillR)

# this time we need some extra materials that Eduardo put together
# his dormancyR package (on github) contains several additional
# chill models
# using a tool from the devtools package, we can directly install
# packages from github

library(devtools)
install_github("EduardoFernandezC/dormancyR")
library(dormancyR)

# Now we make a collection of all the chill models from chillR
#   and from the dormancyR package.
# Some models operate on a daily rather than hourly time step. We'll need slightly
#   different processes to run them, so they go into a separate list.

hourly_models <- list(Chilling_units = chilling_units,
                      Low_chill = low_chill_model,
                      Modified_Utah = modified_utah_model,
                      North_Carolina = north_carolina_model,
                      Positive_Utah = positive_utah_model,
                      Chilling_Hours = Chilling_Hours,
                      Utah_Chill_Units = Utah_Model,
                      Chill_Portions = Dynamic_Model)
daily_models<-list(Rate_of_Chill = rate_of_chill,
                   Chill_Days = chill_days,
                   Exponential_Chill = exponential_chill,
                   Triangula_Chill_Haninnen = triangular_chill_1,
                   Triangular_Chill_Legave = triangular_chill_2)

metrics<-c(names(daily_models),names(hourly_models))

model_labels=c("Rate of Chill",
               "Chill Days",
               "Exponential Chill",
               "Triangular Chill (Häninnen)",
               "Triangular Chill (Legave)",
               "Chilling Units",
               "Low-Chill Chill Units",
               "Modified Utah Chill Units",
               "North Carolina Chill Units",
               "Positive Utah Chill Units",
               "Chilling Hours",
               "Utah Chill Units",
               "Chill Portions")

data.frame(Metric=model_labels,'Function name'=metrics)

# We'll apply the models to the Bonn_temps dataset as well as the historic
#   weather scenarios we generated earlier

Bonn_temps<-read_tab("data/Bonn_temps.csv")
Temps<-load_temperature_scenarios("data/Weather","Bonn_historic")

# Now we apply the models to the Bonn dataset. Eduardo produced a function
#   to apply the daily models to daily temperature data. This is similar to
#   "tempResponse_daily_list", except that it works directly with the daily
#   data, instead of first generating hourly values.

Start_JDay<-305
End_JDay<-59

# apply daily models to past scenarios

daily_models_past_scenarios<-tempResponse_list_daily(
  Temps,
  Start_JDay = Start_JDay,
  End_JDay = End_JDay,
  models=daily_models)
daily_models_past_scenarios<-lapply(
  daily_models_past_scenarios,
  function(x) x[which(x$Perc_complete>90),])

# apply hourly models to past scenarios

hourly_models_past_scenarios<-tempResponse_daily_list(
  Temps,
  latitude=50.866,
  Start_JDay = Start_JDay,
  End_JDay = End_JDay,
  models=hourly_models,
  misstolerance = 10)

past_scenarios<-daily_models_past_scenarios
past_scenarios<-lapply(
  names(past_scenarios),
  function(x)
    cbind(past_scenarios[[x]],
          hourly_models_past_scenarios[[x]][,names(hourly_models)]))
names(past_scenarios)<-names(daily_models_past_scenarios)

# apply daily models to past observations

daily_models_observed<-tempResponse_daily(
  Bonn_temps,
  Start_JDay = Start_JDay,
  End_JDay = End_JDay,
  models=daily_models)
daily_models_observed<-
  daily_models_observed[which(daily_models_observed$Perc_complete>90),]

# apply hourly models to past observations

hourly_models_observed<-tempResponse_daily_list(
  Bonn_temps,
  latitude=50.866,
  Start_JDay = Start_JDay,
  End_JDay = End_JDay,
  models=hourly_models,
  misstolerance = 10)

past_observed<-cbind(
  daily_models_observed,
  hourly_models_observed[[1]][,names(hourly_models)])

# save all the results

save_temperature_scenarios(past_scenarios,
                           "data/chill",
                           "Bonn_multichill_historic")
write.csv(past_observed,
          "data/chill/Bonn_multichill_observed.csv",
          row.names=FALSE)

# Now the same procedure for future scenarios
# We'll use a loop again to process the RCPs and Time slices; otherwise the
#   procedure is the same as for the historic data.
#   (This step takes a bit of time - may not want to run this live)

RCPs<-c("rcp45","rcp85")
Times<-c(2050,2085)

for(RCP in RCPs)
  for(Time in Times)
  {
    Temps<-load_temperature_scenarios(
      "data/Weather",
      paste0("Bonn_",Time,"_",RCP))
    
    daily_models_future_scenarios<-tempResponse_list_daily(
      Temps,
      Start_JDay = Start_JDay,
      End_JDay = End_JDay,
      models=daily_models)
    daily_models_future_scenarios<-lapply(
      daily_models_future_scenarios,
      function(x) x[which(x$Perc_complete>90),])
    hourly_models_future_scenarios<-
      tempResponse_daily_list(
        Temps,
        latitude=50.866,
        Start_JDay = Start_JDay,
        End_JDay = End_JDay,
        models=hourly_models,
        misstolerance = 10)
    
    future_scenarios<-daily_models_future_scenarios
    future_scenarios<-lapply(
      names(future_scenarios),
      function(x)
        cbind(future_scenarios[[x]],
              hourly_models_future_scenarios[[x]][,names(hourly_models)]))
    names(future_scenarios)<-names(daily_models_future_scenarios)
    
    chill<-future_scenarios
    save_temperature_scenarios(
      chill,
      "data/chill",
      paste0("Bonn_multichill_",Time,"_",RCP))
  }

# Now we have produced all the chill projections and saved them for later use.

# Let's make scenarios we can plot.

chill_past_scenarios<-load_temperature_scenarios(
  "data/chill",
  "Bonn_multichill_historic")
chill_observed<-read_tab("data/chill/Bonn_multichill_observed.csv")

chills <-make_climate_scenario(chill_past_scenarios,
                               caption = "Historic",
                               historic_data = chill_observed,
                               time_series = TRUE)

for(RCP in RCPs)
  for(Time in Times)
  {
    chill<-load_temperature_scenarios(
      "data/chill",
      paste0("Bonn_multichill_",Time,"_",RCP))
    if(RCP=="rcp45") RCPcaption <- "RCP4.5"
    if(RCP=="rcp85") RCPcaption <- "RCP8.5"
    if(Time=="2050") Time_caption <- "2050"
    if(Time=="2085") Time_caption <- "2085"
    chills <-make_climate_scenario(chill,
                                   caption =c(RCPcaption,Time_caption),
                                   add_to = chills)
  }


# This time we want to make a heat map that just shows Safe Winter Chill (SWC).
# The main motivation for this is that we want to show results for lots of
#   chill models - the usual diagrams can only show one metric at a time,
#   which makes it hard to compare results.

# First we have to compute the SWC for all scenarios and get the results into
#   a structure we can then plot easily with ggplot.

for(i in 1:length(chills))
{ch<-chills[[i]]
if(ch$caption[1]=="Historic")
{GCMs<-rep("none",length(names(ch$data)))
RCPs<-rep("none",length(names(ch$data)))
Years<-as.numeric(ch$labels)
Scenario<-rep("Historic",length(names(ch$data)))} else
{GCMs<-names(ch$data)
RCPs<-rep(ch$caption[1],length(names(ch$data)))
Years<-rep(as.numeric(ch$caption[2]),length(names(ch$data)))
Scenario<-rep("Future",length(names(ch$data)))}
for(nam in names(ch$data))
{for(met in metrics)
{temp_res<-data.frame(Metric=met,
                      GCM=GCMs[which(nam==names(ch$data))],
                      RCP=RCPs[which(nam==names(ch$data))],
                      Year=Years[which(nam==names(ch$data))],
                      Result=quantile(ch$data[[nam]][,met],0.1),
                      Scenario=Scenario[which(nam==names(ch$data))])
if(i==1&nam==names(ch$data)[1]&met==metrics[1])
  results<-temp_res else
    results<-rbind(results,temp_res)
  
}}}

for(met in metrics)
  results[which(results$Metric==met),"SWC"]<-
  results[which(results$Metric==met),"Result"]/
  results[which(results$Metric==met&results$Year==1980),"Result"]-1


# Now we're ready for plotting
# We'll work on the future first.

library(ggplot2)

rng = range(results$SWC)

p_future<-ggplot(results[which(!results$GCM=="none"),],
                 aes(GCM, y=factor(Metric, levels=metrics),
                     fill = SWC)) +
  geom_tile()

p_future

p_future <-
  p_future +
  facet_grid(RCP ~ Year) 
p_future

p_future <-
  p_future +
  theme_bw(base_size = 15) +
  theme(axis.text = element_text(size=8))

p_future

library(colorRamps)
p_future <-
  p_future +
  scale_fill_gradientn(colours=matlab.like(15),
                       labels = scales::percent,
                       limits=rng)
p_future


p_future <-
  p_future  +
  theme(axis.text.x = element_text(angle = 75, hjust = 1, vjust = 1)) +
  labs(fill = "Change in\nSafe Winter Chill\nsince 1975") +
  scale_y_discrete(labels=model_labels) +
  ylab("Chill metric")
p_future

# Now we have a nice plot for the future data. Moving on to the past

p_past<-
  ggplot(results[which(results$GCM=="none"),],
         aes(Year, y=factor(Metric, levels=metrics),
             fill = SWC)) +
  geom_tile()

p_past<-
  p_past +
  theme_bw(base_size = 15) +
  theme(axis.text = element_text(size=8))

p_past<-
  p_past +
  scale_fill_gradientn(colours=matlab.like(15),
                       labels = scales::percent,
                       limits=rng)

p_past<-
  p_past +
  scale_x_continuous(position = "top") 

p_past<-
  p_past +
  labs(fill = "Change in\nSafe Winter Chill\nsince 1975") +
  scale_y_discrete(labels=model_labels) +
  ylab("Chill metric")

p_past

# Now we can combine the past and future plots. We'll use the patchwork
#   package for this again

require(patchwork)

chill_comp_plot<-
  (p_past +
     p_future +
     plot_layout(guides = "collect",nrow=2, heights=c(1,2))) &
  theme(legend.position = "right",strip.background = element_blank(),
        strip.text = element_text(face = "bold"))

chill_comp_plot

## Now we have a nice model comparison plot. As you can see, model choice
#   matters a lot!


# In our modern world, some people only take things seriously when they are
#   presented as moving pictures. Let's do that.
# The gganimate package makes this easy.

# Some pre-processing is necessary

hist_results<-results[which(results$GCM=="none"),]
hist_results$RCP<-"RCP4.5"
hist_results_2<-hist_results
hist_results_2$RCP<-"RCP8.5"
hist_results<-rbind(hist_results,hist_results_2)

future_results<-results[which(!results$GCM=="none"),]

GCM_aggregate<-aggregate(
  future_results$SWC,
  by=list(future_results$Metric,future_results$RCP,future_results$Year),
  FUN=mean)
colnames(GCM_aggregate)<-c("Metric","RCP","Year","SWC")

RCP_Time_series<-rbind(hist_results[,c("Metric","RCP","Year","SWC")],
                       GCM_aggregate)

# Now we make a static plot of chill development over time according to all the 
#   chill models.

chill_change_plot<-
  ggplot(data=RCP_Time_series,
         aes(x=Year,y=SWC,col=factor(Metric,levels=metrics))) +
  geom_line(lwd=1.3) +
  facet_wrap(~RCP,nrow=2) +
  theme_bw(base_size=15) +
  labs(col = "Change in\nSafe Winter Chill\nsince 1975") +
  scale_color_discrete(labels=model_labels) +
  scale_y_continuous(labels = scales::percent) +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold")) +
  ylab("Safe Winter Chill")

chill_change_plot

## Now we use gganimate to animate this.

library(gganimate)
chill_change_plot + transition_reveal(Year)

anim_save("data/chill_comparison_animation.gif", animation = last_animation())





library(chillR)
Alex <- read_tab("data/Alexander_Lucas_bloom_1958_2019.csv")
Alex_first <- Alex[, 1:2]
Alex_first[, "Year"] <- substr(Alex_first$First_bloom, 1, 4)
Alex_first[, "Month"] <- substr(Alex_first$First_bloom, 5, 6)
Alex_first[, "Day"] <- substr(Alex_first$First_bloom, 7, 8)
Alex_first <- make_JDay(Alex_first)
Alex_first <- Alex_first[, c("Pheno_year", "JDay")]
colnames(Alex_first) <- c("Year", "pheno")

temps <- read_tab("data/TMaxTMin1958-2019_patched.csv")
temps_hourly <- stack_hourly_temps(temps, latitude = 50.6)

daychill <- daily_chill(
  hourtemps = temps_hourly,
  running_mean = 1,
  models = list(
    Chilling_Hours = Chilling_Hours,
    Utah_Chill_Units = Utah_Model,
    Chill_Portions = Dynamic_Model,
    GDH = GDH
  )
)


plscf <- PLS_chill_force(
  daily_chill_obj = daychill,
  bio_data_frame = Alex_first,
  split_month = 6,
  chill_models = "Chill_Portions",
  heat_models = "GDH",
  runn_means = 11
)



# Here we need the plot_PLS_chill_force function we defined in lesson 20.

require(ggplot2)

plot_PLS_chill_force(
  plscf,
  chill_metric = "Chill_Portions",
  heat_metric = "GDH",
  chill_label = "CP",
  heat_label = "GDH",
  chill_phase = c(-48, 62),
  heat_phase = c(-5, 105.5)
)



chill_phase <- c(317, 62)
heat_phase <- c(360, 105.5)

chill <- tempResponse(
  hourtemps = temps_hourly,
  Start_JDay = chill_phase[1],
  End_JDay = chill_phase[2],
  models = list(Chill_Portions = Dynamic_Model),
  misstolerance = 10
)

heat <- tempResponse(
  hourtemps = temps_hourly,
  Start_JDay = heat_phase[1],
  End_JDay = heat_phase[2],
  models = list(GDH = GDH)
)






ggplot(data = chill, aes(x = Chill_Portions)) +
  geom_histogram() +
  ggtitle("Chill accumulation during endodormancy (Chill Portions)") +
  xlab("Chill accumulation (Chill Portions)") +
  ylab("Frequency between 1958 and 2019") +
  theme_bw(base_size = 12)




ggplot(data = heat, aes(x = GDH)) +
  geom_histogram() +
  ggtitle("Heat accumulation during ecodormancy (GDH)") +
  xlab("Heat accumulation (Growing Degree Hours)") +
  ylab("Frequency between 1958 and 2019") +
  theme_bw(base_size = 12)





chill_requirement <- mean(chill$Chill_Portions)
chill_req_error <- sd(chill$Chill_Portions)

heat_requirement <- mean(heat$GDH)
heat_req_error <- sd(heat$GDH)




chill_phase <- c(317, 62)
heat_phase <- c(360, 106) # note that the end date here was rounded
# to an integer number, so that a proper
# axis label can be generated.


dir.create("pictures")

mpt <- make_pheno_trend_plot(
  weather_data_frame = temps,
  pheno = Alex_first,
  Start_JDay_chill = chill_phase[1],
  End_JDay_chill = chill_phase[2],
  Start_JDay_heat = heat_phase[1],
  End_JDay_heat = heat_phase[2],
  outpath = "pictures/",
  file_name = "pheno_trend_plot",
  plot_title =
    "Impacts of chilling and forcing temperatures on pear phenology",
  image_type = "png",
  colorscheme = "normal"
)





mean_temp_period <-  function(temps, start_JDay, end_JDay, end_season = end_JDay)
  {
    temps_JDay <- make_JDay(temps)
    temps_JDay[, "Season"] <- temps_JDay$Year
    
    if (start_JDay > end_season)
      temps_JDay$Season[which(temps_JDay$JDay >= start_JDay)] <-
      temps_JDay$Year[which(temps_JDay$JDay >= start_JDay)] + 1
    
    if (start_JDay > end_season)
      sub_temps <- subset(temps_JDay, JDay <= end_JDay | JDay >= start_JDay)
    
    if (start_JDay <= end_JDay)
      sub_temps <- subset(temps_JDay, JDay <= end_JDay & JDay >= start_JDay)
    
    mean_temps <- aggregate(
      sub_temps[, c("Tmin", "Tmax")],
      by = list(sub_temps$Season),
      FUN = function(x)
        mean(x, na.rm = TRUE)
    )
    
    mean_temps[, "n_days"] <- aggregate(sub_temps[, "Tmin"],
                                        by = list(sub_temps$Season),
                                        FUN = length)[, 2]
    
    mean_temps[, "Tmean"] <- (mean_temps$Tmin + mean_temps$Tmax) / 2
    mean_temps <- mean_temps[, c(1, 4, 2, 3, 5)]
    colnames(mean_temps)[1] <- "End_year"
    
    return(mean_temps)
  }

mean_temp_chill <- mean_temp_period(
  temps = temps,
  start_JDay = chill_phase[1],
  end_JDay = chill_phase[2],
  end_season = 60
)

mean_temp_heat <- mean_temp_period(
  temps = temps,
  start_JDay = heat_phase[1],
  end_JDay = heat_phase[2],
  end_season = 60
)



mean_temp_chill <-
  mean_temp_chill[which(mean_temp_chill$n_days >=
                          max(mean_temp_chill$n_days) - 1), ]
mean_temp_heat <-
  mean_temp_heat[which(mean_temp_heat$n_days >=
                         max(mean_temp_heat$n_days) - 1), ]

mean_chill <- mean_temp_chill[, c("End_year", "Tmean")]
colnames(mean_chill)[2] <- "Tmean_chill"

mean_heat <- mean_temp_heat[, c("End_year", "Tmean")]
colnames(mean_heat)[2] <- "Tmean_heat"

phase_Tmeans <- merge(mean_chill, mean_heat, by = "End_year")


pheno <- Alex_first
colnames(pheno)[1] <- "End_year"

Tmeans_pheno <- merge(phase_Tmeans, pheno, by = "End_year")



head(Tmeans_pheno)





library(fields)
k <- Krig(x = as.matrix(Tmeans_pheno[, c("Tmean_chill", "Tmean_heat")]),
          Y = Tmeans_pheno$pheno)

pred <- predictSurface(k)
colnames(pred$z) <- pred$y
rownames(pred$z) <- pred$x

library(reshape2)
melted <- melt(pred$z)

library(metR)
library(colorRamps)

colnames(melted) <- c("Tmean_chill", "Tmean_heat", "value")


ggplot(melted,
       aes(x = Tmean_chill, y = Tmean_heat, z = value)) +
  geom_contour_fill(bins = 100) +
  scale_fill_gradientn(colours = alpha(matlab.like(15)),
                       name = "Bloom date \n(day of the year)") +
  geom_contour(col = "black")  +
  geom_point(data = Tmeans_pheno,
             aes(x = Tmean_chill, y = Tmean_heat, z = NULL),
             size = 0.7) +
  geom_text_contour(stroke = 0.2) +
  ylab(expression(paste("Forcing phase ", T[mean], " (", degree, "C)"))) +
  xlab(expression(paste("Chilling phase ", T[mean], " (", degree, "C)")))  +
  theme_bw(base_size = 15)





pheno_trend_ggplot <- function(temps,
                               pheno,
                               chill_phase,
                               heat_phase,
                               phenology_stage = "Bloom")
{
  library(fields)
  library(reshape2)
  library(metR)
  library(ggplot2)
  library(colorRamps)
  
  # first, a sub-function (function defined within a function) to
  # compute the temperature means
  
  mean_temp_period <- function(temps,
                               start_JDay,
                               end_JDay,
                               end_season = end_JDay)
  {
    temps_JDay <- make_JDay(temps)
    temps_JDay[, "Season"] <- temps_JDay$Year
    if (start_JDay > end_season)
      temps_JDay$Season[which(temps_JDay$JDay >= start_JDay)] <-
      temps_JDay$Year[which(temps_JDay$JDay >= start_JDay)] + 1
    if (start_JDay > end_season)
      sub_temps <- subset(temps_JDay, JDay <= end_JDay |
                            JDay >= start_JDay)
    if (start_JDay <= end_JDay)
      sub_temps <- subset(temps_JDay, JDay <= end_JDay &
                            JDay >= start_JDay)
    mean_temps <- aggregate(
      sub_temps[, c("Tmin", "Tmax")],
      by = list(sub_temps$Season),
      FUN = function(x)
        mean(x, na.rm = TRUE)
    )
    mean_temps[, "n_days"] <- aggregate(sub_temps[, "Tmin"],
                                        by = list(sub_temps$Season),
                                        FUN = length)[, 2]
    mean_temps[, "Tmean"] <- (mean_temps$Tmin + mean_temps$Tmax) / 2
    mean_temps <- mean_temps[, c(1, 4, 2, 3, 5)]
    colnames(mean_temps)[1] <- "End_year"
    return(mean_temps)
  }
  
  mean_temp_chill <- mean_temp_period(
    temps = temps,
    start_JDay = chill_phase[1],
    end_JDay = chill_phase[2],
    end_season = heat_phase[2]
  )
  
  mean_temp_heat <- mean_temp_period(
    temps = temps,
    start_JDay = heat_phase[1],
    end_JDay = heat_phase[2],
    end_season = heat_phase[2]
  )
  
  mean_temp_chill <-
    mean_temp_chill[which(mean_temp_chill$n_days >=
                            max(mean_temp_chill$n_days) - 1), ]
  mean_temp_heat <-
    mean_temp_heat[which(mean_temp_heat$n_days >=
                           max(mean_temp_heat$n_days) - 1), ]
  mean_chill <- mean_temp_chill[, c("End_year", "Tmean")]
  colnames(mean_chill)[2] <- "Tmean_chill"
  mean_heat <- mean_temp_heat[, c("End_year", "Tmean")]
  colnames(mean_heat)[2] <- "Tmean_heat"
  phase_Tmeans <- merge(mean_chill, mean_heat, by = "End_year")
  
  colnames(pheno) <- c("End_year", "pheno")
  Tmeans_pheno <- merge(phase_Tmeans, pheno, by = "End_year")
  
  # Kriging interpolation
  k <- Krig(x = as.matrix(Tmeans_pheno[, c("Tmean_chill", "Tmean_heat")]),
            Y = Tmeans_pheno$pheno)
  pred <- predictSurface(k)
  colnames(pred$z) <- pred$y
  rownames(pred$z) <- pred$x
  melted <- melt(pred$z)
  colnames(melted) <- c("Tmean_chill", "Tmean_heat", "value")
  
  ggplot(melted, aes(x = Tmean_chill, y = Tmean_heat, z = value)) +
    geom_contour_fill(bins = 60) +
    scale_fill_gradientn(
      colours = alpha(matlab.like(15)),
      name = paste(phenology_stage, "date \n(day of the year)")
    ) +
    geom_contour(col = "black") +
    geom_text_contour(stroke = 0.2) +
    geom_point(data = Tmeans_pheno,
               aes(x = Tmean_chill, y = Tmean_heat, z = NULL),
               size = 0.7)  +
    ylab(expression(paste("Forcing phase ", T[mean], " (", degree, "C)"))) +
    xlab(expression(paste("Chilling phase ", T[mean], " (", degree, "C)"))) +
    theme_bw(base_size = 15)
}


chill_phase <- c(317, 62)
heat_phase <- c(360, 105.5)

pheno_trend_ggplot(
  temps = temps,
  pheno = Alex_first,
  chill_phase = chill_phase,
  heat_phase = heat_phase,
  phenology_stage = "Bloom"
)





Cali_temps <- read_tab("data/Davis_weather.csv")
Walnut_pheno <- read_tab("data/Davis_Payne_leaf_out.csv")
Walnut_pheno[, "Year"] <-
  as.numeric(substr(Walnut_pheno$Leaf.date, 7, 8))
Walnut_pheno$Year <- Walnut_pheno$Year + (19 + (Walnut_pheno$Year < 25)) *
  100
Walnut_pheno[, "Month"] <-
  as.numeric(substr(Walnut_pheno$Leaf.date, 4, 5))
Walnut_pheno[, "Day"] <-
  as.numeric(substr(Walnut_pheno$Leaf.date, 1, 2))
Walnut_pheno <- make_JDay(Walnut_pheno)
Walnut_pheno <- Walnut_pheno[, c("Year", "JDay")]
colnames(Walnut_pheno) <- c("Year", "pheno")

Cali_temps_hourly <- stack_hourly_temps(Cali_temps, latitude = 38.5)

Cali_daychill <- daily_chill(
  hourtemps = Cali_temps_hourly,
  running_mean = 1,
  models = list(
    Chilling_Hours = Chilling_Hours,
    Utah_Chill_Units = Utah_Model,
    Chill_Portions = Dynamic_Model,
    GDH = GDH
  )
)


plscf <- PLS_chill_force(
  daily_chill_obj = Cali_daychill,
  bio_data_frame = Walnut_pheno,
  split_month = 6,
  chill_models = "Chill_Portions",
  heat_models = "GDH",
  runn_means = 11
)

plot_PLS_chill_force(
  plscf,
  chill_metric = "Chill_Portions",
  heat_metric = "GDH",
  chill_label = "CP",
  heat_label = "GDH",
  chill_phase = c(-56, 5),
  heat_phase = c(19, 77)
)




pheno_trend_ggplot(
  temps = Cali_temps,
  pheno = Walnut_pheno,
  chill_phase = c(309, 5),
  heat_phase = c(19, 77),
  phenology_stage = "Leaf emergence"
)


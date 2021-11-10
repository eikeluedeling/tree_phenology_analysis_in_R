## this lesson is about saving and loading data

require(chillR)

## sometimes we generate objects through processes that take a long time
## an example is the weather generation stuff from the last lesson

Temp<-temperature_generation(KA_weather,
                             years=c(1998,2005),
                             sim_years = c(2001,2100))
Temperatures<-cbind(KA_weather[
  which(KA_weather$Year %in% 1998:2005),] ,Data_source="observed")
generated_temps<-cbind(Temp[[1]][,c("Year","Month","Day","Tmin","Tmax")],
                       Data_source="simulated")
Temperatures<-rbind(Temperatures,generated_temps)
Temperatures[,"Date"]<-as.Date(ISOdate(2000,
                                       Temperatures$Month,
                                       Temperatures$Day))

## we may want to use this again, and then we probably don't want to run the
## generation procedure again. So we need to save this.
## the most straightforward option for tables is write.csv
## I'm adding row.names = FALSE to suppress row numbers in the output file

write.csv(Temperatures, file="data/Temperatures.csv", row.names = FALSE)

## We can easily read this again with read.csv

Temperatures <- read.csv("data/Temperatures.csv")

## This sometimes causes problems on non-English systems, so chillR contains
## an alternative function that can accommodate a comma as a decimal symbol
## and semicolons as separators (as used in German, French and Spanish)

Temperatures <- read_tab("data/Temperatures.csv")

## Things are a bit harder, when we want to save a list

test_list <- list(Number=1,
                  String="Thanks for using chillR!",
                  DateFrame=data.frame(a=c(1,2,3),b=c(3,2,1),c=c(5,4,3)))

test_list

## this structure can be saved with the save_temperature_scenarios function
## (which doesn't only work with temperature scenarios, but also with other
## reasonably simple lists)

save_temperature_scenarios(test_list,"data","test_list")

## this can be loaded again with load_temperature_scenarios

test_list <- load_temperature_scenarios("data","test_list")

### Hiding stuff from the readers of your markdown document

# For this we should move over to an Rmd document



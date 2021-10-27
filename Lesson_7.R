library(chillR)

?Chilling_Hours

Chilling_Hours

Chilling_Hours(Winters_hours_gaps$Temp,summ=FALSE)

Utah_Model

Utah_Model
function(HourTemp,summ=TRUE)
  return(step_model(HourTemp,
                    df=data.frame(lower=c(-1000,1.4,2.4,9.1,12.4,15.9,18),
                                  upper=c(1.4,2.4,9.1,12.4,15.9,18,1000),
                                  weight=c(0,0.5,1,0.5,0,-0.5,-1)),
                    summ=summ))

data.frame(lower=c(-1000,1.4,2.4,9.1,12.4,15.9,18),
           upper=c(1.4,2.4,9.1,12.4,15.9,18,1000),
           weight=c(0,0.5,1,0.5,0,-0.5,-1))


plot(step_model(Winters_hours_gaps$Temp,
           df=data.frame(lower=c(-1000,1.4,2.4,9.1,12.4,15.9,18),
                         upper=c(1.4,2.4,9.1,12.4,15.9,18,1000),
                         weight=c(0,0.5,1,0.5,0,-0.5,-1))))

Utah_Model(Winters_hours_gaps$Temp,summ=TRUE)


newFrame<-data.frame(lower=c(-1000,-5,2.4,9.1,12.4,15.9,18),
                     upper=c(-5,2.4,9.1,12.4,15.9,18,1000),
                     weight=c(0,10,1,0.5,0,-0.5,-1))

myModel<-function(HourTemps,summ=TRUE)
  {step_model(HourTemps,
              df=newFrame,
              summ=summ)
}
myModel(Winters_hours_gaps$Temp)

Dynamic_Model(Winters_hours_gaps$Temp)[nrow(Winters_hours_gaps)]

chilling

chilling(make_JDay(Winters_hours_gaps))

chilling(make_JDay(Winters_hours_gaps),Start_JDay = 100,End_JDay = 200)

tempResponse(make_JDay(Winters_hours_gaps),
             Start_JDay = 100,
             End_JDay = 200,
             models = list(Chilling_Hours = Chilling_Hours,
                           Utah_Chill_Units = Utah_Model,
                           Chill_Portions = Dynamic_Model,
                           GDH = GDH,
                           myModel = myModel))


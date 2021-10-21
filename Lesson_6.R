library(chillR)
Winters_hours_gaps

hourtemps<-Winters_hours_gaps[,c("Year","Month","Day","Hour","Temp")]


hourtemps[c(10:20),c(2,3)]

a=1
a<-1

hourtemps[1,"Temp"]<-20
hourtemps[1:5,]

hourtemps[,"myColumn"]<-"Hi"

hourtemps<-hourtemps[,c("Year","Month","Day","Hour","Temp")]

1>2
3<4
1==2
2>=2
2<=3
!(2==2)

(2==2)&(3==3)

(2==3)|(2==3)

a<-3

b<-2


(a==b)

c(1,2,3,4,5)==4

hourtemps$Temp>=0
hourtemps$Temp<=7.2

Harry<-(hourtemps$Temp>=0)&(hourtemps$Temp<=7.2)
Harry

hourtemps[,"Chilling_Hour"]<-(hourtemps$Temp>=0)&(hourtemps$Temp<=7.2)

sum(hourtemps$Chilling_Hour)

Start_Date<-which(hourtemps$Year==2008 &
                    hourtemps$Month==10 &
                    hourtemps$Day==1 &
                    hourtemps$Hour==12)
End_Date<-which(hourtemps$Year==2008 &
                    hourtemps$Month==10 &
                    hourtemps$Day==31 &
                    hourtemps$Hour==12)

1:10
## Sum up chilling hours in October
sum(hourtemps$Chilling_Hour[Start_Date:End_Date])

?sum

test_function<-function(x) x+1

test_function(10)

test_2<-function(x,y)
{z<-x*y
 a<-z^4
 d<-z+a
 return(d)  
}

test_2(3,4)

CH<-function(Temp)
{
  (Temp>=0)&(Temp<=7.2)
}

CH(2)
CH(8)

hourtemps[,"CH"]<-CH(hourtemps$Temp)

CH_flex<-function(Temp,lower=0,upper=7.2)
{
  (Temp>=lower)&(Temp<=upper)
}

hourtemps[,"CH"]<-CH_flex(hourtemps$Temp)

hourtemps[,"CH_5_15"]<-CH_flex(hourtemps$Temp,lower=5,upper=15)

## add start and end dates and make this applicable to data.frame

CH_flex<-function(hourly,Start_Date=NA,End_Date=NA,lower=0,upper=7.2)
{
  hourly[,"CH"]<-(hourly$Temp>=lower)&(hourly$Temp<=upper)
  return(hourly)
}

hourtemps<-Winters_hours_gaps[,c("Year","Month","Day","Hour","Temp")]

CH_flex(hourtemps)



date<-20080324
date_to_date_components<-function(date)
{
year<-trunc(date/10000)
month<-trunc((date-year*10000)/100)
day<-date-(month*100)-year*10000
return(list(Year=year,Month=month,Day=day,type="Date"))
}


date_to_date_components(20041203)

CH_flex<-function(hourly,Start_Date=NA,End_Date=NA,lower=0,upper=7.2)
{
  hourly[,"CH"]<-(hourly$Temp>=lower)&(hourly$Temp<=upper)
  start<-date_to_date_components(Start_Date)
  end<-date_to_date_components(End_Date)
  
  Start_row<-which(hourly$Year==start$Year &
                     hourly$Month==start$Month &
                     hourly$Day==start$Day &
                     hourly$Hour==12)
  End_row<-which(hourly$Year==end$Year &
                   hourly$Month==end$Month &
                   hourly$Day==end$Day &
                   hourly$Hour==12)
  
  CH<-sum(hourly$CH[Start_row:End_row])
  return(CH)
}

CH_flex(hourtemps,
        Start_Date=20081001,
        End_Date=20081031,
        lower=10,upper=20)

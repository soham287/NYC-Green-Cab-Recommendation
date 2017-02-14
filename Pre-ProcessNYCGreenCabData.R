#install.packages("data.table")
#library(data.table)
#install.packages("ggmap")
#library(ggmap)
#install.packages("sp")
require("sp")
install.packages("maptools")
library(maptools)
install.packages("plotrix")
library(plotrix)
library(ggplot2)
#system.time(fread('green_tripdata_2016-02.csv', header = T, sep = ','))
#data<-read.csv('https://s3.amazonaws.com/nyc-tlc/trip+data/green_tripdata_2016-02.csv', header = T, sep = ',')
#Changing the pickup and drop-off time to POSIX
#data$lpep_pickup_datetime<-as.POSIXct(strptime(data$lpep_pickup_datetime, "%Y-%m-%d %H:%M:%S"))
#data$Lpep_dropoff_datetime<-as.POSIXct(strptime(data$Lpep_dropoff_datetime, "%Y-%m-%d %H:%M:%S"))

#Taking the data only between 1st and 14th of February.
mainData<-subset(data,data$Lpep_dropoff_datetime<"2016-02-14 23:59:59 EST")

# Added Burrough and area information to the data by reversegeocoding the latitude and longitude using ggmap
if(FALSE){
Borrough<-c()
Locality<-c()
for(i in 1:nrow(mainData)){
  temp=revgeocode(location=c(mainData$Pickup_longitude[i],mainData$Pickup_latitude[i]), output = c("more"), messaging = FALSE, sensor = FALSE, override_limit = FALSE, client = "", signature = "")
  if(is.null(temp$political)){
    Borrough[i]="BAD LOCATION"
  } else {
  Borrough[i]=as.character(na.omit(temp$political))
  }
  if(is.null(temp$neighborhood)){
    Locality[i]="BAD LOCATION"
  }
  else{
  Locality[i]=as.character(na.omit(temp$neighborhood))
  }
}
tempData<-data.frame(Borrough,Locality);
mainData<-cbind(mainData,tempData)
}
#if(FALSE){
#mainData<-subset(mainData,mainData$Pickup_longitude<=-72 & mainData$Pickup_longitude>=-78.91255) #& mainData$Pickup_latitude<=43.26759 & mainData$Pickup_latitude>=40)
nyc.shp <- readShapePoly("ZillowNeighborhoods-NY.shp")
proj4string(nyc.shp) <- CRS("+proj=longlat+ellps=WGS84")
coordinates<- data.frame(mainData$Pickup_longitude,mainData$Pickup_latitude)
coordinates.sp<-coordinates
coordinates.sp<-SpatialPoints(coordinates.sp,proj4string=CRS("+proj=longlat+ellps=WGS84"))
city.data<-over(coordinates.sp,nyc.shp)
County<-city.data$COUNTY
Location<-city.data$NAME
mainData<-cbind(mainData,data.frame(County,Location))
#}
noonTime="12:00:00"
eveTime="16:00:00"
nightTime="19:00:00"
peakTimeStart="07:00:00"
peakTimeEnd="11:00:00"
beforeMidNightTime="23:59:59"
midnightTime="00:00:00"
morningTime="04:00:00"
#Breaking this data into Morning Slot
mornData<-subset(mainData,strftime(mainData$lpep_pickup_datetime, format="%H:%M:%S")>morningTime & strftime(mainData$lpep_pickup_datetime, format="%H:%M:%S")<=noonTime)
mornLocData<-table(unlist(mornData$Location))
mornLocData<-sort(mornLocData,decreasing=TRUE)
mornLocData<-head(mornLocData,10)
mornDataFrame<-data.frame(mornLocData,names(mornLocData))
locationPlot<-ggplot(data=mornDataFrame, aes(x=mornDataFrame$names.mornLocData, y=mornDataFrame$mornLocData,label=mornDataFrame$names.mornLocData)) +
  +     geom_bar(stat="identity") +geom_text(fontface = "bold",position=position_jitter(width=0.5,height=10))
mornBorroData<-table(unlist(mornData$County))
mornBorroData<-sort(mornBorroData,decreasing=TRUE)
mornBorroData<-head(mornBorroData,5)
borroplot<-pie3D(mornBorroData, labels = names(mornBorroData), main = "Peak Hours Maximum Pickup Burroughs", explode=0.1, radius=.9, labelcex = 1.2,  start=0.7)
#Breaking this data into Peak hours Morning Slot
#peakmornData<-subset(mainData,strftime(mainData$lpep_pickup_datetime, format="%H:%M:%S")>peakTimeStart & strftime(mainData$lpep_pickup_datetime, format="%H:%M:%S")<=peakTimeEnd)

#Breaking this data into Noon Slot
#noonData<-subset(mainData,strftime(mainData$lpep_pickup_datetime, format="%H:%M:%S")>noonTime & strftime(mainData$lpep_pickup_datetime, format="%H:%M:%S")<=eveTime)

#Breaking this data into Evening Slot
#eveData<-subset(mainData,strftime(mainData$lpep_pickup_datetime, format="%H:%M:%S")>eveTime & strftime(mainData$lpep_pickup_datetime, format="%H:%M:%S")<=nightTime)

#Breaking this data into Night Slot
#nightData<-subset(mainData,strftime(mainData$lpep_pickup_datetime, format="%H:%M:%S")>nightTime & strftime(mainData$lpep_pickup_datetime, format="%H:%M:%S")<=beforeMidNightTime)

#Breaking this data into MidNight Slot
#midnightData<-subset(mainData,strftime(mainData$lpep_pickup_datetime, format="%H:%M:%S")>=midnightTime & strftime(mainData$lpep_pickup_datetime, format="%H:%M:%S")<=morningTime)



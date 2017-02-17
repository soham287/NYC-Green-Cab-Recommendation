#install.packages("data.table")
#library(data.table)
#install.packages("ggmap")
#library(ggmap)
#install.packages("sp")
#require("sp")
#install.packages("maptools")
#library(maptools)
#install.packages("plotrix")
#library(plotrix)
#library(ggplot2)
#install.packages("gridExtra")
library(gridExtra)
#system.time(fread('green_tripdata_2016-02.csv', header = T, sep = ','))
#data<-read.csv('green_tripdata_2016-02.csv', header = T, sep = ',')
#Changing the pickup and drop-off time to POSIX
#data$lpep_pickup_datetime<-as.POSIXct(strptime(data$lpep_pickup_datetime, "%Y-%m-%d %H:%M:%S"))
#data$Lpep_dropoff_datetime<-as.POSIXct(strptime(data$Lpep_dropoff_datetime, "%Y-%m-%d %H:%M:%S"))

#Taking the data only between 1st and 14th of February.
#mainData<-subset(data,data$Lpep_dropoff_datetime<"2016-02-14 23:59:59 EST")

# Added Burrough and area information to the data by reversegeocoding the latitude and longitude using ggmap

if(FALSE){
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
mainData$County<-as.character(mainData$County)
mainData$County[is.na(mainData$County)]<-"Staten Island"
}

noonTime="12:00:00"
eveTime="16:00:00"
nightTime="19:00:00"
peakTimeStart="07:00:00"
peakTimeEnd="11:00:00"
beforeMidNightTime="23:59:59"
midnightTime="00:00:00"
morningTime="04:00:00"
#Breaking this data into Morning Slot
if(FALSE){
mornData<-subset(mainData,strftime(mainData$lpep_pickup_datetime, format="%H:%M:%S")>=morningTime & strftime(mainData$lpep_pickup_datetime, format="%H:%M:%S")<=noonTime)
mornData<-mornData[!(mornData$lpep_pickup_datetime %in% peakmornData$lpep_pickup_datetime),]
mornLocData<-table(unlist(mornData$Location))
mornLocData<-sort(mornLocData,decreasing=TRUE)
mornLocData<-head(mornLocData,10)
mornDataFrame<-data.frame(mornLocData,names(mornLocData))
tableData<-data.frame(names(mornLocData),mornDataFrame$Freq)
colnames(tableData)<-c("LOCATIONS","PEAK-HOUR PICKUPCOUNT")
grid.table(tableData)
dev.off()
locationPlot<-ggplot(data=mornDataFrame, aes(x=1:10,y=mornDataFrame$Freq,label=mornDataFrame$names.mornLocData)) +
  geom_bar(stat="identity") +geom_text(fontface = "bold",position=position_jitter(width=.1,height=1),size=3.5)+xlab("Top 10 pickup Locations in the city")+ylab("Number of Cab trips in Morning-hours")
mornBorroData<-table(unlist(mornData$County))
mornBorroData<-sort(mornBorroData,decreasing=TRUE)
mornBorroData<-head(mornBorroData,5)
pie3D(mornBorroData, labels = c(paste("Manhattan ",floor(mornBorroData[1]/sum(mornBorroData)*100),"%"),paste("Brooklyn",floor(mornBorroData[2]/sum(mornBorroData)*100),"%"),paste("Queens",floor(mornBorroData[3]/sum(mornBorroData)*100),"%"),paste("Staten Island",floor(mornBorroData[4]/sum(mornBorroData)*100),"%"),paste("Bronx",floor(mornBorroData[5]/sum(mornBorroData)*100),"%")), main = "Morning-Hours Green Cab Trips in Borroghs", explode=0.1, radius=1.5, labelcex = 1.2,  start=0.7)
}
#Breaking this data into Peak hours Morning Slot
if(FALSE){
peakmornData<-subset(mainData,strftime(mainData$lpep_pickup_datetime, format="%H:%M:%S")>peakTimeStart & strftime(mainData$lpep_pickup_datetime, format="%H:%M:%S")<=peakTimeEnd)
mornLocData<-table(unlist(peakmornData$Location))
mornLocData<-sort(mornLocData,decreasing=TRUE)
mornLocData<-head(mornLocData,10)
mornDataFrame<-data.frame(mornLocData,names(mornLocData))
percentData<-c()
for(i in 1:10){
  percentData<-paste(percentData,"")
}
locationPlot<-ggplot(data=mornDataFrame, aes(x=1:10,y=mornDataFrame$Freq,label=mornDataFrame$names.mornLocData)) +
  geom_bar(stat="identity") +geom_text(fontface = "bold",position=position_jitter(width=.1,height=1),size=3.5)+xlab("Top 10 pickup Locations in the city")+ylab("Number of Cab trips in Peak-hours")
mornBorroData<-table(unlist(peakmornData$County))
mornBorroData<-sort(mornBorroData,decreasing=TRUE)
mornBorroData<-head(mornBorroData,5)
pie3D(mornBorroData, labels = c(paste("Manhattan ",floor(mornBorroData[1]/sum(mornBorroData)*100),"%"),paste("Brooklyn",floor(mornBorroData[2]/sum(mornBorroData)*100),"%"),paste("Queens",floor(mornBorroData[3]/sum(mornBorroData)*100),"%"),paste("Staten Island",floor(mornBorroData[4]/sum(mornBorroData)*100),"%"),paste("Bronx",floor(mornBorroData[5]/sum(mornBorroData)*100),"%")), main = "Peak-Hours Green Cab Trips in Borroghs", explode=0.1, radius=1.5, labelcex = 1.2,  start=0.7)
}
#Breaking this data into Noon Slot
#if(FALSE){
noonData<-subset(mainData,strftime(mainData$lpep_pickup_datetime, format="%H:%M:%S")>noonTime & strftime(mainData$lpep_pickup_datetime, format="%H:%M:%S")<=eveTime)
mornLocData<-table(unlist(noonData$Location))
mornLocData<-sort(mornLocData,decreasing=TRUE)
mornLocData<-head(mornLocData,10)
mornDataFrame<-data.frame(mornLocData,names(mornLocData))
percentData<-c()
for(i in 1:10){
  percentData<-paste(percentData,"")
}
locationPlot<-ggplot(data=mornDataFrame, aes(x=1:10,y=mornDataFrame$Freq,label=mornDataFrame$names.mornLocData)) +
  geom_bar(stat="identity") +geom_text(fontface = "bold",position=position_jitter(width=.1,height=1),size=3.5)+xlab("Top 10 pickup Locations in the city")+ylab("Number of Cab trips in Noon-hours")
mornBorroData<-table(unlist(noonData$County))
mornBorroData<-sort(mornBorroData,decreasing=TRUE)
mornBorroData<-head(mornBorroData,5)

tableData<-data.frame(names(mornLocData),mornDataFrame$Freq)
colnames(tableData)<-c("LOCATIONS","EVENING-HOUR PICKUPCOUNT")
grid.table(tableData)
dev.off()
pie3D(mornBorroData, labels = c(paste("Manhattan ",floor(mornBorroData[1]/sum(mornBorroData)*100),"%"),paste("Brooklyn",floor(mornBorroData[2]/sum(mornBorroData)*100),"%"),paste("Queens",floor(mornBorroData[3]/sum(mornBorroData)*100),"%"),paste("Staten Island",floor(mornBorroData[4]/sum(mornBorroData)*100),"%"),paste("Bronx",floor(mornBorroData[5]/sum(mornBorroData)*100),"%")), main = "Noon Green Cab Trips in Borroghs", explode=0.1, radius=1.5, labelcex = 1.2,  start=0.7)
#}
#Breaking this data into Evening Slot
if(FALSE){
eveData<-subset(mainData,strftime(mainData$lpep_pickup_datetime, format="%H:%M:%S")>eveTime & strftime(mainData$lpep_pickup_datetime, format="%H:%M:%S")<=nightTime)
mornLocData<-table(unlist(eveData$Location))
mornLocData<-sort(mornLocData,decreasing=TRUE)
mornLocData<-head(mornLocData,10)
mornDataFrame<-data.frame(mornLocData,names(mornLocData))
percentData<-c()
for(i in 1:10){
  percentData<-paste(percentData,"")
}
locationPlot<-ggplot(data=mornDataFrame, aes(x=1:10,y=mornDataFrame$Freq,label=mornDataFrame$names.mornLocData)) +
  geom_bar(stat="identity") +geom_text(fontface = "bold",position=position_jitter(width=.1,height=1),size=3.5)+xlab("Top 10 pickup Locations in the city")+ylab("Number of Cab trips in Noon-hours")
mornBorroData<-table(unlist(eveData$County))
mornBorroData<-sort(mornBorroData,decreasing=TRUE)
mornBorroData<-head(mornBorroData,5)
tableData<-data.frame(names(mornLocData),mornDataFrame$Freq)
colnames(tableData)<-c("LOCATIONS","EVENING-HOUR PICKUPCOUNT")
grid.table(tableData)
dev.off()
pie3D(mornBorroData, labels = c(paste("Manhattan ",floor(mornBorroData[1]/sum(mornBorroData)*100),"%"),paste("Brooklyn",floor(mornBorroData[2]/sum(mornBorroData)*100),"%"),paste("Queens",floor(mornBorroData[3]/sum(mornBorroData)*100),"%"),paste("Staten Island",floor(mornBorroData[4]/sum(mornBorroData)*100),"%"),paste("Bronx",floor(mornBorroData[5]/sum(mornBorroData)*100),"%")), main = "Evening Green Cab Trips in Borroghs", explode=0.1, radius=1.5, labelcex = 1.2,  start=0.7)
}
#Breaking this data into Night Slot
#if(FALSE){
nightData<-subset(mainData,strftime(mainData$lpep_pickup_datetime, format="%H:%M:%S")>nightTime & strftime(mainData$lpep_pickup_datetime, format="%H:%M:%S")<=beforeMidNightTime)
mornLocData<-table(unlist(nightData$Location))
mornLocData<-sort(mornLocData,decreasing=TRUE)
mornLocData<-head(mornLocData,10)
mornDataFrame<-data.frame(mornLocData,names(mornLocData))
percentData<-c()
for(i in 1:10){
  percentData<-paste(percentData,"")
}
locationPlot<-ggplot(data=mornDataFrame, aes(x=1:10,y=mornDataFrame$Freq,label=mornDataFrame$names.mornLocData)) +
  geom_bar(stat="identity") +geom_text(fontface = "bold",position=position_jitter(width=.1,height=1),size=3.5)+xlab("Top 10 pickup Locations in the city")+ylab("Number of Cab trips in Night-hours")
mornBorroData<-table(unlist(nightData$County))
mornBorroData<-sort(mornBorroData,decreasing=TRUE)
mornBorroData<-head(mornBorroData,5)
tableData<-data.frame(names(mornLocData),mornDataFrame$Freq)
colnames(tableData)<-c("LOCATIONS","NIGHT-HOUR PICKUPCOUNT")
grid.table(tableData)
dev.off()
pie3D(mornBorroData, labels = c(paste("Manhattan ",floor(mornBorroData[1]/sum(mornBorroData)*100),"%"),paste("Brooklyn",floor(mornBorroData[2]/sum(mornBorroData)*100),"%"),paste("Queens",floor(mornBorroData[3]/sum(mornBorroData)*100),"%"),paste("Staten Island",floor(mornBorroData[4]/sum(mornBorroData)*100),"%"),paste("Bronx",floor(mornBorroData[5]/sum(mornBorroData)*100),"%")), main = "Night Green Cab Trips in Borroghs", explode=0.1, radius=1.5, labelcex = 1.2,  start=0.7)
#}
#Breaking this data into MidNight Slot
#if(FALSE){
midnightData<-subset(mainData,strftime(mainData$lpep_pickup_datetime, format="%H:%M:%S")>=midnightTime & strftime(mainData$lpep_pickup_datetime, format="%H:%M:%S")<=morningTime)
mornLocData<-table(unlist(midnightData$Location))
mornLocData<-sort(mornLocData,decreasing=TRUE)
mornLocData<-head(mornLocData,10)
mornDataFrame<-data.frame(mornLocData,names(mornLocData))
percentData<-c()
for(i in 1:10){
  percentData<-paste(percentData,"")
}
locationPlot<-ggplot(data=mornDataFrame, aes(x=1:10,y=mornDataFrame$Freq,label=mornDataFrame$names.mornLocData)) +
  geom_bar(stat="identity") +geom_text(fontface = "bold",position=position_jitter(width=.1,height=1),size=3.5)+xlab("Top 10 pickup Locations in the city")+ylab("Number of Cab trips in MidNight-hours")
mornBorroData<-table(unlist(midnightData$County))
mornBorroData<-sort(mornBorroData,decreasing=TRUE)
mornBorroData<-head(mornBorroData,5)
tableData<-data.frame(names(mornLocData),mornDataFrame$Freq)
colnames(tableData)<-c("LOCATIONS","MIDNIGHT PICKUPCOUNT")
grid.table(tableData)
dev.off()
pie3D(mornBorroData, labels = c(paste("Manhattan ",floor(mornBorroData[1]/sum(mornBorroData)*100),"%"),paste("Brooklyn",floor(mornBorroData[2]/sum(mornBorroData)*100),"%"),paste("Queens",floor(mornBorroData[3]/sum(mornBorroData)*100),"%"),paste("Staten Island",floor(mornBorroData[4]/sum(mornBorroData)*100),"%"),paste("Bronx",floor(mornBorroData[5]/sum(mornBorroData)*100),"%")), main = "Midnight Green Cab Trips in Borroghs", explode=0.1, radius=1.5, labelcex = 1.2,  start=0.7)
#}
#TIme-Slots vs Number of Pickups
if(FALSE){
pickupNo<-c(nrow(mornData),nrow(peakmornData),nrow(noonData),nrow(eveData),nrow(nightData),nrow(midnightData))
timeSlots<-c("Morning","Peak-Hours","Afternoon","Evening","Night","Midnight")
timeSlotData<-data.frame(pickupNo,timeSlots)
PickupPlottime<-ggplot(data=timeSlotData, aes(x=timeSlotData$timeSlots, y=timeSlotData$pickupNo,label=timeSlotData$pickupNo))+ 
geom_bar(stat="identity") +geom_text(fontface = "bold",position=position_jitter(width=.1,height=1),size=3)+xlab("Time Slots")+ylab("Number of cab Trips")
}
#Breaking the Data in terms of Borros
manhattanData<-subset(mainData,mainData$County=="New York")
bronxData<-subset(mainData,mainData$County=="Bronx")
brooklynData<-subset(mainData,mainData$County=="Kings")
queensData<-subset(mainData,mainData$County=="Queens")
statenIsland<-subset(mainData,mainData$County=="Richmond")
#Area-wise fare and tip amount amount
if(FALSE){
brookAmount<-sum(brooklynData$Fare_amount)
bronxAmount<-sum(bronxData$Fare_amount)
manhattanData<-sum(manhattanData$Fare_amount)
queensData<-sum(queensData$Fare_amount)
statenIslandData<-sum(statenIsland$Fare_amount)
totalSumData<-c(brookAmount,bronxAmount,manhattanData,queensData)
x<-totalSumData/sum(mainData$Fare_amount)*100
names<-c("Brooklyn","Bronx","Manhattan","Queens")
amountFrame<-data.frame(names,totalSumData,ceiling(x))
colnames(amountFrame)<-c("Borrogh Name","Total Revenue","% Total Revenue")
amountFrame<-data.frame(totalSumData,names)
pie3D(totalSumData, labels = names, main = "Area-wise Share of Revenue", explode=0.1, radius=1.5, labelcex = 1.2,  start=0.7)
locationPlotNight<-ggplot(data=amountFrame, aes(x=amountFrame$names, y=ceiling(amountFrame$totalSumData),label=ceiling(amountFrame$totalSumData))) +
geom_bar(stat="identity") +geom_text(fontface = "bold",position=position_jitter(width=.1,height=1),size=3)+xlab("Borroghs")+ylab("Total Revenue in the observed 14 days")
amountFrame<-data.frame(names,totalSumData,x)
}
#Area-wise tip amount
if(FALSE){
brookAmount<-sum(brooklynData$Tip_amount)
bronxAmount<-sum(bronxData$Tip_amount)
manhattanData<-sum(manhattanData$Tip_amount)
queensData<-sum(queensData$Tip_amount)
totalSumData<-c(brookAmount,bronxAmount,manhattanData,queensData)
names<-c("Brooklyn","Bronx","Manhattan","Queens")
pie3D(totalSumData, labels = names, main = "Area-wise Share of Tips", explode=0.1, radius=1.5, labelcex = 1.2,  start=0.7)
amountFrame<-data.frame(totalSumData,names)
locationPlotNight<-ggplot(data=amountFrame, aes(x=amountFrame$names, y=amountFrame$totalSumData,label=amountFrame$totalSumData)) +
 geom_bar(stat="identity") +geom_text(fontface = "bold",position=position_jitter(width=.1,height=1),size=3)+xlab("Borroghs")+ylab("Total Tips in the observed 14 days")
}

#Area-wise trip lengths
if(FALSE){
brookAmount<-sum(brooklynData$Trip_distance)
bronxAmount<-sum(bronxData$Trip_distance)
manhattanData<-sum(manhattanData$Trip_distance)
queensData<-sum(queensData$Trip_distance)
totalSumData<-c(brookAmount,bronxAmount,manhattanData,queensData)
names<-c("Brooklyn","Bronx","Manhattan","Queens")
amountFrame<-data.frame(totalSumData,names)
locationPlotNight<-ggplot(data=amountFrame, aes(x=amountFrame$names, y=amountFrame$totalSumData,label=amountFrame$totalSumData)) +
  geom_bar(stat="identity") +geom_text(fontface = "bold",position=position_jitter(width=.1,height=1),size=3)+ylab("Total Distance")+xlab("Borroghs")
}
#Area-wise toll amount
#if(FALSE){
brookAmount<-sum(brooklynData$Tolls_amount)
bronxAmount<-sum(bronxData$Tolls_amount)
manhattanData<-sum(manhattanData$Tolls_amount)
queensData<-sum(queensData$Tolls_amount)
totalSumData<-c(brookAmount,bronxAmount,manhattanData,queensData)
names<-c("Brooklyn","Bronx","Manhattan","Queens")
amountFrame<-data.frame(totalSumData,names)
locationPlotNight<-ggplot(data=amountFrame, aes(x=amountFrame$names, y=amountFrame$totalSumData,label=amountFrame$totalSumData)) +
  geom_bar(stat="identity") +geom_text(fontface = "bold",position=position_jitter(width=.1,height=1),size=3)+xlab("Borroghs")+ylab("Toll Amount")
#}
#Area-wise Passenger Count
if(FALSE){
brookAmount<-sum(brooklynData$Passenger_count)
bronxAmount<-sum(bronxData$Passenger_count)
manhattanData<-sum(manhattanData$Passenger_count)
queensData<-sum(queensData$Passenger_count)
totalSumData<-c(brookAmount,bronxAmount,manhattanData,queensData)
names<-c("Brooklyn","Bronx","Manhattan","Queens")
amountFrame<-data.frame(totalSumData,names)
locationPlotNight<-ggplot(data=amountFrame, aes(x=amountFrame$names, y=amountFrame$totalSumData,label=amountFrame$totalSumData)) +
  geom_bar(stat="identity") +geom_text(fontface = "bold",position=position_jitter(width=.1,height=1),size=3)
}
#Weather-Data and comparison
#if(FALSE){
weather<-read.csv("WeatherData.csv",header=T,sep=",")
weather$AVG<-paste(as.character(weather$AVG)," 'F")
weather<-weather[-c(15,16),]
dateData<-table(unlist(as.Date(mainData$lpep_pickup_datetime,tz="EST")))
dateDataFrame=data.frame(dateData,weather$AVG)
dateDataPlot<-ggplot(data=dateDataFrame, aes(x=as.character(1:14), y=dateDataFrame$Freq,label=dateDataFrame$weather.AVG)) +
 geom_bar(stat="identity") +geom_text(fontface = "bold",position=position_jitter(width=.1,height=1),size=3.5)+xlab("Dates from 1-14 Feb,2016")+ylab("Number Of Trips")
#}

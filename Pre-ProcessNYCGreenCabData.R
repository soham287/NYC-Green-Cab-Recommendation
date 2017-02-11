#install.packages("data.table")
#library(data.table)
#system.time(fread('green_tripdata_2016-02.csv', header = T, sep = ','))
#data<-read.csv('https://s3.amazonaws.com/nyc-tlc/trip+data/green_tripdata_2016-02.csv', header = T, sep = ',')
#Changing the pickup and drop-off time to POSIX
#data$lpep_pickup_datetime<-as.POSIXct(strptime(data$lpep_pickup_datetime, "%Y-%m-%d %H:%M:%S"))
#data$Lpep_dropoff_datetime<-as.POSIXct(strptime(data$Lpep_dropoff_datetime, "%Y-%m-%d %H:%M:%S"))

#Taking the data only between 1st and 14th of February.
#mainData<-subset(data,data$Lpep_dropoff_datetime<"2016-02-14 23:59:59 EST")
noonTime="12:00:00"
eveTime="16:00:00"
nightTime="19:00:00"
beforeMidNightTime="23:59:59"
midnightTime="00:00:00"
morningTime="04:00:00"
#Breaking this data into Morning Slot
mornData<-subset(mainData,strftime(mainData$lpep_pickup_datetime, format="%H:%M:%S")>morningTime & strftime(mainData$lpep_pickup_datetime, format="%H:%M:%S")<=noonTime)

#Breaking this data into Noon Slot
noonData<-subset(mainData,strftime(mainData$lpep_pickup_datetime, format="%H:%M:%S")>noonTime & strftime(mainData$lpep_pickup_datetime, format="%H:%M:%S")<=eveTime)

#Breaking this data into Evening Slot
eveData<-subset(mainData,strftime(mainData$lpep_pickup_datetime, format="%H:%M:%S")>eveTime & strftime(mainData$lpep_pickup_datetime, format="%H:%M:%S")<=nightTime)

#Breaking this data into Night Slot
nightData<-subset(mainData,strftime(mainData$lpep_pickup_datetime, format="%H:%M:%S")>nightTime & strftime(mainData$lpep_pickup_datetime, format="%H:%M:%S")<=beforeMidNightTime)

#Breaking this data into MidNight Slot
midnightData<-subset(mainData,strftime(mainData$lpep_pickup_datetime, format="%H:%M:%S")>=midnightTime & strftime(mainData$lpep_pickup_datetime, format="%H:%M:%S")<=morningTime)
#install.packages("data.table")
#library(data.table)
#system.time(fread('green_tripdata_2016-02.csv', header = T, sep = ','))
#data<-read.csv('https://s3.amazonaws.com/nyc-tlc/trip+data/green_tripdata_2016-02.csv', header = T, sep = ',')
#Changing the pickup and drop-off time to POSIX
data$lpep_pickup_datetime<-as.POSIXct(strptime(data$lpep_pickup_datetime, "%Y-%m-%d %H:%M:%S"))
data$Lpep_dropoff_datetime<-as.POSIXct(strptime(data$Lpep_dropoff_datetime, "%Y-%m-%d %H:%M:%S"))

#Taking the data only between 1st and 14th of February.
mainData<-subset(data,data$Lpep_dropoff_datetime<"2016-02-14 23:59:59 EST")
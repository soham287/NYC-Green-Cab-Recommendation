#install.packages("data.table")
#library(data.table)
#system.time(fread('green_tripdata_2016-02.csv', header = T, sep = ','))
data<-read.csv('https://s3.amazonaws.com/nyc-tlc/trip+data/green_tripdata_2016-02.csv', header = T, sep = ',')
p<-as.POSIXct(strptime(data[1,3], "%Y-%m-%d %H:%M:%S"))
p<"2016-02-14 21:59:59"
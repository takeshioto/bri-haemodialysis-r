getwd()

filter_info <- read.csv("haemofiltration_ptassessment_processed.csv", na.strings="")
filter_info
head(filter_info)
summary(filter_info)
str(filter_info)

#Task:
#1. How many patients are receiving RRT?
#2. How many days of RRT are we providing?

#Data preparation
#Changing time to POISXct time format 
#Nulling the original time column and moving PosixTime to the original (3rd) column
filter_info$PosixTime <- as.POSIXct(filter_info$time, format="%d/%m/%Y %H:%M")
filter_info$time <- NULL
filter_info <- filter_info[ , c(1,2,18,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)]

#changing blood flow rate and return venous pressure from factor to doubles
#theres a lot of NaN or nan entries in the access pressure. what is this? gonna change them to NA
filter_info$Blood.Flow.Rate <- as.character(filter_info$Blood.Flow.Rate)
for(i in 1:nrow(filter_info)){
  if(filter_info[i,4]=="nan" | filter_info[i,4]=="NaN"){ 
    filter_info[i,4] <- NA
  }
}



filter_info[1,3]

test_time <- as.POSIXct("13/07/2015 15:00", format="%d/%m/%Y %H:%M")

test_time < filter_info[1,3]

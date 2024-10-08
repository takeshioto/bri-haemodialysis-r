getwd()

filter_info <- read.csv("haemofiltration_ptassessment_processed.csv", na.strings="")
filter_info
head(filter_info)
summary(filter_info)
str(filter_info)

#Task:
#1. analyse with mean and SD the filter duration over time
#2. separate 1. into planned and unplanned stoppages of filter duration

#Data preparation
#Changing time to POISXct time format 
#Nulling the original time column and moving PosixTime to the original (3rd) column
filter_info$PosixTime <- as.POSIXct(filter_info$time, format="%d/%m/%Y %H:%M")
filter_info$time <- NULL
filter_info <- filter_info[ , c(1,2,18,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)]

#changing blood flow rate and return venous pressure from factor to doubles
#theres some "NaN" or "nan" 
#entries in the access pressure. what is this? gonna change them to NA
filter_info$Blood.Flow.Rate <- as.character(filter_info$Blood.Flow.Rate)
for(i in 1:nrow(filter_info)){
  if(filter_info[i,4]=="nan" | filter_info[i,4]=="NaN"){ 
    filter_info[i,4] <- NA
  }
}

#getting the positions where there is an entry in reason.for.filter.loss as a vector of those row positions
positions_reasons_for_filter_loss <- which(!is.na(filter_info$Reason.for.Filter.Loss) == TRUE) 


#a function which takes an argument x which is an integer which directs to the row number in filter_info whereby there is an entry 
#in reason.for.filter loss. Returns a true value when the following conditions are met 1.there is a same encounterID in the row above, 
#2.there is no reason why the filter has stopped, and 3. ensuring there is EITHER an entry in access or return pressure or fluid removed
move.up.query <- function(x){
  before <- x-1
  if(encounter == filter_info[before,"encounterId"]){ #encounter will be initialised later in the loop
    if(is.na(filter_info[before,"Reason.for.Filter.Loss"])){
      if(!is.na(filter_info[before,"Blood.Flow.Rate"]) | !is.na(filter_info[before,"Return..Venous..Pressure"]) 
         | !is.na(filter_info[before,"Fluid.Removed"])){
        TRUE
      }else{
        FALSE
      }
    }else{
      FALSE
    }
  }else{
    FALSE
  }
}

#creating some empty vectors to stor the starting.row.position, the ending.row.position and the reason.filter.loss
filter_started_position_vector <- c()
initial_position_vector <- c()
initial_reason_filter_loss_vector <- c()

#heres the main loop. cycles through the row positions where there is an entry in reason for filter loss
#for each of those positions it starts a while loop that keeps moving up until the function returns false. it does not move up if 
#my function returns FALSE
for(i in positions_reasons_for_filter_loss){
  
  initial_position_vector <- c(initial_position_vector, i)
  initial_reason_filter_loss_vector <- c(initial_reason_filter_loss_vector, as.character(filter_info[i,"Reason.for.Filter.Loss"]))
  encounter <- filter_info[i,2]
  
  #a while loop to keep moving up while there my function says it's ok to do so
  while(move.up.query(i) == TRUE){
    i <- i-1
  }
  
  filter_started_position_vector <- c(filter_started_position_vector,i)
  
}#END OF LOOP

#joining up the vectors to make a dataframe
dataframe_length_positions <- data.frame(filter_started_position_vector,initial_position_vector,initial_reason_filter_loss_vector)
colnames(dataframe_length_positions) <- c("starting.row.position", "ending.row.position", "reason.filter.loss")


#a function that takes a dataframe as an argument where the dataframe is of structure:
#[filter.start.row.position , filter.end.row.position , reason.for.filter.loss , ect other columns ive added on.....]
#returns a vector made up of the time differences in hours
calculate.time.differences <- function(x){
  time_differences <- c()
  for (i in 1:nrow(x)){
    start_position <- x[i,1]
    end_position <- x[i,2]
    individual_time_difference <- difftime(filter_info[end_position,3] , filter_info[start_position,3] , units="hours")
    time_differences <- c(time_differences,individual_time_difference)
  }
  time_differences
}

#lets look at splitting these up over time. theres probably not enough data per month so lets look at q1/q2/q3/q4
#the first entry in the database is from: min(filter_info$PosixTime)        output: [1] "2015-02-03 16:00:00 GMT"
#the last entry in the database at time of writing is [1] "2019-04-14 19:00:00 BST"
#so lets start from the first full quarter which will be Q2 2015. the analysis will need to end at current quarter-1

#im gonna alter the dataframe_length_positions adding columns for starting time, year and month that the starting row positions sit in
dataframe_length_positions$start.time <- filter_info[dataframe_length_positions$starting.row.position,"PosixTime"] 
dataframe_length_positions$year <- as.factor(format(dataframe_length_positions$start.time , "%Y"))
dataframe_length_positions$quarter <- as.factor(quarters(dataframe_length_positions$start.time))
dataframe_length_positions$period <- paste(dataframe_length_positions$year , dataframe_length_positions$quarter)
dataframe_length_positions$duration <- calculate.time.differences(dataframe_length_positions)


#lets create 2 separate dataframes one for planned stopping and one for unplanned
unplanned_filter_loss <- subset(dataframe_length_positions, reason.filter.loss=="Clotted" | reason.filter.loss=="Clogged" | 
                                  reason.filter.loss=="Vascular Access Issues")
planned_filter_loss <- subset(dataframe_length_positions, reason.filter.loss=="Electively stopped" | 
                                reason.filter.loss=="End of Treatment" | reason.filter.loss=="Transfer to Theatre/CT")


#Final function that takes argument h, a dataframe of the following structure
#[filter.start.row.position , filter.end.row.position , reason.for.filter.loss , ect other columns ive added on.....]
#make sure to subset the dataframe_length_positions to look at the tings you want

calculate.quarterly.mean.filter.durations <- function(h){
  
  #empty vectors for the function
  year_temp_vector <- c()
  quarter_temp_vector <- c()
  period_temp_vector <- c()
  mean_duration_temp_vector <- c()
  standard_deviation_temp_vector <- c()
  number_of_values_temp_vector <- c()
  
  quarter_temp <- as.factor(c("Q1" , "Q2" , "Q3" , "Q4"))
  
  i <- 2015 #Starting year for all data
  #a while loop that cycles through each year form 2015 onwards
  while (i <= as.numeric(format(Sys.Date() , "%Y"))){ #while i is less than the current year
    yearly_temp_subset <- subset(h , year == i)
    
    # a for loop that cycles throught the defined quarters and for each quarter calculate the mean and standard deviation 
    #of filter duration
    for(b in quarter_temp){
      quarterly_temp_subset <- subset(yearly_temp_subset , quarter == b)
      
      year_temp_vector <- c(year_temp_vector , i)
      quarter_temp_vector <- c(quarter_temp_vector , b)
      period_temp_vector <- c(period_temp_vector , paste(i,b))
      mean_duration_temp_vector <- c(mean_duration_temp_vector , mean(calculate.time.differences(quarterly_temp_subset)))
      standard_deviation_temp_vector <- c(standard_deviation_temp_vector , sd(calculate.time.differences(quarterly_temp_subset)))
      number_of_values_temp_vector <- c(number_of_values_temp_vector , nrow(quarterly_temp_subset))
    }
    i <- i+1
  }
  
  #joingin up the vectors
  dataframe_quarterly_analysis <- data.frame(year_temp_vector , quarter_temp_vector , period_temp_vector , mean_duration_temp_vector 
                                             , standard_deviation_temp_vector , number_of_values_temp_vector)
  colnames(dataframe_quarterly_analysis) <- c("year", "quarter", "period" , "mean.filter.duration" , "standard.deviation" , "    n")
  
  dataframe_quarterly_analysis
  
}

calculate.quarterly.mean.filter.durations(dataframe_length_positions)
calculate.quarterly.mean.filter.durations(unplanned_filter_loss)
calculate.quarterly.mean.filter.durations(planned_filter_loss)

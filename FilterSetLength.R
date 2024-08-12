getwd()

filter_info <- read.csv("haemofiltration_ptassessment_processed.csv", na.strings="")
filter_info
head(filter_info)
summary(filter_info)
str(filter_info)

# Task: to determine how long a filter set stays up for

#notes: 
#column for encounterID = 2
#column for access pressure (or blood flow rate) = 4
#column for reason for filter loss = 15
#column for return venous pressure = 16

#Data preparation
#Changing time to POISXct time format 
#Nulling the original time column and moving PosixTime to the original (3rd) column
filter_info$PosixTime <- as.POSIXct(filter_info$time, format="%d/%m/%Y %H:%M")
filter_info$time <- NULL
filter_info <- filter_info[ , c(1,2,18,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)]

#changing blood flow rate and return venous pressure from factor to characters
#theres a lot of NaN or nan entries in the access pressure. what is this? gonna change them to NA
filter_info$Blood.Flow.Rate <- as.character(filter_info$Blood.Flow.Rate)
for(i in 1:nrow(filter_info)){
  if(filter_info[i,"Blood.Flow.Rate"]=="nan" | filter_info[i,"Blood.Flow.Rate"]=="NaN"){ 
    filter_info[i,"Blood.Flow.Rate"] <- NA
  }
}
filter_info$Blood.Flow.Rate <- as.double(filter_info$Blood.Flow.Rate)
#TODO tidy up data types in the columns


#I will assume that I can only analyse the filter runs which end with an entry in the reason for filter loss column

#first lets pick out all the entries with a reason in the filter loss column
table(!is.na(filter_info$Reason.for.Filter.Loss)==TRUE) # looks like theres 751 entries in the reason.for.loss column
#another way which just returns an integer for the number of "TRUE"...    table(!is.na(filter_info$Reason.for.Filter.Loss))["TRUE"]
#getting the positions of the entries where there is an entry for reason.for.filter loss and saving it to a vector
positions_reasons_for_filter_loss <- which(!is.na(filter_info$Reason.for.Filter.Loss) == TRUE) 

#a loop to to start at the positions where there is a reason for filter loss then count backwards in time
#in this run as long as there is EITHER a entry for blood flow or return pressure i will assume the filter is working
#will calculate the length of time a filter runs until there is neither an entry for blood flow or venous pressure

#creating some empty vectors to stor the starting.row.position, the ending.row.position and the reason.filter.loss
filter_started_position_vector <- c()
initial_position_vector <- c()
initial_reason_filter_loss_vector <- c()

#heres the main loop
for(i in positions_reasons_for_filter_loss){
  
  initial_position_vector <- c(initial_position_vector, i)
  initial_reason_filter_loss_vector <- c(initial_reason_filter_loss_vector, as.character(filter_info[i,"Reason.for.Filter.Loss"]))
  encounter <- filter_info[i,2]

  #a function that returns a true value when 1.there is a same encounterID in the row above, 2. there is no reason why the 
  #filter has clotted, and 3. ensuring there is either an entry in access or return pressure or fluid removed
  move.up.query <- function(x){
    before <- x-1
    if(encounter == filter_info[before,"encounterId"]){
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
  
  #a while loop to keep moving up while there my function says it's ok to do se
  while(move.up.query(i) == TRUE){
    i <- i-1
  }
  
  filter_started_position_vector <- c(filter_started_position_vector,i)
  
}#END OF THIS MAMMOTH FOR LOOP

#joining up the vectors to make a dataframe
dataframe_length_positions <- data.frame(filter_started_position_vector,initial_position_vector,initial_reason_filter_loss_vector)
colnames(dataframe_length_positions) <- c("starting.row.position", "ending.row.position", "reason.filter.loss")

#OK now lets start figuring out times and durations of things
#lets create 2 separate dataframes one for planned stopping and one for unplanned

unplanned_filter_loss <- subset(dataframe_length_positions, reason.filter.loss=="Clotted" | reason.filter.loss=="Clogged" | 
                                  reason.filter.loss=="Vascular Access Issues")
planned_filter_loss <- subset(dataframe_length_positions, reason.filter.loss=="Electively stopped" | 
                                reason.filter.loss=="End of Treatment" | reason.filter.loss=="Transfer to Theatre/CT")


#a function that takes a dataframe as an argument where the dataframe is of structure:
#[filter.start.row.position , filter.end.row.position , reason.for.filter.loss, ect other columns ive added on.....]
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



#averages
mean_unplanned_filter_duration <- mean(calculate.time.differences(unplanned_filter_loss))
mean_planned_filter_duration <- mean(calculate.time.differences(planned_filter_loss))


#DELIVERABLE
mean_unplanned_filter_duration
mean_planned_filter_duration


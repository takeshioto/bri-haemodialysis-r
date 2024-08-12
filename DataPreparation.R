getwd()

lab_results <- read.csv("haemofiltration_labresults_processed.csv")
lab_results
head(lab_results)
summary(lab_results)
str(lab_results)

patient_details <- read.csv("haemofiltration_patient_summary_processed.csv")
patient_details
head(patient_details)
summary(patient_details)
str(patient_details)

filter_info <- read.csv("haemofiltration_ptassessment_processed.csv")
filter_info
head(filter_info)
summary(filter_info)
str(filter_info)


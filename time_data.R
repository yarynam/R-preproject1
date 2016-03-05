alldata=merge(grades,activity,by="student_id")
setwd("/Users/iaryna/Documents/Mizzou/DataScience/R/preproject1")
write.csv(alldata, file = "alldata.csv")


#read time data
alldata$start_time <- strptime(alldata$start_time,"%d-%m-%Y %H:%M:%S")
alldata$end_time <- strptime(alldata$end_time,"%d-%m-%Y %H:%M:%S")

# create time difference column (in seconds)
alldata$timedif <- alldata$end_time - alldata$start_time


#install data.table package
install.packages("data.table")
library(data.table)

alldata <-  data.table(alldata)

# compute time lapse per student 
time_spend <- aggregate(alldata$timedif, by=list(student_id=alldata$student_id), FUN=sum)
#convert to numeric
time_spend$x <- as.numeric(time_spend$x)
#remane culumn
names(time_spend)[names(time_spend)=="x"] <- "time_sum_per_id"
#merge two satasets
alldata=merge(alldata,time_spend,by="student_id")

# compute time lapse per student for different activities
#aggregate by two columns
time_spend2 <- aggregate(alldata$timedif, by=list(student_id=alldata$student_id, activity=alldata$activity), FUN=sum)
time_spend2$x <- as.numeric(time_spend2$x)


#filter all study activities for all exercises 
#use regex to find all study activities
study <- grep("Study_Es",  alldata$activity, value = TRUE)
study_time <-  time_spend2[ time_spend2$activity %in% study,] 
# aggregate by id
study_time <- aggregate(study_time$x, by=list(student_id=study_time$student_id), FUN=sum)
names(study_time)[names(study_time)=="x"] <- "study_time_per_id"


alldata=merge(alldata,study_time,by="student_id")


#filter all deeds activities for all exercises 
deeds <- grep("Deeds_Es",  alldata$activity, value = TRUE)
deeds_time <-  time_spend2[ time_spend2$activity %in% deeds,] 
deeds_time <- aggregate(deeds_time$x, by=list(student_id=deeds_time$student_id), FUN=sum)
names(deeds_time)[names(deeds_time)=="x"] <- "deeds_time_per_id"


alldata=merge(alldata,deeds_time,by="student_id")



#get data frame of time spent by every student on other activity 
other_activity <- time_spend2[ which(time_spend2$activity==smth), ]
names(other_activity)[names(other_activity)=="x"] <- "time_texteditor_per_id"
other_activity$activity <- NULL
other_activity

alldata=merge(alldata,other_activity,by="student_id")


library(ggplot2)

ggplot(alldata, aes(x=study_time_per_id, y=ex_1_total)) +
  geom_point(shape=1) 

#compute correlation
cor(alldata$study_time_per_id, alldata$ex_1_total, use="pairwise.complete.obs")

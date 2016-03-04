alldata=merge(grades,activity,by="student_id")
setwd("/Users/iaryna/Documents/Mizzou/DataScience/R/preproject1")
write.csv(alldata, file = "alldata.csv")

#read time data
alldata$start_time <- strptime(alldata$start_time,"%m-%d-%Y %H:%M:%S")
alldata$end_time <- strptime(alldata$end_time,"%m-%d-%Y %H:%M:%S")

# create time difference column
alldata$timedif <- alldata$end_time - alldata$start_time

library(plyr)
time_spend <- ddply(alldata, .(student_id), summarize, Sum=sum(timedif, na.rm = TRUE)
time_spend$Sum <- as.numeric(time_spend$Sum)

alldata=merge(alldata,time_spend,by="student_id")

library(ggplot2)

ggplot(alldata, aes(x=ex_2_total, y=ex_1_total)) +
  geom_point(shape=1) +
  geom_smooth(method=lm) 


                            
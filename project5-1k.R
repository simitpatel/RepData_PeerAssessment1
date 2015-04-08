###GLOBAL TO ALL PLOTS###

library(plyr)
library(dplyr)
library(ggplot2)
library(Hmisc)

raw <- read.csv("./R/course5/sourcedata/activity.csv")

#calculate number of steps taken per day. first, group by day; then, summarize steps. 

tsteps <- tapply(raw$steps,raw$date,sum)
tsteps <- as.data.frame(tsteps)
tsteps <- mutate(tsteps,date=row.names(tsteps))
tsteps <- tsteps[,c(2,1)]

#plot histogram of steps by day

hist(tsteps$tsteps,xlab="Steps",main="Histogram of Steps by Day")

#calculate mean and median of steps by day

complete <- na.omit(tsteps)
meansteps <- mean(complete$tsteps)
mediansteps <- median(complete$tsteps)


#Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, 
#averaged across all days (y-axis)

raw$interval <- as.factor(raw$interval)
clean <- na.omit(raw)

cleaninterval <- tapply(clean$steps,clean$interval,mean)
cleaninterval <- as.data.frame(cleaninterval)
cleaninterval <- mutate(cleaninterval,interval=row.names(cleaninterval))
cleaninterval <- cleaninterval[,c(2,1)]
names(cleaninterval) <- c("interval","AvgSteps")
ggplot(data=cleaninterval, aes(x=interval, y=AvgSteps, group=1)) + geom_line() + geom_point() +
  xlab("Interval") + ylab("Average Steps") +
  ggtitle("Average Steps by Interval")


#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

cleaninterval$AvgSteps <- as.numeric(cleaninterval$AvgSteps)
maxinterval <- filter(cleaninterval, AvgSteps==max(AvgSteps))

#Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

missingrows <- nrow(raw) - nrow(clean)

#replace NAs with the mean steps of that intervals

rawfill <- ddply(raw, "interval", mutate, imputed.value = impute(steps, mean))

#Make a histogram of the total number of steps taken each day 

rawfill$imputed.value <- as.numeric(rawfill$imputed.value)
imputedset <- rawfill %>% group_by(date) %>% summarise(sum(imputed.value))
names(imputedset) <-c("date","steps")

hist(imputedset$steps,xlab="Steps",main="Histogram of Steps by Day")

#and Calculate and report the mean and median total number of steps taken per day. 

meanimputed <- mean(imputedset$steps)
medianimputed <- median(imputedset$steps)

#Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating 
#whether a given date is a weekday or weekend day.

rawfill$date <- as.Date(rawfill$date)

rawadded <- mutate(rawfill,DayType =ifelse(weekdays(rawfill$date) %in% c("Sunday","Saturday"), "Weekend","Weekday")) 

#Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, 
#averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this 
#plot should look like using simulated data.

rawadded$imputed.value <- as.numeric(rawadded$imputed.value)
stepsintervaltype <- rawadded %>% group_by(interval,DayType) %>% summarise(mean(imputed.value))
names(stepsintervaltype) <- c("interval","DayType","AvgSteps")
stepsintervaltype$AvgSteps <- round(stepsintervaltype$AvgSteps,digits=2)
ggplot(stepsintervaltype, aes(x=interval,y=AvgSteps,group=DayType,color=DayType))+geom_line()+
  facet_grid(DayType ~ .)

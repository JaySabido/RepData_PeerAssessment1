# Course: Reproducible Research
# Course Project 1
# by Jay Sabido

setwd("~/Documents/Jay/Courses/Data Science/Course 5 - Reproducible Research/Week 2/RepData_PeerAssessment1")

# if (!file.exists("data")) {
#   dir.create("data")
# }

# fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
# download.file(fileUrl, destfile = "./data/activity.zip", method = "curl")
# date.Downloaded <- Sys.Date()



## Loading and preprocessing the data

# file already exists in the repository. Just need to unzip it
unzip("activity.zip")

# read the csv file
activity <- read.csv("activity.csv", header = TRUE, na.strings = "", stringsAsFactors = FALSE)

# Pre=process the data
activity$steps <- as.numeric(activity$steps)     # convert to numeric
activity$date <- as.Date(activity$date)     # convert to date
activity$wday <- weekdays(activity$date)     # get the weekday

# determine if weekend. Since only Saturday and Sunday, use grepl. TRUE if weekend.
activity$wend <- grepl("^S", activity$wday)





## What is mean total number of steps taken per day?

### 1. Calculate the total number of steps taken per day

library("plyr")

######### actStatsDate <- ddply(activity, .(date), summarise, sum = sum(steps, na.rm = TRUE), mean = mean(steps, na.rm = TRUE), median = median(steps, na.rm = TRUE))

actStatsDate <- ddply(activity, .(date), summarise, TotalSteps = sum(steps, na.rm = TRUE))
                     


### 2. Make a histogram of the total number of steps taken each day

library("ggplot2")
g <- ggplot(actStatsDate, aes(TotalSteps))
g1 <- g + geom_histogram(color = "black", fill = "blue", binwidth = 1000)
g2 <- g1 + labs(title = "Histogram of the total number of steps taken each day") + 
  theme(plot.title = element_text(hjust = 0.5)) + labs(x = "Steps per Day", y = "Day Count") 
g3 <- g2 + scale_y_continuous(breaks=seq(0,10,2)) + scale_x_continuous(breaks = seq(0,22500, 2000))
print(g3)

# http://www.sthda.com/english/wiki/ggplot2-axis-ticks-a-guide-to-customize-tick-marks-and-labels



### 3. Calculate and report the mean and median of the totl number of steps taken per day

MeanStepsPerDay <- round(mean(actStatsDate$TotalSteps, na.rm = TRUE), digits = 0)
MeanStepsPerDay
# Use R Markdown in-line calculations to display

MedianStepsPerDay <- round(median(actStatsDate$TotalSteps, na.rm = TRUE), digits = 0)
MedianStepsPerDay
# Use R Markdown in-line calculations to display



## What is the average daily activity pattern?

#################### gs <- ggplot(actStatsDate, aes(date))
#################### gs1 <- gs + geom_line(aes(y = sum, color = "sum")) + 
#  geom_line(aes(y = mean, color = "mean")) +
#  geom_line(aes(y = median, color = "median"))
  
#################### gs1 <- gs + geom_line(aes(y = mean, color = "mean")) +
#  geom_line(aes(y = median, color = "median"))
#####################



### 1. Make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

actStatsInt <- ddply(activity, .(interval), summarise, IntervalMean = mean(steps, na.rm = TRUE))

gt <- ggplot(actStatsInt, aes(interval, IntervalMean)) 
gt1 <- gt + geom_line(color = "red") 
gt2 <- gt1 + scale_x_continuous(breaks=c(0,300, 600, 900, 1200, 1500, 1800, 2100, 2400), 
                                labels = c("00:00","03:00", "06:00", "09:00", "12:00", "15:00", "18:00", "21:00", "24:00"))
gt3 <- gt2 + labs(title = "Time Series Plot of the Average Number of Steps Taken") + 
  theme(plot.title = element_text(hjust = 0.5)) + labs(x = "Time of Day", y = "Number of Steps Taken, Mean") 
print(gt3)

# http://www.sthda.com/english/wiki/ggplot2-axis-ticks-a-guide-to-customize-tick-marks-and-labels


### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

library("dplyr")
MaxInterval <- filter(actStatsInt, IntervalMean == max(IntervalMean))
MaxInterval$interval
# Use R Markdown in-line calculations to display
round(MaxInterval$IntervalMean, digits = 0)



## Imputing missing values


### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with `NA`s)

colSums(is.na(activity))
# Results are:
# steps         date     interval         wday         wend IntervalMean 
# 2304            0            0            0            0            0 

colSums(is.na(activity))[1]
# Use R Markdown in-line calculations to display


### 2. Devise a strategy for filling in all of the missing values in the dataset. 
#### The strategy does not need to be sophisticated. 
#### For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

# Strategy for filling in all of the missing values in the dataset:
# I decided to use the mean for that 5-minute interval, for the 2-month period. 
# This is a better representation than the mean for that day in my opinion since the steps vary per time of day.
# Since the mean per 5-minute time interval have already been calculated, I just have to integrate thse via the plyr::join function

activity <- join(activity, actStatsInt, by = "interval")  

for (i in 1:length(activity$steps)) {
  if (is.na(activity$steps[i])) {activity$steps[i] <- activity$IntervalMean[i]}
}


### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

NewActivity <- select(activity, steps:interval)


### 4. Make a histogram of the total number of steps taken each day and 

NewActStatsDate <- ddply(NewActivity, .(date), summarise, TotalSteps = sum(steps, na.rm = TRUE))

g <- ggplot(NewActStatsDate, aes(TotalSteps))
g1 <- g + geom_histogram(color = "black", fill = "blue", binwidth = 1000)
g2 <- g1 + labs(title = "New Histogram of the total number of steps taken each day") + 
  theme(plot.title = element_text(hjust = 0.5)) + labs(x = "Steps per Day (imputed)", y = "Day Count") 
g3 <- g2 + scale_y_continuous(breaks=seq(0,16,2)) + scale_x_continuous(breaks = seq(0,22500, 2000))
print(g3)

### Calculate and report the **mean** and **median** total number of steps taken per day. 

NewMeanStepsPerDay <- round(mean(NewActStatsDate$TotalSteps, na.rm = TRUE), digits = 0)
NewMeanStepsPerDay
# Use R Markdown in-line calculations to display

NewMedianStepsPerDay <- round(median(NewActStatsDate$TotalSteps, na.rm = TRUE), digits = 0)
NewMedianStepsPerDay
# Use R Markdown in-line calculations to display

### Do these values differ from the estimates from the first part of the assignment? 

# To summarize:
#          Original      With Imputed
# Mean       9354         10766  
# Median    10395         10766

# Yes, the values differ a bit. The mean and median are higher with imputed values

### What is the impact of imputing missing data on the estimates of the total daily number of steps?

# The impact of imputting missing data is to increase the mean and median slightly. These are expected since mean values are replaced.





## Are there differences in activity patterns between weekdays and weekends?

# For this part the `weekdays()` function may be of some help here. Use the dataset with the filled-in missing values for this part.

### 1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" 
### indicating whether a given date is a weekday or weekend day.

# activity$wend <- grepl("^S", activity$wday)

NewActivity$wday <- weekdays(NewActivity$date)     # get the weekday
NewActivity$NoWork <- as.factor(grepl("^S", NewActivity$wday))
# NewActivity$NoWork
levels(NewActivity$NoWork) <- c("weekday", "weekend")

### 2. Make a panel plot containing a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) 
### and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 
### The plot should look something like the following, which was created using **simulated data**:

WeekdayData <- subset(NewActivity, NoWork == "weekday")
WeekendData <- subset(NewActivity, NoWork == "weekend")

# par(mfrow = c(2,1), mar = c(4,4,4,3), oma = c(0,0,2,0))
WDayStatsInt <- ddply(WeekdayData, .(interval), summarise, IntervalMean = mean(steps, na.rm = TRUE))

gd <- ggplot(WDayStatsInt, aes(interval, IntervalMean)) 
gd1 <- gd + geom_line(color = "red") 
gd2 <- gd1 + scale_x_continuous(breaks=c(0,300, 600, 900, 1200, 1500, 1800, 2100, 2400), 
                                labels = c("00:00","03:00", "06:00", "09:00", "12:00", "15:00", "18:00", "21:00", "24:00"))
gd3 <- gd2 + labs(title = "Time Series Plot of the Average Number of Steps\n Taken during WeekDays") + 
  theme(plot.title = element_text(hjust = 0.5)) + labs(x = "Time of Day", y = "Number of Steps Taken\n during Weekdays, Mean") 
# print(gd3)

WEndStatsInt <- ddply(WeekendData, .(interval), summarise, IntervalMean = mean(steps, na.rm = TRUE))

ge <- ggplot(WEndStatsInt, aes(interval, IntervalMean)) 
ge1 <- ge + geom_line(color = "blue") 
ge2 <- ge1 + scale_x_continuous(breaks=c(0,300, 600, 900, 1200, 1500, 1800, 2100, 2400), 
                                labels = c("00:00","03:00", "06:00", "09:00", "12:00", "15:00", "18:00", "21:00", "24:00"))
ge3 <- ge2 + labs(title = "Time Series Plot of the Average Number of Steps\n Taken during WeekEnds") + 
  theme(plot.title = element_text(hjust = 0.5)) + labs(x = "Time of Day", y = "Number of Steps Taken\n during Weekends, Mean") 
# print(ge3)

library(gridExtra)
grid.arrange(gd3, ge3, nrow = 2, ncol = 1)

NewActStatsDate1 <- ddply(NewActivity, .(date), summarise, TotalSteps = sum(steps, na.rm = TRUE))
NewActStatsDate1$wday <- weekdays(NewActStatsDate1$date) 
NewActStatsPerDay <- ddply(NewActStatsDate1, .(wday), summarise, MeanStepsPerDay = mean(TotalSteps, na.rm = TRUE))
NewActStatsPerDay$wday <- factor(NewActStatsPerDay$wday, levels= c("Sunday", "Monday", 
                                         "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
NewActStatsPerDay <- arrange(NewActStatsPerDay, wday)
NewActStatsPerDay

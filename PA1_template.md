---
title: "Coursera Reproducible Research_PeerAssesment1"
author: "Iwona Grad"
date: "5 december 2016"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
##This document contains the assignment for the 1st Peer Assesment Project in Reproducible Research course in  Coursera Data Science Specialization.

###Project Description from the course website:

####*"This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day."*

It is always good to start work from cleaning up your working environment:
```{r clean}
rm(list = ls())
Sys.setlocale("LC_TIME","English")
```
&nbsp;

### Data loading and pre-processing

First the data should be downloaded from the address given in the course website (unless it is already downloaded) : 
```{r data downloading}

if(!file.exists("getdata-projectfiles-UCI HAR Dataset.zip")) {
  temp <- tempfile()
  download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)
  unzip(temp)
  unlink(temp)
}
```

After downloading, the data should be read into the R environment and we know that the dataset is stored in a comma-separated-value (CSV) file so read.csv is perfect. There should be a total of 17,568 observations in this dataset.:

```{r}
data <- read.csv("activity.csv")
```

The data should contain variables:

steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)

date: The date on which the measurement was taken in YYYY-MM-DD format

interval: Identifier for the 5-minute interval in which measurement was taken

Let's examine if the data looks correct:

```{r data structure checkup}
str(data)
```

As we see, there are missing values in variable **steps** and variable **date** has a wrong format, let's correct it:

```{r data pre-processing}
data$date <- as.Date(data$date) #converting int to date type
datanoNA<-na.omit(data) #omitting NA values
```

Let's examine the data again:

```{r data structure checkup2}
str(datanoNA)
```
&nbsp;

###What is mean total number of steps taken per day?
#### *1.Make a histogram of the total number of steps taken each day*

Calculation of sum of steps for each day:

```{r sum of steps each day}
StepsPerDay <- aggregate(steps ~ date, datanoNA, sum)
```
&nbsp;

Plotting the sum of steps per day:

```{r}
plot(StepsPerDay$date, StepsPerDay$steps, type = "h", main = paste("Total Steps Per Day"), col="grey", lwd = 8, xlab="Day", ylab="Number of steps")
```
&nbsp;

#### *2.Calculate and report the mean and median total number of steps taken per day*

Calculating mean and median of steps overall

```{r mean and median}
meansteps <- mean(StepsPerDay$steps)
mediansteps <- median(StepsPerDay$steps)
meansteps
mediansteps
```
&nbsp;

###What is the average daily activity pattern?
#### *1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)*

Calculation of mean of steps for each interval integrated across days

```{r mean number of steps in each interval}
StepsPerInterval <- aggregate(steps ~ interval, datanoNA, mean)
```

Plotting the sum of steps per interval

```{r plot the above}
plot(StepsPerInterval$interval,StepsPerInterval$steps, type="l", xlab="Interval", ylab="Number of Steps",main="Average Number of Steps per Interval")
```
&nbsp;

#### *2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?*

Finding the interval with maximum value of steps

```{r}
maxStepsInterval <- StepsPerInterval[which.max(StepsPerInterval$steps),1]
maxStepsInterval
```
&nbsp;

### Imputing missing values

#### *1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)*

```{r}
length(which(is.na(data$steps)))
```
&nbsp;

#### *2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.*

**Values of steps differ a lot depending on the interval, therefore best NA replacement seems to be mean of the steps in a given 5min interval, which were calculated in StepsPerInterval**

```{r replacing missing values}
dataNEW<-merge(data, StepsPerInterval, by = "interval") #merging the main data with mean for given interval
indices_of_NA_Values<- which(is.na(dataNEW$steps.x)) #finding which rows contain NA values
dataNEW$steps.x[indices_of_NA_Values] <- dataNEW$steps.y[indices_of_NA_Values] #replacing those values with mean
dataNEW <- dataNEW[order(dataNEW$date),] #sorting to the date as in the original
```
&nbsp;

#### *3. Create a new dataset that is equal to the original dataset but with the missing data filled in.*

```{r original with issing values replaced}
dataReplaced<-data
dataReplaced$steps<-dataNEW$steps.x

```
&nbsp;

#### *4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?*

Calculation of sum of steps for each day for the new dataset with replaced missing values:

```{r sum of steps each day for dataNEW}
StepsPerDayNEW <- aggregate(steps.x ~ date, dataNEW, sum)
```
&nbsp;

Plotting the sum of steps per day with replaced missing values:

```{r plot for dataNEW}
plot(StepsPerDayNEW$date, StepsPerDayNEW$steps.x, type = "h", main = paste("Total Steps Each Day"), col="grey", lwd = 8, xlab="Day", ylab="Number of steps")
```
&nbsp;

Calculating mean and median of steps per day with replaced missing values:

```{r new mean and median}
meanstepsNEW <- mean(StepsPerDayNEW$steps.x)
medianstepsNEW <- median(StepsPerDayNEW$steps.x)
meanstepsNEW
medianstepsNEW
```
&nbsp;

**The new mean value is identical to the old one, as expected since mean was used for missing values. Median became identical to mean and the data did not change much since median and mean were almost ideantical before imputing the missing values.**
&nbsp;

### Are there differences in activity patterns between weekdays and weekends?

Creating a function to sort days into weekdays and weekends and adding a variable of day type to the dataframe:

```{r}
day_of_week <- function(date) {
    if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
        "weekend"
    } else {
        "weekday"
    }
}
dataReplaced$day <- as.factor(sapply(dataReplaced$date, day_of_week))
```
&nbsp;

Comparing graphically the activity during weekdays and weekends:

```{r}
dataReplacedWeekday <- dataReplaced[which(dataReplaced$day == "weekday"),]
dataReplacedWeekend <- dataReplaced[which(dataReplaced$day == "weekend"),]                                   

StepsPerIntervalWeekday <- aggregate(steps ~ interval, dataReplacedWeekday, mean)
StepsPerIntervalWeekend <- aggregate(steps ~ interval, dataReplacedWeekend, mean)

par(mfrow=c(2, 1))
plot(StepsPerIntervalWeekday$interval,StepsPerIntervalWeekday$steps, type="l", xlab="Interval", ylab="Number of Steps",main="Average Number of Steps per Interval during Weekdays")
plot(StepsPerIntervalWeekend$interval,StepsPerIntervalWeekend$steps, type="l", xlab="Interval", ylab="Number of Steps",main="Average Number of Steps per Interval during Weekends")

```
&nbsp;


**As seen from the plots the activity during weekdays and weekends differs, as expected. Overall during the weekends the activity starts later, is lower but more persistant throu out all time intervals. It aso finishes slightly later than during the weekdays.**


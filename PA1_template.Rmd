---
title: "Reproducible Reserch Assignment 1"
output: html_document
---


```{r global_options, include=TRUE}
knitr::opts_chunk$set(fig.path='figure/',
                      echo=TRUE, warning=FALSE, message=FALSE)
```

```{r}
activity <- read.csv("activity.csv")
activity <- na.omit(activity)
summary(activity)
```
What is mean total number of steps taken per day?

Making a histogram of the total number of steps taken each day
```{r}
library(dplyr)
activity$date <- as.Date(activity$date)
total_steps_day <- activity %>% group_by(date) %>% summarize(sum(steps))
hist(total_steps_day$`sum(steps)`, col="red", main="Histogram of Total Daily Steps", xlab="Total Daily Steps",ylab = "Count")
```

3) Mean and median number of steps taken each day
Calculate and report the mean and median of the total number of steps taken per day

```{r echo=FALSE}
paste("Mean number of steps taken each day is", mean(total_steps_day$`sum(steps)`))
paste("Median number of steps taken each day is", median(total_steps_day$`sum(steps)`))
```

**The Mean number of steps taken each day is approximately 10766 steps.**

**The Median number of steps taken each day is approximately 10765 steps.**



What is the average daily activity pattern?

4) Time series plot of the average number of steps taken
Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken averaged across all days (y-axis)

Grouping data by date and interval and average number of steps
```{r}
Avg_steps_day_interval <- activity %>% group_by(date,interval) %>% summarize(mean(steps))
```

Grouping data by date and average number of steps
```{r}
Avg_steps_day <- activity %>% group_by(date) %>% summarize(mean(steps))
```

Grouping data by interval and average of steps
```{r}
Avg_steps_interval <- aggregate(steps ~ interval,FUN=mean,data = activity)

plot(Avg_steps_interval$interval, Avg_steps_interval$steps, type="l", ylab=" Average of Number of Steps", xlab="Interval",main= "Average number of steps versus the 5-minute intervals across all days")
```


5) The 5-minute interval that, on average, contains the maximum number of steps Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

Finding the time block with maximum steps
```{r}
max_interval <- Avg_steps_interval[Avg_steps_interval$steps == max(Avg_steps_interval$steps),]
```
 
**The time interval with maximum number of steps which is approximately 206 is around 8:35 am.**

Imputing missing values
6) Code to describe and show a strategy for imputing missing data
Imputing missing values
Calculate and report the total number of missing values in the dataset 
```{r}
activity <- read.csv("activity.csv")
summary(activity) # 2304  NA's in steps
```

Using the mean steps by interval by day as strategy for filling in all of the missing values in the dataset.  

Average of steps aggregated by interval
```{r}
Avg_steps_interval <- aggregate(steps ~ interval,FUN=mean,data = activity)
colnames(Avg_steps_interval) <- c("interval", "Avg steps")
```


Create a new dataset that is equal to the original dataset but with the missing data filled in.
merging dataframes - activity and 'average of steps aggregated by interval' on interval
```{r}
activity_merged_interval <- merge(activity,Avg_steps_interval, by="interval", all.x=TRUE)
```

Replacing all NAs in steps with average number of steps by interval by date.
Verified no NAs in steps in above dataset after performing step below.
```{r}
activity_merged_interval$steps[is.na(activity_merged_interval$steps)] <- activity_merged_interval$`Avg steps`[is.na(activity_merged_interval$steps)]
summary(activity_merged_interval) 
```



7) Histogram of the total number of steps taken each day after missing values are imputed

```{r}
total_steps_day_newdata <- activity_merged_interval %>% group_by(date) %>% summarize(sum(steps))
hist(total_steps_day_newdata$`sum(steps)`, col="blue", main="Histogram of Daily Steps with NAs/Missing Values", xlab="Steps per day")
```


Mean and median number of steps taken each day
```{r}
Avg_steps_day_newdata <- activity_merged_interval %>% group_by(date) %>% summarize(mean(steps))
Median_steps_day_newdata <- activity_merged_interval %>% group_by(date) %>% summarize(median(steps))
```


8) Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
Are there differences in activity patterns between weekdays and weekends?

creating a new column to mark day of week as per day
creating new column to mark day of weeek as weekday or weekend
```{r}
library(lubridate)
activity_merged_interval$Day = wday(activity_merged_interval$date, label = TRUE)
activity_merged_interval$DayType <- "Weekday"
activity_merged_interval$DayType[which(activity_merged_interval$Day %in% c("Sat","Sun"))] <- "Weekend"


summary(activity_merged_interval)
Avg_steps_daytype <- activity_merged_interval %>% group_by(DayType) %>% summarize(mean(steps))

Avg_steps_daytype_interval <- aggregate(steps ~ DayType + interval,FUN=mean,data = activity_merged_interval)
```


splitting this dataframe into two separate dataframes separated by DayType= Weekday/Weekend
```{r}
weekday_df <-subset(Avg_steps_daytype_interval,Avg_steps_daytype_interval$DayType=="Weekday")
weekend_df <-subset(Avg_steps_daytype_interval,Avg_steps_daytype_interval$DayType=="Weekend")
```


Making the plot
```{r}
par(mfrow = c(2, 1)) 
plot(weekend_df$interval,weekend_df$steps, type="l", ylab="Number of Steps", xlab="Interval", col="blue", main="Weekend")
plot(weekday_df$interval,weekday_df$steps, type="l", ylab="Number of Steps", xlab="Interval", col="blue",main="Weekday")
```

**So based on the plots above we can infer that there are differences in activity patterns between weekdays and weekends.**

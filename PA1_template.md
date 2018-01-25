---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
Show any code that is needed to

Load the data (i.e. read.csv())
Process/transform the data (if necessary) into a format suitable for your analysis


```r
# Load data from CSV zipped file
activity <- read.csv(unz("activity.zip","activity.csv"), header = TRUE, sep = ",")
# Data with NAs removed
activityNoNAs <- subset(activity, is.na(activity$steps) == FALSE)
```


## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day

```r
stepsPerDay <- aggregate(activityNoNAs$steps, list(day = activityNoNAs$date), sum)
```

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

```r
hist(stepsPerDay$x, main = "Histogram of total number of steps taken each day", ylab="Number of days", xlab="Steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day

```r
mean(stepsPerDay$x)
```

```
## [1] 10766.19
```

```r
median(stepsPerDay$x)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
avgStepsPerInterval <- aggregate(steps ~ interval, activityNoNAs, mean)
plot(avgStepsPerInterval$interval, avgStepsPerInterval$steps, type="l",main="Average number of steps by daily interval", xlab="Interval", ylab="Average number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
avgStepsPerInterval[which.max(avgStepsPerInterval$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
# Strategy is to replace NAs with mean number of steps for that interval for all days
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
# Strategy is to replace NAs with mean number of steps for that interval for all days
activityNAs <- is.na(activity)
activityFactorizedInterval <- activity
activityFactorizedInterval$interval <- factor(activityFactorizedInterval$interval)
avgStepsPerInterval$interval <- factor(avgStepsPerInterval$interval)

for(i in 1:nrow(activityFactorizedInterval)){
  if(is.na(activityFactorizedInterval$steps[i])){
    activityFactorizedInterval$steps[i] <- avgStepsPerInterval$steps[which(avgStepsPerInterval$interval == activityFactorizedInterval$interval[i])]
  }
}
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
stepsPerDayImputed <- aggregate(steps ~ date, activityFactorizedInterval, sum)
hist(stepsPerDayImputed$steps, main = "Histogram of IMPUTED total number of steps taken each day", ylab="Number of days", xlab="Steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
mean(stepsPerDayImputed$steps)
```

```
## [1] 10766.19
```

```r
median(stepsPerDayImputed$steps)
```

```
## [1] 10766.19
```

```r
# Note that mean and median values have NOT changed after imputing values instead of NULLs
```

## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
activityFactorizedInterval$weekday <- weekdays(as.Date(stepsPerDayImputed$date))
activityFactorizedInterval$weekday <- ifelse(activityFactorizedInterval$weekday %in% c("Saturday", "Sunday"), "weekend", "weekday")
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
library(lattice)
library(plyr)
average <- ddply(activityFactorizedInterval, ~interval + weekday, summarise, steps=mean(steps))
average$interval <- as.numeric(as.character(average$interval))
```

```r
xyplot(steps ~ interval | weekday, average, type = "l", layout = c(1, 2), xlab = "Interval", ylab = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

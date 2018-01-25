activity <- read.csv(unz("activity.zip","activity.csv"), header = TRUE, sep = ",")
activityNoNAs <- subset(activity, is.na(activity$steps) == FALSE)
stepsPerDay <- aggregate(activityNoNAs$steps, list(day = activityNoNAs$date), sum)
hist(stepsPerDay$x, main = "Histogram of total number of steps taken each day", ylab="Number of days", xlab="Steps per day")
mean(stepsPerDay$x)
median(stepsPerDay$x)
avgStepsPerInterval <- aggregate(steps ~ interval, activityNoNAs, mean)
plot(avgStepsPerInterval$interval, avgStepsPerInterval$steps, type="l",main="Average number of steps by daily interval", xlab="Interval", ylab="Average number of steps")
avgStepsPerInterval[which.max(avgStepsPerInterval$steps),]
sum(is.na(activity$steps))
# Strategy is to replace NAs with mean number of steps for that interval
activityNAs <- is.na(activity)
activityFactorizedInterval <- activity
activityFactorizedInterval$interval <- factor(activityFactorizedInterval$interval)
avgStepsPerInterval$interval <- factor(avgStepsPerInterval$interval)

for(i in 1:nrow(activityFactorizedInterval)){
  if(is.na(activityFactorizedInterval$steps[i])){
    activityFactorizedInterval$steps[i] <- avgStepsPerInterval$steps[which(avgStepsPerInterval$interval == activityFactorizedInterval$interval[i])]
  }
}
stepsPerDayImputed <- aggregate(steps ~ date, activityFactorizedInterval, sum)
hist(stepsPerDayImputed$steps, main = "Histogram of IMPUTED total number of steps taken each day", ylab="Number of days", xlab="Steps per day")
mean(stepsPerDayImputed$steps)
median(stepsPerDayImputed$steps)
activityFactorizedInterval$weekday <- weekdays(as.Date(stepsPerDayImputed$date))
activityFactorizedInterval$weekday <- ifelse(activityFactorizedInterval$weekday %in% c("Saturday", "Sunday"), "weekend", "weekday")
library(lattice)
library(plyr)
average <- ddply(activityFactorizedInterval, ~interval + weekday, summarise, steps=mean(steps))
average$interval <- as.numeric(as.character(average$interval))
xyplot(steps ~ interval | weekday, average, type = "l", layout = c(1, 2), xlab = "Interval", ylab = "Number of steps")





# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
activityData <- read.csv(unz("activity.zip", "activity.csv"), header=T, quote="\"", sep=",")
```

## What is mean total number of steps taken per day?


```r
activityData$date <- as.Date(activityData$date)
activityData <- na.omit(activityData)
numberOfStepsTakenDaily <- rowsum(activityData$steps, format(activityData$date, '%Y-%m-%d'))
numberOfStepsTakenDaily <- data.frame(numberOfStepsTakenDaily)
names(numberOfStepsTakenDaily) <- ("steps")
```


## What is the average daily activity pattern?

![](PA1_template_files/figure-html/histogram-1.png) 

```
##     interval     mean
## 104      835 206.1698
```

## Imputing missing values


```r
# load the data
activityData <- read.csv(unz("activity.zip", "activity.csv"), header=T, quote="\"", sep=",")
activityData$date <- as.Date(activityData$date)
activityDataWithoutNAs <- activityData
activityDataWithoutNAs[is.na(activityDataWithoutNAs$steps), "steps"] <- 0
stepsPerDayNoNAs <- aggregate(steps ~ date, activityDataWithoutNAs, sum)
```

## Are there differences in activity patterns between weekdays and weekends?


```r
weekdays <- c("Monday","Tuesday","Wednesday","Thursday","Friday")
activityData$dow = as.factor(ifelse(is.element(weekdays(as.Date(activityData$date)),weekdays), "Weekday", "Weekend"))
steps_by_interval_i <- aggregate(steps ~ interval + dow, activityData, mean)

# Load the lattice graphical library
library(lattice)

# Draw the Time Series Plot
xyplot(steps_by_interval_i$steps ~ steps_by_interval_i$interval|steps_by_interval_i$dow, 
       main="Average Steps per Day by Interval",
       xlab="Interval", 
       ylab="Steps",
       layout=c(1,2), 
       type="l", 
       col = "purple")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

There are slight differences in the patterns between weekdays and weekends.

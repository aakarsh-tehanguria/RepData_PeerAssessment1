---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


```r
# Load dataset
activity <- read.csv("activity.csv")

# Convert date to Date format
activity$date <- as.Date(activity$date)

# Check structure
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
summary(activity)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```

```r
## What is mean total number of steps taken per day?
```

```r
steps_per_day <- aggregate(steps ~ date, data = activity, sum, na.rm = TRUE)

head(steps_per_day)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

```r
### Histogram
```

```r
hist(steps_per_day$steps,
     main="Total Steps Taken Per Day",
     xlab="Steps per Day",
     col="skyblue",
     breaks=20)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
### Mean and Median
```

```r
mean_steps <- mean(steps_per_day$steps)
median_steps <- median(steps_per_day$steps)

mean_steps
```

```
## [1] 10766.19
```

```r
median_steps
```

```
## [1] 10765
```

```r
## What is the average daily activity pattern?

### Average steps per interval
```

```r
avg_steps_interval <- aggregate(steps ~ interval, data = activity, mean, na.rm = TRUE)

head(avg_steps_interval)
```

```
##   interval     steps
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396
```

```r
### Time Series Plot
```

```r
plot(avg_steps_interval$interval,
     avg_steps_interval$steps,
     type="l",
     col="blue",
     xlab="5-minute Interval",
     ylab="Average Steps",
     main="Average Daily Activity Pattern")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
### Interval with Maximum Steps
```

```r
max_interval <- avg_steps_interval[which.max(avg_steps_interval$steps), ]
max_interval
```

```
##     interval    steps
## 104      835 206.1698
```

```r
## Imputing missing values
```

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

```r
### Strategy: Replace NA with Mean of that Interval
```

```r
# mean steps per interval
interval_means <- aggregate(steps ~ interval, data=activity, mean, na.rm=TRUE)

# copy dataset
activity_filled <- activity

# fill NA
for(i in 1:nrow(activity_filled)){
  if(is.na(activity_filled$steps[i])){
    interval <- activity_filled$interval[i]
    activity_filled$steps[i] <- interval_means$steps[interval_means$interval == interval]
  }
}
```

```r
steps_per_day_filled <- aggregate(steps ~ date, data = activity_filled, sum)

hist(steps_per_day_filled$steps,
     main="Total Steps Per Day (After Imputation)",
     xlab="Steps per Day",
     col="lightgreen",
     breaks=20)
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
### Mean and Median After Imputation
```

```r
mean(steps_per_day_filled$steps)
```

```
## [1] 10766.19
```

```r
median(steps_per_day_filled$steps)
```

```
## [1] 10766.19
```

```r
## Are there differences in activity patterns between weekdays and weekends?
### Create Factor Variable
```

```r
activity_filled$day_type <- ifelse(weekdays(activity_filled$date) %in% 
                                   c("Saturday","Sunday"),
                                   "weekend","weekday")

activity_filled$day_type <- as.factor(activity_filled$day_type)



### Average Steps by Interval and Day Type
```

```r
library(lattice)

avg_steps_daytype <- aggregate(steps ~ interval + day_type,
                               data = activity_filled,
                               mean)



### Panel Plot
```

```r
xyplot(steps ~ interval | day_type,
       data = avg_steps_daytype,
       type="l",
       layout=c(1,2),
       xlab="Interval",
       ylab="Number of Steps",
       main="Average Steps: Weekday vs Weekend")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

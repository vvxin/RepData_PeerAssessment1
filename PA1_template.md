# Reproducible Research: Peer Assessment 1
WANG Xin
Feb,7 2016


#### Loading and preprocessing the data

##### 1. Load the data (i.e. `read.csv()`)

```r
activity <- read.csv("activity.csv")
```

##### 2. Process/transform the data (if necessary) into a format suitable for your analysis

```r
activity$date <- as.Date(as.character(activity$date),"%Y-%m-%d")
```

---

#### What is the mean total number of steps taken per day?

##### 1. Calculate the total number of steps taken per day

```r
stepsByDay <- tapply(activity$steps, activity$date, sum, na.rm=TRUE)
```

##### 2. Make a histogram of the total number of steps taken each day

```r
library(ggplot2)
qplot(stepsByDay, binwidth = 500, xlab='Total steps per day') + 
        theme_bw()
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)

##### 3. Calculate and report the **mean** and **median** total number of steps taken per day

```r
stepsMean <- mean(stepsByDay)
stepsMedian <- median(stepsByDay)
```
Mean:   9354.2295082   
Median: 10395
---

#### What is the average daily activity pattern? 

##### 1. Make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
ave_steps <- aggregate(steps ~ interval, activity, mean)
```

```r
ggplot(ave_steps, aes(x = interval, y = steps)) + 
        geom_line() +
        theme_bw() +
        labs(title = "Time Series of Average Steps Taken", 
             y = "Average Number of Steps Taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)

##### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
ave_steps$interval[which.max(ave_steps$steps)]
```

```
## [1] 835
```

---

#### Imputing missing values

##### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

##### 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.  

The missing values are filling with the mean for that 5-minutes interval from total dataset. 

##### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
filled_activity <- activity
for (i in 1:nrow(activity)) {
        if(is.na(activity[i,]$steps)) {
                filled_activity[i,]$steps <- ave_steps[ave_steps$interval == activity[i,]$interval,]$steps
  }
}
```

##### 4. Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
stepsByDay_filled <- tapply(filled_activity$steps, filled_activity$date, sum, na.rm=TRUE)
```

```r
qplot(stepsByDay_filled, binwidth = 500, 
      xlab='Total steps per day (filled missing values)') + 
        theme_bw()
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)

```r
stepsMeanf <- mean(stepsByDay_filled)
stepsMedianf <- median(stepsByDay_filled)
```
Mean with filled data:   1.0766189\times 10^{4}  
Median with filled data: 1.0766189\times 10^{4}

By filling missing values with that 5-minutes mean, the steps are more normal distributed.

---

#### Are there differences in activity patterns between weekdays and weekends?

##### 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
library(lubridate)
filled_activity$week <- wday(filled_activity$date)
for (i in 1:nrow(filled_activity)) {
        if (filled_activity$week[i] %in% c(1,6)) {
                filled_activity$week[i] <- "weekend"
        } else  {
                filled_activity$week[i] <- "weekday"
        }
}
```

##### 2. Make a panel plot containing a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
ave_steps2 <- aggregate(steps ~ interval + week, filled_activity, mean)
```

```r
library(lattice)
xyplot(steps ~ interval | factor(week),
       data = ave_steps2,
       xlab = "Interval",
       ylab = "Number of steps",
       layout = c(1, 2),
       type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-16-1.png)

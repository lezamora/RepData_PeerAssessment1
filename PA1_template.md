---
title: "Reproducible Research: Peer Assessment 1"
author: 'Lucas Zamora'
output: 
  html_document:
    keep_md: true
---




## Import libreries

```r
library(dplyr)
library(ggplot2)
```


## Loading and preprocessing the data

```r
# Reading the data
unzip("activity.zip")
activity <- read.csv("activity.csv")
```


```r
# Converting date string to date format
activity$date <- as.Date(activity$date)
```


## What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day.
2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day.

```r
steps_by_day <- activity %>% 
                       filter(!is.na(activity)) %>%
                       group_by(date) %>%
                       summarise(total_steps = sum(steps))
ggplot(steps_by_day, aes(x=total_steps)) + 
        geom_histogram() +
        ggtitle("Histogram of the total number of steps taken each day") +
        xlab("Number of steps") +
        ylab("")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day.

```r
print(paste0("Mean: ", mean(steps_by_day$total_steps, rm.na = TRUE)))
```

```
## [1] "Mean: 10766.1886792453"
```

```r
print(paste0("Median: ", median(steps_by_day$total_steps, rm.na = TRUE)))
```

```
## [1] "Median: 10765"
```


## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = 'l') of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).

```r
intervals_mean <- steps_by_day <- activity %>% 
                       filter(!is.na(activity)) %>%
                       group_by(interval) %>%
                       summarise(mean_steps = mean(steps))

ggplot(data=intervals_mean, aes(x=interval, y=mean_steps)) +
        geom_line() +
        ggtitle("Average daily activity pattern") +
        xlab("5-minute interval") +
        ylab("average number of steps taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

2. From there, we can find which 5-minute interval contains the maximum number of steps on average.

```r
max_interval <- filter(intervals_mean, mean_steps == max(mean_steps))
print(paste0("Max interval: ", max_interval$interval))
```

```
## [1] "Max interval: 835"
```

```r
print(paste0("Max interval: ", max_interval$mean_steps))
```

```
## [1] "Max interval: 206.169811320755"
```


## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs).

```r
print(paste0("Total number of missing values: ", sum(is.na(activity$steps))))
```

```
## [1] "Total number of missing values: 2304"
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
activity_fill <- activity %>%
                 mutate(steps = ifelse(is.na(steps), as.integer(filter(intervals_mean, interval == interval)$mean_steps), steps))
```

3. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
steps_by_day <- activity_fill %>% 
                       group_by(date) %>%
                       summarise(total_steps = sum(steps))
ggplot(steps_by_day, aes(x=total_steps)) + 
        geom_histogram() +
        ggtitle("Histogram of the total number of steps taken each day") +
        xlab("Number of steps") +
        ylab("")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
print(paste0("Mean: ", mean(steps_by_day$total_steps, rm.na = TRUE)))
```

```
## [1] "Mean: 10749.7704918033"
```

```r
print(paste0("Median: ", median(steps_by_day$total_steps, rm.na = TRUE)))
```

```
## [1] "Median: 10641"
```

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
get_day_type <- function(date) {
    day <- weekdays(date)
    if (day %in% c("lunes", "martes", "miércoles", "jueves", "viernes"))
        return("weekday")
    else if (day %in% c("sábado", "domingo"))
        return("weekend")
    else
        stop("Error with the day.")
      
}

activity_fill$day_type <- sapply(activity_fill$date, FUN=get_day_type)
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
activity_fill %>% group_by(interval, day_type) %>%
                  summarise(mean_steps = mean(steps)) %>%
                  ggplot(aes(interval, mean_steps)) + 
                  geom_line() + 
                          facet_grid(day_type ~ .) +
                          ggtitle("Activity patterns between weekdays and weekends") +
                          xlab("5-minute interval") +
                          ylab("Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

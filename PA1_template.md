---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---



## Loading and preprocessing the data


```r
#Unzip the file
unzip(zipfile = 'activity.zip', overwrite = TRUE)

# Read the file.
# Convert date to POSIX date, add an interval id.
# Make sure the data is ordered correctly
data <- read.csv('./activity.csv') %>%
  mutate(date = ymd(date),
         interval = as.POSIXct(today() + hm(paste(interval %/% 100, ':',
                                                  interval %% 100,
                                                  sep = '')))) %>%
  arrange(date, interval)
```

## What is mean total number of steps taken per day?


```r
stepsPerDay <- data %>%
  group_by(date) %>%
  summarize(steps = sum(steps, na.rm = TRUE))
summaryBeforeImpute <- stepsPerDay %>%
  summarise(mean = round(mean(steps, na.rm = TRUE), 2),
            median = round(median(steps, na.rm = TRUE), 2))
qplot(x = steps, data = stepsPerDay, geom = 'histogram')
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

The mean of the total number of steps taken per day is:
9354.23.  
The median of the total number of steps taken per day is:
10395.  

## What is the average daily activity pattern?


```r
stepsPerInterval <- data %>%
  group_by(interval) %>%
  summarize(steps = mean(steps, na.rm = TRUE))
qplot(x = interval, y = steps, data = stepsPerInterval, geom = 'line') +
  scale_x_datetime(breaks = '4 hours', labels = date_format('%H:%M'))
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

```r
stepsPerInterval <- stepsPerInterval %>%
  filter(steps == max(steps)) %>%
  mutate(steps = round(steps, 2))
```

The interval with the maximum number of steps is at
08:35 (206.17 steps on
average).

## Imputing missing values


```r
incompleteRowCount <- sum(!complete.cases(data))
```

The total number of rows with NAs is: 2304  

The imputing strategy consists in replacing the NAs with the mean of the number
of steps taken in the same interval on all days.


```r
incompleteRowCount <- sum(!complete.cases(data))
impute <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
newData <- data %>%
  ddply(~ interval, transform, steps = impute(steps)) %>%
  arrange(date, interval)
stepsPerDay <- newData %>%
  group_by(date) %>%
  summarize(steps = sum(steps, na.rm = TRUE))
summaryAfterImpute = stepsPerDay %>%
  summarise(mean = round(mean(steps, na.rm = TRUE), 2),
            median = round(median(steps, na.rm = TRUE), 2))
qplot(x = steps, data = stepsPerDay, geom = 'histogram')
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

The new mean of the total number of steps taken per day is: 
10766.19, compared to the previous 
9354.23, a difference of 
1411.96.  
The median of the total number of steps taken per day is: 
10766.19, compared to the previous 
10395, a difference of 
371.19.  
As a result of imputing the missing data, the new mean and median are now equal.

## Are there differences in activity patterns between weekdays and weekends?


```r
newData <- newData %>%
  mutate(daytype = factor(!(wday(date) %in% 2:6),
                          labels = c('weekday', 'weekend')))
qplot(x = interval, y = steps, data = newData, geom = 'line', stat = 'summary',
      fun.y = 'mean') +
  facet_wrap(~ daytype, ncol = 1) +
  scale_x_datetime(breaks = '4 hours', labels = date_format('%H:%M'))
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

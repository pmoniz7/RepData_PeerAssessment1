---
title: "Reproducible Research - Monitoring Human Activity"
author: "Paulo Moniz"
date: "25 de março de 2019"
output: 
  html_document: 
    keep_md: yes
---
#Introduction
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

Dataset: Activity monitoring data
The variables included in this dataset are:

steps: Number of steps taking in a 5-minute interval (missing values are coded as NA) 
date: The date on which the measurement was taken in YYYY-MM-DD format 
interval: Identifier for the 5-minute interval in which measurement was taken 
The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

#Loading and preprocessing the data





```r
require(ggplot2)
```

```
## Loading required package: ggplot2
```

```r
require(data.table)
```

```
## Loading required package: data.table
```

```
## Warning: package 'data.table' was built under R version 3.5.2
```

```r
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl, destfile = paste0(getwd(), '/repdata%2Fdata%2Factivity.zip'), method = "curl")
unzip("repdata%2Fdata%2Factivity.zip",exdir = "data")

activity <- read.csv(file="data/activity.csv", header=TRUE, sep=",")

head(activity,5)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
```


##What is mean total number of steps taken per day?


1.Calculate the total number of steps taken per day


```r
df_St_Per_Day <- aggregate(steps ~ date, data = activity, sum, na.action=na.pass)
 
head(df_St_Per_Day,10)
```

```
##          date steps
## 1  2012-10-01    NA
## 2  2012-10-02   126
## 3  2012-10-03 11352
## 4  2012-10-04 12116
## 5  2012-10-05 13294
## 6  2012-10-06 15420
## 7  2012-10-07 11015
## 8  2012-10-08    NA
## 9  2012-10-09 12811
## 10 2012-10-10  9900
```

If you do not understand the difference between a histogram and a barplot, research the difference between them. <br/>
Make a histogram of the total number of steps taken each day.


```r
ggplot(df_St_Per_Day, aes(x = steps)) +
    geom_histogram(fill = "coral1", binwidth = 1000) +
    labs(title = "Steps taken per day", x = "Steps", y = "Frequency")
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->
1.Calculate and report the mean and median of the total number of steps taken per day   


```r
c(Mean=mean(df_St_Per_Day$steps,na.rm = TRUE), Median=median(df_St_Per_Day$steps,na.rm = TRUE))
```

```
##     Mean   Median 
## 10766.19 10765.00
```


#What is the average daily activity pattern?


```r
df_St_Per_int <- aggregate(steps ~ interval, activity, mean)

ggplot(df_St_Per_int, aes(x = interval , y = steps)) + geom_line(color="coral1", size=1) + labs(title = "Avg. Daily Steps", x = "Interval", y = "Average steps per day") 
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
activity=as.data.table(activity)

activity[,.SD[which.max(steps)],]
```

```
##    steps       date interval
## 1:   806 2012-11-27      615
```

#Imputing missing values

1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
nrow(activity[is.na(steps),])
```

```
## [1] 2304
```

1.Devise a strategy for filling in all of the missing values in the dataset. 
The strategy does not need to be sophisticated. <br/> 
 For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
activity = transform(activity, steps = ifelse(is.na(steps),
           median(steps, na.rm=TRUE), steps))
```

At this stage there are no missing values "NAs" in the "steps" column

```r
sapply(activity, function(x) sum(is.na(x)))
```

```
##    steps     date interval 
##        0        0        0
```

Create a new dataset that is equal to the original dataset but with
the missing data filled in.

```r
df_agreg_step_day <- aggregate(steps ~ date, data = activity, sum)
```

1.Make a histogram of the total number of steps taken each day and calculate and report 
the mean and median total number of steps taken per day. <br/> 
Do these values differ from the estimates from the first part of the assignment?<br/>  What is the impact of imputing missing data on the estimates of the total daily number of steps? 


```r
ggplot(df_agreg_step_day, aes(x = steps)) + geom_histogram(fill = "coral1", binwidth = 1000) + labs(title = "Total steps per day.", x = "Steps", y = "Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

First Part (with na in Colunm "steps")

```r
c(Mean=mean(df_St_Per_Day$steps,na.rm = TRUE), Median=median(df_St_Per_Day$steps, na.rm = TRUE))
```

```
##     Mean   Median 
## 10766.19 10765.00
```
Second Part (There are no missing values "NAs")

```r
c(Mean=mean(df_agreg_step_day$steps), Median=median(df_agreg_step_day$steps))
```

```
##     Mean   Median 
##  9354.23 10395.00
```

#Are there differences in activity patterns between weekdays and weekends?

1.Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
Sys.setlocale("LC_ALL","English")
```

```
## [1] "LC_COLLATE=English_United States.1252;LC_CTYPE=English_United States.1252;LC_MONETARY=English_United States.1252;LC_NUMERIC=C;LC_TIME=English_United States.1252"
```

```r
activity <- data.table(activity)
activity[, date := as.POSIXct(date, format = "%Y-%m-%d")]
activity[,`Day of Week`:= weekdays(x = date)]
activity[grepl(pattern = "Monday|Tuesday|Wednesday|Thursday|Friday",
                     x = `Day of Week`), "weekday or weekend"] <- "weekday"
activity[grepl(pattern = "Saturday|Sunday", x = `Day of Week`),
                         "weekday or weekend"] <- "weekend"
activity[, `weekday or weekend` := as.factor(`weekday or weekend`)]
head(activity, 5)
```

```
##    steps       date interval Day of Week weekday or weekend
## 1:     0 2012-10-01        0      Monday            weekday
## 2:     0 2012-10-01        5      Monday            weekday
## 3:     0 2012-10-01       10      Monday            weekday
## 4:     0 2012-10-01       15      Monday            weekday
## 5:     0 2012-10-01       20      Monday            weekday
```

```r
df_St_Per_wDay <- aggregate(steps ~ (`weekday or weekend`) + interval, activity, mean)

ggplot(df_St_Per_wDay , aes(x = interval , y = steps, color=`weekday or weekend`)) + geom_line() + labs(title = "Average steps by Weekday/weekend", 
       x = "Interval", y = "No. of Steps") + facet_wrap(~`weekday or weekend` , ncol = 1, nrow=2)
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

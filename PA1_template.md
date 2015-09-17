# Rep Data Peer Assessment 1
Craig Martyn  
Wednesday, September 16, 2015  
###Loading and preprocessing the data

Show any code that is needed to

1. Load the data (i.e. read.csv())  
The following code loads the data and displays the first few rows to get a feel for how the data is structured:

```r
##Assumes the file activity.csv is loaded in the working directory
activity <- read.csv("activity.csv")
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

2. Process/transform the data (if necessary) into a format suitable for your analysis  
From reviewing the next steps in the analysis, we are going to need to look at the total number of steps by day and the mean steps by interval. The following code calculates total steps by day and mean steps by interval 


```r
library(reshape2)
```

```
## Warning: package 'reshape2' was built under R version 3.1.3
```

```r
## Sum steps by day, load into "stepsbyday" and display the first few rows
datemelt<-melt(activity, id="date", measure="steps")
stepsbyday<-dcast(datemelt, date ~ variable, sum)
head(stepsbyday)
```

```
##         date steps
## 1 2012-10-01    NA
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
## 6 2012-10-06 15420
```

```r
## Calculate the mean steps by interval, load into "stepsbyint" and display the first few rows
intmelt<-melt(activity, id="interval", measure="steps")
stepsbyint<-dcast(intmelt, interval ~ variable, mean, na.rm=TRUE)
head(stepsbyint)
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

###What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1. Make a histogram of the total number of steps taken each day  
The following histogram shows the total number of steps per day. I have interpreted the direction to ignore missing values to mean not to display days where all values are missing.


```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.1.3
```

```r
ggplot(stepsbyday, aes(x=steps, na.rm=TRUE))+geom_histogram()
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

2. Calculate and report the mean and median total number of steps taken per day


```r
mean(stepsbyday$steps, na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(stepsbyday$steps, na.rm=TRUE)
```

```
## [1] 10765
```

###What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
ggplot(stepsbyint, aes(x=interval, y=steps))+geom_line()
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 


2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
##Determine the interval with the maximum number of steps, then display the interval
maxsteps<-max(stepsbyint$steps)
maxstepint<-stepsbyint[(stepsbyint$steps == maxsteps),]
maxstepint$interval
```

```
## [1] 835
```

###Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.  
I followed one of the suggested approaches, and replaced missing values with the mean for that 5 minute interval, using the means calculated earlier.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
##Create new dataset called "modactivity"
modactivity<-activity
##Add a column with the mean number of steps for each interval
##Note that the cbind function recycles the mean steps by interval over all days
modactivity<-cbind(modactivity,stepsbyint$steps)
##If the step data is missing, use the means in the new column (4), otherwise use the original step data
modactivity$steps<-ifelse(is.na(modactivity$steps),modactivity[,4],modactivity$steps)
##Delete the column with the means
modactivity[,4]<-NULL
##Display the first few rows of the new data set
head(modactivity)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
##Sums up steps by day in the new data set and loads into "modstepsbyday"
moddatemelt<-melt(modactivity, id="date", measure="steps")
modstepsbyday<-dcast(moddatemelt, date ~ variable, sum)
##Display histogram, mean and median of new data set
ggplot(modstepsbyday, aes(x=steps))+geom_histogram()
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

```r
mean(modstepsbyday$steps)
```

```
## [1] 10766.19
```

```r
median(modstepsbyday$steps)
```

```
## [1] 10766.19
```

Compared to the first part of the assignment, the histograms are different in that the new histogram has a higher peak with 12 observations in the bin just over 10,000 steps. This peak corresponds to the days that were missing all data in the original data set, and the data for these days is now filled in with the mean number of steps for each interval.

The mean number of steps per day has not changed, whereas the median changed slightly from 10765 to 10766.19. The median now corresponds to the days that were missing all values in the original data set; now those days show as having the mean number of steps for each interval, which sums to 10766.19.

###Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
##Add new variable to distinguish weekdays from weekends and display the first few rows
modactivity$wday<-ifelse(weekdays(as.Date(modactivity$date)) %in% c("Saturday", "Sunday"),"weekend","weekday")
head(modactivity)
```

```
##       steps       date interval    wday
## 1 1.7169811 2012-10-01        0 weekday
## 2 0.3396226 2012-10-01        5 weekday
## 3 0.1320755 2012-10-01       10 weekday
## 4 0.1509434 2012-10-01       15 weekday
## 5 0.0754717 2012-10-01       20 weekday
## 6 2.0943396 2012-10-01       25 weekday
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
##Calculte the mean number of steps by interval for weekdays and weekends
wdayintmelt<-melt(modactivity, id=c("interval","wday"), measure="steps")
stepsbyintwday<-dcast(wdayintmelt, interval + wday ~ variable, mean)
##Plot the reults
p<-ggplot(stepsbyintwday, aes(x=interval, y=steps)) + geom_line()
p+facet_grid(wday ~ .)
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png) 

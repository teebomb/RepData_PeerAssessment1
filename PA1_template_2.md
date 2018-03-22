Loading and preprocessing the data
----------------------------------

    df1<-read.csv('C:/Users/TB/Documents/COURSERA DATA SCIENCE test/COURSE5WK2/PROJECT/ACTIVITY.csv')
    summary(df1)

    ##      steps                date          interval     
    ##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
    ##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
    ##  Median :  0.00   2012-10-03:  288   Median :1177.5  
    ##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
    ##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
    ##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
    ##  NA's   :2304     (Other)   :15840

What is mean total number of steps taken per day?
-------------------------------------------------

    df2<-df1
    #Mean total number of steps taken per day
    dailymean1<-tapply(df2$steps,df2$date,mean, na.rm=T)
    #Calculate the total number of steps taken per day
    dailysum1<-tapply(df2$steps, df2$date, sum, na.rm=T)
    hist(as.numeric(dailysum1), col="red", xlab="# of Steps", main="Steps per Day")

![](PA1_template_2_files/figure-markdown_strict/unnamed-chunk-3-1.png)

    #MEAN OF TOTAL STEPS PER DAY
    meandailysteptot1<-mean(dailysum1, na.rm=T)
    #MEDIAN OF TOTAL STEPS PER DAY
    mediandailysteptot1 <- median(dailysum1, na.rm=T)

The mean of the total numbers of steps taken per day is 9354.2295082.  
The median of the total numbers of steps taken per day is 10395.

What is the average daily activity pattern?
-------------------------------------------

    #CALC MEAN STEPS PER INTERVAL 
    intstepdaily<-with(df2, tapply(steps, interval, mean, na.rm=T))
    is1<-as.data.frame(matrix(unlist(intstepdaily), byrow=T))
    is2<-cbind(is1, levels(factor(df2$interval)))
    colnames(is2)<-c("steps","interval")

    #MAKE INTERVAL NUMERIC AND SORT ACCORDINGLY
    is2$interval<-as.numeric(as.character(is2$interval))
    is2<-arrange(is2,is2$interval)

    #Time Series Plot
    par(mfrow=c(1,1))
    with(is2, plot(interval, steps, type='l', ylab="Avg Steps per Interval", xlab="Interval", col="black"))

![](PA1_template_2_files/figure-markdown_strict/unnamed-chunk-4-1.png)

    #Max steps and associated interval
    maxint<-is2$interval[grep(max(is2$steps), is2$steps)]

The 5-minute interval that, on average, contains the maximum number of
steps is 835.

Imputing missing values
-----------------------

    #The total number of missing values in the dataset
    totNA<-length(df1$steps[is.na(df1$steps)])

    #Create a new dataset that is equal to the original dataset but with the missing data filled in.
    df4<-df1
    is2.1<-is2
    colnames(is2.1)<-c("avgsteps","interval")
    df5<-merge(df4, is2.1, by="interval")
    df5<-arrange(df5, date)
    df6<-df5

    #Fill the missing values with the corresponding interval's average value
    df6$steps[is.na(df6$steps)]<-df6$avgsteps[is.na(df6$steps)]
    #df6 is the new dataset  
    summary(df6)

    ##     interval          steps                date          avgsteps      
    ##  Min.   :   0.0   Min.   :  0.00   2012-10-01:  288   Min.   :  0.000  
    ##  1st Qu.: 588.8   1st Qu.:  0.00   2012-10-02:  288   1st Qu.:  2.486  
    ##  Median :1177.5   Median :  0.00   2012-10-03:  288   Median : 34.113  
    ##  Mean   :1177.5   Mean   : 37.38   2012-10-04:  288   Mean   : 37.383  
    ##  3rd Qu.:1766.2   3rd Qu.: 27.00   2012-10-05:  288   3rd Qu.: 52.835  
    ##  Max.   :2355.0   Max.   :806.00   2012-10-06:  288   Max.   :206.170  
    ##                                    (Other)   :15840

    #Histogram of the total number of steps taken each day
    dailysum1.1<-tapply(df6$steps, df6$date, sum)
    ds1.1<-as.data.frame(matrix(unlist(dailysum1.1), byrow=T)) #GIVES NUMBER OF STEPS PER DAY WITHOUT DATE

    hist(ds1.1[,1], col="red", xlab="# of Steps", main="Steps per Day with Imputed Values")

![](PA1_template_2_files/figure-markdown_strict/unnamed-chunk-6-1.png)

    #What is the impact of imputing missing data on the estimates of the total daily number of steps?

    #MEAN OF TOTAL STEPS PER DAY
    meandailysteptot1.1 <- mean(ds1.1[,1])

    #MEDIAN OF TOTAL STEPS PER DAY
    mediandailysteptot1.1<- median(ds1.1[,1])

The total number of missing values from the original dataset is 2304.  
The previous mean of the total numbers of steps taken per day is
9354.2295082. The new mean is 1.076618910^{4}.  
The previous median of the total numbers of steps taken per day is
10395. The new median is 1.076618910^{4}.

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

    #Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
    df7<-df6
    df7$date<-as.POSIXct(df7$date)
    wkday<-weekdays(df7$date)
    wkday[grep("Saturday\ |Sunday",wkday,invert=TRUE)]<-"weekday"
    wkday[grep("Saturday\ |Sunday",wkday)]<-"weekend"
    df7$wkday<-wkday

    #Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)

    #CALC MEAN PER INTERVAL BASED ON WKDAY OR WKEND
    intstepwkday<-with(df7[grep("weekday",df7$wkday),], tapply(steps, interval, mean))
    intstepwkend<-with(df7[grep("weekend",df7$wkday),], tapply(steps, interval, mean))

    #CREATE DF OF AVG STEPS PER INTERVAL
    is1.2<-as.data.frame(matrix(unlist(intstepwkday), byrow=T)) #GIVES NUMBER OF STEPS PER DAY WITHOUT DATE
    is1.3<-as.data.frame(matrix(unlist(intstepwkend), byrow=T)) #GIVES NUMBER OF STEPS PER DAY WITHOUT DATE
    is2.2<-cbind(is1.2, levels(factor(df7$interval)))
    is2.3<-cbind(is1.3, levels(factor(df7$interval)))
    colnames(is2.2)<-c("steps","interval")
    colnames(is2.3)<-c("steps","interval")

    #MAKE INTERVAL NUMERIC AND SORT ACCORDINGLY
    is2.2$interval<-as.numeric(as.character(is2.2$interval))
    is2.3$interval<-as.numeric(as.character(is2.3$interval))
    is2.2<-arrange(is2.2,is2.2$interval)
    is2.3<-arrange(is2.3,is2.3$interval)

    #Create Plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days
    par(mfrow=c(2,1))
    with(is2.2, plot(interval, steps, type='l', ylab="Number of Steps", xlab="Interval", col="blue", main="Weekday"))
    with(is2.3, plot(interval, steps, type='l', ylab="Number of Steps", xlab="Interval", col="blue", main="Weekend"))

![](PA1_template_2_files/figure-markdown_strict/unnamed-chunk-7-1.png)

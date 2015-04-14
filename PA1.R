## Loading and preprocessing the data

data<- read.csv("activity.csv")

## What is mean total number of steps taken per day?
tot <- aggregate(steps ~ date, data, sum)
hist(tot$steps, xlab="Total Steps", main="Total Steps/Day") 

mean(tot$steps)
median(tot$steps)

## What is the average daily activity pattern?

avgDaily <- aggregate(steps ~ interval, data, mean)
plot(avgDaily$interval, avgDaily$steps, type="l", 
     main="Steps during interval", xlab="Interval", ylab="Steps")

avgDaily[which(avgDaily$steps == max(avgDaily$steps)),]

nrow(avgDaily)

## Imputing missing values

length(which(is.na(data$steps)))
length(which(is.na(data$date)))
length(which(is.na(data$interval)))

names(data)

imputemiss <- function(data, avg)
{
  for (j in 1:nrow(data))
  {
    if(is.na(data$steps[j]))
    {
      data$steps[j] = avg[which(avg$interval == data$interval[j]),]$steps
    }
  }
  data
}

iData <- imputemiss(data,avgDaily)

length(which(is.na(iData$steps)))

itot <- aggregate(steps ~ date, iData, sum)
hist(itot$steps, xlab="Total Steps", main="Total Steps/Day") 

mean(itot$steps)
median(itot$steps)

## Are there differences in activity patterns between weekdays and weekends?

setweekday<- function(d)
{
  for (i in 1:nrow(d))
  {
    if(d$day[i]=="Saturday" | d$day[i]=="Sunday" )
      d$day[i]="weekend"
    else
      d$day[i]="weekday"
  }
  d
}

iData$day<-weekdays(as.Date(iData$date))
wiData <- setweekday(iData)
iavgDaily <- aggregate(steps ~ interval + day, wiData, mean)
head(iavgDaily)
tail(iavgDaily)

library(lattice)
xyplot(steps ~ interval | day, data=iavgDaily, type="l",layout=c(1,2))


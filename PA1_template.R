## Loading and processing the data

data <- read.csv("activity.csv")


## What is the mean total number of steps taken per day

totalSteps <- tapply(data$steps, data$date, sum)
hist(totalSteps, xlab = "Total Steps", ylab = "Day", main = "Total Number of Steps Taken per Day")

mean(totalSteps, na.rm = TRUE)
median(totalSteps, na.rm = TRUE)


## What is the average daily activity pattern
avgSteps <- tapply(data$steps, data$interval, mean, na.rm = TRUE)
plot(unique(data$interval), avgSteps, type = "l", xlab = "5-Minute Interval", ylab = "Average Steps", main = "Average Daily Activity Partner")

avgSteps[which.max(avgSteps)]


## Imputing  missing values
sum(is.na(data))

tapply(data$steps, data$interval, mean, na.rm = TRUE)

fullData <- data
fullData[is.na(fullData)] <- 0

totalSteps <- tapply(fullData$steps, fullData$date, sum)
hist(totalSteps, xlab = "Number of Steps", ylab = "Day", main = "Number of steps taken each day")


mean(totalSteps)
median(totalSteps)


## Are there differences in activity patterns between weekdays and weekends?

fullData$weekday <- factor(fullData$date, levels = c("Weekdays", "Weekends"))
fullData$weekday <- ifelse(weekdays(as.Date(fullData$date)) == "Monday" | weekdays(as.Date(fullData$date)) == "Tuesday" | weekdays(as.Date(fullData$date)) == "Wednesday" | weekdays(as.Date(fullData$date)) == "Thursday" | weekdays(as.Date(fullData$date)) == "Friday", "Weekday", "Weekend")
avgStepsW <- aggregate(steps ~ interval + weekday, fullData, mean, na.rm = TRUE)

library(lattice)
xyplot(steps ~ interval | weekday, avgStepsW, type = "l", layout = c(1, 2), xlab = "Interval", ylab = "Number of Steps", main = "Average Number of Steps Taken per Weekday/Weekend")

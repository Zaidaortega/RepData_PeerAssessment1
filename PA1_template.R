## INSTALLING DATA
activitydata <- read.csv("activity.csv", colClasses = c("numeric", "character", 
                                                    "numeric"))
## INSTALLING R PACKAGES ##
library(lattice)
library(knitr)
opts_chunk$set(echo = TRUE, results = 'hold')
library(data.table)
library(ggplot2) 

## PREPARING DATA ##
activitydata$date <- as.Date(activitydata$date, "%Y-%m-%d")
activitydata$interval <- as.factor(activitydata$interval)

## STEPS PER DAY ##
daysteps <- aggregate(steps ~ date, activitydata, sum)
colnames(daysteps) <- c("date","steps")
head(daysteps)

## HISTOGRAM OF STEPS PER DAY ##
ggplot(daysteps, aes(x = steps)) + 
  geom_histogram(fill = "grey", binwidth = 600) + 
  labs(title="Steps per Day", 
       x = "Steps per Day", y = "Frequency") + theme_bw() 

## MEAN AND MEDIAN OF STEPS PER DAY ##
steps_mean   <- mean(daysteps$steps, na.rm=TRUE)
steps_median <- median(daysteps$steps, na.rm=TRUE)

## AVERAGE DAILY ACTIVITY PATTERN ##
time_data <- tapply(activitydata$steps, activitydata$interval, mean, na.rm = TRUE)
plot(row.names(time_data), time_data, type = "l", xlab = "5 minutes interval", 
     ylab = "Average by days", main = "Average steps", 
     col = "gray") ##Plotting 5 minutes intervals with average number of steps
max <- which.max(time_data)
names(max) ##To know which 5 minutes interval represents the maximun number of steps

## IMPUTING MISSING VALUES ##
activitydataNA <- sum(is.na(activitydata))
activitydataNA

## FILLING IN ALL MISSING VALUES ##
stepsdata <- aggregate(steps ~ interval, data = activitydata, FUN = mean)
NA_fill <- numeric()
for (i in 1:nrow(activitydata)) {
  obs <- activitydata[i, ]
  if (is.na(obs$steps)) {
    steps <- subset(stepsdata, interval == obs$interval)$steps
  } else {
    steps <- obs$steps
  }
  NA_fill <- c(NA_fill, steps)
}

## NEW DATASET WITH FILLED MISSING VALUES##
new_data <- activitydata
new_data$steps <- NA_fill

## HISTOGRAM STEPS PER DAY NEW DATASET ##
TotalSteps <- aggregate(steps ~ date, data = new_data, sum, na.rm = TRUE)
hist(TotalSteps$steps, main = "Steps per day", xlab = "day", col = "grey")

## MEAN AND MEDIAN STEPS PER DAY NEW DATASET ##
mean(TotalSteps$steps)
median(TotalSteps$steps)

## WEEKDAYS VS WEEDENDS ##
day <- weekdays(activitydata$date)
daylevel <- vector()
for (i in 1:nrow(activitydata)) {
  if (day[i] == "Saturday") {
    daylevel[i] <- "Weekend"
  } else if (day[i] == "Sunday") {
    daylevel[i] <- "Weekend"
  } else {
    daylevel[i] <- "Weekday"
  }
}
activitydata$daylevel <- daylevel
activitydata$daylevel <- factor(activitydata$daylevel)

daySteps <- aggregate(steps ~ interval + daylevel, data = activitydata, mean)
names(daySteps) <- c("interval", "daylevel", "steps")

## PLOT OF STEPS BY WEEKDAY OR WEEKEND ##
xyplot(steps ~ interval | daylevel, daySteps, type = "l", layout = c(1, 2), 
       xlab = "5-mins interval", ylab = "Steps")

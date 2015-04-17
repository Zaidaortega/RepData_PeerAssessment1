# REPRODUCIBLE RESEARCH, Peer assessment 1
#### Zaida Ortega, April 2015
## 1. INSTALLING AND PREPARING DATA
### 1.1 Installing data
```{r} 
activitydata <- read.csv("activity.csv", colClasses = c("numeric", "character","numeric"))
```
###1.2 Installing packages
```{r} 
library(lattice)
library(knitr)
opts_chunk$set(echo = TRUE, results = 'hold')
library(data.table)
library(ggplot2) 
```
###1.3 Preparing data
```{r} 
activitydata$date <- as.Date(activitydata$date, "%Y-%m-%d")
activitydata$interval <- as.factor(activitydata$interval)
```
##2. What is mean total number of steps taken per day?
###2.1 Preparing data
```{r} 
daysteps <- aggregate(steps ~ date, activitydata, sum)
colnames(daysteps) <- c("date","steps")
head(daysteps)
```
###2.2 Histogram of  number of steps per day
```{r} 
ggplot(daysteps, aes(x = steps)) + 
  geom_histogram(fill = "grey", binwidth = 600) + 
  labs(title="Steps per Day", 
       x = "Steps per Day", y = "Frequency") + theme_bw()
```
![Bilby Stampede](https://cloud.githubusercontent.com/assets/10145348/7201549/cd55647c-e508-11e4-94a5-2122e729827d.png)
###2.3 Mean and median steps per day       
```{r} 
steps_mean   <- mean(daysteps$steps, na.rm=TRUE)
steps_median <- median(daysteps$steps, na.rm=TRUE)
```
## 3. What is the average daily activity pattern?
### 3.1 Preparing data
```{r}
time_data <- tapply(activitydata$steps, activitydata$interval, mean, na.rm = TRUE)
```
###3.2 Line plot of average steps per 5 minutes intervals
```{r}
plot(row.names(time_data), time_data, type = "l", xlab = "5 minutes interval", 
     ylab = "Average by days", main = "Average steps", 
     col = "gray") ##Plotting 5 minutes intervals with average number of steps
```
![Bilby Stampede](https://cloud.githubusercontent.com/assets/10145348/7201565/ea0c7d30-e508-11e4-8713-a75cdae678b3.png)
###3.3 Maximun number of steps in a 5 minutes interval
```{r}
max <- which.max(time_data)
names(max) ##To know which 5 minutes interval represents the maximun number of steps
```
##4. Imputing missing values
###4.1 Preparing data
```{r}
activitydataNA <- sum(is.na(activitydata))
activitydataNA
```
###4.2 Filling NA
```{r}
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
```
###4.3 New dataset with NA filled
```{r}
new_data <- activitydata
new_data$steps <- NA_fill
```
###4.4 Histogram of steps per day of the data with NA filled
```{r}
TotalSteps <- aggregate(steps ~ date, data = new_data, sum, na.rm = TRUE)
hist(TotalSteps$steps, main = "Steps per day", xlab = "day", col = "grey")
```
![Image](https://cloud.githubusercontent.com/assets/10145348/7201568/f33d3b88-e508-11e4-8c45-46c6e16a5b3c.png)
###4.5 Mean and median steps per day of the data with NA filled
```{r}
mean(TotalSteps$steps)
median(TotalSteps$steps)
```
##5. Steps on weekdays VS steps on weekends ##
###5.1 Preparing data
```{r}
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
```
###5.2 Plotting steps of weekdays and weekends
```{r}
xyplot(steps ~ interval | daylevel, daySteps, type = "l", layout = c(1, 2), 
       xlab = "5-mins interval", ylab = "Steps")the first time.
```
![image](https://cloud.githubusercontent.com/assets/10145348/7201573/fbb59576-e508-11e4-9fee-3dea470a7858.png)

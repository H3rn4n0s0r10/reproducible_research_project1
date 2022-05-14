# Set Global Echo = On

# Load data
if (!file.exists("activity.csv") )
{
      dlurl <- 'http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'  
      download.file(dlurl,destfile='repdata%2Fdata%2Factivity.zip',mode='wb')  
      unzip('repdata%2Fdata%2Factivity.zip')
}

# Read data
data <- read.csv("activity.csv")

steps_by_day <- aggregate(steps ~ date, data, sum)
hist(steps_by_day$steps, main = paste("Total Steps Each Day"), col="green",xlab="Number of Steps")

rmean <- mean(steps_by_day$steps)
rmean

rmedian <- median(steps_by_day$steps)
rmedian

steps_by_interval <- aggregate(steps ~ interval, data, mean)
plot(steps_by_interval$interval,steps_by_interval$steps, type="l", xlab="Interval", ylab="Number of Steps",main="Average Number of Steps per Day by Interval")

NATotal <- sum(!complete.cases(data))
NATotal

StepsAverage <- aggregate(steps ~ interval, data = data, FUN = mean)
fillNA <- numeric()
for (i in 1:nrow(data)) {
      obs <- data[i, ]
      if (is.na(obs$steps)) {
            steps <- subset(StepsAverage, interval == obs$interval)$steps
      } else {
            steps <- obs$steps
      }
      fillNA <- c(fillNA, steps)
}

new_activity <- data
new_activity$steps <- fillNA

StepsTotalUnion <- aggregate(steps ~ date, data = new_activity, sum, na.rm = TRUE)
hist(StepsTotalUnion$steps, main = paste("Total Steps Each Day"), col="blue", xlab="Number of Steps")
#Histogram to show difference. 
hist(steps_by_day$steps, main = paste("Total Steps Each Day"), col="green", xlab="Number of Steps", add=T)
legend("topright", c("Imputed", "Non-imputed"), col=c("blue", "green"), lwd=10)


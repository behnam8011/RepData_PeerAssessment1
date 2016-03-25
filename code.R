setwd("C:/Users/Behnam/Documents/Ben/Analytics Training/Data-Science-Coursera/Reproducible Research/")

if (!require(lubridate)) {
        install.packages("lubridate")
        library(lubridate)
}
if (!require(dplyr)) {
        install.packages("dplyr")
        library(dplyr)
}

## read data
raw <- read.csv("./activity.csv",header = T)
raw$date <- ymd(raw$date)
raw$interval <- as.factor(raw$interval)
head(raw)

## q1
daily <- raw %>%
        group_by(date) %>%
        summarise(total.steps = sum(steps,na.rm = T))
mean(daily$total.steps)
median(daily$total.steps)
hist(x = daily$total.steps,breaks = 10, main = "Histogram of Total Daily Steps", xlab = "Total Daily Steps", col = rgb(.75,.75,0))
rug(x = daily$total.steps, col = "red")
abline(v = mean(daily$total.steps), col = rgb(0.5,0.25,0.75), lwd = 5)
abline(v = median(daily$total.steps), col = "blue", lwd = 5)

## q2
time.series <- raw %>%
        group_by(interval) %>%
        summarise(avg.steps = mean(steps,na.rm = T))
plot(x = time.series$interval, y = time.series$avg.steps, xlib = "Interval", ylib = "Average Steps", type = "l")
time.series$interval[which.max(time.series$avg.steps)]
abline(v = time.series$interval[which.max(time.series$avg.steps)], col = "red", lwd = 2, lty = 2)

#q3
nrow(raw) - sum(complete.cases(raw))
imputed <- merge(raw,time.series, by="interval")
imputed <- imputed %>%
        mutate(temp = as.integer(is.na(steps))*avg.steps)
        
imputed[is.na(imputed$steps),"steps"] <- 0

imputed <- imputed %>%
        mutate(steps.imputed = steps + temp) %>%
        select(interval,date,steps.imputed)

daily <- imputed %>%
        group_by(date) %>%
        summarise(total.steps = sum(steps.imputed))
mean(daily$total.steps)
median(daily$total.steps)
hist(x = daily$total.steps,breaks = 10, main = "Histogram of Total Daily Steps", xlab = "Total Daily Steps", col = rgb(.75,.75,0))
rug(x = daily$total.steps, col = "red")
abline(v = mean(daily$total.steps), col = rgb(0.5,0.25,0.75), lwd = 5)
abline(v = median(daily$total.steps), col = "blue", lwd = 5)


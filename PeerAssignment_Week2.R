# Required Packages :
# install.packages("lubridate") 
# install.packages("ggplot2")

# Loading required packages:
library(lubridate)
library(ggplot2)

# Setting up working directory for database
setwd("/Users/maulikchaudhary/Desktop/R/Coursera/Reproducible research/Week 2/")

# Downloading dataset from provided URL
# URL Access date and time:  "2018-03-17 19:02:30 AEDT"
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", destfile = "Activity.zip")
unzip("Activity.zip") # This function will unzip all files from "Activity.zip"
unlink("Activity.zip") # This function will delete file "Activity.zip". 

# 1. Code for reading in the dataset and/or processing the data.

# Loading Activity dataset to global environment
list.files() # To check all unzipped files. We will load "activity.csv" in the following step.
ActivityDataset <- read.csv("activity.csv")

# Pondering little over dataset.
head(ActivityDataset)
str(ActivityDataset) # Checking structure all variables
mean(complete.cases(ActivityDataset))  # Checking for complete cases. 86.89% of data is available.

# Date column is recorded as factor instead of date. Converting date column to date variable.
ActivityDataset$date <- ymd(ActivityDataset$date)

# Complete cases dataset
activityWithoutNA <- ActivityDataset[complete.cases(ActivityDataset),]

# As 86.89% data is available, we will work with it by removing NA's from our dataset.
# Further computations are carried out with complete cases.

# 2. Histogram of the total number of steps taken each day
png("Plot1.png")
hist(round(tapply(activityWithoutNA$steps, activityWithoutNA$date, sum), 0), 
     xlab = "", main = "Total Steps taken", breaks = 61, col = "Red")
dev.off()

# 3. Mean and median number of steps taken each day
MeanSteps <- as.data.frame(as.table(round(tapply(activityWithoutNA$steps, activityWithoutNA$date, mean), 0)))
names(MeanSteps) <- c("date", "steps")
MedianSteps <- as.data.frame(as.table(round(tapply(activityWithoutNA$steps, activityWithoutNA$date, median), 0)))
names(MedianSteps) <- c("date", "steps")
MeanSteps$date <- ymd(MeanSteps$date)

# 4. Time series plot of the average number of steps taken
png("Plot2.png")
ggplot(MeanSteps, aes(date, steps)) + geom_point(size = 0.1) + geom_line(col = "red")
dev.off()        

# 5. The 5-minute interval that, on average, contains the maximum number of steps
fiveMinAvgSteps <- as.data.frame(as.table(round(with(activityWithoutNA, tapply(steps, interval, mean)),0)))
png("Plot3.png")
ggplot(fiveMinAvgSteps, aes(Var1, Freq)) + geom_point() + 
        coord_cartesian(xlim = c(0,288), ylim = c(0,220), expand = T) +
        geom_vline(xintercept = as.integer(fiveMinAvgSteps[fiveMinAvgSteps$Freq == max(fiveMinAvgSteps$Freq),]$Var1))
dev.off()
fiveMinAvgSteps[fiveMinAvgSteps$Freq == max(fiveMinAvgSteps$Freq),]$Freq # interval 835 contains maximum no. of steps

# 6. Code to describe and show a strategy for imputing missing data.
activityWithNA <- ActivityDataset[!complete.cases(ActivityDataset),]
table(activityWithNA$date) # In total, 8 days data is missing
table(ActivityDataset$date) # 30 days dataset.
# Plotting graph to check for outliers.
png("Plot4.png")
ggplot(ActivityDataset, aes(date, steps)) + geom_point(alpha = 0.5) 
dev.off()
# After plotting we can see there are outliers which can vary results.
# Outliers can affect mean imputation.
# Median imputation will be more effective than mean.
# Finding median values for all time intervals from "activityWithoutNA" dataset.

medianValues <- as.data.frame(as.table(with(activityWithoutNA, tapply(steps, interval, median))))
names(medianValues) <- c("interval", "steps")
mergedDataset <- merge(medianValues, activityWithNA, by = "interval")
activityWithNA <- mergedDataset[order(mergedDataset$date, mergedDataset$interval),c(1,2,4)]
names(activityWithNA) <- c("interval", "steps", "date")
activityWithNA <- activityWithNA[,c(2,3,1)]

# Merging data with NA and without NA.
imputedDataset <- rbind(activityWithNA, activityWithoutNA)
rownames(imputedDataset) <- NULL

# 7. Histogram of the total number of steps taken each day after missing values are imputed
png("Plot5.png")
hist(with(imputedDataset,
     tapply(steps, date, sum)), breaks = 30, xlab = "", main = "Total number of steps",
     col = "Red")
dev.off()

# 8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
imputedDataset$day <- wday(imputedDataset$date, label = TRUE)
weekdaysDataset <- imputedDataset[!(imputedDataset$day == "Sat" | imputedDataset$day == "Sun"), ]
weekendsDataset <- imputedDataset[imputedDataset$day == "Sat" | imputedDataset$day == "Sun", ]
png("Plot6.png")
par(mfrow = c(2,1))
plot(with(weekdaysDataset, tapply(steps, interval, mean)), ylab = "", xlab = "Steps", main = "Weekdays", type ="l")
plot(with(weekendsDataset, tapply(steps, interval, mean)), ylab = "", xlab = "Steps", main = "Weekends", type ="l")
dev.off()



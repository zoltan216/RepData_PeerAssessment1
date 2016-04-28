---
title: "Reproducible Data: Assignment #1"
author: "By: Zola Javzmaa"
date: "April 27, 2016"
output: html_document
---

####Set Working Directory
```{r}

setwd("~/R-Coursera/5) Reproducible Research/Week 1/Assignment 1")
```

####load data
```{r}
data <- read.csv("C:/Users/zjavzmaa/Documents/R-Coursera/5) Reproducible Research/Week 1/Assignment 1/activity.csv", 
                 header=TRUE, sep = ",")
```

####look at the data
```{r}
head(data)
summary(data)
```

##Part 1
####1. Find out the total amount of steps for each day
```{r}
steps <- aggregate(data$steps, by=list(date=data$date), sum)
head(steps) #look at data
```

####2. Make a histogram
```{r}
with(steps,{
        barplot(height=x, names.arg=date, xlab="Date", 
                ylim=c(0,25000), ylab="Number of Steps", panel.first=grid())
})
# Construct the mean and median lines. The red line with dashes 
# is the mean and the solid green line is the median.
abline(h=mean(steps$x,na.rm=TRUE), col="red", lwd=3, lty=2)
abline(h=mean(steps$x,na.rm=TRUE), col="green", lwd=1)
```

####3. Calculate the mean and median
```{r}
steps_mean <- mean(steps$x, na.rm=TRUE)
steps_median <- median(steps$x,na.rm=TRUE)
```


##Part 2
####Construction activities at 5 minute intervals
Aggregate by intervals
```{r}
intervals_steps <- aggregate(steps ~ interval, data=data, FUN=mean)
```
####1. Plot
```{r}
with(intervals_steps,{
        plot(interval, steps, type="l", xlab="Intervals Throughout the Day", 
             ylab="Average Number of Steps",
             main="Average Steps by 5 Minute Intervals", panel.first = grid(),
             col.axis="white")
        # Add minor tick marks
        xMarks <- seq(from = 0, to = 2400, by = 100)
        yMarks <- seq(from = 0, to = 200, by = 50)
        axis(1, at=xMarks, col.axis="blue", las=2, tck=0.02)
        axis(2, at=yMarks, col.axis="blue", las=2, tck=0.02)
})
```

####2. max steps
```{r}
max_steps <- intervals_steps$steps[which.max(intervals_steps$steps)]
max_steps
```

##Part 3
####1. total number of missing dates
```{r}
total_na <- sum(is.na(data$steps))
total_na
```


2. A strategy for filling in all of the missing values in the dataset could use the mean for that 5-min interval. And that's what we do:


```{r+}
data$steps_2 <- data$steps   # create a new column of steps derivated from 1st one
nObs <- nrow(data)
iRow <- 1
while(iRow <= nObs) {
  if( is.na(data$steps[iRow]) ) {
		intervalRow <- data$interval[iRow]
		data$steps_2[iRow] =intervals_steps$steps[which(intervals_steps$interval==intervalRow)]
	}	
	iRow <- iRow+1
}
head(data)
```
####3. See 2 above
####4. plot
```{r}
new_steps <- aggregate(data$steps_2, by=list(date=data$date),sum)
new_mean <- mean(new_steps$x)
new_median <- median(new_steps$x)
```
Plot
```{r}
with(new_steps,{
        barplot(height=x, names.arg=date, xlab="Date", 
                ylim=c(0,25000), ylab="Number of Steps", panel.first=grid())
})

# Construct the mean and median lines. The red line with dashes 
# is the mean and the solid green line is the median.
abline(h=mean(steps$x,na.rm=TRUE), col="red", lwd=3, lty=2)
abline(h=mean(steps$x,na.rm=TRUE), col="green", lwd=1)
```
Conclusion: almost no change in the mean and median from replacing the NA's

##Part 4
Label Weekend or Weekday
```{r}
day_of_week <- c("Weekend", "Weekday")

week_period <- function(v_date) {
        d <- as.POSIXlt(as.Date(v_date))$wday
        return( ifelse(d == 0 || d == 6, day_of_week[1], day_of_week[2]) )
}

data$Part_Wk <- as.factor( sapply(data$date, week_period) )
head(data)
```


#### create a panel layout 2x1 and establish margin values
```{r}
par(mfrow=c(2, 1), mar=c(3, 3, 2, 1), oma=c(1.5, 1, 2, 1)) 
```
Plot
```{r}
## plot both line graphs side by side showing differences
for( td in day_of_week ) {
        stepsByday_of_week <- aggregate(steps_2 ~ interval, data = data, 
                                        subset = (data$Part_Wk == td), 
                                        FUN = mean)
        plot(stepsByday_of_week, type = "l", main = td, xlab="", ylab="", 
             ylim=c(0,250), xlim=c(0,2400), 
             panel.first=grid(), col.axis='white')
        
        xMarks <- seq(from = 0, to = 2400, by = 100)
        axis(1, at=xMarks, col.axis="blue", las=2, tck=0.02)
        yMarks <- seq(from = 0, to = 250, by = 50)
        axis(2, at=yMarks, col.axis="blue", las=2, tck=0.02)
}
mtext( "interval (5-minute)", side=1, outer=TRUE, col="blue", font=1 )  
mtext( "Mean number of steps", side=2, outer=TRUE, col="blue", font=1, cex=0.9 )  
mtext( "Activity patterns between weekdays and weekends", side=3, outer=TRUE, 
       col="maroon", font=2 )  
box("outer", col="maroon") 
```



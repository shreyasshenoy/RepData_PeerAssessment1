# Reproducible Research: Peer Assessment 1

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

## Loading and preprocessing the data

1. The data file is downloaded if it is not present
2. It is uncompressed and read as a data frame.
3. The interval column is converted to factor type.
4. The date column is converted to Date type.
5. The data is examined by using summary and str methods on it.


```r
library(ggplot2) # we shall use ggplot2 for plotting figures

# download and read the data, convert columns for convenience
read_data <- function() {
    fname = "activity.zip"
    source_url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    if(!file.exists(fname)) {
        download.file(source_url, destfile=fname)
    }
    con <- unz(fname, "activity.csv")
    tbl <- read.csv(con, header=T, colClasses=c("numeric", "character", "numeric"))
    tbl$interval <- factor(tbl$interval)
    tbl$date <- as.Date(tbl$date, format="%Y-%m-%d")
    tbl
}
tbl <- read_data()
```

```
## Error: unsupported URL scheme
```

Examine data.

```r
summary(tbl)
```

```
## Error: object 'tbl' not found
```

```r
str(tbl)
```

```
## Error: object 'tbl' not found
```

## What is mean total number of steps taken per day?

Below is a histogram of the daily total number of steps taken, plotted with a bin interval of 1500 steps. Also marked on the plot are the mean and median of the daily total steps.


```r
calc_steps_per_day <- function(tbl) {
    steps_per_day <- aggregate(steps ~ date, tbl, sum)
    colnames(steps_per_day) <- c("date", "steps")
    steps_per_day
}

plot_steps_per_day <- function(steps_per_day, mean_steps, median_steps) {
    col_labels=c(paste("Mean:", mean_steps), paste("Median:", median_steps))
    cols = c("green", "yellow")
    
    ggplot(steps_per_day, aes(x=steps)) + 
        geom_histogram(fill="steelblue", binwidth=1500) + 
        geom_point(aes(x=mean_steps, y=0, color="green"), size=4, shape=15) + 
        geom_point(aes(x=median_steps, y=0, color="yellow"), size=4, shape=15) + 
        scale_color_manual(name=element_blank(), labels=col_labels, values=cols) + 
        labs(title="Histogram of Steps Taken per Day", x="Number of Steps", y="Count") + 
        theme_bw() + theme(legend.position = "bottom")    
}

steps_per_day <- calc_steps_per_day(tbl)
```

```
## Error: object 'tbl' not found
```

```r
mean_steps = round(mean(steps_per_day$steps), 2)
```

```
## Error: object 'steps_per_day' not found
```

```r
median_steps = round(median(steps_per_day$steps), 2)
```

```
## Error: object 'steps_per_day' not found
```

```r
plot_steps_per_day(steps_per_day, mean_steps, median_steps)
```

```
## Error: object 'mean_steps' not found
```












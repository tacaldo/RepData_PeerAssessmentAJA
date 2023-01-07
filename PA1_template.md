---
title: "Reproducible Research, Week 2 - Course Project 1"
author: "Tony Acaldo"
date: '2023-01-05'
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

knitr::opts_chunk$set(fig.path = "figures/")


library(tidyverse)

```

### Analysis Activity Monitoring Data
The following report will analize data from a personal activity monitoring device.

This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

Several plots are rendered showing:

* **Histogram of Total Steps Daily  **
* **Time Series of Average Daily Activity**
* **Histogram of Total Steps Daily With Missing Values Imputed  **
* **Time Series of Weekend vs. Weekday Activity**



### R Code Behind Analysis



```{r loadData}
# Calculate the total number of steps taken per day
# Make a histogram of the total number of steps taken each day

# Calculate and report the mean and median of the total number of steps taken per day
activity_data <- read.csv(unz("repdata_data_activity.zip", "activity.csv"), header = TRUE,
                 sep = ",")


# Remove NA data
activity_data_cleaned <- activity_data %>%  filter(!is.na(steps))

# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the 
# average number of steps taken, averaged across all days (y-axis)

# Which 5-minute interval, on average across all the days in the dataset, contains the 
# maximum number of steps?

max_row <- activity_data_cleaned[which.max(activity_data_cleaned$steps), ]
max_row_interval <- max_row$interval

max_interval_label <- paste("max interval =", max_row_interval)
max_steps_label <- paste("----[max steps =", max_row$steps)

p_avg_daily_pattern <-  ggplot(activity_data_cleaned, aes(x=interval,y=steps)) + 
  geom_line(color = "mediumspringgreen") +
  geom_vline(xintercept=max_row_interval, linetype="dashed") +
  annotate("text", x=max_row_interval, y=-40,  label=max_interval_label, angle=0) +
  annotate("text", x=900, y=806, label=max_steps_label, angle=0) +
  labs(title = "Average Daily Activity Pattern", subtitle = "Interval With Maximum Steps", caption = "Figure 2.0")
  #ggtitle(label = 'Average Daily Activity Pattern', 'Interval With Maximum Steps', 'Test this..')
  


activity_data_cleaned$day <- weekdays(as.Date(activity_data_cleaned$date))

# Calculate and report the mean and median of the total number of steps taken per day
activity_data_steps_sum_by_date <- activity_data_cleaned %>%   group_by(date) %>%  summarise(steps = sum(steps))

mean_total_steps_per_day <- mean(activity_data_steps_sum_by_date$steps)
mean_total_steps_per_day <- round(mean_total_steps_per_day, digits = 2)

activity_data_steps_mean_by_date <- activity_data_cleaned %>%   group_by(date) %>%  summarise(steps = mean(steps))

activity_data_steps_median_by_date <- activity_data_cleaned %>%   group_by(date) %>%  summarise(steps = median(steps))

median_total_steps_per_day <- median(activity_data_steps_sum_by_date$steps)
median_total_steps_per_day <- round(median_total_steps_per_day, digits = 2)

#count total missing values 
missing_steps_sum <- sum(is.na(activity_data$steps))
missing_steps_sum <- round(missing_steps_sum, digits = 2)

#nothing found
missing_interval_sum <- sum(is.na(activity_data$interval))
missing_interval_sum <- round(missing_interval_sum, digits = 2)

# This is used for imputed value
mean_of_means_activity_data <- mean(activity_data_steps_mean_by_date$steps)
mean_of_means_activity_data <- round(mean_of_means_activity_data, digits = 2)

activity_data_new_steps <- activity_data %>%
rowwise %>%
mutate(new_steps=ifelse(is.na(steps),mean_of_means_activity_data,steps))

activity_data_imputed <- activity_data_new_steps %>% select(-steps)

activity_data_imputed <- activity_data_imputed %>% rename("steps" = "new_steps")

# Create a new dataset that is equal to the original dataset but with the 
# missing data filled in.
activity_data_imputed <- activity_data_imputed  %>% relocate(steps, date, interval)
activity_data_imputed$day <- weekdays(as.Date(activity_data_imputed$date))
activity_data_imputed$we = ifelse(activity_data_imputed$day %in% c("Saturday", "Sunday"), "weekend", "weekday")

p_activity_weekweekend <- ggplot(activity_data_imputed, aes(x=interval,y=steps)) + geom_line(color = "salmon") +
  facet_grid(we ~ .) +
  labs(title = "Activity Patterns", subtitle = "Weekday vs. Weekend", caption = "Figure 4.0")

act_imputed_weekend <- filter(activity_data_imputed, day == "Saturday" | day == "Sunday")
act_imputed_weekday <- filter(activity_data_imputed, day != "Saturday" & day != "Sunday")

# Get sum, mean, median for by date for activity_data_imputed
activity_data_imputed_steps_sum_by_date <- activity_data_imputed %>%   group_by(date) %>%  summarise(steps = sum(steps))

mean_total_steps_per_day_imputed <- mean(activity_data_imputed_steps_sum_by_date$steps)
mean_total_steps_per_day_imputed <- round(mean_total_steps_per_day_imputed, digits = 2)

activity_data_impututed_steps_mean_by_date <- activity_data_imputed_steps_sum_by_date %>%   group_by(date) %>%  summarise(steps = mean(steps))

activity_data_imputed_steps_median_by_date <- activity_data_imputed_steps_sum_by_date %>%   group_by(date) %>%  summarise(steps = median(steps))

median_total_steps_per_day_imputed <- median(activity_data_imputed_steps_sum_by_date$steps)
median_total_steps_per_day_imputed <- round(median_total_steps_per_day_imputed, digits = 2)

total_daily_label = paste("Mean =", mean_total_steps_per_day, ", ", "Median =", median_total_steps_per_day)
p_total_daily_missing_ignored <- ggplot(activity_data_steps_sum_by_date, aes(x=steps, fill = date)) + 
  geom_histogram(binwidth = 10000) +
  geom_vline(xintercept=mean_total_steps_per_day, linetype="dashed") +
  geom_vline(xintercept=median_total_steps_per_day, linetype="dotted", color="red") +
  annotate("label", x = mean_total_steps_per_day, y=15, label = total_daily_label) +
  labs(title = "Total Step Daily", subtitle = "Missing Values Ignored", caption = "Figure 1.0") +
  xlab("steps") + ylab("count")

total_daily_label_imputed = paste("Mean =", mean_total_steps_per_day_imputed, ", ", "Median =", median_total_steps_per_day_imputed)
imputed_calc_label = paste("Imputed value is based on mean of means = ", mean_of_means_activity_data)

p_total_daily_missing_imputed <- ggplot(activity_data_imputed_steps_sum_by_date, aes(x=steps)) + 
  geom_histogram(binwidth = 10000, fill="cadetblue") +
  geom_vline(xintercept=mean_total_steps_per_day_imputed, linetype="dashed") +
  geom_vline(xintercept=median_total_steps_per_day_imputed, linetype="dotted", color="red") +
  annotate("label", x = mean_total_steps_per_day_imputed, y=15, label = total_daily_label_imputed) +
  annotate("label", x = mean_total_steps_per_day_imputed, y=20, label = imputed_calc_label) +
  labs(title = "Total Step Daily", subtitle = "Missing Values Imputed", caption = "Figure 3.0") +
  xlab("steps") + ylab("count")

# Histogram of the total number of steps taken each day
# Mean and median number of steps taken each day (labeled on plot)
p_total_daily_missing_ignored

# Time series plot of the average number of steps taken
# The 5-minute interval that, on average, contains the maximum number of steps (labeled on plot)
p_avg_daily_pattern

# Code to describe and show a strategy for imputing missing data
# Histogram of the total number of steps taken each day after missing values are imputed
# This is the code used for imputed value:
# activity_data_steps_mean_by_date <- activity_data_cleaned %>%   group_by(date) %>%  summarise(steps = mean(steps))
# mean_of_means_activity_data <- mean(activity_data_steps_mean_by_date$steps)
# mean_of_means_activity_data <- round(mean_of_means_activity_data, digits = 2)
# activity_data_new_steps <- activity_data %>%
# rowwise %>%
# mutate(new_steps=ifelse(is.na(steps),mean_of_means_activity_data,steps))

# activity_data_imputed <- activity_data_new_steps %>% select(-steps)

# activity_data_imputed <- activity_data_imputed %>% rename("steps" = "new_steps")

# Create a new dataset that is equal to the original dataset but with the 
# missing data filled in.
#activity_data_imputed <- activity_data_imputed  %>% relocate(steps, date, interval)

p_total_daily_missing_imputed

# Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
p_activity_weekweekend


```

## Summary

The detailed and reproducible analysis above answers several questions in relation to the data. They are as follows:

**What are the total number the total number of steps taken per day (ignore missing values)?**

*See histogram Figure 1.0*

**What are the mean and median of the number the total number of steps taken per day (ignore missing values)?**

*See histogram Figure 1.0 - annotations*

**What is the average daily activity pattern (ignore missing values)?**

*See time series plot Figure 2.0*

**What 5-minute interval, on average accross all days in the dataset, contains the maximum number of steps?**

*See time series plot Figure 2.0 - annotations *

**What is the total number of missing values (coded as NA)?**

*See missing_steps_sum calculation in R code listing = 2304 *

**How can we impute some missing values?**

*See R code listing describing strategy for imputing values*

**How would a histogram look with the imputed values described?**

*See histogram Figure 3.0 - annotations *

**Are there differences in activity patterns between weekdays and weekends?**

*See faceted time series plots Figure 4.0 *

















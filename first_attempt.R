# a first attempt at loading and analyzing data with R.
# gregmarra, Jan 2017

library(tidyverse)
library(ggplot2)
library(lubridate)

trips <- read.csv('data/201706-hubway-tripdata.csv', stringsAsFactors=FALSE) %>%
  as_tibble()

# Functions

addAges <- function(df) {
  return (df %>%
    filter(birth.year != '\\N') %>%
    mutate(age = 2017 - as.numeric(birth.year)) %>%
    filter(age < 100))
}

addTimes <- function(df) {
  return (df %>%
    mutate(
      starttime = ymd_hms(starttime),
      stoptime = ymd_hms(stoptime),
    ))
}

# Poking at Data

## Rides by Age

trips %>%
  addAges() %>%
  group_by(age) %>%
  summarize(
    n = n(),
    mean_tripduration = mean(tripduration)
  ) %>%
  ggplot(aes(
    x = age,
    y = n,
    fill = mean_tripduration
  )) +
  geom_col() +
  labs(
    x = "Age",
    y = "Number of Rides",
    fill = "Mean Trip Duration (s)",
    title = "Young Riders Dominate",
    subtitle = "Hubway is most used by people between the ages of 20 and 40"
  )
ggsave("rides_by_age.pdf")

## Time of Day

trips %>%
  addTimes() %>%
  mutate(
    start_hour = update(starttime, yday = 1),
    stop_hour = update(stoptime, yday = 1)
  ) %>%
  ggplot(aes(
    x = start_hour
  )) +
  geom_histogram(
    bins = 192
  )

## Day of Week
trips %>%
  addTimes() %>%
  mutate(
    day_of_week = wday(starttime, label = TRUE)
  ) %>%
  ggplot(aes(
    x = day_of_week
  )) +
  geom_bar()

trips %>%
  addTimes() %>%
  mutate(
    start_hour = update(starttime, yday = 1),
    stop_hour = update(stoptime, yday = 1),
    day_of_week = wday(starttime, label = TRUE)
  ) %>%
  ggplot(aes(
    x = start_hour,
    color = day_of_week
  )) +
  geom_freqpoly(bins = 48) +
  labs(
    x = "Trip Start",
    y = "Number of Rides",
    title = "Weekends and Weekdays Differ",
    subtitle = "Weekdays show clear commute spikes; weekends higher afternoon usage"
  )
ggsave("times_by_day_of_week.pdf")


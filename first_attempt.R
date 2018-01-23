# a first attempt at loading and analyzing data with R.
# gregmarra, Jan 2017

library(dplyr)
library(ggplot2)

setwd('~/codez/hubway/')

trips <- read.csv('data/201706-hubway-tripdata.csv')

addAges <- function(df) {
  return (df %>%
    mutate(age=as.numeric(as.character(birth.year))) %>%
    filter(!is.na(age)) %>%
    mutate(age = 2017 - age) %>%
    filter(age < 100))
}

by_age <- group_by(addAges(trips), age)

summary <- summarize(by_age, avg_duration = mean(tripduration), n = n())

qplot(age,
      n,
      color = avg_duration,
      data = summary,
      xlab = "Age",
      ylab = "Number of Rides",
      main = "Hubway is most used by people between the ages of 20 and 40")

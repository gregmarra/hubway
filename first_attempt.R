# a first attempt at loading and analyzing data with R.
# gregmarra, Jan 2017

library(dplyr)
library(ggplot2)

setwd('~/codez/hubway/')

trips <- read.csv('data/201706-hubway-tripdata.csv', stringsAsFactors=FALSE)

addAges <- function(df) {
  return (df %>%
    filter(birth.year != '\\N') %>%
    mutate(age = 2017 - as.numeric(birth.year)) %>%
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


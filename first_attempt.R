# a first attempt at loading and analyzing data with R.
# gregmarra, Jan 2017

library(dplyr)
library(ggplot2)

trips <- read.csv('data/201706-hubway-tripdata.csv', stringsAsFactors=FALSE)

addAges <- function(df) {
  return (df %>%
    filter(birth.year != '\\N') %>%
    mutate(age = 2017 - as.numeric(birth.year)) %>%
    filter(age < 100))
}

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
ggsave("./output/rides_by_age.pdf")


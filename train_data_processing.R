library(tidyverse)
library(lubridate)
library(sf)

test <- read_csv('train_reliability/2022-06.csv')
stop_times <- read_csv('train_reliability/stop_times.txt') %>%
  filter(stop_id == 30198)


test_6_25 <- test %>% 
  mutate(
    day = date(Arrival_Time)
  ) %>%
  filter(
    day == "2022-06-25"
    & `Station_Name` == "Logan Square"
  )

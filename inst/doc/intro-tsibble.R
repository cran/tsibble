## ----initial, echo = FALSE, cache = FALSE, results = 'hide'--------------
knitr::opts_chunk$set(
  warning = FALSE, message = FALSE, echo = TRUE,
  fig.width = 7, fig.height = 6, fig.align = 'centre',
  comment = "#>"
)
options(tibble.print_min = 5)

## ----weather-------------------------------------------------------------
library(tsibble)
library(lubridate)
weather <- nycflights13::weather %>% 
  select(origin, time_hour, temp, humid, precip)
weather

## ----weather-ts, message = TRUE------------------------------------------
weather_tsbl <- as_tsibble(weather, key = id(origin))
weather_tsbl

## ----weather-tsum--------------------------------------------------------
weather_tsbl %>%
  group_by(origin) %>%
  tsummarise(
    date = as_date(time_hour),
    temp_high = max(temp, na.rm = TRUE),
    temp_low = min(temp, na.rm = TRUE)
  )

## ----tourism-------------------------------------------------------------
as_tsibble(tourism, key = id(Region | State, Purpose), index = Quarter)

## ----tourism-sum---------------------------------------------------------
tourism %>%
  group_by(Region | State) %>%
  summarise(Geo_Trips = sum(Trips))

## ----flights-------------------------------------------------------------
flights <- nycflights13::flights %>%
  mutate(sched_date_time = time_hour + minutes(minute)) %>%
  select(
    flight, origin, dest, sched_date_time, 
    dep_delay, arr_delay, air_time, distance
  )

## ----flights-ts----------------------------------------------------------
flights_tsbl <- flights %>%
  as_tsibble(
    key = id(flight, origin, dest), 
    index = sched_date_time, 
    regular = FALSE
  )
flights_tsbl


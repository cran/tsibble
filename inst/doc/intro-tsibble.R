## ----initial, echo = FALSE, cache = FALSE, results = 'hide'--------------
knitr::opts_chunk$set(
  warning = FALSE, message = FALSE, echo = TRUE,
  fig.width = 7, fig.height = 6, fig.align = 'centre',
  comment = "#>"
)
options(tibble.print_min = 5)
Sys.setenv(TZ = "")

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
  mutate(
    sched_dep_datetime = make_datetime(year, month, day, hour, minute, 0),
    flight_num = paste0(carrier, flight)
  )

## ----flights-ts----------------------------------------------------------
flights_tsbl <- flights %>%
  as_tsibble(
    key = id(flight_num), 
    index = sched_dep_datetime, 
    regular = FALSE
  )
flights_tsbl


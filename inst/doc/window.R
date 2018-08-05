## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(tibble.print_min = 5)

## ----full-data, message = FALSE------------------------------------------
library(tsibble)
library(dplyr)
pedestrian_full <- pedestrian %>% 
  fill_na(.full = TRUE)
pedestrian_full

## ----daily-mv------------------------------------------------------------
pedestrian_full %>% 
  group_by(Sensor) %>% 
  mutate(Daily_MA = slide_dbl(Count, 
    mean, na.rm = TRUE, .size = 24, .align = "center-left"
  ))

## ----monthly-mv-pre------------------------------------------------------
pedestrian_mth <- pedestrian_full %>% 
  mutate(YrMth = yearmonth(Date_Time)) %>% 
  nest(-Sensor, -YrMth)
pedestrian_mth

## ----monthly-mv----------------------------------------------------------
pedestrian_mth %>% 
  group_by(Sensor) %>% 
  # (1)
  # mutate(Monthly_MA = slide_dbl(data, 
  #   ~ mean(bind_rows(.)$Count, na.rm = TRUE), .size = 3, .align = "center"
  # ))
  # (2) equivalent to (1)
  mutate(Monthly_MA = slide_dbl(data, 
    ~ mean(.$Count, na.rm = TRUE), .size = 3, .align = "center", .bind = TRUE
  ))

## ----lm------------------------------------------------------------------
my_diag <- function(...) {
  data <- list(...)
  fit <- lm(Count ~ Time, data = data)
  list(fitted = fitted(fit), resid = residuals(fit))
}
pedestrian_full %>%
  filter(Date <= as.Date("2015-03-31")) %>%
  nest(-Sensor) %>%
  mutate(diag = purrr::map(data, ~ pslide_dfr(., my_diag, .size = 24 * 7)))


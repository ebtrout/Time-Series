# Setup
library(tidyverse)
library(rstudioapi)
library(tsibble)
library(fabletools)
library(scales)
library(fable)
library(forecast)
library(imputeTS)
library(feasts)


# I run all intensive scripts through a batch file, to hopefully save runtimes
# this is duplicate code from RMD

# Reading in the data
energy <- read.csv("C:/Users/emant/Desktop/MSA/AA 502 Analytics Methods/Time Series II/Homework1_TS2/hrl_load_metered.csv")
energyTest <- read.csv("C:/Users/emant/Desktop/MSA/AA 502 Analytics Methods/Time Series II/Homework1_TS2/hrl_load_metered - test1.csv")

# Duplicate Spring removal
energy <- energy %>% group_by(datetime_beginning_ept) %>% 
  summarize(mw = mean(mw, na.rm = T),.groups = "drop") %>% 
  rename("time" = datetime_beginning_ept) 

energyTest <- energyTest %>% 
  group_by(datetime_beginning_ept) %>% 
  summarize(mw = mean(mw, na.rm = T),.groups = "drop") %>% 
  rename("time" = datetime_beginning_ept) 

# Fixing time values 
energy$time <- as.POSIXct(energy$time,format = "%m/%d/%y %H:%M",tz = "America/New_York")

energyTest$time <- as.POSIXct(energyTest$time,format = "%m/%d/%y %H:%M",tz = "America/New_York")

# Converting to Tsibble and filling gaps

energy <- energy %>% as_tsibble(index = time) %>% fill_gaps()

energyTest <- energyTest %>% as_tsibble(index = time) %>% fill_gaps()

# Imputing fall
energy <- energy %>% na_interpolation(option = "spline")

energyTest <- energyTest %>% na_interpolation(option = "spline")

## DUMMY VARIABLE MODEL HELPERS

# Add set indicator for separating
energy <- energy %>% mutate(set = "train")
energyTest <- energyTest %>% mutate(set = "test")

# Add all helper variables and combine sets to ensure factor levels are present throughout
energyCombined <- bind_rows(energy,energyTest) %>% 
  mutate(month = factor(month(time))) %>%
  mutate(hour = factor(hour(time))) %>% 
  mutate(dayOfWeek = wday(time))

# Separate sets back out
energy <- energyCombined %>% filter(set == "train")
energyTest <- energyCombined %>% filter(set == "test")

# Deterministic models

## Fourier

# Fourier Transform models K = 1:10
time <- Sys.time()
print(time)
fourierModels <- energy %>% 
  model(
    dayMonthWeekYear1 = ARIMA(
      mw ~ pdq(d = 1) + PDQ(0,0,0) + 
        fourier(period = 24, K = 10) + 
        fourier(period = 24 * 30, K = 3) + 
        fourier(period = 24 * 7, K = 5) +
        fourier(period = 24 * 365, K = 3)
    ), 
    dayMonthWeekYear2 = ARIMA(
      mw ~ pdq(d = 1) + PDQ(0,0,0) + 
        fourier(period = 24, K = 12) + 
        fourier(period = 24 * 30, K = 5) + 
        fourier(period = 24 * 7, K = 7) +
        fourier(period = 24 * 365, K = 5)
    ), 
    dayMonthWeekYear3 = ARIMA(
      mw ~ pdq(d = 1) + PDQ(0,0,0) + 
        fourier(period = 24, K = 12) + 
        fourier(period = 24 * 30, K = 10) + 
        fourier(period = 24 * 7, K = 10) +
        fourier(period = 24 * 365, K = 10)
    ), 
    dayMonthYear2 = ARIMA(
      mw ~ pdq(d = 1) + PDQ(0,0,0) + 
        fourier(period = 24, K = 10) + 
        fourier(period = 24 * 30, K = 5) + 
        fourier(period = 24 * 365, K = 5)
    ),
    dayMonthYear3 = ARIMA(
      mw ~ pdq(d = 1) + PDQ(0,0,0) + 
        fourier(period = 24, K = 10) + 
        fourier(period = 24 * 30, K = 8) + 
        fourier(period = 24 * 365, K = 8)
    ),
    dayMonthYear4 = ARIMA(
      mw ~ pdq(d = 1) + PDQ(0,0,0) + 
        fourier(period = 24, K = 10) + 
        fourier(period = 24 * 30, K = 10) + 
        fourier(period = 24 * 365, K = 10)
    ), 
    dayMonthYear1 = ARIMA(
      mw ~ pdq(d = 1) + PDQ(0,0,0) + 
        fourier(period = 24, K = 12) + 
        fourier(period = 24 * 30, K = 3) + 
        fourier(period = 24 * 365, K = 3)
    ), 
    dayMonthYear5 = ARIMA(
      mw ~ pdq(d = 1) + PDQ(0,0,0) + 
        fourier(period = 24, K = 12) + 
        fourier(period = 24 * 30, K = 12) + 
        fourier(period = 24 * 365, K = 12)
    )
  )

timeDiff <- Sys.time() - time

save(fourierModels,file = "fourierModels.RData")
save(timeDiff,file = "time")
print(timeDiff)


load("C:/Users/emant/Desktop/MSA/AA 502 Analytics Methods/Time Series II/Homework1_TS2/fourierModels.RData")

library(tidyverse)
library(rstudioapi)
library(tsibble)
library(fabletools)
library(scales)
library(fable)
library(forecast)
library(imputeTS)
library(here)
library(rprojroot)
library(feasts)


# I run all intensive scripts through a batch file, to hopefully save runtimes
# this is duplicate code from RMD

# Setup




setwd("C:/Users/athen/Desktop/Homework1_TS2")

print("data reading")
# Reading in the data
energy <- read.csv("data/hrl_load_metered.csv")
energyTest <- read.csv("data/hrl_load_metered - test1.csv")

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
  mutate(week = factor(week(time))) %>%
  mutate(dayOfWeek = factor(wday(time))) %>% 
  mutate(year = factor(year(time))) %>% 
  mutate(day = factor(day(time)))

# Separate sets back out
energy <- energyCombined %>% filter(set == "train")
energyTest <- energyCombined %>% filter(set == "test")

time <- Sys.time()
# Model 2
dummyArima2 <- energy %>% 
  model(
    HourMonthdayOfWeekSearch = ARIMA(
      mw ~ 1 + hour + month + dayOfWeek + 
        pdq(d = 1) + PDQ(D = 0)
    )
  )

print("dummyArima2 Saved")
print(Sys.time() - time)
save(dummyArima2,file = "dummyArima2.RData")


dummyArima3 <- energy %>% 
  model(
    HourMonth1 = ARIMA(
      mw ~ 1 + hour + month + 
        pdq(2,1,3) + PDQ(2,0,2)
    ),
    HourMonth2 = ARIMA(
      mw ~ 1 + hour + month + 
        pdq(2,1,3) + PDQ(2,0,5)
    ),
    HourMonthdayOfWeek1 = ARIMA(
      mw ~ 1 + hour + month + dayOfWeek + 
        pdq(3,1,2) + PDQ(2,0,2)
    ),
    HourMonthdayOfWeek2 = ARIMA(
      mw ~ 1 + hour + month + dayOfWeek + 
        pdq(3,1,2) + PDQ(2,0,5)
    )
  )

print("dummyArima3 Saved")
print(Sys.time() - time)
save(dummyArima3,file = "dummyArima3.RData")

library(tidyverse)
library(datasets)
library(fpp3)
library(tools)
library(stringr)


airquality2 <- airquality %>% mutate(date= lubridate::make_date(1973,Month,Day))

air <- read.csv("usairlines.csv")

air.ts<-air %>% mutate(date=mdy(paste(Month, "1",Year))) %>% 
  mutate(Month.ts = yearmonth(date)) %>% 
  as_tsibble(index = Month.ts)

count_gaps(air.ts)

autoplot(air.ts, Passengers)

min(air.ts$Passengers)

energy <- read.csv("energy_F2024.csv")




energy2 <- energy %>% mutate(Date = date(mdy_hms(datetime_beginning_ept))) %>% group_by(Date) %>% summarize(sum = sum(mw))


energy2

max(energy2$sum)


a <- as_tsibble(energy2,index = (Date))

b <- a[c(-10:-100,-160:-150),]

count_gaps(b)



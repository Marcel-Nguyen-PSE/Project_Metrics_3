### --- DATA PREP 1 --- ###

# --- Load packages ---

library(tidyverse)
library(readxl)
library(rio)
library(xtable)
library(here)
library(gtsummary)
library(glue)
library(scales)
library(patchwork)
library(stargazer)
library(sandwich)
library(lmtest)
library(AER)
library(car)
library(haven)
library(fixest) 
library(sf)
library(did)
library(rdrobust)
library(TwoWayFEWeights)
library(Synth)
library(fredr)
library(tseries)
library(urca)
library(vars)
library(forecast)
library(typstable)
library(lubridate)
library(zoo)
library(gridExtra)
library(grid)

# --- FREDR API KEY ---

fred_key <- Sys.getenv('FRED_API_KEY')

fredr_set_key(fred_key)

# --- 1960-2000 US Macro Data Set ---

gdpd <- fredr(series_id = "GDPDEF",
              observation_start = as.Date('1955-01-01'),
              observation_end   = as.Date('2019-12-31'))

unrate <- fredr(series_id = "UNRATE",
                observation_start = as.Date('1955-01-01'),
                observation_end   = as.Date('2019-12-31'))

ffr <- fredr(series_id = "FEDFUNDS",
             observation_start = as.Date('1955-01-01'),
             observation_end   = as.Date('2019-12-31'))

# Quarterly averages for monthly series (as in the paper)

unrate_q <- unrate %>%
  mutate(date = floor_date(date, "quarter")) %>% 
  group_by(date) %>%
  summarise(u = mean(value), .groups = "drop")

ffr_q <- ffr %>%
  mutate(date = floor_date(date, "quarter")) %>%
  group_by(date) %>%
  summarise(r = mean(value), .groups = "drop")

# --- Final Data Set ---

macro <- gdpd %>%
  transmute(date, gdpd = value) %>%
  left_join(unrate_q, by = "date") %>%
  left_join(ffr_q, by = "date") %>%
  arrange(date) %>%
  mutate(p = 400 * log(gdpd / lag(gdpd))) %>%    
  dplyr::select(date, p, u, r)

macro_1960_2000 <- macro %>%
  filter(date >= as.Date("1960-01-01"),
         date <  as.Date("2001-01-01")) %>%     
  dplyr::select(p,u,r)



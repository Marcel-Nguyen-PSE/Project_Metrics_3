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


fred_key <- Sys.getenv('FRED_API_KEY')

fredr_set_key(fred_key)
######################################################### Main dataset using fred package and monthly obs.


### US

gdpd <- fredr(series_id = "GDPDEF",
              observation_start = as.Date('1960-01-01'),
              observation_end   = as.Date('2019-12-31'))

unrate <- fredr(series_id = "UNRATE",
                observation_start = as.Date('1960-01-01'),
                observation_end   = as.Date('2019-12-31'))

ffr <- fredr(series_id = "FEDFUNDS",
             observation_start = as.Date('1960-01-01'),
             observation_end   = as.Date('2019-12-31'))

# Quarterly averages for monthly series 

unrate_q <- unrate %>%
  mutate(date = floor_date(date, "quarter")) %>% 
  group_by(date) %>%
  summarise(u = mean(value), .groups = "drop")

ffr_q <- ffr %>%
  mutate(date = floor_date(date, "quarter")) %>%
  group_by(date) %>%
  summarise(r = mean(value), .groups = "drop")

# final data set : 3 VAR variables

macro <- gdpd %>%
  transmute(date, gdpd = value) %>%
  left_join(unrate_q, by = "date") %>%
  left_join(ffr_q, by = "date") %>%
  arrange(date) %>%
  mutate(p = 400 * log(gdpd / lag(gdpd))) %>%    
  dplyr::select(date, p, u, r)

### Inflation variable 

macro_us <- macro %>%
  filter(date >= as.Date("1960-01-01"),
         date <  as.Date("2019-01-01")) %>%     
  dplyr::select(date, p,u,r)


### Canada 

gdpd_can <- read_csv("/Users/marcel/Documents/GitHub/Project_Metrics_3/Extension/Extension_1/Can_GDP_Delator_Ind2017.csv") %>%
  mutate(date = as.Date(observation_date))

unrate_can <- fredr(
  series_id = "LRUNTTTTCAM156S",
  observation_start = as.Date("1961-01-01"),
  observation_end   = as.Date("2019-12-31")
)

ffr_can <- fredr(
  series_id = "IRSTCB01CAM156N",
  observation_start = as.Date("1961-01-01"),
  observation_end   = as.Date("2019-12-31")
)

exr_can <- fredr(
  series_id = "CCUSMA02CAM618N",
  observation_start = as.Date("1961-01-01"),
  observation_end   = as.Date("2019-12-31")
)

# Quarterly averages for monthly series

unrate_q_can <- unrate_can %>%
  mutate(date = floor_date(date, "quarter")) %>%
  group_by(date) %>%
  summarise(u_can = mean(value, na.rm = TRUE), .groups = "drop")

exr_q_can <- unrate_can %>%
  mutate(date = floor_date(date, "quarter")) %>%
  group_by(date) %>%
  summarise(exr_can = mean(value, na.rm = TRUE), .groups = "drop")

ffr_q_can <- ffr_can %>%
  mutate(date = floor_date(date, "quarter")) %>%
  group_by(date) %>%
  summarise(r_can = mean(value, na.rm = TRUE), .groups = "drop")

macro_can <- gdpd_can %>%
  transmute(
    date = as.Date(date),
    gdpd_can = CANGDPDEFQISMEI_NBD20170101
  ) %>%
  left_join(unrate_q_can, by = "date") %>%
  left_join(ffr_q_can, by = "date") %>%
  left_join(exr_q_can, by ='date') %>%
  arrange(date) %>%
  mutate(
    p_can = 400 * log(gdpd_can / lag(gdpd_can))
  ) %>%
  dplyr::select(date, p_can, u_can, r_can) %>%
  filter(date <= as.Date('2019-12-31'))

macro_can_us <- macro_can %>%
  right_join(macro_us, by = 'date')

### Mexico 

gdpd_mex <- read_csv('/Users/marcel/Documents/GitHub/Project_Metrics_3/Extension/Extension_1/Mex_GDP_Deflator_Ind2017.csv') %>%
  mutate(date = as.Date(observation_date))

unrate_mex <- fredr(
  series_id = "LRUNTTTTMXM156S",
  observation_start = as.Date("1997-01-01"),
  observation_end   = as.Date("2019-12-31")
)

ffr_mex <- fredr(
  series_id = "IR3TIB01MXM156N",
  observation_start = as.Date("1997-01-01"),
  observation_end   = as.Date("2019-12-31")
)

exr_mex <- fredr(
  series_id = "CCUSMA02MXM618N",
  observation_start = as.Date("1997-01-01"),
  observation_end   = as.Date("2019-12-31")
)

# Quarterly averages for monthly series

unrate_q_mex <- unrate_mex %>%
  mutate(date = floor_date(date, "quarter")) %>%
  group_by(date) %>%
  summarise(u_mex = mean(value, na.rm = TRUE), .groups = "drop")

exr_q_mex <- unrate_mex %>%
  mutate(date = floor_date(date, "quarter")) %>%
  group_by(date) %>%
  summarise(exr_mex = mean(value, na.rm = TRUE), .groups = "drop")

ffr_q_mex <- ffr_mex %>%
  mutate(date = floor_date(date, "quarter")) %>%
  group_by(date) %>%
  summarise(r_mex = mean(value, na.rm = TRUE), .groups = "drop")

macro_mex <- gdpd_mex %>%
  transmute(
    date = as.Date(date),
    gdpd_mex = NGDPDSAIXMXQ_NBD20170101
  ) %>%
  left_join(unrate_q_mex, by = "date") %>%
  left_join(ffr_q_mex, by = "date") %>%
  left_join(exr_q_mex, by ='date') %>%
  arrange(date) %>%
  mutate(
    p_mex = 400 * log(gdpd_mex / lag(gdpd_mex))
  ) %>%
  dplyr::select(date, p_mex, u_mex, r_mex) %>%
  filter(date <= as.Date('2019-12-31'))

macro_mex_us <- macro_mex %>%
  right_join(macro_us, by = 'date') %>%
  na.omit()

macro_mex_us_post2001 <- macro_mex %>%
  right_join(macro_us, by = 'date') %>%
  na.omit() %>%
  filter(date >= as.Date('2001-01-01'))

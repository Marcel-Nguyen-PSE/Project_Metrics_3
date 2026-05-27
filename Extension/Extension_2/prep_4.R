#########################################################
# Monthly dataset: CPI inflation, unemployment, Fed funds
#########################################################

cpi <- fredr(
  series_id = "CPIAUCSL",
  observation_start = as.Date("1955-01-01"),
  observation_end   = as.Date("2019-12-31")
)

unrate_monthly <- unrate %>%
  transmute(date, u = value)

ffr_monthly <- ffr %>%
  transmute(date, r = value)

macro_monthly <- cpi %>%
  transmute(date, cpi = value) %>%
  left_join(unrate_monthly, by = "date") %>%
  left_join(ffr_monthly, by = "date") %>%
  arrange(date) %>%
  mutate(p = 1200 * log(cpi / lag(cpi))) %>%     # monthly CPI growth × 12 × 100 ie p = annualized monthly inflation, in % per year.
  dplyr::select(date, p, u, r) %>%
  drop_na()

#subsamples pre and post crisis

  macro_monthly_precrisis <- macro_monthly %>%
  filter(date >= as.Date("1984-01-01"),
         date <  as.Date("2008-01-01")) %>%
  dplyr::select(p, u, r)

macro_monthly_postcrisis <- macro_monthly %>%
  filter(date >= as.Date("2010-01-01"),
         date <  as.Date("2020-01-01")) %>%
  dplyr::select(p, u, r)
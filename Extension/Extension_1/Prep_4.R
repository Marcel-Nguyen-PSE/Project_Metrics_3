### --- EXTENSION 1 : SPILLOVER EFFECTS ON CAN & MEX DATA PREP --- 
# (PREP_1.R and PREP_2.R and Prep_3.R have to be ran first)

# --- Data Prep for US 

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

# Final data set : 3 VAR variables

macro <- gdpd %>%
  transmute(date, gdpd = value) %>%
  left_join(unrate_q, by = "date") %>%
  left_join(ffr_q, by = "date") %>%
  arrange(date) %>%
  mutate(p = 400 * log(gdpd / lag(gdpd))) %>%    
  dplyr::select(date, p, u, r)

# Inflation variable 

macro_us <- macro %>%
  filter(date >= as.Date("1960-01-01"),
         date <  as.Date("2019-01-01")) %>%     
  dplyr::select(date, p,u,r)

# --- Data Prep for Canada and Mexico

# Canada

gdpd_can <- read_csv("Extension/Extension_1/Data/Can_GDP_Delator_Ind2017.csv") %>%
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

# Format to quarter averages

unrate_q_can <- unrate_can %>%
  mutate(date = floor_date(date, "quarter")) %>%
  group_by(date) %>%
  summarise(u_can = mean(value, na.rm = TRUE), .groups = "drop")

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
  arrange(date) %>%
  mutate(
    p_can = 400 * log(gdpd_can / lag(gdpd_can))
  ) %>%
  dplyr::select(date, p_can, u_can, r_can) %>%
  filter(date <= as.Date('2019-12-31'))

macro_can_us <- macro_can %>%
  right_join(macro_us, by = 'date')

# Mexico 

gdpd_mex <- read_csv('Extension/Extension_1/Data/Mex_GDP_Deflator_Ind2017.csv') %>%
  mutate(date = as.Date(observation_date))

unrate_mex <- fredr(
  series_id = "LRHUTTTTMXM156S",
  observation_start = as.Date("1990-01-01"),
  observation_end   = as.Date("2019-12-31")
)

ffr_mex <- fredr(
  series_id = "IRSTCI01MXQ156N",
  observation_start = as.Date("1990-01-01"),
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

# Merge table

macro_mex_us <- macro_mex %>%
  right_join(macro_us, by = 'date') %>%
  na.omit()

# Filter post 1990

macro_mex_us_post2000 <- macro_mex %>%
  right_join(macro_us, by = 'date') %>%
  na.omit() %>%
  filter(date >= as.Date('1990-01-01'))

# --- Trend plots of MEX variables ---

plot_mex_p <- ggplot(macro_mex, aes(x = date)) + geom_line(aes(y = p_mex)) + theme_minimal()
plot_mex_p
plot_mex_u <- ggplot(macro_mex, aes(x = date)) + geom_line(aes(y = u_mex)) + theme_minimal()
plot_mex_u
plot_mex_r <- ggplot(macro_mex, aes(x = date)) + geom_line(aes(y = r_mex)) + theme_minimal()
plot_mex_r

jpeg(
  "Extension/Extension_1/Figures/plot_mex_var.jpeg",
  width = 2400,
  height = 3200,
  res = 200
)

grid.arrange(
  plot_mex_p,
  plot_mex_u,
  plot_mex_r,
  ncol = 1
)

dev.off()

# --- Data prep for Mexico in months ---

cpi_mex_monthly <- fredr(
  series_id = "CPALTT01MXM657N",
  observation_start = as.Date("1990-01-01"),
  observation_end   = as.Date("2019-12-31")
) %>%
  rename(p_monthly = 'value') %>%
  dplyr::select(date, p_monthly)

u_mex_monthly <- read_csv('Extension/Extension_1/Data/LRHUTTTTMXM156S.csv')%>%
  rename(u_monthly = 'LRHUTTTTMXM156S') %>%
  mutate(date = as.Date(observation_date)) %>%
  dplyr::select(-observation_date)

r_mex_monthly <- fredr(
  series_id = "IRSTCI01MXM156N",
  observation_start = as.Date("1990-01-01"),
  observation_end   = as.Date("2019-12-31")
) %>%
  rename(r_monthly = 'value') %>%
  dplyr::select(date, r_monthly)

macro_mex_monthly <- cpi_mex_monthly %>%
  left_join(u_mex_monthly, by = 'date') %>%
  left_join(r_mex_monthly, by = 'date')

ffr_2000 <- ffr %>%
  filter(date >= as.Date('1990-01-01'))

macro_mex_us_monthly <- macro_mex_monthly %>%
  left_join(ffr_2000, by = 'date') %>%
  rename(r_us = 'value') %>%
  dplyr::select(-series_id, -realtime_start, -realtime_end)




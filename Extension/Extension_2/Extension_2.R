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
  mutate(p = 1200 * log(cpi / lag(cpi))) %>%
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

# stationarity tests

run_stationarity <- function(data) {
  
  stationarity <- data.frame(
    variable = colnames(data),
    PP_stat = NA,
    PP_pvalue = NA,
    DFGLS_stat = NA,
    DFGLS_cv5 = NA,
    KPSS_stat = NA,
    KPSS_pvalue = NA
  )
  
  for (j in seq_along(colnames(data))) {
    
    y <- data[[j]]
    
    pp <- pp.test(y, alternative = "stationary")
    dfgls <- ur.ers(y, type = "DF-GLS", model = "constant")
    kpss <- kpss.test(y, null = "Level")
    
    stationarity$PP_stat[j] <- pp$statistic
    stationarity$PP_pvalue[j] <- pp$p.value
    
    stationarity$DFGLS_stat[j] <- dfgls@teststat
    stationarity$DFGLS_cv5[j] <- dfgls@cval[1, "5pct"]
    
    stationarity$KPSS_stat[j] <- kpss$statistic
    stationarity$KPSS_pvalue[j] <- kpss$p.value
  }
  
  stationarity %>%
    mutate(across(where(is.numeric), ~ round(.x, 3)))
}

stationarity_precrisis <- run_stationarity(macro_monthly_precrisis)
stationarity_postcrisis <- run_stationarity(macro_monthly_postcrisis)

stationarity_precrisis
stationarity_postcrisis

#lag order selection, test up to 12 lags

lag_selection_precrisis <- VARselect(
  y = macro_monthly_precrisis,
  lag.max = 12,
  type = "const"
)

lag_selection_postcrisis <- VARselect(
  y = macro_monthly_postcrisis,
  lag.max = 12,
  type = "const"
)

lag_selection_precrisis$selection
lag_selection_precrisis$criteria

lag_selection_postcrisis$selection
lag_selection_postcrisis$criteria

#portmanteau residual test by lag

run_portmanteau_by_lag <- function(data, max_lag = 12, lags_pt = 24) {
  
  pt_table <- data.frame(
    lag = 1:max_lag,
    PT_pvalue = NA
  )
  
  for (p_lag in 1:max_lag) {
    
    fit <- VAR(
      y = data,
      p = p_lag,
      type = "const"
    )
    
    test <- serial.test(
      fit,
      lags.pt = lags_pt,
      type = "PT.asymptotic"
    )
    
    pt_table$PT_pvalue[p_lag] <- test$serial$p.value
  }
  
  pt_table %>%
    mutate(PT_pvalue = round(PT_pvalue, 5))
}

pt_precrisis <- run_portmanteau_by_lag(macro_monthly_precrisis)
pt_postcrisis <- run_portmanteau_by_lag(macro_monthly_postcrisis)

pt_precrisis
pt_postcrisis

#stability roots for selected lags

var_precrisis <- VAR(
  y = macro_monthly_precrisis,
  p = 2,
  type = "const"
)

var_postcrisis <- VAR(
  y = macro_monthly_postcrisis,
  p = 4,
  type = "const"
)

max(roots(var_precrisis, modulus = TRUE))
max(roots(var_postcrisis, modulus = TRUE))

# If max root < 1: VAR is stable
# If max root close to 1: VAR is stable but highly persistent

roots_precrisis <- data.frame(
  max_root = round(
    max(roots(var_precrisis, modulus = TRUE)),
    3
  )
)

roots_postcrisis <- data.frame(
  max_root = round(
    max(roots(var_postcrisis, modulus = TRUE)),
    3
  )
)

#########################################################
# Export tables for Typst
#########################################################


dir.create("Extension/PrePost/Tables/PNG",
           recursive = TRUE,
           showWarnings = FALSE)

export_png_table <- function(table_obj, filename,
                             width = 2200,
                             height = 800) {
  
  table_obj <- as.data.frame(table_obj)
  rownames(table_obj) <- NULL
  
  png(
    paste0("Extension/PrePost/Tables/PNG/", filename, ".png"),
    width = width,
    height = height,
    res = 180
  )
  
  grid.table(table_obj, rows = NULL)
  
  dev.off()
}

#stationarity tables

export_png_table(
  stationarity_precrisis,
  "stationarity_precrisis",
  width = 2400,
  height = 600
)

export_png_table(
  stationarity_postcrisis,
  "stationarity_postcrisis",
  width = 2400,
  height = 600
)

#joint portmanteau tables

pt_compare <- pt_precrisis %>%
  rename(PT_precrisis = PT_pvalue) %>%
  left_join(
    pt_postcrisis %>%
      rename(PT_postcrisis = PT_pvalue),
    by = "lag"
  )

export_png_table(
  pt_compare,
  "portmanteau_pre_post",
  width = 1400,
  height = 900
)

#joint root tables

roots_compare <- data.frame(
  sample = c("Pre-crisis VAR(2)", "Post-crisis VAR(4)"),
  max_root = c(
    round(max(roots(var_precrisis, modulus = TRUE)), 3),
    round(max(roots(var_postcrisis, modulus = TRUE)), 3)
  )
)

export_png_table(
  roots_compare,
  "roots_compare",
  width = 1200,
  height = 450
)

#separate lag selection tables

lag_selection_precrisis_out <- cbind(
  criterion = rownames(lag_selection_precrisis$criteria),
  round(as.data.frame(lag_selection_precrisis$criteria), 3)
)

lag_selection_postcrisis_out <- cbind(
  criterion = rownames(lag_selection_postcrisis$criteria),
  round(as.data.frame(lag_selection_postcrisis$criteria), 3)
)

export_png_table(
  lag_selection_precrisis_out,
  "lag_selection_precrisis",
  width = 3000,
  height = 700
)

export_png_table(
  lag_selection_postcrisis_out,
  "lag_selection_postcrisis",
  width = 3000,
  height = 700
)


#########################################################
# Benchmark VARs: pre- and post-crisis
#########################################################

var_precrisis <- VAR(
  y = macro_monthly_precrisis,
  p = 2,
  type = "const"
)

var_postcrisis <- VAR(
  y = macro_monthly_postcrisis,
  p = 4,
  type = "const"
)

#########################################################
# IRFs: monetary-policy shock
#########################################################

irf_precrisis <- irf(
  var_precrisis,
  impulse = "r",
  response = c("p", "u", "r"),
  n.ahead = 24,
  ortho = TRUE,
  boot = TRUE,
  runs = 1000,
  ci = 0.66
)

irf_postcrisis <- irf(
  var_postcrisis,
  impulse = "r",
  response = c("p", "u", "r"),
  n.ahead = 24,
  ortho = TRUE,
  boot = TRUE,
  runs = 1000,
  ci = 0.66
)

#########################################################
# Export IRFs
#########################################################

dir.create(
  "Extension/PrePost/Figures",
  recursive = TRUE,
  showWarnings = FALSE
)

jpeg(
  "Extension/PrePost/Figures/irf_precrisis.jpeg",
  width = 1800,
  height = 900,
  res = 150
)

plot(irf_precrisis)

dev.off()

jpeg(
  "Extension/PrePost/Figures/irf_postcrisis.jpeg",
  width = 1800,
  height = 900,
  res = 150
)

plot(irf_postcrisis)

dev.off()
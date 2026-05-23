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

export_png_table <- function(table_obj, filename,
                             width = 2200,
                             height = 800) {
  
  table_obj <- as.data.frame(table_obj)
  rownames(table_obj) <- NULL
  
  png(
    paste0("Extension/Extension_2/Figures/", filename, ".png"),
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
# Side-by-side IRFs: pre-crisis vs post-crisis response to monetary-policy shock IRF
#########################################################

dir.create(
  "Extension/Extension_2/Figures",
  recursive = TRUE,
  showWarnings = FALSE
)

jpeg(
  "Extension/Extension_2/Figures/irf_pre_post_comparison.jpeg",
  width = 2400,
  height = 1800,
  res = 200
)

h <- 0:24

par(mfrow = c(3, 2), mar = c(4, 4, 3, 1))

responses <- c("p", "u", "r")
titles <- c(
  "Inflation response",
  "Unemployment response",
  "Interest rate response"
)

for (i in seq_along(responses)) {
  
  response <- responses[i]
  
  ylim_common <- range(
    irf_precrisis$irf$r[, response],
    irf_precrisis$Lower$r[, response],
    irf_precrisis$Upper$r[, response],
    irf_postcrisis$irf$r[, response],
    irf_postcrisis$Lower$r[, response],
    irf_postcrisis$Upper$r[, response]
  )
  
  plot(
    h,
    irf_precrisis$irf$r[, response],
    type = "l",
    lwd = 2,
    ylim = ylim_common,
    main = paste(titles[i], "- Pre-crisis"),
    xlab = "Months",
    ylab = "Response"
  )
  lines(h, irf_precrisis$Lower$r[, response], lty = 2, col = "red")
  lines(h, irf_precrisis$Upper$r[, response], lty = 2, col = "red")
  abline(h = 0, col = "red")
  
  plot(
    h,
    irf_postcrisis$irf$r[, response],
    type = "l",
    lwd = 2,
    ylim = ylim_common,
    main = paste(titles[i], "- Post-crisis"),
    xlab = "Months",
    ylab = "Response"
  )
  lines(h, irf_postcrisis$Lower$r[, response], lty = 2, col = "red")
  lines(h, irf_postcrisis$Upper$r[, response], lty = 2, col = "red")
  abline(h = 0, col = "red")
}

dev.off()

#for complement : full irfs

irf_precrisis_full <- irf(
  var_precrisis,
  impulse = c("p", "u", "r"),
  response = c("p", "u", "r"),
  n.ahead = 24,
  ortho = TRUE,
  boot = TRUE,
  runs = 1000,
  ci = 0.66
)

irf_postcrisis_full <- irf(
  var_postcrisis,
  impulse = c("p", "u", "r"),
  response = c("p", "u", "r"),
  n.ahead = 24,
  ortho = TRUE,
  boot = TRUE,
  runs = 1000,
  ci = 0.66
)

jpeg(
  "Extension/Extension_2/Figures/irf_precrisis_full.jpeg",
  width = 1800,
  height = 1800,
  res = 150
)

plot(irf_precrisis_full, plot.type = "single")

dev.off()

jpeg(
  "Extension/Extension_2/Figures/irf_postcrisis_full.jpeg",
  width = 1800,
  height = 1800,
  res = 150
)

plot(irf_postcrisis_full, plot.type = "single")

dev.off()

#########################################################
# Full IRFs: pre-crisis and post-crisis
#########################################################

irf_precrisis_full <- irf(
  var_precrisis,
  impulse = c("p", "u", "r"),
  response = c("p", "u", "r"),
  n.ahead = 24,
  ortho = TRUE,
  boot = TRUE,
  runs = 1000,
  ci = 0.66
)

irf_postcrisis_full <- irf(
  var_postcrisis,
  impulse = c("p", "u", "r"),
  response = c("p", "u", "r"),
  n.ahead = 24,
  ortho = TRUE,
  boot = TRUE,
  runs = 1000,
  ci = 0.66
)

dir.create(
  "Extension/Extension_2/Figures",
  recursive = TRUE,
  showWarnings = FALSE
)

jpeg(
  "Extension/Extension_2/Figures/irf_precrisis_full.jpeg",
  width = 1800,
  height = 1800,
  res = 150
)

par(mfrow = c(3, 3), ask = FALSE)

plot(irf_precrisis_full, plot.type = "single", ask = FALSE)

dev.off()

jpeg(
  "Extension/Extension_2/Figures/irf_postcrisis_full.jpeg",
  width = 1800,
  height = 1800,
  res = 150
)

par(mfrow = c(3, 3), ask = FALSE)

plot(irf_postcrisis_full, plot.type = "single", ask = FALSE)

dev.off()



#########################################################
# Backward-looking Taylor rule: pre/post crisis extension
#########################################################

# Taylor-rule coefficients
# p is already annualized monthly inflation: p = 1200 * log(cpi / lag(cpi))
# Therefore we do NOT divide by 12.

fp_back <- 1.5
fu_back <- 0.5 * (-2.5)

#########################################################
# Construct Taylor-rule monetary-policy residual ra
#########################################################

# Theoritical Taylor rule: r = fp_back*p + fu_back*u + ra
# So we should have : ra = r - fp_back*p - fu_back*u 
# but to keep consistency with formula of figure 3 in the replication, we write:
# ra = r + fp_back*p - fu_back*u

taylor_backward_precrisis <- macro_monthly_precrisis %>%
  mutate(
    ra = r + fp_back * p - fu_back * u
  ) %>%
  drop_na() %>%
  dplyr::select(ra, p, u)

taylor_backward_postcrisis <- macro_monthly_postcrisis %>%
  mutate(
    ra = r + fp_back * p - fu_back * u
  ) %>%
  drop_na() %>%
  dplyr::select(ra, p, u)

#########################################################
# Estimate VARs
#########################################################

var_back_precrisis <- VAR(
  y = taylor_backward_precrisis,
  p = 2,
  type = "const"
)

var_back_postcrisis <- VAR(
  y = taylor_backward_postcrisis,
  p = 4,
  type = "const"
)

#########################################################
# Compute IRFs to Taylor-rule monetary-policy shock
#########################################################

irf_back_precrisis <- irf(
  var_back_precrisis,
  impulse = "ra",
  response = c("ra", "p", "u"),
  n.ahead = 24,
  ortho = TRUE,
  boot = TRUE,
  runs = 500,
  ci = 0.66
)

irf_back_postcrisis <- irf(
  var_back_postcrisis,
  impulse = "ra",
  response = c("ra", "p", "u"),
  n.ahead = 24,
  ortho = TRUE,
  boot = TRUE,
  runs = 500,
  ci = 0.66
)

#########################################################
# Recover nominal and real interest-rate responses
#########################################################

recover_irf_backward <- function(irf_obj, fp, fu) {
  
  ra <- irf_obj$irf$ra[, "ra"]
  p  <- irf_obj$irf$ra[, "p"]
  u  <- irf_obj$irf$ra[, "u"]
  
  # Since ra = r + fp*p - fu*u even though not algebraically correct, we keep the initial nominal rate response :
  r_nominal <- ra + fp * p + fu * u
  
  # Real rate response:
  r_real <- r_nominal - p
  
  list(
    p = p,
    u = u,
    r_real = r_real
  )
}

res_back_precrisis <- recover_irf_backward(
  irf_back_precrisis,
  fp_back,
  fu_back
)

res_back_postcrisis <- recover_irf_backward(
  irf_back_postcrisis,
  fp_back,
  fu_back
)

#########################################################
# Export comparison plot
#########################################################

dir.create(
  "Extension/Extension_2/Figures",
  recursive = TRUE,
  showWarnings = FALSE
)

jpeg(
  "Extension/Extension_2/Figures/irf_taylor_backward_pre_post.jpeg",
  width = 1800,
  height = 1200,
  res = 150
)

horizon <- 0:24

par(mfrow = c(2, 2))

# Inflation
ylim_p <- range(res_back_precrisis$p, res_back_postcrisis$p)

plot(
  horizon,
  res_back_precrisis$p,
  type = "l",
  lwd = 2,
  ylim = ylim_p,
  main = "Inflation response",
  xlab = "Months",
  ylab = "Percent"
)

lines(horizon, res_back_postcrisis$p, lty = 2, lwd = 2)
abline(h = 0, col = "red")

legend(
  "topright",
  legend = c("Pre-crisis", "Post-crisis"),
  lty = c(1, 2),
  lwd = 2,
  bty = "n"
)

# Unemployment
ylim_u <- range(res_back_precrisis$u, res_back_postcrisis$u)

plot(
  horizon,
  res_back_precrisis$u,
  type = "l",
  lwd = 2,
  ylim = ylim_u,
  main = "Unemployment response",
  xlab = "Months",
  ylab = "Percent"
)

lines(horizon, res_back_postcrisis$u, lty = 2, lwd = 2)
abline(h = 0, col = "red")

# Real interest rate
ylim_r <- range(res_back_precrisis$r_real, res_back_postcrisis$r_real)

plot(
  horizon,
  res_back_precrisis$r_real,
  type = "l",
  lwd = 2,
  ylim = ylim_r,
  main = "Real interest-rate response",
  xlab = "Months",
  ylab = "Percent"
)

lines(horizon, res_back_postcrisis$r_real, lty = 2, lwd = 2)
abline(h = 0, col = "red")

dev.off()
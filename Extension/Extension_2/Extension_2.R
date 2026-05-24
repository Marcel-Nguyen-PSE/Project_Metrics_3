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


#########################################################
# Forward-looking Taylor rule: pre/post crisis extension k=12
#########################################################

# This computes forward-looking Taylor-rule coefficients from the VAR dynamics.
# Monthly data are used, so k = 12 corresponds to a one-year forward-looking horizon.

compute_forward_coefficients <- function(var_model, k = 12) {
  
  # Number of variables in the VAR
  n_var <- length(var_model$varresult)
  
  # Number of lags used in the VAR
  p_lag <- var_model$p
  
  # Size of the companion matrix
  n_state <- n_var * p_lag
  
  # Extract dynamic VAR coefficients only.
  # This excludes the constant term.
  A <- sapply(
    var_model$varresult,
    function(eq) coef(eq)[1:n_state]
  ) %>%
    t()
  
  # Build companion matrix.
  # Top block = estimated VAR dynamics.
  # Bottom block = mechanical lag shifting.
  Phi <- rbind(
    A,
    cbind(
      diag(n_state - n_var),
      matrix(0, n_state - n_var, n_var)
    )
  )
  
  # Compute average forecast operator:
  # F_k = (1/k) * sum_{i=1}^k Phi^i
  Phi_i <- diag(n_state)
  F_k <- matrix(0, n_state, n_state)
  
  for (i in 1:k) {
    Phi_i <- Phi %*% Phi_i
    F_k <- F_k + Phi_i
  }
  
  F_k <- F_k / k
  
  # Primitive Taylor-rule coefficients
  # p is already annualized monthly CPI inflation.
  fp0 <- 1.5
  fu0 <- 0.5 * (-2.5)
  
  # Dynamic effects of current p, u, and r on expected future p and u
  fp_raw <- fp0 * F_k[1, 1] + fu0 * F_k[2, 1]
  fu_raw <- fp0 * F_k[1, 2] + fu0 * F_k[2, 2]
  fr_raw <- fp0 * F_k[1, 3] + fu0 * F_k[2, 3]
  
  # Fixed-point correction because current r affects future expectations
  fp_fwd <- fp_raw / (1 - fr_raw)
  fu_fwd <- fu_raw / (1 - fr_raw)
  
  data.frame(
    fp_fwd = fp_fwd,
    fu_fwd = fu_fwd,
    fr_raw = fr_raw
  )
}

#coeffcients

coeff_fwd_precrisis <- compute_forward_coefficients(var_precrisis, k = 12)
coeff_fwd_postcrisis <- compute_forward_coefficients(var_postcrisis, k = 12)

#NB: diagnostic: check forward-looking coefficients

# If abs(1 - fr_raw) is close to zero, the fixed-point correction is unstable.
# This can produce implausibly large forward-looking Taylor-rule coefficients.
# Therefore, forward-looking Taylor-rule IRFs are treated only as exploratory robustness checks.
coeff_fwd_precrisis
coeff_fwd_postcrisis

1 - coeff_fwd_precrisis$fr_raw
1 - coeff_fwd_postcrisis$fr_raw

#observation : in fp_fwd = fp_raw / (1 - fr_raw)  ;  fu_fwd = fu_raw / (1 - fr_raw) the denominator is very close to zero and negative. That mechanically blows up the
# fixed-point correction => abs(1 - fr_raw) is small, the forward-looking correction is unstable.
#reason why fu_fwd = 13.55 => not credible for a Taylor-rule unemployment coefficient.

#########################################################
# Construct forward-looking Taylor-rule residuals
#########################################################

fp_fwd_pre <- coeff_fwd_precrisis$fp_fwd
fu_fwd_pre <- coeff_fwd_precrisis$fu_fwd

fp_fwd_post <- coeff_fwd_postcrisis$fp_fwd
fu_fwd_post <- coeff_fwd_postcrisis$fu_fwd

# Same forward-looking specification as Figure 3:
# ra = r - fp_fwd*p - fu_fwd*u

taylor_forward_precrisis <- macro_monthly_precrisis %>%
  mutate(
    ra = r - fp_fwd_pre * p - fu_fwd_pre * u
  ) %>%
  drop_na() %>%
  dplyr::select(ra, p, u)

taylor_forward_postcrisis <- macro_monthly_postcrisis %>%
  mutate(
    ra = r - fp_fwd_post * p - fu_fwd_post * u
  ) %>%
  drop_na() %>%
  dplyr::select(ra, p, u)

  #########################################################
# Estimate forward-looking Taylor-rule VARs
#########################################################

var_forw_precrisis <- VAR(
  y = taylor_forward_precrisis,
  p = 2,
  type = "const"
)

var_forw_postcrisis <- VAR(
  y = taylor_forward_postcrisis,
  p = 4,
  type = "const"
)

#########################################################
# IRFs to forward-looking Taylor-rule monetary shock
#########################################################

irf_forw_precrisis <- irf(
  var_forw_precrisis,
  impulse = "ra",
  response = c("ra", "p", "u"),
  n.ahead = 24,
  ortho = TRUE,
  boot = TRUE,
  runs = 500,
  ci = 0.66
)

irf_forw_postcrisis <- irf(
  var_forw_postcrisis,
  impulse = "ra",
  response = c("ra", "p", "u"),
  n.ahead = 24,
  ortho = TRUE,
  boot = TRUE,
  runs = 500,
  ci = 0.66
)

#########################################################
# Recover real interest-rate responses
#########################################################

recover_irf_forward <- function(irf_obj, fp, fu) {
  
  ra <- irf_obj$irf$ra[, "ra"]
  p  <- irf_obj$irf$ra[, "p"]
  u  <- irf_obj$irf$ra[, "u"]
  
  # Figure 3 forward-looking convention:
  # ra = r - fp*p - fu*u
  # so r = ra + fp*p + fu*u
  r_nominal <- ra + fp * p + fu * u
  
  # Real interest-rate response
  r_real <- r_nominal - p
  
  list(
    p = p,
    u = u,
    r_real = r_real
  )
}

res_forw_precrisis <- recover_irf_forward(
  irf_forw_precrisis,
  fp_fwd_pre,
  fu_fwd_pre
)

res_forw_postcrisis <- recover_irf_forward(
  irf_forw_postcrisis,
  fp_fwd_post,
  fu_fwd_post
)

#########################################################
# Export forward-looking Taylor-rule IRFs
#########################################################

jpeg(
  "Extension/Extension_2/Figures/irf_taylor_forward_pre_post.jpeg",
  width = 1800,
  height = 1200,
  res = 150
)

horizon <- 0:24
par(mfrow = c(2, 2))

plot(
  horizon, res_forw_precrisis$p,
  type = "l", lwd = 2,
  ylim = range(res_forw_precrisis$p, res_forw_postcrisis$p),
  main = "Inflation response",
  xlab = "Months", ylab = "Percent"
)
lines(horizon, res_forw_postcrisis$p, lty = 2, lwd = 2)
abline(h = 0, col = "red")
legend(
  "topright",
  legend = c("Pre-crisis", "Post-crisis"),
  lty = c(1, 2),
  lwd = 2,
  bty = "n"
)

plot(
  horizon, res_forw_precrisis$u,
  type = "l", lwd = 2,
  ylim = range(res_forw_precrisis$u, res_forw_postcrisis$u),
  main = "Unemployment response",
  xlab = "Months", ylab = "Percent"
)
lines(horizon, res_forw_postcrisis$u, lty = 2, lwd = 2)
abline(h = 0, col = "red")

plot(
  horizon, res_forw_precrisis$r_real,
  type = "l", lwd = 2,
  ylim = range(res_forw_precrisis$r_real, res_forw_postcrisis$r_real),
  main = "Real interest-rate response",
  xlab = "Months", ylab = "Percent"
)
lines(horizon, res_forw_postcrisis$r_real, lty = 2, lwd = 2)
abline(h = 0, col = "red")

dev.off()


#########################################################
# Optional robustness: forward-looking Taylor rule (k=4)
#########################################################
# Not retained in the main analysis because the fixed-point correction remains unstable and produces economically implausible coefficients.


compute_forward_coefficients <- function(var_model, k = 4) {
  
  n_var <- length(var_model$varresult)
  p_lag <- var_model$p
  n_state <- n_var * p_lag
  
  # Extract VAR lag coefficients, excluding the constant
  A <- sapply(
    var_model$varresult,
    function(eq) coef(eq)[1:n_state]
  ) %>%
    t()
  
  # Companion matrix
  Phi <- rbind(
    A,
    cbind(
      diag(n_state - n_var),
      matrix(0, n_state - n_var, n_var)
    )
  )
  
  # Average forecast operator: F_k = (1/k) sum Phi^i
  Phi_i <- diag(n_state)
  F_k <- matrix(0, n_state, n_state)
  
  for (i in 1:k) {
    Phi_i <- Phi %*% Phi_i
    F_k <- F_k + Phi_i
  }
  
  F_k <- F_k / k
  
  # Primitive Taylor-rule coefficients
  fp0 <- 1.5
  fu0 <- 0.5 * (-2.5)
  
  # Forward-looking coefficients
  fp_raw <- fp0 * F_k[1, 1] + fu0 * F_k[2, 1]
  fu_raw <- fp0 * F_k[1, 2] + fu0 * F_k[2, 2]
  fr_raw <- fp0 * F_k[1, 3] + fu0 * F_k[2, 3]
  
  fp_fwd <- fp_raw / (1 - fr_raw)
  fu_fwd <- fu_raw / (1 - fr_raw)
  
  data.frame(
    fp_fwd = fp_fwd,
    fu_fwd = fu_fwd,
    fr_raw = fr_raw,
    denominator = 1 - fr_raw
  )
}

#########################################################
# Compute coefficients
#########################################################

coeff_fwd_precrisis_k4 <- compute_forward_coefficients(
  var_precrisis,
  k = 4
)

coeff_fwd_postcrisis_k4 <- compute_forward_coefficients(
  var_postcrisis,
  k = 4
)

coeff_fwd_precrisis_k4
coeff_fwd_postcrisis_k4

#########################################################
# Construct forward-looking Taylor residuals
#########################################################

fp_fwd_pre_k4 <- coeff_fwd_precrisis_k4$fp_fwd
fu_fwd_pre_k4 <- coeff_fwd_precrisis_k4$fu_fwd

fp_fwd_post_k4 <- coeff_fwd_postcrisis_k4$fp_fwd
fu_fwd_post_k4 <- coeff_fwd_postcrisis_k4$fu_fwd

# Same forward-looking specification as Figure 3:
# ra = r - fp*p - fu*u

taylor_forward_precrisis_k4 <- macro_monthly_precrisis %>%
  mutate(
    ra = r - fp_fwd_pre_k4 * p - fu_fwd_pre_k4 * u
  ) %>%
  drop_na() %>%
  dplyr::select(ra, p, u)

taylor_forward_postcrisis_k4 <- macro_monthly_postcrisis %>%
  mutate(
    ra = r - fp_fwd_post_k4 * p - fu_fwd_post_k4 * u
  ) %>%
  drop_na() %>%
  dplyr::select(ra, p, u)

#########################################################
# Estimate forward-looking Taylor-rule VARs
#########################################################

var_forw_precrisis_k4 <- VAR(
  y = taylor_forward_precrisis_k4,
  p = 2,
  type = "const"
)

var_forw_postcrisis_k4 <- VAR(
  y = taylor_forward_postcrisis_k4,
  p = 4,
  type = "const"
)

#########################################################
# Compute IRFs
#########################################################

irf_forw_precrisis_k4 <- irf(
  var_forw_precrisis_k4,
  impulse = "ra",
  response = c("ra", "p", "u"),
  n.ahead = 24,
  ortho = TRUE,
  boot = TRUE,
  runs = 500,
  ci = 0.66
)

irf_forw_postcrisis_k4 <- irf(
  var_forw_postcrisis_k4,
  impulse = "ra",
  response = c("ra", "p", "u"),
  n.ahead = 24,
  ortho = TRUE,
  boot = TRUE,
  runs = 500,
  ci = 0.66
)

#########################################################
# Recover real interest-rate responses
#########################################################

recover_irf_forward <- function(irf_obj, fp, fu) {
  
  ra <- irf_obj$irf$ra[, "ra"]
  p  <- irf_obj$irf$ra[, "p"]
  u  <- irf_obj$irf$ra[, "u"]
  
  # Forward-looking convention:
  # ra = r - fp*p - fu*u
  # so r = ra + fp*p + fu*u
  r_nominal <- ra + fp * p + fu * u
  r_real <- r_nominal - p
  
  list(
    p = p,
    u = u,
    r_real = r_real
  )
}

res_forw_precrisis_k4 <- recover_irf_forward(
  irf_forw_precrisis_k4,
  fp_fwd_pre_k4,
  fu_fwd_pre_k4
)

res_forw_postcrisis_k4 <- recover_irf_forward(
  irf_forw_postcrisis_k4,
  fp_fwd_post_k4,
  fu_fwd_post_k4
)

#########################################################
# Export forward-looking Taylor-rule IRFs: k = 4
#########################################################

dir.create(
  "Extension/Extension_2/Figures",
  recursive = TRUE,
  showWarnings = FALSE
)

jpeg(
  "Extension/Extension_2/Figures/irf_taylor_forward_pre_post_k4.jpeg",
  width = 1800,
  height = 1200,
  res = 150
)

horizon <- 0:24
par(mfrow = c(2, 2))

plot(
  horizon, res_forw_precrisis_k4$p,
  type = "l", lwd = 2,
  ylim = range(res_forw_precrisis_k4$p, res_forw_postcrisis_k4$p),
  main = "Inflation response",
  xlab = "Months", ylab = "Percent"
)
lines(horizon, res_forw_postcrisis_k4$p, lty = 2, lwd = 2)
abline(h = 0, col = "red")
legend(
  "topright",
  legend = c("Pre-crisis", "Post-crisis"),
  lty = c(1, 2),
  lwd = 2,
  bty = "n"
)

plot(
  horizon, res_forw_precrisis_k4$u,
  type = "l", lwd = 2,
  ylim = range(res_forw_precrisis_k4$u, res_forw_postcrisis_k4$u),
  main = "Unemployment response",
  xlab = "Months", ylab = "Percent"
)
lines(horizon, res_forw_postcrisis_k4$u, lty = 2, lwd = 2)
abline(h = 0, col = "red")

plot(
  horizon, res_forw_precrisis_k4$r_real,
  type = "l", lwd = 2,
  ylim = range(res_forw_precrisis_k4$r_real, res_forw_postcrisis_k4$r_real),
  main = "Real interest-rate response",
  xlab = "Months", ylab = "Percent"
)
lines(horizon, res_forw_postcrisis_k4$r_real, lty = 2, lwd = 2)
abline(h = 0, col = "red")

dev.off()
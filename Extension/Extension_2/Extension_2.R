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

jpeg(
  "Extension/Extension_2/Figures/irf_combined.jpeg",
  width = 1800,
  height = 1800,
  res = 150
)

par(mfrow = c(3,3), mar = c(3,3,2,1))

variables <- c("p", "u", "r")

for (imp in variables) {
  for (resp in variables) {
    
    pre_irf <- irf_precrisis_full$irf[[imp]][, resp]
    post_irf <- irf_postcrisis_full$irf[[imp]][, resp]
    
    pre_low <- irf_precrisis_full$Lower[[imp]][, resp]
    pre_up  <- irf_precrisis_full$Upper[[imp]][, resp]
    
    post_low <- irf_postcrisis_full$Lower[[imp]][, resp]
    post_up  <- irf_postcrisis_full$Upper[[imp]][, resp]
    
    horizon <- 0:24
    
    ylim_range <- range(
      c(pre_low, pre_up, post_low, post_up),
      na.rm = TRUE
    )
    
    plot(
      horizon,
      pre_irf,
      type = "l",
      col = "darkgreen",
      lwd = 2,
      ylim = ylim_range,
      xlab = "Months",
      ylab = resp,
      main = paste("Orthogonal Impulse Response from", imp)
    )
    
    lines(horizon, pre_low,
          col = "darkgreen",
          lty = 2)
    
    lines(horizon, pre_up,
          col = "darkgreen",
          lty = 2)
    
    lines(horizon, post_irf,
          col = "blue",
          lwd = 2)
    
    lines(horizon, post_low,
          col = "blue",
          lty = 2)
    
    lines(horizon, post_up,
          col = "blue",
          lty = 2)
    
    abline(h = 0, col = "red")
    
    legend(
      "topright",
      legend = c("Pre-crisis", "Post-crisis"),
      col = c("darkgreen", "blue"),
      lwd = 2,
      bty = "n",
      cex = 0.8
    )
  }
}

dev.off()

#########################################################
# Backward-looking Taylor rule: pre/post crisis extension
#########################################################

# Coefficients as in the corrected replication.
# p is annualized monthly CPI inflation, so coefficients are not divided by 12.
fp_back <- 1.5
fu_back <- 0.5 * (-2.5)

# Taylor residual:
# r = fp*p + fu*u + ra
# so ra = r - fp*p - fu*u

taylor_backward_precrisis <- macro_monthly_precrisis %>%
  mutate(ra = r - fp_back * p - fu_back * u) %>%
  drop_na() %>%
  dplyr::select(ra, p, u)

taylor_backward_postcrisis <- macro_monthly_postcrisis %>%
  mutate(ra = r - fp_back * p - fu_back * u) %>%
  drop_na() %>%
  dplyr::select(ra, p, u)

# Estimate VARs with selected lag orders
var_back_precrisis <- VAR(taylor_backward_precrisis, p = 2, type = "const")
var_back_postcrisis <- VAR(taylor_backward_postcrisis, p = 4, type = "const")

# IRFs to the Taylor-rule monetary residual shock
irf_back_precrisis <- irf(
  var_back_precrisis,
  impulse = "ra",
  response = c("ra", "p", "u"),
  n.ahead = 24,
  ortho = TRUE,
  boot = FALSE
)

irf_back_postcrisis <- irf(
  var_back_postcrisis,
  impulse = "ra",
  response = c("ra", "p", "u"),
  n.ahead = 24,
  ortho = TRUE,
  boot = FALSE
)

# Recover nominal rate response and normalize to a 1 pp nominal-rate shock
recover_irf_sd <- function(irf_obj, taylor_data, fp, fu) {
  
  ra <- irf_obj$irf$ra[, "ra"]
  p  <- irf_obj$irf$ra[, "p"]
  u  <- irf_obj$irf$ra[, "u"]
  
  r <- ra + fp * p + fu * u
  
  std <- sd(taylor_data$ra, na.rm = TRUE)
  
  list(
    p = p / std,
    u = u / std,
    r_real = (r - p) / std
  )
}
res_back_precrisis <- recover_irf_sd(
  irf_back_precrisis,
  taylor_backward_precrisis,
  fp_back,
  fu_back
)

res_back_postcrisis <- recover_irf_sd(
  irf_back_postcrisis,
  taylor_backward_postcrisis,
  fp_back,
  fu_back
)

sd(taylor_backward_precrisis$ra)
sd(taylor_backward_postcrisis$ra)

#########################################################
# Export backward-looking Taylor-rule IRFs
#########################################################

jpeg(
  "Extension/Extension_2/Figures/irf_taylor_backward_pre_post.jpeg",
  width = 1800,
  height = 1200,
  res = 150
)

horizon <- 0:24
par(mfrow = c(2, 2))

plot(
  horizon, res_back_precrisis$p,
  type = "l", lwd = 2,
  ylim = range(res_back_precrisis$p, res_back_postcrisis$p),
  main = "Inflation response",
  xlab = "Months",
  ylab = "Percent"
)
lines(horizon, res_back_postcrisis$p, lty = 2, lwd = 2)
abline(h = 0, col = "red")
legend("topright",
       legend = c("Pre-crisis", "Post-crisis"),
       lty = c(1, 2), lwd = 2, bty = "n")

plot(
  horizon, res_back_precrisis$u,
  type = "l", lwd = 2,
  ylim = range(res_back_precrisis$u, res_back_postcrisis$u),
  main = "Unemployment response",
  xlab = "Months",
  ylab = "Percent"
)
lines(horizon, res_back_postcrisis$u, lty = 2, lwd = 2)
abline(h = 0, col = "red")

plot(
  horizon, res_back_precrisis$r_real,
  type = "l", lwd = 2,
  ylim = range(res_back_precrisis$r_real, res_back_postcrisis$r_real),
  main = "Real interest-rate response",
  xlab = "Months",
  ylab = "Percent"
)
lines(horizon, res_back_postcrisis$r_real, lty = 2, lwd = 2)
abline(h = 0, col = "red")

dev.off()

#########################################################
# Forward-looking Taylor rule: pre/post crisis extension
#########################################################

# This function computes forward-looking Taylor-rule coefficients
# from the VAR companion matrix, as in the corrected replication.

compute_forward_coefficients <- function(var_model, k = 12) {
  
  n_var <- length(var_model$varresult)
  p_lag <- var_model$p
  n_state <- n_var * p_lag
  
  # Extract lag coefficients, excluding the constant
  A <- sapply(
    var_model$varresult,
    function(eq) coef(eq)[1:n_state]
  ) %>%
    t()
  
  # Companion matrix:
  # top block = VAR coefficients
  # bottom block = lag shifting
  Phi <- rbind(
    A,
    cbind(
      diag(n_state - n_var),
      matrix(0, n_state - n_var, n_var)
    )
  )
  
  # Average forecast operator:
  # F_k = (1/k) sum Phi^i
  Phi_i <- diag(n_state)
  F_k <- matrix(0, n_state, n_state)
  
  for (i in 1:k) {
    Phi_i <- Phi %*% Phi_i
    F_k <- F_k + Phi_i
  }
  
  F_k <- F_k / k
  
  # Primitive Taylor-rule coefficients (p already annualized, so no division by 12)
  fp0 <- 1.5
  fu0 <- 0.5 * (-2.5)
  
  # Implied forward-looking coefficients : Dynamic effects of current p, u, and r on expected future p and u
  fp_raw <- fp0 * F_k[1, 1] + fu0 * F_k[2, 1]
  fu_raw <- fp0 * F_k[1, 2] + fu0 * F_k[2, 2]
  fr_raw <- fp0 * F_k[1, 3] + fu0 * F_k[2, 3]
  
  # Fixed-point correction because current r affects future expectations
  fp_fwd <- fp_raw / (1 - fr_raw)
  fu_fwd <- fu_raw / (1 - fr_raw)
  
  data.frame(
    fp_fwd = fp_fwd,
    fu_fwd = fu_fwd,
    fr_raw = fr_raw,
    denominator = 1 - fr_raw
  )
}

# k = 12 corresponds to a one-year forward-looking horizon in monthly data
coeff_fwd_precrisis <- compute_forward_coefficients(var_precrisis, k = 12)
coeff_fwd_postcrisis <- compute_forward_coefficients(var_postcrisis, k = 12)

# Diagnostic: unstable denominators imply fragile forward-looking identification
# If denominator is close to zero, the fixed-point correction is unstable.
# This can produce implausibly large forward-looking Taylor-rule coefficients.
# Therefore, forward-looking Taylor-rule IRFs are treated only as exploratory robustness checks.

coeff_fwd_precrisis
coeff_fwd_postcrisis

coeff_fwd_precrisis$denominator
coeff_fwd_postcrisis$denominator


fp_fwd_pre <- coeff_fwd_precrisis$fp_fwd
fu_fwd_pre <- coeff_fwd_precrisis$fu_fwd

fp_fwd_post <- coeff_fwd_postcrisis$fp_fwd
fu_fwd_post <- coeff_fwd_postcrisis$fu_fwd

# Forward-looking residual:
# ra = r - fp_fwd*p - fu_fwd*u

taylor_forward_precrisis <- macro_monthly_precrisis %>%
  mutate(ra = r - fp_fwd_pre * p - fu_fwd_pre * u) %>%
  drop_na() %>%
  dplyr::select(ra, p, u)

taylor_forward_postcrisis <- macro_monthly_postcrisis %>%
  mutate(ra = r - fp_fwd_post * p - fu_fwd_post * u) %>%
  drop_na() %>%
  dplyr::select(ra, p, u)

# Estimate VARs with selected lag orders
var_forw_precrisis <- VAR(taylor_forward_precrisis, p = 2, type = "const")
var_forw_postcrisis <- VAR(taylor_forward_postcrisis, p = 4, type = "const")

irf_forw_precrisis <- irf(
  var_forw_precrisis,
  impulse = "ra",
  response = c("ra", "p", "u"),
  n.ahead = 24,
  ortho = TRUE,
  boot = TRUE,
  runs = 1000,
  ci = 0.66
)

irf_forw_postcrisis <- irf(
  var_forw_postcrisis,
  impulse = "ra",
  response = c("ra", "p", "u"),
  n.ahead = 24,
  ortho = TRUE,
  boot = TRUE,
  runs = 1000,
  ci = 0.66
)


# Recover normalized IRFs to a 1 pp nominal-rate shock using the same standard-deviation normalization as for the backward-looking Taylor rule.

recover_irf <- function(irf_obj, taylor_data, fp, fu) {
  
  ra <- irf_obj$irf$ra[, "ra"]
  p  <- irf_obj$irf$ra[, "p"]
  u  <- irf_obj$irf$ra[, "u"]
  
  r <- ra + fp * p + fu * u
  
  std <- sd(taylor_data$ra, na.rm = TRUE)
  
  list(
    p = p / std,
    u = u / std,
    r_real = (r - p) / std
  )
}

# IRFs to forward-looking Taylor-rule shock 

res_forw_precrisis <- recover_irf(
  irf_forw_precrisis,
  taylor_forward_precrisis,
  fp_fwd_pre,
  fu_fwd_pre
)

res_forw_postcrisis <- recover_irf(
  irf_forw_postcrisis,
  taylor_forward_postcrisis,
  fp_fwd_post,
  fu_fwd_post
)


#########################################################
# Export forward-looking Taylor-rule IRFs
#########################################################

jpeg(
  "Extension/Extension_2/Figures/irf_taylor_backward_forward_pre_post.jpeg",
  width = 1800,
  height = 1200,
  res = 150
)

horizon <- 0:24

par(
  mfrow = c(2, 3),
  mar = c(3, 3, 2, 1),
  cex.main = 0.9,
  cex.lab = 0.8,
  cex.axis = 0.7
)


plot(
  horizon, res_back_precrisis$p,
  type = "l", lwd = 2,
  ylim = range(res_back_precrisis$p, res_back_postcrisis$p),
  main = "Backward: inflation response",
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
  bty = "n",
  cex = 0.8
)

plot(
  horizon, res_back_precrisis$u,
  type = "l", lwd = 2,
  ylim = range(res_back_precrisis$u, res_back_postcrisis$u),
  main = "Backward: unemployment response",
  xlab = "Months",
  ylab = "Percent"
)
lines(horizon, res_back_postcrisis$u, lty = 2, lwd = 2)
abline(h = 0, col = "red")

plot(
  horizon, res_back_precrisis$r_real,
  type = "l", lwd = 2,
  ylim = range(res_back_precrisis$r_real, res_back_postcrisis$r_real),
  main = "Backward: real interest-rate response",
  xlab = "Months",
  ylab = "Percent"
)
lines(horizon, res_back_postcrisis$r_real, lty = 2, lwd = 2)
abline(h = 0, col = "red")

plot(
  horizon, res_forw_precrisis$p,
  type = "l", lwd = 2,
  ylim = range(res_forw_precrisis$p, res_forw_postcrisis$p),
  main = "Forward: inflation response",
  xlab = "Months",
  ylab = "Percent"
)
lines(horizon, res_forw_postcrisis$p, lty = 2, lwd = 2)
abline(h = 0, col = "red")

plot(
  horizon, res_forw_precrisis$u,
  type = "l", lwd = 2,
  ylim = range(res_forw_precrisis$u, res_forw_postcrisis$u),
  main = "Forward: unemployment response",
  xlab = "Months",
  ylab = "Percent"
)
lines(horizon, res_forw_postcrisis$u, lty = 2, lwd = 2)
abline(h = 0, col = "red")

plot(
  horizon, res_forw_precrisis$r_real,
  type = "l", lwd = 2,
  ylim = range(res_forw_precrisis$r_real, res_forw_postcrisis$r_real),
  main = "Forward: real interest-rate response",
  xlab = "Months",
  ylab = "Percent"
)
lines(horizon, res_forw_postcrisis$r_real, lty = 2, lwd = 2)
abline(h = 0, col = "red")

dev.off()


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

#########################################################
# Backward-looking Taylor rule IRFs
#########################################################

fp_back <- 1.5
fu_back <- 0.5 * (-2.5)

taylor_backward_precrisis <- macro_monthly_precrisis %>%
  mutate(ra = r - fp_back * p - fu_back * u) %>%
  drop_na() %>%
  dplyr::select(ra, p, u)

taylor_backward_postcrisis <- macro_monthly_postcrisis %>%
  mutate(ra = r - fp_back * p - fu_back * u) %>%
  drop_na() %>%
  dplyr::select(ra, p, u)

var_back_precrisis <- VAR(
  taylor_backward_precrisis,
  p = 2,
  type = "const"
)

var_back_postcrisis <- VAR(
  taylor_backward_postcrisis,
  p = 4,
  type = "const"
)

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

#########################################################
# Forward-looking Taylor rule IRFs
#########################################################

compute_forward_coefficients <- function(var_model, k = 12) {
  
  n_var <- length(var_model$varresult)
  p_lag <- var_model$p
  n_state <- n_var * p_lag
  
  A <- sapply(
    var_model$varresult,
    function(eq) coef(eq)[1:n_state]
  ) %>%
    t()
  
  Phi <- rbind(
    A,
    cbind(
      diag(n_state - n_var),
      matrix(0, n_state - n_var, n_var)
    )
  )
  
  Phi_i <- diag(n_state)
  F_k <- matrix(0, n_state, n_state)
  
  for (i in 1:k) {
    Phi_i <- Phi %*% Phi_i
    F_k <- F_k + Phi_i
  }
  
  F_k <- F_k / k
  
  fp0 <- 1.5
  fu0 <- 0.5 * (-2.5)
  
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

coeff_fwd_precrisis <- compute_forward_coefficients(var_precrisis, k = 12)
coeff_fwd_postcrisis <- compute_forward_coefficients(var_postcrisis, k = 12)

fp_fwd_pre <- coeff_fwd_precrisis$fp_fwd
fu_fwd_pre <- coeff_fwd_precrisis$fu_fwd

fp_fwd_post <- coeff_fwd_postcrisis$fp_fwd
fu_fwd_post <- coeff_fwd_postcrisis$fu_fwd

taylor_forward_precrisis <- macro_monthly_precrisis %>%
  mutate(ra = r - fp_fwd_pre * p - fu_fwd_pre * u) %>%
  drop_na() %>%
  dplyr::select(ra, p, u)

taylor_forward_postcrisis <- macro_monthly_postcrisis %>%
  mutate(ra = r - fp_fwd_post * p - fu_fwd_post * u) %>%
  drop_na() %>%
  dplyr::select(ra, p, u)

var_forw_precrisis <- VAR(
  taylor_forward_precrisis,
  p = 2,
  type = "const"
)

var_forw_postcrisis <- VAR(
  taylor_forward_postcrisis,
  p = 4,
  type = "const"
)

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

jpeg(
  "Extension/Extension_2/Figures/irf_r_only_combined.jpeg",
  width = 1800,
  height = 600,
  res = 150
)

par(mfrow = c(1, 3), mar = c(3, 3, 2, 1))

imp <- "r"
responses <- c("p", "u", "r")

for (resp in responses) {
  
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
  
  lines(horizon, pre_low, col = "darkgreen", lty = 2)
  lines(horizon, pre_up,  col = "darkgreen", lty = 2)
  
  lines(horizon, post_irf, col = "blue", lwd = 2)
  lines(horizon, post_low, col = "blue", lty = 2)
  lines(horizon, post_up,  col = "blue", lty = 2)
  
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

dev.off()


### --- MONETARY TAYLOR RULE / IRF FORWARD BACKWARD ---

# --- Coefficients of rules (cf. rep package) ---

# Backward looking 

fp_back <- 1.5 / 4 
fu_back <- (0.5 / 4) * (-2.5)

# Forward looking

fp_fwd <- 0.758
fu_fwd <- -3.359

# --- Comoputation of VAR bwd/fwd ---

# Backward 

taylor_backward <- macro_1960_2000 %>%
  mutate(
    ra = r + fp_back * p - fu_back * u 
  ) %>%
  drop_na() %>%
  dplyr::select(ra, p, u)

# Forward

taylor_forward <- macro_1960_2000 %>%
  mutate(
    ra = r - fp_fwd * p - fu_fwd * u
  ) %>%
  drop_na() %>%
  dplyr::select(ra, p, u)

# --- Var Models ---

# Backward 

var_back <- VAR(taylor_backward, p = 4, type = "const")

# Forward 

var_forw <- VAR(taylor_forward,  p = 4, type = "const")

# --- Get IRF ---

# Backward 

irf_back <- irf(
  var_back,
  impulse = "ra",
  response = c("ra", "p", "u"),
  n.ahead = 24,
  ortho = TRUE,
  boot = TRUE,
  runs = 500,
  ci = 0.66
)

# Forward 

irf_forw <- irf(
  var_forw,
  impulse = "ra",
  response = c("ra", "p", "u"),
  n.ahead = 24,
  ortho = TRUE,
  boot = TRUE,
  runs = 500,
  ci = 0.66
)

# Function that recovers the IRF curve for each model, in order to plot them on the same graph 
# Idea: get the irf of each variable (e.g, ra, p, u), compute point-wise estimates of nominal interest rate chosen by the rule (r_nominal) and the real interest rate

recover_irf <- function(irf_obj, fp, fu) {
  ra <- irf_obj$irf$ra[, "ra"]
  p  <- irf_obj$irf$ra[, "p"]
  u  <- irf_obj$irf$ra[, "u"]
  r_nominal <- ra + fp * p + fu * u
  r_real    <- r_nominal - p
  list(
  p = p,
  u = u,
  r_real = r_real
)
}

# Apply the function to the forward looking and the backward looking given previous coefficients 

# Backward 

res_back <- recover_irf(irf_back, fp_back, fu_back)

# Forward 

res_forw <- recover_irf(irf_forw, fp_fwd, fu_fwd)

# Set horizon from 1 to 24 

horizon <- 0:24

# --- Final output of plot given monetary rule ---

# Inflation

jpeg(
  "Replication/Figures/Fig_3/irf_taylor.jpeg",
  width = 1800,
  height = 1200,
  res = 150
)

par(mfrow = c(2, 2))

plot(
  horizon, res_back$p,
  type = "l",
  lwd = 2,
  main = "Response of Inflation",
  xlab = "Lag",
  ylab = "Percent"
)

lines(horizon, res_forw$p, lty = 2, lwd = 2)

abline(h = 0, col = "red")

legend(
  "topright",
  legend = c("Backward-looking", "Forward-looking"),
  lty = c(1, 2),
  lwd = 2,
  bty = "n"
)

# Unemployment

plot(
  horizon, res_back$u,
  type = "l",
  lwd = 2,
  main = "Response of Unemployment",
  xlab = "Lag",
  ylab = "Percent"
)

lines(horizon, res_forw$u, lty = 2, lwd = 2)

abline(h = 0, col = "red")

# Interest rate

plot(
  horizon, res_back$r_real,
  type = "l",
  lwd = 2,
  main = "Response of Real Interest Rate",
  xlab = "Lag",
  ylab = "Percent"
)

lines(horizon, res_forw$r_real, lty = 2, lwd = 2)

abline(h = 0, col = "red")

dev.off()

#diagnostic for myself 

coeff_fwd_precrisis <- compute_forward_coefficients(var_precrisis, k = 4)
coeff_fwd_postcrisis <- compute_forward_coefficients(var_postcrisis, k = 4)

coeff_fwd_precrisis
coeff_fwd_postcrisis

1 - coeff_fwd_precrisis$fr_raw
1 - coeff_fwd_postcrisis$fr_raw

#########################################################
# Forward-looking Taylor rule: k = 4 robustness check
#########################################################

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
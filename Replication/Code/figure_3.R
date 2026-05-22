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



fp_back <- 1.5 / 4
fu_back <- (0.5 / 4) * (-2.5)
fp_fwd <- 0.862
fu_fwd <- 0.085

taylor_backward <- macro_1960_2000 %>%
  mutate(
    ra = r - fp_back * p - fu_back * u
  ) %>%
  drop_na() %>%
  dplyr::select(ra, p, u)

taylor_forward <- macro_1960_2000 %>%
  mutate(
    ra = r - fp_fwd * p - fu_fwd * u
  ) %>%
  drop_na() %>%
  dplyr::select(ra, p, u)

var_back <- VAR(taylor_backward, p = 4, type = "const")
var_forw <- VAR(taylor_forward,  p = 4, type = "const")

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

res_back <- recover_irf(irf_back, fp_back, fu_back)
res_forw <- recover_irf(irf_forw, fp_fwd, fu_fwd)

horizon <- 0:24

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

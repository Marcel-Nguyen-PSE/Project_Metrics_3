# --- Unrestricted CAN model IRFs ---

irf_can_r <- irf(
  var_us_can,
  impulse = "r",
  response = c("p_can", "u_can", "r_can"),
  n.ahead = 20,
  ortho = TRUE,
  boot = TRUE,
  runs = 500,
  ci = 0.95
)

irf_can_p <- irf(
  var_us_can,
  impulse = "p",
  response = c("p_can", "u_can", "r_can"),
  n.ahead = 20,
  ortho = TRUE,
  boot = TRUE,
  runs = 500,
  ci = 0.95
)

# --- Restricted CAN model IRF ---

irf_can_r_rest <- irf(
  var_us_can_rest,
  impulse = "r",
  response = c("p_can", "u_can", "r_can"),
  n.ahead = 20,
  ortho = TRUE,
  boot = TRUE,
  runs = 500,
  ci = 0.95
)

# --- Unrestricted MEX model IRFs ---

irf_mex_r <- irf(
  var_us_mex,
  impulse = "r",
  response = c("p_mex", "u_mex", "r_mex"),
  n.ahead = 20,
  ortho = TRUE,
  boot = TRUE,
  runs = 500,
  ci = 0.95
)

irf_mex_p <- irf(
  var_us_mex,
  impulse = "p",
  response = c("p_mex", "u_mex", "r_mex"),
  n.ahead = 20,
  ortho = TRUE,
  boot = TRUE,
  runs = 500,
  ci = 0.95
)

# --- Restricted MEX model IRF ---

irf_mex_r_rest <- irf(
  var_us_mex_rest,
  impulse = "r",
  response = c("p_mex", "u_mex", "r_mex"),
  n.ahead = 20,
  ortho = TRUE,
  boot = TRUE,
  runs = 500,
  ci = 0.95
)

irf_mex_r_2001 <- irf(
  var_us_mex_2001,
  impulse = "r",
  response = c("p_mex", "u_mex", "r_mex"),
  n.ahead = 20,
  ortho = TRUE,
  boot = TRUE,
  runs = 500,
  ci = 0.95
)

irf_mex_r_monthly <- irf(
  var_us_mex_monthly_fit,
  impulse = "r_us",
  response = c("p_monthly", "u_monthly", "r_monthly"),
  n.ahead = 20,
  ortho = TRUE,
  boot = TRUE,
  runs = 500,
  ci = 0.95
)

irf_can_r_rest <- irf(
  var_us_can_rest,
  impulse = "r",
  response = c("r", "p_can", "u_can", "r_can"),
  n.ahead = 20,
  ortho = TRUE,
  boot = TRUE,
  runs = 500,
  ci = 0.95
)

irf_mex_r_rest <- irf(
  var_us_mex_rest,
  impulse = "r",
  response = c("r", "p_mex", "u_mex", "r_mex"),
  n.ahead = 20,
  ortho = TRUE,
  boot = TRUE,
  runs = 500,
  ci = 0.95
)

jpeg(
  "Extension/Extension_1/Figures/irf_can_mex_restricted.jpeg",
  width = 2400,
  height = 600,
  res = 200
)

par(mfrow = c(1, 4), mar = c(4,4,3,1))

h <- 0:20

# --- Inflation ---

plot(
  h,
  irf_can_r_rest$irf$r[, "p_can"],
  type = "l",
  lwd = 2,
  col = "blue",
  main = "Inflation response",
  xlab = "Horizon",
  ylab = "Response",
  ylim = range(
    irf_can_r_rest$irf$r[, "p_can"],
    irf_mex_r_rest$irf$r[, "p_mex"]
  )
)

lines(
  h,
  irf_mex_r_rest$irf$r[, "p_mex"],
  lwd = 2,
  lty = 2,
  col = "darkgreen"
)

abline(h = 0, col = "red")

legend(
  "topright",
  legend = c("Canada", "Mexico"),
  col = c("blue", "darkgreen"),
  lty = c(1,2),
  lwd = 2,
  bty = "n"
)

# --- Unemployment ---

plot(
  h,
  irf_can_r_rest$irf$r[, "u_can"],
  type = "l",
  lwd = 2,
  col = "blue",
  main = "Unemployment response",
  xlab = "Horizon",
  ylab = "Response",
  ylim = range(
    irf_can_r_rest$irf$r[, "u_can"],
    irf_mex_r_rest$irf$r[, "u_mex"]
  )
)

lines(
  h,
  irf_mex_r_rest$irf$r[, "u_mex"],
  lwd = 2,
  lty = 2,
  col = "darkgreen"
)

abline(h = 0, col = "red")

# --- Domestic interest rates ---

plot(
  h,
  irf_can_r_rest$irf$r[, "r_can"],
  type = "l",
  lwd = 2,
  col = "blue",
  main = "Interest-rate response",
  xlab = "Horizon",
  ylab = "Response",
  ylim = range(
    irf_can_r_rest$irf$r[, "r_can"],
    irf_mex_r_rest$irf$r[, "r_mex"]
  )
)

lines(
  h,
  irf_mex_r_rest$irf$r[, "r_mex"],
  lwd = 2,
  lty = 2,
  col = "darkgreen"
)

abline(h = 0, col = "red")

# --- US rate response ---

plot(
  h,
  irf_can_r_rest$irf$r[, "r"],
  type = "l",
  lwd = 2,
  col = "blue",
  main = "US rate response",
  xlab = "Horizon",
  ylab = "Response",
  ylim = range(
    irf_can_r_rest$irf$r[, "r"],
    irf_mex_r_rest$irf$r[, "r"]
  )
)

lines(
  h,
  irf_mex_r_rest$irf$r[, "r"],
  lwd = 2,
  lty = 2,
  col = "darkgreen"
)

abline(h = 0, col = "red")

dev.off()
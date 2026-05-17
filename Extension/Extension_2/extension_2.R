# For this extension, prep_1.R and prep_2.R have to be ran first.
# This extension compares the impulse response functions in function of lag orders 

var_4 <- VAR(
  y = macro_1960_2000,
  p = 4, 
  type = 'const'
)

var_3 <- VAR(
  y = macro_1960_2000,
  p = 3, 
  type = 'const'
)

var_5 <- VAR(
  y = macro_1960_2000,
  p = 5, 
  type = 'const'
)

irf_var_4 <- irf(
  var_4,
  impulse = c("p", "u", "r"),
  response = c("p", "u", "r"),
  n.ahead = 24,
  ortho = TRUE,    
  boot = TRUE,       
  runs = 1000,
  ci = 0.66          
)

irf_var_5 <- irf(
  var_5,
  impulse = c("p", "u", "r"),
  response = c("p", "u", "r"),
  n.ahead = 24,
  ortho = TRUE,    
  boot = TRUE,       
  runs = 1000,
  ci = 0.66          
)

irf_var_3 <- irf(
  var_3,
  impulse = c("p", "u", "r"),
  response = c("p", "u", "r"),
  n.ahead = 24,
  ortho = TRUE,    
  boot = TRUE,       
  runs = 1000,
  ci = 0.66          
)

jpeg(
  "Extension/Extension_2/irf_lag_comparison.jpeg",
  width = 2400,
  height = 800,
  res = 200
)

h <- 0:24

par(mfrow = c(1, 3))

ylim_p <- range(
  irf_var_3$irf$r[, "p"],
  irf_var_4$irf$r[, "p"],
  irf_var_5$irf$r[, "p"]
)

plot(
  h,
  irf_var_3$irf$r[, "p"],
  type = "l",
  lwd = 2,
  lty = 1,
  ylim = ylim_p,
  main = "Inflation response to r shock",
  xlab = "Quarters",
  ylab = "Response"
)

lines(
  h,
  irf_var_4$irf$r[, "p"],
  lwd = 2,
  lty = 2
)

lines(
  h,
  irf_var_5$irf$r[, "p"],
  lwd = 2,
  lty = 3
)

abline(h = 0, col = "red")
legend(
  "topright",
  legend = c("VAR(3)", "VAR(4)", "VAR(5)"),
  lty = c(1, 2, 3),
  lwd = 2,
  bty = "n"
)

ylim_u <- range(
  irf_var_3$irf$r[, "u"],
  irf_var_4$irf$r[, "u"],
  irf_var_5$irf$r[, "u"]
)

plot(
  h,
  irf_var_3$irf$r[, "u"],
  type = "l",
  lwd = 2,
  lty = 1,
  ylim = ylim_u,
  main = "Unemployment response to r shock",
  xlab = "Quarters",
  ylab = "Response"
)

lines(
  h,
  irf_var_4$irf$r[, "u"],
  lwd = 2,
  lty = 2
)

lines(
  h,
  irf_var_5$irf$r[, "u"],
  lwd = 2,
  lty = 3
)

abline(h = 0, col = "red")

ylim_r <- range(
  irf_var_3$irf$r[, "r"],
  irf_var_4$irf$r[, "r"],
  irf_var_5$irf$r[, "r"]
)

plot(
  h,
  irf_var_3$irf$r[, "r"],
  type = "l",
  lwd = 2,
  lty = 1,
  ylim = ylim_r,
  main = "Interest rate response to r shock",
  xlab = "Quarters",
  ylab = "Response"
)

lines(
  h,
  irf_var_4$irf$r[, "r"],
  lwd = 2,
  lty = 2
)

lines(
  h,
  irf_var_5$irf$r[, "r"],
  lwd = 2,
  lty = 3
)

abline(h = 0, col = "red")

dev.off()

# This extension study the spillover effect of U.S. monetary policies on neighbour countries

# Canada/U.S 

var_us_can_data <- macro_can_us %>%
  dplyr::select(-date) %>%
  na.omit()

lag_sel <- VARselect(
  var_us_can_data,
  lag.max = 8,
  type = 'const'
)

lag_sel$selection

var_us_can <- VAR(
  var_us_can_data, p = 2, type = 'const'
)

summary(var_us_can)

causality(var_us_can, cause = c("p", "u", "r"))

roots_mod_can <- roots(var_us_can, modulus = TRUE)
max(roots_mod_can)

irf_can_r <- irf(
  var_us_can,
  impulse = "r",
  response = c('p_can', 'u_can', 'r_can', 'exr_can'),
  n.ahead = 20,
  ortho = TRUE,
  boot = TRUE,
  runs = 500,
  ci = 0.95
)

irf_can_p <- irf(
  var_us_can,
  impulse = "p",
  response = c('p_can', 'u_can', 'r_can', 'exr_can'),
  n.ahead = 20,
  ortho = TRUE,
  boot = TRUE,
  runs = 500,
  ci = 0.95
)

par(mfrow = c(2, 2))

plot(
  irf_can_r,
  plot.type = "single"
)

plot(
  irf_can_p,
  plot.type = 'single'
)

fevd_fit_can <- fevd(var_us_can, n.ahead = 20)

jpeg("Extension/Extension_1/Figures/irf_can_us_r.jpeg",
     width = 1800, height = 1800, res = 150)
par(mfrow = c(2, 2))
plot(irf_can_r, plot.type = "single")
dev.off()

jpeg("Extension/Extension_1/Figures/irf_can_us_p.jpeg",
     width = 1800, height = 1800, res = 150)
par(mfrow = c(2, 2))
plot(irf_can_p, plot.type = "single")
dev.off()




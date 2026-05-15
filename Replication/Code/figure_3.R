pi_star <- 2
u_star  <- mean(macro_1960_2000$u, na.rm = TRUE)  # add this
r_star  <- 2

taylor_backward <- macro_1960_2000 %>%
  mutate(
    pi_bar = rollmean(p, 4, fill = NA, align = "right"),
    u_bar  = rollmean(u, 4, fill = NA, align = "right"),
    pol = r - (r_star + 1.5 * (lag(pi_bar, 1) - pi_star) - 1.25 * (lag(u_bar, 1) - u_star))
  ) %>%
  drop_na() %>%
  dplyr::select(p, u, pol)

taylor_forward <- macro_1960_2000 %>%
  drop_na() %>%
  mutate(pi_f4 = NA_real_,
         u_f4  = NA_real_) 
  

for (t in 20:nrow(taylor_forward)) {
  f <- predict(VAR(taylor_forward[1:t, c("p", "u", "r")], p = 4, type = "const"),
               n.ahead = 4)$fcst
  taylor_forward[t, "pi_f4"] <- f$p[4, "fcst"]
  taylor_forward[t, "u_f4"]  <- f$u[4, "fcst"]
}

taylor_forward <- taylor_forward %>%
  mutate(
    pol = r - (r_star + 1.5 * (pi_f4 - pi_star) - 1.25 * (u_f4 - u_star))
  ) %>%
  drop_na() %>%
  dplyr::select(p, u, pol)


# VARs
var_back <- VAR(taylor_backward, p = 4, type = 'const')
var_forw <- VAR(taylor_forward,  p = 4, type = 'const')


irf_back <- irf(var_back, impulse = "pol", response = c("p", "u", "pol"),
                n.ahead = 24, ortho = TRUE, boot = TRUE, runs = 500, ci = 0.66)
irf_fwd  <- irf(var_forw, impulse = "pol", response = c("p", "u", "pol"),
                n.ahead = 24, ortho = TRUE, boot = TRUE, runs = 500, ci = 0.66)
horizon <- 0:24

jpeg("Replication/Figures/Fig_3/irf_taylor.jpeg", width = 1800, height = 1800, res = 150)
par(mfrow = c(2, 2))

titles    <- c("Response of Inflation", "Response of Unemployment", "Response of Interest Rate")
responses <- c("p", "u", "pol")

for (i in seq_along(responses)) {
  resp <- responses[i]
  ylim <- range(c(irf_back$irf$pol[, resp],
                  irf_fwd$irf$pol[,  resp],
                  irf_back$Lower$pol[, resp],
                  irf_back$Upper$pol[, resp]))

  plot(horizon, irf_back$irf$pol[, resp], type = "l", lwd = 2,
       ylim = ylim, main = titles[i], xlab = "Lag", ylab = "Percent")
  abline(h = 0, col = "red")
  lines(horizon, irf_fwd$irf$pol[, resp], lty = 2, lwd = 2)
}

dev.off()






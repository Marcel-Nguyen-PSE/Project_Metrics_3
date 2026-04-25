taylor_backward <- macro_1960_2000 %>%
  mutate(
    pol = r - 1.5*rollmean(p, 4, fill = NA, align ='right') + 1.25*rollmean(u, 4, fill = NA, align = 'right')
  ) %>%
      drop_na() %>% dplyr::select(p,u, pol)

taylor_forward <- macro_1960_2000 %>%
  mutate(
    pol = r - 1.5*rollmean(p, 4, fill = NA, align ='right') + 1.25*rollmean(u, 4, fill = NA, align = 'left')
  ) %>%
      drop_na() %>% dplyr::select(p,u, pol)

var_back <- VAR(
  taylor_backward[, c("p", "u", "pol")],
  p= 4,
  type = 'const'
)
var_forw <- VAR(
  taylor_forward[, c("p", "u", "pol")],
  p= 4,
  type = 'const'
)

irf_back <- irf(var_back, impulse = "pol", response = c("p", "u", "pol"),
                n.ahead = 24, ortho = TRUE, boot = TRUE, runs = 1000, ci = 0.66)
irf_fwd <- irf(var_forw, impulse = "pol", response = c("p", "u", "pol"),
                n.ahead = 24, ortho = TRUE, boot = TRUE, runs = 1000, ci = 0.66)

jpeg("Replication/Figures/Fig_3/irf_taylor.jpeg", width = 1800, height = 1800, res = 150)

par(mfrow = c(2, 2))

titles    <- c("Response of Inflation", "Response of Unemployment", "Response of Real Interest Rates")
responses <- c("p", "u", "pol")

for (i in seq_along(responses)) {
  resp <- responses[i]
  ylim <- range(c(irf_back$irf$pol[, resp], irf_fwd$irf$pol[, resp]))

  plot(horizon, irf_back$irf$pol[, resp], type = "l", lwd = 2,
       ylim = ylim, main = titles[i], xlab = "Lag", ylab = "Percent")
  abline(h = 0, col = "grey")
  lines(horizon, irf_fwd$irf$pol[, resp], lty = 2, lwd = 2)
}

dev.off()



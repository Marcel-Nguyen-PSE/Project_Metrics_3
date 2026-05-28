#########################################################
# --- FEVD : CAN restricted model
#########################################################

fevd_fit_can <- fevd(var_us_can_rest, n.ahead = 20)

fevd_df <- lapply(names(fevd_fit_can), function(v) {
  d <- as.data.frame(fevd_fit_can[[v]])
  d$h <- seq_len(nrow(d))
  d$variable <- v
  d
}) |>
  bind_rows() |>
  pivot_longer(
    -c(h, variable),
    names_to = "shock",
    values_to = "share"
  ) |>
  mutate(
    variable = factor(
      variable,
      levels = c("r", "p_can", "u_can", "r_can"),
      labels = c(
        "US Fed Rate",
        "CAN Inflation",
        "CAN Unemployment",
        "CAN Interest Rate"
      )
    ),
    shock = factor(
      shock,
      levels = c("r", "p_can", "u_can", "r_can"),
      labels = c(
        "US Fed Rate shock",
        "CAN Inflation shock",
        "CAN Unemployment shock",
        "CAN Interest Rate shock"
      )
    ),
    share = 100 * share
  )

p_fevd <- ggplot(fevd_df, aes(h, share, fill = shock)) +
  geom_area(alpha = 0.85) +
  facet_wrap(~ variable, ncol = 3) +
  labs(
    title = "Forecast error variance decomposition",
    x = "Quarters ahead",
    y = "Share of variance",
    fill = NULL
  ) +
  scale_fill_manual(
    values = c(
      "US Fed Rate shock"       = "#6BAED6",
      "CAN Inflation shock"     = "#00441B",
      "CAN Unemployment shock"  = "#238B45",
      "CAN Interest Rate shock" = "#74C476"
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(
  'Extension/Extension_1/Figures/FEVD_CAN_US.jpeg',
  p_fevd,
  width = 12,
  height = 8
)


#########################################################
# --- FEVD : MEX monthly model
#########################################################

fevd_fit_mex_monthly <- fevd(
  var_us_mex_monthly_fit,
  n.ahead = 20
)

fevd_df_mex_monthly <- lapply(names(fevd_fit_mex_monthly), function(v) {

  d <- as.data.frame(fevd_fit_mex_monthly[[v]])
  d$h <- seq_len(nrow(d))
  d$variable <- v

  d

}) |>
  bind_rows() |>
  pivot_longer(
    -c(h, variable),
    names_to = "shock",
    values_to = "share"
  ) |>
  mutate(
    variable = factor(
      variable,
      levels = c("r_us", "p_monthly", "u_monthly", "r_monthly"),
      labels = c(
        "US Fed Rate",
        "MEX Inflation",
        "MEX Unemployment",
        "MEX Interest Rate"
      )
    ),
    shock = factor(
      shock,
      levels = c("r_us", "p_monthly", "u_monthly", "r_monthly"),
      labels = c(
        "US Fed Rate shock",
        "MEX Inflation shock",
        "MEX Unemployment shock",
        "MEX Interest Rate shock"
      )
    ),
    share = 100 * share
  )

p_fevd_mex_monthly <- ggplot(
  fevd_df_mex_monthly,
  aes(x = h, y = share, fill = shock)
) +
  geom_area(alpha = 0.85) +
  facet_wrap(~ variable, ncol = 2) +
  labs(
    title = "Forecast error variance decomposition: Mexico monthly VAR",
    x = "Months ahead",
    y = "Share of forecast-error variance",
    fill = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom"
  )

ggsave(
  "Extension/Extension_1/Figures/FEVD_MEX_monthly_US.jpeg",
  p_fevd_mex_monthly,
  width = 12,
  height = 8
)

#########################################################
# --- FEVD : MEX restricted model
#########################################################

fevd_fit_mex_rest <- fevd(
  var_us_mex_rest,
  n.ahead = 20
)

fevd_df_mex_rest <- lapply(names(fevd_fit_mex_rest), function(v) {
  d <- as.data.frame(fevd_fit_mex_rest[[v]])
  d$h <- seq_len(nrow(d))
  d$variable <- v
  d
}) |>
  bind_rows() |>
  pivot_longer(
    -c(h, variable),
    names_to = "shock",
    values_to = "share"
  ) |>
  mutate(

    variable = factor(
      variable,
      levels = c(
        "r",
        "p_mex",
        "u_mex",
        "r_mex"
      ),
      labels = c(
        "US Fed Rate",
        "MEX Inflation",
        "MEX Unemployment",
        "MEX Interest Rate"
      )
    ),

    shock = factor(
      shock,
      levels = c(
        "r",
        "p_mex",
        "u_mex",
        "r_mex"
      ),
      labels = c(
        "US Fed Rate shock",
        "MEX Inflation shock",
        "MEX Unemployment shock",
        "MEX Interest Rate shock"
      )
    ),

    share = 100 * share

  )

p_fevd_mex_rest <- ggplot(
  fevd_df_mex_rest,
  aes(x = h, y = share, fill = shock)
) +

  geom_area(alpha = 0.85) +

  facet_wrap(
    ~ variable,
    ncol = 2
  ) +

  labs(
    title = "Forecast error variance decomposition: Mexico quarterly VAR",
    x = "Quarters ahead",
    y = "Share of forecast-error variance",
    fill = NULL
  ) +

  theme_minimal(base_size = 11) +

  theme(
    legend.position = "bottom"
  )

ggsave(
  "Extension/Extension_1/Figures/FEVD_MEX_US_rest.jpeg",
  p_fevd_mex_rest,
  width = 12,
  height = 8
)

#########################################################
# IRF comparison: full vs restricted models
# Canada and Mexico, U.S. interest-rate shock
#########################################################

irf_can_full <- irf(
  var_us_can,
  impulse = "r",
  response = c("p_can", "u_can", "r_can"),
  n.ahead = 20,
  ortho = TRUE,
  boot = TRUE,
  runs = 500,
  ci = 0.95
)

irf_can_rest <- irf(
  var_us_can_rest,
  impulse = "r",
  response = c("p_can", "u_can", "r_can"),
  n.ahead = 20,
  ortho = TRUE,
  boot = TRUE,
  runs = 500,
  ci = 0.95
)

irf_mex_full <- irf(
  var_us_mex,
  impulse = "r",
  response = c("p_mex", "u_mex", "r_mex"),
  n.ahead = 20,
  ortho = TRUE,
  boot = TRUE,
  runs = 500,
  ci = 0.95
)

irf_mex_rest <- irf(
  var_us_mex_rest,
  impulse = "r",
  response = c("p_mex", "u_mex", "r_mex"),
  n.ahead = 20,
  ortho = TRUE,
  boot = TRUE,
  runs = 500,
  ci = 0.95
)

jpeg(
  "Extension/Extension_1/Figures/irf_full_vs_restricted_can_mex.jpeg",
  width = 2400,
  height = 1200,
  res = 200
)

par(mfrow = c(2, 3), mar = c(4, 4, 3, 1))

h <- 0:20

# --- Canada inflation ---

plot(
  h,
  irf_can_full$irf$r[, "p_can"],
  type = "l",
  lwd = 2,
  main = "Canada inflation response",
  xlab = "Horizon",
  ylab = "Response",
  ylim = range(
    irf_can_full$irf$r[, "p_can"],
    irf_can_rest$irf$r[, "p_can"]
  )
)

lines(h, irf_can_rest$irf$r[, "p_can"], lwd = 2, lty = 2)
abline(h = 0, col = "red")

legend(
  "topright",
  legend = c("Full VAR", "Restricted VAR"),
  lty = c(1, 2),
  lwd = 2,
  bty = "n"
)

# --- Canada unemployment ---

plot(
  h,
  irf_can_full$irf$r[, "u_can"],
  type = "l",
  lwd = 2,
  main = "Canada unemployment response",
  xlab = "Horizon",
  ylab = "Response",
  ylim = range(
    irf_can_full$irf$r[, "u_can"],
    irf_can_rest$irf$r[, "u_can"]
  )
)

lines(h, irf_can_rest$irf$r[, "u_can"], lwd = 2, lty = 2)
abline(h = 0, col = "red")

# --- Canada interest rate ---

plot(
  h,
  irf_can_full$irf$r[, "r_can"],
  type = "l",
  lwd = 2,
  main = "Canada interest-rate response",
  xlab = "Horizon",
  ylab = "Response",
  ylim = range(
    irf_can_full$irf$r[, "r_can"],
    irf_can_rest$irf$r[, "r_can"]
  )
)

lines(h, irf_can_rest$irf$r[, "r_can"], lwd = 2, lty = 2)
abline(h = 0, col = "red")

# --- Mexico inflation ---

plot(
  h,
  irf_mex_full$irf$r[, "p_mex"],
  type = "l",
  lwd = 2,
  main = "Mexico inflation response",
  xlab = "Horizon",
  ylab = "Response",
  ylim = range(
    irf_mex_full$irf$r[, "p_mex"],
    irf_mex_rest$irf$r[, "p_mex"]
  )
)

lines(h, irf_mex_rest$irf$r[, "p_mex"], lwd = 2, lty = 2)
abline(h = 0, col = "red")

# --- Mexico unemployment ---

plot(
  h,
  irf_mex_full$irf$r[, "u_mex"],
  type = "l",
  lwd = 2,
  main = "Mexico unemployment response",
  xlab = "Horizon",
  ylab = "Response",
  ylim = range(
    irf_mex_full$irf$r[, "u_mex"],
    irf_mex_rest$irf$r[, "u_mex"]
  )
)

lines(h, irf_mex_rest$irf$r[, "u_mex"], lwd = 2, lty = 2)
abline(h = 0, col = "red")

# --- Mexico interest rate ---

plot(
  h,
  irf_mex_full$irf$r[, "r_mex"],
  type = "l",
  lwd = 2,
  main = "Mexico interest-rate response",
  xlab = "Horizon",
  ylab = "Response",
  ylim = range(
    irf_mex_full$irf$r[, "r_mex"],
    irf_mex_rest$irf$r[, "r_mex"]
  )
)

lines(h, irf_mex_rest$irf$r[, "r_mex"], lwd = 2, lty = 2)
abline(h = 0, col = "red")

dev.off()
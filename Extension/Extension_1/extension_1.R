# This extension study the spillover effect of U.S. monetary policies on neighbour countries
# Prep_3.R have to be ran first 

# Canada/U.S 

var_us_can_data <- macro_can_us %>%
  dplyr::select(
    p,
    u,
    r,
    p_can,
    u_can,
    r_can
  ) %>%
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
  response = c('p_can', 'u_can', 'r_can'),
  n.ahead = 20,
  ortho = TRUE,
  boot = TRUE,
  runs = 500,
  ci = 0.95
)

irf_can_p <- irf(
  var_us_can,
  impulse = "p",
  response = c('p_can', 'u_can', 'r_can'),
  n.ahead = 20,
  ortho = TRUE,
  boot = TRUE,
  runs = 500,
  ci = 0.95
)

# Restricted model with only US FED Rate 

var_us_can_data_rest <- macro_can_us %>%
  dplyr::select(
    r,
    p_can,
    u_can,
    r_can
  ) %>%
  na.omit()

lag_sel_rest <- VARselect(
  var_us_can_data_rest,
  lag.max = 8,
  type = 'const'
)

lag_sel_rest$selection

var_us_can_rest <- VAR(
  var_us_can_data_rest, p = 2, type = 'const'
)

summary(var_us_can_rest)

causality(var_us_can_rest, cause = 'r')

roots_mod_can_rest <- roots(var_us_can_rest, modulus = TRUE)
max(roots_mod_can_rest)

irf_can_r_rest <- irf(
  var_us_can_rest,
  impulse = "r",
  response = c('p_can', 'u_can', 'r_can'),
  n.ahead = 20,
  ortho = TRUE,
  boot = TRUE,
  runs = 500,
  ci = 0.95
)

jpeg(
  "Extension/Extension_1/Figures/irf_full_vs_restricted.jpeg",
  width = 2400,
  height = 800,
  res = 200
)

h <- 0:(nrow(irf_can_r$irf$r) - 1)

par(mfrow = c(1, 3))

plot(
  h,
  irf_can_r$irf$r[, "p_can"],
  type = "l",
  lwd = 2,
  lty = 1,
  ylim = c(-1,1),
  main = "Canada inflation response",
  xlab = "Quarters",
  ylab = "Response"
)

lines(h, irf_can_r_rest$irf$r[, "p_can"], lwd = 2, lty = 2)
abline(h = 0, col = "red")

legend(
  "topright",
  legend = c("Full VAR", "Restricted VAR"),
  lty = c(1, 2),
  lwd = 2,
  bty = "n"
)

plot(
  h,
  irf_can_r$irf$r[, "u_can"],
  type = "l",
  lwd = 2,
  lty = 1,
  ylim = c(-1,1),
  main = "Canada unemployment response",
  xlab = "Quarters",
  ylab = "Response"
)

lines(h, irf_can_r_rest$irf$r[, "u_can"], lwd = 2, lty = 2)
abline(h = 0, col = "red")

plot(
  h,
  irf_can_r$irf$r[, "r_can"],
  type = "l",
  lwd = 2,
  lty = 1,
  ylim = c(-1,1),
  main = "Canada interest rate response",
  xlab = "Quarters",
  ylab = "Response"
)

lines(h, irf_can_r_rest$irf$r[, "r_can"], lwd = 2, lty = 2)
abline(h = 0, col = "red")

dev.off()

h <- c(1, 4, 8, 12)

mse_results <- data.frame()

for(h in horizons) {
  errors_full <- c()
  errors_rest <- c()
  for(t in 40:(nrow(var_us_can_data) - h)) {
    train_full <- var_us_can_data[1:t, ]
    fit_full <- VAR(train_full, p = 2, type = "const")
    fc_full <- predict(fit_full, n.ahead = h)$fcst
    forecast_full <- fc_full$p_can[h, "fcst"]
    actual_full <- var_us_can_data$p_can[t + h]
    errors_full <- c(errors_full, actual_full - forecast_full)
    train_rest <- var_us_can_data_rest[1:t, ]
    fit_rest <- VAR(train_rest, p = 2, type = "const")
    fc_rest <- predict(fit_rest, n.ahead = h)$fcst
    forecast_rest <- fc_rest$p_can[h, "fcst"]
    actual_rest <- var_us_can_data_rest$p_can[t + h]
    errors_rest <- c(errors_rest, actual_rest - forecast_rest)
  }
  mse_results <- rbind(
    mse_results,
    data.frame(
      horizon = h,
      mse_full = mean(errors_full^2),
      mse_restricted = mean(errors_rest^2)
    )
  )
}
mse_results <- mse_results %>% 
  mutate(across(where(is.numeric), ~ round(.x, 3)))
mse_results_table <- tt_save(tt(mse_results, rownames = FALSE), 'Extension/Extension_1/Figures/MSE_rest_full.typ')

jpeg("Extension/Extension_1/Figures/irf_can_us_r_rest.jpeg",
     width = 1800, height = 1800, res = 150)
par(mfrow = c(2, 2))
plot(irf_can_r_rest, plot.type = "single")
dev.off()

lag_selection_can_out <- as.data.frame(lag_sel_rest$criteria) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

lag_selection_can_typst <- tt(lag_selection_can_out, rownames = TRUE)
tt_save(lag_selection_can_typst, 'Extension/Extension_1/Figures/lag_table_can_rest.typ')

jpeg(
  "Extension/Extension_1/Figures/acf_pacf_restricted.jpeg",
  width = 2400,
  height = 3200,
  res = 200
)

par(mfrow = c(4, 2))

acf(var_us_can_data_rest$r,
    main = "ACF of US federal funds rate r",
    lag.max = 20)
pacf(var_us_can_data_rest$r,
     main = "PACF of US federal funds rate r",
     lag.max = 20)
acf(var_us_can_data_rest$p_can,
    main = "ACF of Canada inflation p_can",
    lag.max = 20)
pacf(var_us_can_data_rest$p_can,
     main = "PACF of Canada inflation p_can",
     lag.max = 20)
acf(var_us_can_data_rest$u_can,
    main = "ACF of Canada unemployment u_can",
    lag.max = 20)
pacf(var_us_can_data_rest$u_can,
     main = "PACF of Canada unemployment u_can",
     lag.max = 20)
acf(var_us_can_data_rest$r_can,
    main = "ACF of Canada interest rate r_can",
    lag.max = 20)
pacf(var_us_can_data_rest$r_can,
     main = "PACF of Canada interest rate r_can",
     lag.max = 20)
dev.off()

# Mexico/US

var_us_mex_data <- macro_mex_us_post2000 %>%
  dplyr::select(p, u, r, p_mex, u_mex, r_mex) %>%
  na.omit()

lag_sel_mex <- VARselect(
  var_us_mex_data,
  lag.max = 7,
  type = 'const'
)

lag_sel_mex$selection

lag_selection_mex_out <- as.data.frame(lag_sel_mex$criteria) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

lag_selection_mex_typst <- tt(lag_selection_mex_out, rownames = TRUE)
tt_save(lag_selection_mex_typst, 'Extension/Extension_1/Figures/lag_table_mex_rest.typ')

var_us_mex <- VAR(
  var_us_mex_data, p = 2, type = 'const'
)

summary(var_us_mex)

causality(var_us_mex, cause = c("p", "u", "r"))

roots_mod_mex <- roots(var_us_mex, modulus = TRUE)
max(roots_mod_mex)

irf_mex_r <- irf(
  var_us_mex,
  impulse = "r",
  response = c('p_mex', 'u_mex', 'r_mex'),
  n.ahead = 20,
  ortho = TRUE,
  boot = TRUE,
  runs = 500,
  ci = 0.95
)

irf_mex_p <- irf(
  var_us_mex,
  impulse = "p",
  response = c('p_mex', 'u_mex', 'r_mex'),
  n.ahead = 20,
  ortho = TRUE,
  boot = TRUE,
  runs = 500,
  ci = 0.95
)

par(mfrow = c(2, 2))

plot(
  irf_mex_r,
  plot.type = "single"
)

plot(
  irf_mex_p,
  plot.type = 'single'
)

jpeg("Extension/Extension_1/Figures/irf_mex_us_r.jpeg",
     width = 1800, height = 1800, res = 150)
par(mfrow = c(2, 2))
plot(irf_mex_r, plot.type = "single")
dev.off()

jpeg("Extension/Extension_1/Figures/irf_mex_us_p.jpeg",
     width = 1800, height = 1800, res = 150)
par(mfrow = c(2, 2))
plot(irf_mex_p, plot.type = "single")
dev.off()

plot(var_us_mex_data$r_mex)

# Restricted 

var_us_mex_data_rest <- macro_mex_us %>%
  dplyr::select(
    r,
    p_mex,
    u_mex,
    r_mex
  ) %>%
  na.omit()

lag_sel_rest_mex <- VARselect(
  var_us_mex_data_rest,
  lag.max = 8,
  type = 'const'
)

lag_sel_rest_mex$selection

var_us_can_rest <- VAR(
  var_us_can_data_rest, p = 2, type = 'const'
)

summary(var_us_can_rest)

causality(var_us_can_rest, cause = 'r')

roots_mod_can_rest <- roots(var_us_can_rest, modulus = TRUE)
max(roots_mod_can_rest)

irf_can_r_rest <- irf(
  var_us_can_rest,
  impulse = "r",
  response = c('p_can', 'u_can', 'r_can'),
  n.ahead = 20,
  ortho = TRUE,
  boot = TRUE,
  runs = 500,
  ci = 0.95
)



#### FEVD decomp
fevd_fit_mex <- fevd(var_us_mex, n.ahead = 20)
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
      levels = c("p", "u", "r", "p_can", "u_can", "r_can"),
      labels = c(
        "US Inflation",
        "US Unemployment",
        "US Fed Rate",
        "CAN Inflation",
        "CAN Unemployment",
        "CAN Interest Rate"
      )
    ),
    shock = factor(
      shock,
      levels = c("p", "u", "r", "p_can", "u_can", "r_can"),
      labels = c(
        "US Inflation shock",
        "US Unemployment shock",
        "US Fed Rate shock",
        "CAN Inflation shock",
        "CAN Unemployment shock",
        "CAN Interest Rate shock"
      )
    ),
    share = 100 * share
  )

p_fevd <- ggplot(fevd_df, aes(h, share, fill = shock)) +
  geom_area(alpha = 0.85) +                                 # Stacked area plot
  facet_wrap(~ variable, ncol = 3) +
  labs(title = "Forecast error variance decomposition",
       x = "Quarters ahead", y = "Share of variance", fill = NULL) +
  scale_fill_manual(
    values = c(
      "US Inflation shock"      = "#08306B",
      "US Unemployment shock"   = "#2171B5",
      "US Fed Rate shock"       = "#6BAED6",
      "CAN Inflation shock"     = "#00441B",
      "CAN Unemployment shock"  = "#238B45",
      "CAN Interest Rate shock" = "#74C476"
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave('Extension/Extension_1/Figures/FEVD_CAN_US.jpeg', p_fevd, width = 12, height = 8)

fevd_df_mex <- lapply(names(fevd_fit_mex), function(v) {
  d <- as.data.frame(fevd_fit_mex[[v]])
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
      levels = c("p", "u", "r", "p_mex", "u_mex", "r_mex"),
      labels = c(
        "US Inflation",
        "US Unemployment",
        "US Fed Rate",
        "MEX Inflation",
        "MEX Unemployment",
        "MEX Interest Rate"
      )
    ),
    shock = factor(
      shock,
      levels = c("p", "u", "r", "p_mex", "u_mex", "r_mex"),
      labels = c(
        "US Inflation shock",
        "US Unemployment shock",
        "US Fed Rate shock",
        "MEX Inflation shock",
        "MEX Unemployment shock",
        "MEX Interest Rate shock"
      )
    ),
    share = 100 * share
  )

p_fevd_mex <- ggplot(fevd_df_mex, aes(h, share, fill = shock)) +
  geom_area(alpha = 0.85) +                      
  facet_wrap(~ variable, ncol = 3) +
  labs(title = "Forecast error variance decomposition",
       x = "Quarters ahead", y = "Share of variance", fill = NULL) +
  scale_fill_manual(
    values = c(
      "US Inflation shock"      = "#08306B",
      "US Unemployment shock"   = "#2171B5",
      "US Fed Rate shock"       = "#6BAED6",
      "MEX Inflation shock"     = "#00441B",
      "MEX Unemployment shock"  = "#238B45",
      "MEX Interest Rate shock" = "#74C476"
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave('Extension/Extension_1/Figures/FEVD_MEX_US.jpeg', p_fevd_mex, width = 12, height = 8)

### Robustness cheks 

# Check for mexico instability 

var_us_mex_data_2001 <- macro_mex_us_post2001 %>%
  dplyr::select(p, u, r, p_mex, u_mex, r_mex) %>%
  na.omit()

lag_sel_mex_2001 <- VARselect(
  var_us_mex_data_2001,
  lag.max = 7,
  type = 'const'
)

lag_sel_mex_2001$selection

var_us_mex_2001 <- VAR(
  var_us_mex_data_2001, p = 2, type = 'const'
)

summary(var_us_mex_2001)

causality(var_us_mex_2001, cause = c("p", "u", "r"))

roots_mod_mex_2001 <- roots(var_us_mex_2001, modulus = TRUE)
max(roots_mod_mex_2001)

irf_mex_r_2001 <- irf(
  var_us_mex_2001,
  impulse = "r",
  response = c('p_mex', 'u_mex', 'r_mex'),
  n.ahead = 20,
  ortho = TRUE,
  boot = TRUE,
  runs = 500,
  ci = 0.95
)

### VAR for MEX (monthly revised)

var_us_mex_monthly <- macro_mex_us_monthly %>%
  dplyr::select(r_us, p_monthly, u_monthly, r_monthly) %>%
  na.omit()

lag_sel_mex_monthly <- VARselect(
  var_us_mex_monthly,
  lag.max = 7,
  type = 'const'
)

lag_sel_mex_monthly$selection

lag_selection_mex_out_monthly <- as.data.frame(lag_sel_mex_monthly$criteria) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

lag_selection_mex_monthly_typst <- tt(lag_selection_mex_out_monthly, rownames = TRUE)
tt_save(lag_selection_mex_monthly_typst, 'Extension/Extension_1/Figures/lag_table_mex_rest_monthly.typ')

var_us_mex_monthly <- VAR(
  var_us_mex_monthly, p = 2, type = 'const'
)

summary(var_us_mex_monthly)

causality(var_us_mex_monthly, cause = c("p", "u", "r"))

roots_mod_mex_monthly <- roots(var_us_mex_monthly, modulus = TRUE)
max(roots_mod_mex_monthly)

irf_mex_r_monthly <- irf(
  var_us_mex_monthly,
  impulse = "r_us",
  response = c('p_monthly', 'u_monthly', 'r_monthly'),
  n.ahead = 20,
  ortho = TRUE,
  boot = TRUE,
  runs = 500,
  ci = 0.95
)

jpeg(
  "Extension/Extension_1/Figures/irf_mex_r_monthly.jpeg",
  width = 1800,
  height = 900,
  res = 200
)

plot(irf_mex_r_monthly)

dev.off()

fevd_fit_mex_monthly <- fevd(
  var_us_mex_monthly,
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

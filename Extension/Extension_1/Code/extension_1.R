### --- EXTENSION 1: SPILLOVER EFFECTS ON MEX AND CAN ---

# --- CAN/US ---

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

var_us_can <- VAR(
  var_us_can_data,
  p = 2,
  type = 'const'
)

causality(var_us_can, cause = c("p", "u", "r"))

roots_mod_can <- roots(var_us_can, modulus = TRUE)
max(roots_mod_can)

# --- Restricted CAN model ---

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

var_us_can_rest <- VAR(
  var_us_can_data_rest,
  p = 2,
  type = 'const'
)

lag_selection_can_out <- as.data.frame(lag_sel_rest$criteria) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

lag_selection_can_typst <- tt(lag_selection_can_out, rownames = TRUE)

tt_save(
  lag_selection_can_typst,
  'Extension/Extension_1/Figures/lag_table_can_rest.typ'
)

causality(var_us_can_rest, cause = 'r')

roots_mod_can_rest <- roots(var_us_can_rest, modulus = TRUE)
max(roots_mod_can_rest)

# --- Mexico ---

var_us_mex_data <- macro_mex_us_post2000 %>%
  dplyr::select(
    p,
    u,
    r,
    p_mex,
    u_mex,
    r_mex
  ) %>%
  na.omit()

lag_sel_mex <- VARselect(
  var_us_mex_data,
  lag.max = 7,
  type = 'const'
)

lag_selection_mex_out <- as.data.frame(lag_sel_mex$criteria) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

lag_selection_mex_typst <- tt(lag_selection_mex_out, rownames = TRUE)

tt_save(
  lag_selection_mex_typst,
  'Extension/Extension_1/Figures/lag_table_mex_rest.typ'
)

var_us_mex <- VAR(
  var_us_mex_data,
  p = 2,
  type = 'const'
)

causality(var_us_mex, cause = c("p", "u", "r"))

roots_mod_mex <- roots(var_us_mex, modulus = TRUE)
max(roots_mod_mex)

# --- Restricted MEX model ---

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

var_us_mex_rest <- VAR(
  var_us_mex_data_rest,
  p = 2,
  type = 'const'
)

causality(var_us_mex_rest, cause = 'r')

roots_mod_mex_rest <- roots(var_us_mex_rest, modulus = TRUE)
max(roots_mod_mex_rest)

# --- MEX POST 2000 ---

var_us_mex_data_2001 <- macro_mex_us_post2000 %>%
  dplyr::select(
    p,
    u,
    r,
    p_mex,
    u_mex,
    r_mex
  ) %>%
  na.omit()

lag_sel_mex_2001 <- VARselect(
  var_us_mex_data_2001,
  lag.max = 7,
  type = 'const'
)

var_us_mex_2001 <- VAR(
  var_us_mex_data_2001,
  p = 2,
  type = 'const'
)

causality(var_us_mex_2001, cause = c("p", "u", "r"))

roots_mod_mex_2001 <- roots(var_us_mex_2001, modulus = TRUE)
max(roots_mod_mex_2001)

# --- MEX Monthly model ---

var_us_mex_monthly <- macro_mex_us_monthly %>%
  dplyr::select(
    r_us,
    p_monthly,
    u_monthly,
    r_monthly
  ) %>%
  na.omit()

lag_sel_mex_monthly <- VARselect(
  var_us_mex_monthly,
  lag.max = 7,
  type = 'const'
)

lag_selection_mex_out_monthly <- as.data.frame(
  lag_sel_mex_monthly$criteria
) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

lag_selection_mex_monthly_typst <- tt(
  lag_selection_mex_out_monthly,
  rownames = TRUE
)

tt_save(
  lag_selection_mex_monthly_typst,
  'Extension/Extension_1/Figures/lag_table_mex_rest_monthly.typ'
)

var_us_mex_monthly_fit <- VAR(
  var_us_mex_monthly,
  p = 2,
  type = "const"
)

# --- Combined IRF plot only ---

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

# --- Computation of Mean Square Errors --- 

horizons <- c(1, 4, 8, 12)

mse_results <- data.frame()

for(h in horizons) {

  errors_full <- c()
  errors_rest <- c()

  for(t in 40:(nrow(var_us_can_data) - h)) {

    train_full <- var_us_can_data[1:t, ]

    fit_full <- VAR(
      train_full,
      p = 2,
      type = "const"
    )

    fc_full <- predict(fit_full, n.ahead = h)$fcst

    forecast_full <- fc_full$p_can[h, "fcst"]

    actual_full <- var_us_can_data$p_can[t + h]

    errors_full <- c(
      errors_full,
      actual_full - forecast_full
    )

    train_rest <- var_us_can_data_rest[1:t, ]

    fit_rest <- VAR(
      train_rest,
      p = 2,
      type = "const"
    )

    fc_rest <- predict(fit_rest, n.ahead = h)$fcst

    forecast_rest <- fc_rest$p_can[h, "fcst"]

    actual_rest <- var_us_can_data_rest$p_can[t + h]

    errors_rest <- c(
      errors_rest,
      actual_rest - forecast_rest
    )
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

mse_results_table <- tt_save(
  tt(mse_results, rownames = FALSE),
  'Extension/Extension_1/Figures/MSE_rest_full.typ'
)

horizons <- c(1, 4, 8, 12)

mse_results_mex <- data.frame()

for(h in horizons) {
  
  errors_full <- c()
  errors_rest <- c()
  
  for(t in 40:(nrow(var_us_mex_data) - h)) {
    
    train_full <- var_us_mex_data[1:t, ]
    
    fit_full <- VAR(
      train_full,
      p = 2,
      type = "const"
    )
    
    fc_full <- predict(fit_full, n.ahead = h)$fcst
    
    forecast_full <- fc_full$p_mex[h, "fcst"]
    
    actual_full <- var_us_mex_data$p_mex[t + h]
    
    errors_full <- c(
      errors_full,
      actual_full - forecast_full
    )
    
    train_rest <- var_us_mex_data_rest[1:t, ]
    
    fit_rest <- VAR(
      train_rest,
      p = 2,
      type = "const"
    )
    
    fc_rest <- predict(fit_rest, n.ahead = h)$fcst
    
    forecast_rest <- fc_rest$p_mex[h, "fcst"]
    
    actual_rest <- var_us_mex_data_rest$p_mex[t + h]
    
    errors_rest <- c(
      errors_rest,
      actual_rest - forecast_rest
    )
  }
  
  mse_results_mex <- rbind(
    mse_results_mex,
    data.frame(
      horizon = h,
      mse_full = mean(errors_full^2),
      mse_restricted = mean(errors_rest^2)
    )
  )
}

mse_results_mex <- mse_results_mex %>% 
  mutate(across(where(is.numeric), ~ round(.x, 3)))

mse_results_mex_table <- tt_save(
  tt(mse_results_mex, rownames = FALSE),
  'Extension/Extension_1/Figures/MSE_mex_rest_full.typ'
)

# --- FEVD MEX/CAN ---

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

# --- Add. test: Granger causality ---

granger_can_full <- causality(
  var_us_can,
  cause = c("p", "u", "r")
)

granger_can_rest <- causality(
  var_us_can_rest,
  cause = 'r'
)

granger_mex_2001 <- causality(
  var_us_mex_2001,
  cause = c("p", "u", "r")
)

granger_mex_rest <- causality(
  var_us_mex_rest,
  cause = 'r'
)

granger_results <- data.frame(
  Model = c(
    "US-CAN",
    "US-CAN-REST",
    "US-MEX-2001",
    "US-MEX-REST"
  ),
  Cause = c(
    "p,u,r",
    "r",
    "p,u,r",
    "r"
  ),
  Statistic = c(
    granger_can_full$Granger$statistic,
    granger_can_rest$Granger$statistic,
    granger_mex_2001$Granger$statistic,
    granger_mex_rest$Granger$statistic
  ),
  P_value = c(
    granger_can_full$Granger$p.value,
    granger_can_rest$Granger$p.value,
    granger_mex_2001$Granger$p.value,
    granger_mex_rest$Granger$p.value
  ),
  DF = c(
    granger_can_full$Granger$parameter,
    granger_can_rest$Granger$parameter,
    granger_mex_2001$Granger$parameter,
    granger_mex_rest$Granger$parameter
  )
) %>%
  mutate(across(where(is.numeric), ~round(.x, 3)))

tt_save(
  tt(granger_results, rownames = FALSE),
  'Extension/Extension_1/Figures/granger_tests_can_mex.typ'
)
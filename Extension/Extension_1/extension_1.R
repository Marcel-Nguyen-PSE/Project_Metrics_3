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

par(mfrow = c(2, 2))

plot(
  irf_can_r,
  plot.type = "single"
)

plot(
  irf_can_p,
  plot.type = 'single'
)

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

# Mexico/US

var_us_mex_data <- macro_mex_us %>%
  dplyr::select(p, u, r, p_mex, u_mex, r_mex) %>%
  na.omit()

lag_sel_mex <- VARselect(
  var_us_mex_data,
  lag.max = 7,
  type = 'const'
)

lag_sel_mex$selection

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

#### FEVD decomp
fevd_fit_mex <- fevd(var_us_mex, n.ahead = 20)
fevd_fit_can <- fevd(var_us_can, n.ahead = 20)

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

print(p_fevd)

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
  geom_area(alpha = 0.85) +                                 # Stacked area plot
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

print(p_fevd_mex)

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
  var_us_mex_data_2001, p = 1, type = 'const'
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

plot(
  irf_mex_r
)


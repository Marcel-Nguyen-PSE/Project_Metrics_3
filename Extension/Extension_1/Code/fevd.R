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
# --- FEVD : MEX unrestricted model
#########################################################

fevd_fit_mex <- fevd(var_us_mex, n.ahead = 20)

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
  labs(
    title = "Forecast error variance decomposition",
    x = "Quarters ahead",
    y = "Share of variance",
    fill = NULL
  ) +
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

ggsave(
  'Extension/Extension_1/Figures/FEVD_MEX_US.jpeg',
  p_fevd_mex,
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
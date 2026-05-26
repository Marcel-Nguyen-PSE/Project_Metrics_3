### --- PRELIMINARIES & DATA PREP 2 ---

# --- Stationarity tests ---

# Creates a loop that fills the stationarity_1 data frame with corresponding statistics and p-value

stationarity_1 <- data.frame(
  variable = c('p', 'u', 'r'),
  PP_stat = NA,
  PP_pvalue = NA,
  DFGLS_stat = NA,
  DFGLS_cv5 = NA,
  KPSS_stat = NA,
  KPSS_pvalue = NA
)

for (j in seq_along(c('p','u','r'))) {
  y <- macro_1960_2000[[j]]
  
  pp_1 <- pp.test(y, alternative = "stationary")
  dfgls_1 <- ur.ers(y, type = "DF-GLS", model = "constant")
  kpss_1 <- kpss.test(y, null = "Level")
  
  stationarity_1$PP_stat[j] <- pp_1$statistic
  stationarity_1$PP_pvalue[j] <- pp_1$p.value
  
  stationarity_1$DFGLS_stat[j] <- dfgls_1@teststat # (We use @ and not $ because the package urca returns a S4 variable, with slots and not list)
  stationarity_1$DFGLS_cv5[j] <- dfgls_1@cval[1, "5pct"]
  
  stationarity_1$KPSS_stat[j] <- kpss_1$statistic
  stationarity_1$KPSS_pvalue[j] <- kpss_1$p.value
}

# --- Final output ---

# TYPST

stationarity_1_out <- stationarity_1 %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

stationarity_1_typst <- tt_save(tt(stationarity_1_out, rownames = FALSE), 'Replication/Figures/Preliminaries/Typst/stationarity_1_out.typ')

# LATEX 

stargazer(
  stationarity_1_out,
  type = "latex",
  summary = FALSE,
  rownames = FALSE,
  title = "Stationarity Tests",
  label = "tab:stationarity_1",
  out = "Replication/Figures/Preliminaries/LaTeX/stationarity_1.tex"
)

# PDF 

pdf("Replication/Figures/Preliminaries/PDF/stationarity_1.pdf", width = 10, height = 4)
grid.table(stationarity_1_out)
dev.off()

# --- Lag order selection tests ---

# Criteria 

lag_selection_1 <- VARselect(
  y = macro_1960_2000,
  lag.max = 8,
  type = "const"
)

lag_selection_1$selection
lag_selection_1$criteria

# Portmanteau tests

pt_table <- data.frame(
  lag = 1:8,
  PT_pvalue = NA
)

# Creates a loop that determines the PT stat for each lag order (1->8)

for (p in 1:8) {
  fit <- VAR(
    macro_1960_2000,
    p = p,
    type = "const"
  )
  test <- serial.test(
    fit,
    lags.pt = 16,
    type = "PT.asymptotic"
  )
  pt_table$PT_pvalue[p] <- test$serial$p.value
}

# --- Final output ---

# Portmanteau 

pt_table <- pt_table %>%
  mutate(across(where(is.numeric), ~ round(.x, 5)))

tt_save(tt(pt_table, rownames = FALSE), 'Replication/Figures/Preliminaries/Typst/pt_series_test.typ')

# Lag criteria 

lag_selection_1_out <- as.data.frame(lag_selection_1$criteria) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

# TYPST

lag_selection_typst <- tt(lag_selection_1_out, rownames = TRUE)
tt_save(lag_selection_typst, 'Replication/Figures/Preliminaries/Typst/lag_selection_test.typ')

# LATEX

stargazer(
  lag_selection_1_out,
  type = "latex",
  summary = FALSE,
  title = "VAR Lag Order Selection",
  label = "tab:lag_selection_1",
  out = "Replication/Figures/Preliminaries/LaTeX/lag_selection_1.tex"
)

# PDF 

pdf("Replication/Figures/Preliminaries/PDF/lag_selection_1.pdf", width = 10, height = 4)
grid.table(lag_selection_1_out)
dev.off()

# --- VAR of 1960-2000 US Macro ---

var_1 <- VAR(
  y = macro_1960_2000,
  p = 4,
  type = "const"
)

saveRDS(
  var_1,
  file = "Replication/R_objects/var_1.rds"
)

# --- ACF and PCF ---

par(mfrow = c(3, 2))

acf(macro_1960_2000$p, main = "ACF of inflation p", lag.max = 20)
pacf(macro_1960_2000$p, main = "PACF of inflation p", lag.max = 20)

acf(macro_1960_2000$u, main = "ACF of unemployment u", lag.max = 20)
pacf(macro_1960_2000$u, main = "PACF of unemployment u", lag.max = 20)

acf(macro_1960_2000$r, main = "ACF of federal funds rate r", lag.max = 20)
pacf(macro_1960_2000$r, main = "PACF of federal funds rate r", lag.max = 20)

par(mfrow = c(1, 1))

residuals_1 <- residuals(var_1)

acf(residuals_1[, "p"], main = "Residual ACF: p equation", lag.max = 20)
acf(residuals_1[, "u"], main = "Residual ACF: u equation", lag.max = 20)
acf(residuals_1[, "r"], main = "Residual ACF: r equation", lag.max = 20)

par(mfrow = c(1, 1))

# --- Stability (determines the unit roots of the VAR)

roots_1 <- roots(var_1, modulus = TRUE)

roots_1
max(roots_1)

serial_test_1 <- serial.test(
  var_1,
  lags.pt = 12,
  type = "PT.asymptotic"
)

roots_1_out <- data.frame(
  root_number = seq_along(roots_1),
  modulus = round(roots_1, 3)
)

# --- Final Output ---

# TYPST 

roots_1_typst <- tt_save(tt(roots_1_out, rownames = FALSE), 'Replication/Figures/Preliminaries/Typst/roots_1.typ')

# LATEX

stargazer(
  roots_1_out,
  type = "latex",
  summary = FALSE,
  rownames = FALSE,
  title = "VAR Stability Roots",
  label = "tab:roots_1",
  out = "Replication/Figures/Preliminaries/LaTeX/roots_1.tex"
)

# PDF

pdf("Replication/Figures/Preliminaries/PDF/roots_1.pdf", width = 7, height = 5)
grid.table(roots_1_out)
dev.off()

# --- Residual normality test ---

normality_test_1 <- normality.test(
  var_1,
  multivariate.only = TRUE
)

normality_test_1

# --- Final Output ---

diagnostics_1 <- data.frame(
  test = c(
    "Portmanteau serial correlation",
    "Multivariate Jarque-Bera normality"
  ),
  null_hypothesis = c(
    "No residual serial correlation",
    "Residuals are normally distributed"
  ),

  p_value = c(
    format.pval(serial_test_1$serial$p.value, digits = 3, eps = 0.001),       
    format.pval(normality_test_1$jb.mul$JB$p.value, digits = 3, eps = 0.001)
  )
)

# LATEX

stargazer(
  diagnostics_1,
  type = "latex",
  summary = FALSE,
  rownames = FALSE,
  title = "Residual Diagnostics",
  label = "tab:diagnostics_1",
  out = "Replication/Figures/Preliminaries/LaTeX/diagnostics_1.tex"
)

# PDF

pdf("Replication/Figures/Preliminaries/PDF/diagnostics_1.pdf", width = 10, height = 3)
grid.table(diagnostics_1)
dev.off()

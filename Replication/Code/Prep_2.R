#########################################################
# Step 0: Check data
#########################################################

str(macro_1960_2000)
summary(macro_1960_2000)
colnames(macro_1960_2000)

# Expected names:
# p = inflation
# u = unemployment
# r = federal funds rate

#########################################################
# Step 1: Stationarity tests
#########################################################

stationarity_1 <- data.frame(
  variable = colnames(macro_1960_2000),
  PP_stat = NA,
  PP_pvalue = NA,
  DFGLS_stat = NA,
  DFGLS_cv5 = NA,
  KPSS_stat = NA,
  KPSS_pvalue = NA
)

for (j in seq_along(colnames(macro_1960_2000))) {
  
  y <- macro_1960_2000[[j]]
  
  pp_1 <- pp.test(y, alternative = "stationary")
  dfgls_1 <- ur.ers(y, type = "DF-GLS", model = "constant")
  kpss_1 <- kpss.test(y, null = "Level")
  
  stationarity_1$PP_stat[j] <- pp_1$statistic
  stationarity_1$PP_pvalue[j] <- pp_1$p.value
  
  stationarity_1$DFGLS_stat[j] <- dfgls_1@teststat
  stationarity_1$DFGLS_cv5[j] <- dfgls_1@cval[1, "5pct"]
  
  stationarity_1$KPSS_stat[j] <- kpss_1$statistic
  stationarity_1$KPSS_pvalue[j] <- kpss_1$p.value
}

# PP test/ DF-GLS test:
# H0 = unit root / non-stationary
# Reject H0 if p-value < 0.05 or if test statistic is more negative than the 5% critical value

# KPSS test:
# H0 = stationary
# Reject H0 if p-value < 0.05

#########################################################
# Step 2: Lag order selection
#########################################################

lag_selection_1 <- VARselect(
  y = macro_1960_2000,
  lag.max = 8,
  type = "const"
)

lag_selection_1$selection
lag_selection_1$criteria

pt_table <- data.frame(
  lag = 1:8,
  PT_pvalue = NA
)

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
pt_table <- pt_table %>%
  mutate(across(where(is.numeric), ~ round(.x, 5)))

tt_save(tt(pt_table, rownames = FALSE), 'Replication/Figures/Preliminaries/Typst/pt_series_test.typ')

# FPE(n): Final prediction error
# Lower value = preferred lag length.

#########################################################
# Step 4: Estimate Figure 1 VAR
#########################################################


var_1 <- VAR(
  y = macro_1960_2000,
  p = 4,
  type = "const"
)

summary(var_1)

#########################################################
# Save objects for other scripts
#########################################################

dir.create(
  "Replication/R_objects",
  recursive = TRUE,
  showWarnings = FALSE
)

saveRDS(
  var_1,
  file = "Replication/R_objects/var_1.rds"
)

saveRDS(
  macro_1960_2000,
  file = "Replication/R_objects/macro_1960_2000.rds"
)

#########################################################
# Step 5: ACF and PACF
#########################################################

par(mfrow = c(3, 2))

acf(macro_1960_2000$p, main = "ACF of inflation p", lag.max = 20)
pacf(macro_1960_2000$p, main = "PACF of inflation p", lag.max = 20)

acf(macro_1960_2000$u, main = "ACF of unemployment u", lag.max = 20)
pacf(macro_1960_2000$u, main = "PACF of unemployment u", lag.max = 20)

acf(macro_1960_2000$r, main = "ACF of federal funds rate r", lag.max = 20)
pacf(macro_1960_2000$r, main = "PACF of federal funds rate r", lag.max = 20)

par(mfrow = c(1, 1))

# ACF: checks persistence and autocorrelation.
# PACF: checks direct lag relationships after controlling for earlier lags.
# Slow ACF decay suggests high persistence.

#########################################################
# Step 6: VAR stability
#########################################################

roots_1 <- roots(var_1, modulus = TRUE)

roots_1
max(roots_1)

# VAR is stable if all roots are below 1.
# max(roots_1) < 1 means stable.
# max(roots_1) close to 1 means highly persistent.

#########################################################
# Step 7: Residual serial correlation
#########################################################

serial_test_1 <- serial.test(
  var_1,
  lags.pt = 12,
  type = "PT.asymptotic"
)

serial_test_1

# H0 = no residual serial correlation.
# p-value > 0.05: residual serial correlation not detected.
# p-value < 0.05: residuals still autocorrelated.

#########################################################
# Step 8: Residual normality
#########################################################

normality_test_1 <- normality.test(
  var_1,
  multivariate.only = TRUE
)

normality_test_1

# H0 = residuals are normally distributed.
# p-value > 0.05: fail to reject normality.
# p-value < 0.05: reject normality.
# Macro VAR residuals often reject normality.

#########################################################
# Step 9: Residual ACF plots
#########################################################

residuals_1 <- residuals(var_1)

par(mfrow = c(3, 1))

acf(residuals_1[, "p"], main = "Residual ACF: p equation", lag.max = 20)
acf(residuals_1[, "u"], main = "Residual ACF: u equation", lag.max = 20)
acf(residuals_1[, "r"], main = "Residual ACF: r equation", lag.max = 20)

par(mfrow = c(1, 1))

# This visually checks whether residual autocorrelation remains
# equation by equation.

#########################################################
# Export preliminary results: LaTeX + PDF + View
#########################################################

dir.create("Replication/Figures/Preliminaries/LaTeX", recursive = TRUE, showWarnings = FALSE)
dir.create("Replication/Figures/Preliminaries/PDF", recursive = TRUE, showWarnings = FALSE)
dir.create("Replication/Figures/Preliminaries/PNG", recursive = TRUE, showWarnings = FALSE)

# Stationarity test results

stationarity_1_out <- stationarity_1 %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

stationarity_1_typst <- tt_save(tt(stationarity_1_out, rownames = FALSE), 'Replication/Figures/Preliminaries/Typst/stationarity_1_out.typ')

# Visualize in R
View(stationarity_1_out)
print(stationarity_1_out)

# Export LaTeX
stargazer(
  stationarity_1_out,
  type = "latex",
  summary = FALSE,
  rownames = FALSE,
  title = "Stationarity Tests",
  label = "tab:stationarity_1",
  out = "Replication/Figures/Preliminaries/LaTeX/stationarity_1.tex"
)

# Export PDF
pdf("Replication/Figures/Preliminaries/PDF/stationarity_1.pdf", width = 10, height = 4)
grid.table(stationarity_1_out)
dev.off()


#lag selection table

lag_selection_1_out <- as.data.frame(lag_selection_1$criteria) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

View(lag_selection_1_out)
print(lag_selection_1_out)

lag_selection_typst <- tt(lag_selection_1_out, rownames = TRUE)
tt_save(lag_selection_typst, 'Replication/Figures/Preliminaries/Typst/lag_selection_test.typ')

stargazer(
  lag_selection_1_out,
  type = "latex",
  summary = FALSE,
  title = "VAR Lag Order Selection",
  label = "tab:lag_selection_1",
  out = "Replication/Figures/Preliminaries/LaTeX/lag_selection_1.tex"
)

pdf("Replication/Figures/Preliminaries/PDF/lag_selection_1.pdf", width = 10, height = 4)
grid.table(lag_selection_1_out)
dev.off()


#stability root table 

roots_1_out <- data.frame(
  root_number = seq_along(roots_1),
  modulus = round(roots_1, 3)
)

View(roots_1_out)
print(roots_1_out)

stargazer(
  roots_1_out,
  type = "latex",
  summary = FALSE,
  rownames = FALSE,
  title = "VAR Stability Roots",
  label = "tab:roots_1",
  out = "Replication/Figures/Preliminaries/LaTeX/roots_1.tex"
)

pdf("Replication/Figures/Preliminaries/PDF/roots_1.pdf", width = 7, height = 5)
grid.table(roots_1_out)
dev.off()

# Residual diagnostics table

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
    format.pval(serial_test_1$serial$p.value, digits = 3, eps = 0.001),         #because otherwise it put results like 1e-04 or 0e+00
    format.pval(normality_test_1$jb.mul$JB$p.value, digits = 3, eps = 0.001)
  )
)
View(diagnostics_1)
print(diagnostics_1)

stargazer(
  diagnostics_1,
  type = "latex",
  summary = FALSE,
  rownames = FALSE,
  title = "Residual Diagnostics",
  label = "tab:diagnostics_1",
  out = "Replication/Figures/Preliminaries/LaTeX/diagnostics_1.tex"
)

pdf("Replication/Figures/Preliminaries/PDF/diagnostics_1.pdf", width = 10, height = 3)
grid.table(diagnostics_1)
dev.off()

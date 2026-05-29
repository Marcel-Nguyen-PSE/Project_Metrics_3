#########################################################
# Stationarity tests
#########################################################

run_stationarity <- function(data) {
  
  stationarity <- data.frame(
    variable = colnames(data),
    PP_stat = NA,
    PP_pvalue = NA,
    DFGLS_stat = NA,
    DFGLS_cv5 = NA,
    KPSS_stat = NA,
    KPSS_pvalue = NA
  )
  
  for (j in seq_along(colnames(data))) {
    
    y <- data[[j]]
    
    pp <- pp.test(y, alternative = "stationary")
    dfgls <- ur.ers(y, type = "DF-GLS", model = "constant")
    kpss <- kpss.test(y, null = "Level")
    
    stationarity$PP_stat[j] <- pp$statistic
    stationarity$PP_pvalue[j] <- pp$p.value
    
    stationarity$DFGLS_stat[j] <- dfgls@teststat
    stationarity$DFGLS_cv5[j] <- dfgls@cval[1, "5pct"]
    
    stationarity$KPSS_stat[j] <- kpss$statistic
    stationarity$KPSS_pvalue[j] <- kpss$p.value
  }
  
  stationarity %>%
    mutate(across(where(is.numeric), ~ round(.x, 3)))
}

stationarity_precrisis <- run_stationarity(macro_monthly_precrisis)
stationarity_postcrisis <- run_stationarity(macro_monthly_postcrisis)

stationarity_precrisis
stationarity_postcrisis


#########################################################
# Lag order selection
#########################################################

lag_selection_precrisis <- VARselect(
  y = macro_monthly_precrisis,
  lag.max = 12,
  type = "const"
)

lag_selection_postcrisis <- VARselect(
  y = macro_monthly_postcrisis,
  lag.max = 12,
  type = "const"
)

lag_selection_precrisis$selection
lag_selection_precrisis$criteria

lag_selection_postcrisis$selection
lag_selection_postcrisis$criteria


#########################################################
# Portmanteau residual tests by lag
#########################################################

run_portmanteau_by_lag <- function(data, max_lag = 12, lags_pt = 24) {
  
  pt_table <- data.frame(
    lag = 1:max_lag,
    PT_pvalue = NA
  )
  
  for (p_lag in 1:max_lag) {
    
    fit <- VAR(
      y = data,
      p = p_lag,
      type = "const"
    )
    
    test <- serial.test(
      fit,
      lags.pt = lags_pt,
      type = "PT.asymptotic"
    )
    
    pt_table$PT_pvalue[p_lag] <- test$serial$p.value
  }
  
  pt_table %>%
    mutate(PT_pvalue = round(PT_pvalue, 5))
}

pt_precrisis <- run_portmanteau_by_lag(macro_monthly_precrisis)
pt_postcrisis <- run_portmanteau_by_lag(macro_monthly_postcrisis)

pt_precrisis
pt_postcrisis


#########################################################
# Stability roots for selected lag orders
#########################################################

var_precrisis <- VAR(
  y = macro_monthly_precrisis,
  p = 2,
  type = "const"
)

var_postcrisis <- VAR(
  y = macro_monthly_postcrisis,
  p = 4,
  type = "const"
)

max(roots(var_precrisis, modulus = TRUE))
max(roots(var_postcrisis, modulus = TRUE))

roots_precrisis <- data.frame(
  max_root = round(
    max(roots(var_precrisis, modulus = TRUE)),
    3
  )
)

roots_postcrisis <- data.frame(
  max_root = round(
    max(roots(var_postcrisis, modulus = TRUE)),
    3
  )
)


#########################################################
# Export test tables for Typst
#########################################################

export_png_table <- function(table_obj, filename,
                             width = 2200,
                             height = 800) {
  
  table_obj <- as.data.frame(table_obj)
  rownames(table_obj) <- NULL
  
  png(
    paste0("Extension/Extension_2/Figures/", filename, ".png"),
    width = width,
    height = height,
    res = 180
  )
  
  grid.table(table_obj, rows = NULL)
  
  dev.off()
}

# Stationarity tables

export_png_table(
  stationarity_precrisis,
  "stationarity_precrisis",
  width = 2400,
  height = 600
)

export_png_table(
  stationarity_postcrisis,
  "stationarity_postcrisis",
  width = 2400,
  height = 600
)

# Joint Portmanteau table

pt_compare <- pt_precrisis %>%
  rename(PT_precrisis = PT_pvalue) %>%
  left_join(
    pt_postcrisis %>%
      rename(PT_postcrisis = PT_pvalue),
    by = "lag"
  )

export_png_table(
  pt_compare,
  "portmanteau_pre_post",
  width = 1400,
  height = 900
)

# Joint root table

roots_compare <- data.frame(
  sample = c("Pre-crisis VAR(2)", "Post-crisis VAR(4)"),
  max_root = c(
    round(max(roots(var_precrisis, modulus = TRUE)), 3),
    round(max(roots(var_postcrisis, modulus = TRUE)), 3)
  )
)

export_png_table(
  roots_compare,
  "roots_compare",
  width = 1200,
  height = 450
)

# Lag selection tables

lag_selection_precrisis_out <- cbind(
  criterion = rownames(lag_selection_precrisis$criteria),
  round(as.data.frame(lag_selection_precrisis$criteria), 3)
)

lag_selection_postcrisis_out <- cbind(
  criterion = rownames(lag_selection_postcrisis$criteria),
  round(as.data.frame(lag_selection_postcrisis$criteria), 3)
)

export_png_table(
  lag_selection_precrisis_out,
  "lag_selection_precrisis",
  width = 3000,
  height = 700
)

export_png_table(
  lag_selection_postcrisis_out,
  "lag_selection_postcrisis",
  width = 3000,
  height = 700
)


#########################################################
# Taylor-rule forward-looking coefficient diagnostics
#########################################################

compute_forward_coefficients <- function(var_model, k = 12) {
  
  n_var <- length(var_model$varresult)
  p_lag <- var_model$p
  n_state <- n_var * p_lag
  
  A <- sapply(
    var_model$varresult,
    function(eq) coef(eq)[1:n_state]
  ) %>%
    t()
  
  Phi <- rbind(
    A,
    cbind(
      diag(n_state - n_var),
      matrix(0, n_state - n_var, n_var)
    )
  )
  
  Phi_i <- diag(n_state)
  F_k <- matrix(0, n_state, n_state)
  
  for (i in 1:k) {
    Phi_i <- Phi %*% Phi_i
    F_k <- F_k + Phi_i
  }
  
  F_k <- F_k / k
  
  fp0 <- 1.5
  fu0 <- 0.5 * (-2.5)
  
  fp_raw <- fp0 * F_k[1, 1] + fu0 * F_k[2, 1]
  fu_raw <- fp0 * F_k[1, 2] + fu0 * F_k[2, 2]
  fr_raw <- fp0 * F_k[1, 3] + fu0 * F_k[2, 3]
  
  fp_fwd <- fp_raw / (1 - fr_raw)
  fu_fwd <- fu_raw / (1 - fr_raw)
  
  data.frame(
    fp_fwd = fp_fwd,
    fu_fwd = fu_fwd,
    fr_raw = fr_raw,
    denominator = 1 - fr_raw
  )
}

coeff_fwd_precrisis <- compute_forward_coefficients(var_precrisis, k = 12)
coeff_fwd_postcrisis <- compute_forward_coefficients(var_postcrisis, k = 12)

coeff_fwd_precrisis
coeff_fwd_postcrisis

coeff_fwd_precrisis$denominator
coeff_fwd_postcrisis$denominator
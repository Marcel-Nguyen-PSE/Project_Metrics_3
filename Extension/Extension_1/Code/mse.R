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

##################################

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




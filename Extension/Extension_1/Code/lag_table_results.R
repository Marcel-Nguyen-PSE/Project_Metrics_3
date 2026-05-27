# --- CAN/US unrestricted ---

lag_sel <- VARselect(
  var_us_can_data,
  lag.max = 8,
  type = 'const'
)

# --- CAN/US restricted ---

lag_sel_rest <- VARselect(
  var_us_can_data_rest,
  lag.max = 8,
  type = 'const'
)

lag_selection_can_out <- as.data.frame(lag_sel_rest$criteria) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

lag_selection_can_typst <- tt(lag_selection_can_out, rownames = TRUE)

tt_save(
  lag_selection_can_typst,
  'Extension/Extension_1/Figures/lag_table_can_rest.typ'
)

# --- MEX unrestricted ---

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

# --- MEX restricted ---

lag_sel_rest_mex <- VARselect(
  var_us_mex_data_rest,
  lag.max = 8,
  type = 'const'
)

# --- MEX post-2000 ---

lag_sel_mex_2001 <- VARselect(
  var_us_mex_data_2001,
  lag.max = 7,
  type = 'const'
)

# --- MEX monthly ---

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
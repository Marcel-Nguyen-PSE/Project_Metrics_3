
#### Reduced form VAR 

var_1 <- VAR(
  y = macro_1960_2000,
  p = 4, 
  type = 'const'
)

summary(var_1)

###### VAR Descriptive statistics (Table 1) 

### Panel A : Manual Granger Causality table -> get all p-values from each sub-regression

# p dependent variable 

var_1_p <- var_1$varresult$p

p_val_p_p <- linearHypothesis(var_1_p, c(
  "p.l1 = 0", "p.l2 = 0", "p.l3 = 0", "p.l4 = 0"
))[2, "Pr(>F)"]

p_val_p_u <- linearHypothesis(var_1_p, c(
  "u.l1 = 0", "u.l2 = 0", "u.l3 = 0", "u.l4 = 0"
))[2, "Pr(>F)"]

p_val_p_r <- linearHypothesis(var_1_p, c(
  "r.l1 = 0", "r.l2 = 0", "r.l3 = 0", "r.l4 = 0"
))[2, "Pr(>F)"]

# u dependent variable 

var_1_u <- var_1$varresult$u

p_val_u_p <- linearHypothesis(var_1_u, c(
  "p.l1 = 0", "p.l2 = 0", "p.l3 = 0", "p.l4 = 0"
))[2, "Pr(>F)"]

p_val_u_u <- linearHypothesis(var_1_u, c(
  "u.l1 = 0", "u.l2 = 0", "u.l3 = 0", "u.l4 = 0"
))[2, "Pr(>F)"]

p_val_u_r <- linearHypothesis(var_1_u, c(
  "r.l1 = 0", "r.l2 = 0", "r.l3 = 0", "r.l4 = 0"
))[2, "Pr(>F)"]

# r dependent variable 

var_1_r <- var_1$varresult$r

p_val_r_p <- linearHypothesis(var_1_r, c(
  "p.l1 = 0", "p.l2 = 0", "p.l3 = 0", "p.l4 = 0"
))[2, "Pr(>F)"]

p_val_r_u <- linearHypothesis(var_1_r, c(
  "u.l1 = 0", "u.l2 = 0", "u.l3 = 0", "u.l4 = 0"
))[2, "Pr(>F)"]

p_val_r_r <- linearHypothesis(var_1_r, c(
  "r.l1 = 0", "r.l2 = 0", "r.l3 = 0", "r.l4 = 0"
))[2, "Pr(>F)"]

table_1 <- t(matrix(c(
  p_val_p_p, p_val_p_u, p_val_p_r,
  p_val_u_p, p_val_u_u, p_val_u_r,
  p_val_r_p, p_val_r_u, p_val_r_r
), nrow = 3, byrow = TRUE))

rownames(table_1) <- c("p", "u", "r")
colnames(table_1) <- c("p", "u", "r")

round(table_1, 2)

# Final output

table_1_panelA <- tt(
  data.frame(table_1),
  rownames = TRUE,       
  align = "center"
)

tt_save(table_1_panelA, 'Figures/Fig_1/Typst/table_1_panelA_Granger.typ') #Tyspt Export
stargazer(table_1, type = 'latex', out = 'Figures/Fig_1/LaTeX/table_1_panelA_Granger.tex')


### Variance decomposition : Panel B, C, D

var_comp <- fevd(var_1, n.ahead = 12)

var_comp_p <- var_comp$p[c(1,4,8,12),]
var_comp_u <- var_comp$u[c(1,4,8,12),]
var_comp_r <- var_comp$r[c(1,4,8,12),]

var_comp_comb <- bind_cols(
  var_comp_p, var_comp_u, var_comp_r
) %>%
mutate(across(where(is.numeric), ~ round(.x, 2)))

table1_var_comp <- tt(data.frame(var_comp_comb), rownames = FALSE, align = 'center')

stargazer(data.frame(var_comp_comb), type = 'latex', out = 'Figures/Fig_1/Latex/table_1_var_decomp_comb.tex')
tt_save(table1_var_comp, 'Figures/Fig_1/Typst/table_1_var_decomp_comb.typ')

### Forecast errors

fe <- predict(var_1, n.ahead = 12, ci = 0.95)

se_fe_p <- (fe$fcst$p[c(1,4,8,12), 'upper'] - fe$fcst$p[c(1,4,8,12), 'lower']) / (2 *1.96)
se_fe_u <- (fe$fcst$u[c(1,4,8,12), 'upper'] - fe$fcst$u[c(1,4,8,12), 'lower']) / (2 *1.96)
se_fe_r <- (fe$fcst$r[c(1,4,8,12), 'upper'] - fe$fcst$r[c(1,4,8,12), 'lower']) / (2 *1.96)

fe_err_comb <- bind_cols(
  se_fe_p, se_fe_u, se_fe_r
) %>%
mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
mutate(horizon = c(1,4,8,12))

table1_fe_err <- tt(data.frame(fe_err_comb), rownames = FALSE, align = 'center')

stargazer(data.frame(fe_err_comb), type = 'latex', out = 'Figures/Fig_1/LaTeX/table_1_forecast_err.tex')
tt_save(table1_fe_err, 'Figures/Fig_1/Typst/table_1_forecast_err.typ')




library(tidyverse)
library(readxl)
library(rio)
library(xtable)
library(here)
library(gtsummary)
library(glue)
library(scales)
library(patchwork)
library(stargazer)
library(sandwich)
library(lmtest)
library(AER)
library(car)
library(haven)
library(fixest) 
library(sf)
library(did)
library(rdrobust)
library(TwoWayFEWeights)
library(Synth)
library(fredr)
library(tseries)
library(urca)
library(vars)
library(forecast)
library(typstable)
library(lubridate)

fred_key <- Sys.getenv('FRED_API_KEY')

if (fred_key == "") {
  stop("FRED_API_KEY is not set.")
}

fredr_set_key(fred_key)
######################################################### Main dataset using fred package and monthly obs.

gdpd <- fredr(series_id = "GDPDEF",
              observation_start = as.Date('1955-01-01'),
              observation_end   = as.Date('2019-12-31'))

unrate <- fredr(series_id = "UNRATE",
                observation_start = as.Date('1955-01-01'),
                observation_end   = as.Date('2019-12-31'))

ffr <- fredr(series_id = "FEDFUNDS",
             observation_start = as.Date('1955-01-01'),
             observation_end   = as.Date('2019-12-31'))

# Quarterly averages for monthly series 

unrate_q <- unrate %>%
  mutate(date = floor_date(date, "quarter")) %>% 
  group_by(date) %>%
  summarise(u = mean(value), .groups = "drop")

ffr_q <- ffr %>%
  mutate(date = floor_date(date, "quarter")) %>%
  group_by(date) %>%
  summarise(r = mean(value), .groups = "drop")

# building the final data set : 3 VAR variables

macro <- gdpd %>%
  transmute(date, gdpd = value) %>%
  left_join(unrate_q, by = "date") %>%
  left_join(ffr_q, by = "date") %>%
  arrange(date) %>%
  mutate(p = 400 * log(gdpd / lag(gdpd))) %>%    #annualized quaterly inflation
  dplyr::select(date, p, u, r)

### Inflation variable 

macro_1960_2000 <- macro %>%
  filter(date >= as.Date("1960-01-01"),
         date <  as.Date("2001-01-01")) %>%      #keeps 1960:I to 2000:IV.
  dplyr::select(p,u,r)

#### Reduced form VAR : 4 lags and a constant.

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

tt_save(table_1_panelA, 'table_1_panel_A.typ') #Tyspt Export
stargazer(table_1, type = 'latex', out = 'table_1_panelA.tex')


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
tt_save(table1_var_comp, 'table_1_var_decomp_comb.typ')

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
tt_save(table1_fe_err, 'table_1_forecast_err.typ')

horizons <- c(1, 4, 8, 12)

panelA <- round(table_1, 2)

panelB_p <- data.frame(
  h = horizons,
  se = round(se_fe_p, 2),
  round(var_comp$p[horizons, ] * 100, 0)
)

panelB_u <- data.frame(
  h = horizons,
  se = round(se_fe_u, 2),
  round(var_comp$u[horizons, ] * 100, 0)
)

panelB_r <- data.frame(
  h = horizons,
  se = round(se_fe_r, 2),
  round(var_comp$r[horizons, ] * 100, 0)
)

make_rows <- function(df) {
  paste0(
    df$h, " & ", df$se, " & ", df$p, " & ", df$u, " & ", df$r, " \\\\",
    collapse = "\n"
  )
}

latex_code <- paste0(
"\\begin{table}[!htbp]
\\centering
\\caption{VAR Descriptive Statistics for $(p,u,r)$}

\\begin{tabular}{lccc}
\\hline
\\multicolumn{4}{l}{\\textbf{A. Granger-Causality Tests}} \\\\
\\hline
 & \\multicolumn{3}{c}{Dependent Variable} \\\\
\\cline{2-4}
Regressor & $p$ & $u$ & $r$ \\\\
\\hline
$p$ & ", panelA["p","p"], " & ", panelA["p","u"], " & ", panelA["p","r"], " \\\\
$u$ & ", panelA["u","p"], " & ", panelA["u","u"], " & ", panelA["u","r"], " \\\\
$r$ & ", panelA["r","p"], " & ", panelA["r","u"], " & ", panelA["r","r"], " \\\\
\\hline
\\end{tabular}

\\vspace{0.3cm}

\\begin{tabular}{lcccc}
\\multicolumn{5}{l}{\\textbf{B.i. Variance Decomposition of $p$}} \\\\
\\hline
Horizon & SE & $p$ & $u$ & $r$ \\\\
\\hline
", make_rows(panelB_p), "
\\hline
\\end{tabular}

\\vspace{0.3cm}

\\begin{tabular}{lcccc}
\\multicolumn{5}{l}{\\textbf{B.ii. Variance Decomposition of $u$}} \\\\
\\hline
Horizon & SE & $p$ & $u$ & $r$ \\\\
\\hline
", make_rows(panelB_u), "
\\hline
\\end{tabular}

\\vspace{0.3cm}

\\begin{tabular}{lcccc}
\\multicolumn{5}{l}{\\textbf{B.iii. Variance Decomposition of $r$}} \\\\
\\hline
Horizon & SE & $p$ & $u$ & $r$ \\\\
\\hline
", make_rows(panelB_r), "
\\hline
\\end{tabular}

\\end{table}"
)

writeLines(latex_code, "table_1_full.tex")

saveRDS(var_1, "var_1_reduced_form.rds")
saveRDS(macro_1960_2000, "macro_1960_2000.rds")
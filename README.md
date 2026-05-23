# Project Metrics 3

This repo replicates the paper *'Monetary Policy Forecasts and Shocks Spillovers of U.S on Canada and Mexico: replication and extension of a structural VAR model'*.

# Replication 

## Code summary
0) Data prep files: **Prep_1.R**, **Prep_2.R** and **Prep_3.R** have to be fully ran first.  
1) **figure_1.R**: reproduces the Variance Decomposition Tables of the paper (i.e Table 1)  
2) **figure_2.R**: reproduces Impulse Responses in the Inflation-Unemployment Recursive VAR (i.e, Figure 1)  
3) **figure_3.R**: recomputes coefficients of monetary rule and replicates Impulse Responses of Monetary Shocks for Different Taylor Rules (i.e, Figure 2)  
4) **Prep_2.R**: produces preliminaries on lag-order criteria, asymptotic normality, stationarity of VAR tests  
5) **Prep_3.R**: produces a comparison between lag order specifications  

## Figures summary 
NB: (**.typ** and **.tex** only change the file format, not the content) 

### 1) Fig_1
- **table_1_forecast_err** : Forecast Standard Error Decomposition (Panel B, Table 1)
- **table_1_panelA_Granger** : Granger Causality Tests of p/u/r in US (Panel A, Table 1)
- **table_1_var_decomp_comb** : Variance Decomposition Table of all variables given an impulse variable (p/u/r) (Panel B, Table 1)

### 2) Fig_2 
- **irf_var_1.jpeg** : Impulse Response Functions in the original VAR with backward-looking Taylor Rule 

### 3) Fig_3 
- **irf_taylor.jpeg** : Impulse Response Functions in both specifications of Taylor Rule (backward-looking and forward-looking)

### 4) Preliminaries 
- **diagnostics_1.pdf** : Portmanteau serial correlation and Multivariate Jarque-Bera normality tests result table
- **lag_selection_1.pdf** : AIC, HQ, SC, FPE lag-selection order tests result table 
- **roots_1.pdf** : Table of root moduli given lag orders 
- **stationarity_1.pdf** : PP, DFGLS, KPSS statistics and associated p-values for each variable 
- **irf_lag_comparison.jpeg** : Comparison plot of impulse response functions across lag-order specifications


# Extensions 

## Data summary 

### 1) Extension_1 
- **Can_GDP_Deflator_Ind2017.csv** : quarterly observations from 1961 to 2023 of GDP deflator index (base 2017) for Canada (source: OECD DATABASE PORTAL)
- **LRHUTTTTMXM1562** : monthly observations from 1987 to 2026 of Mexican unemployment rate for individuals above 15 years old (source: FRED DATABASE)
- **Mex_GDP_Deflator_Ind2017.csv** : quarterly observations from 1961 to 2023 of GDP deflator index (base 2017) for Mexico (source: OECD DATABASE PORTAL)

## Code summary 

0) Data prep files: **Prep_1.R**, **Prep_2.R**, **Prep_3.R**, **Prep_4.R** have to be ran first.  
1) **extension_1.R**: produces outputs for the study of spillovers on Canada (1960–2019) and Mexico (2000–2019)  
2) **extension_2.R**: produces outputs for the study of pre/post-2008 financial crisis dynamics in the US  

## Figures summary 

### 1) Extension_1 
- **acf_pacf_restricted** : ACF and PACF plots of the restricted CAN model 
- **FEVD_CAN_US** : Forecast Error Variance Decomposition plot of the restricted CAN model 
- **FEVD_MEX_monthly_US** : Forecast Error Variance Decomposition of the monthly restricted MEX model
- **FEVD_MEX_US_rest** : Forecast Error Variance Decomposition of the restricted quarterly MEX model 
- **FEVD_MEX_US** : Forecast Error Variance Decomposition of the unrestricted quarterly MEX model
- **irf_can_us_r_rest** : IRFs of the restricted CAN model in response to a FED interest-rate shock 
- **irf_can_us_r** : IRFs of the unrestricted CAN model in response to a FED interest-rate shock
- **irf_full_vs_restricted** : Comparison of restricted and unrestricted CAN IRFs following a FED interest-rate shock 
- **irf_mex_monthly** : IRFs of the monthly restricted MEX model in response to a FED interest-rate shock 
- **irf_mex_us_p** : IRFs of the unrestricted MEX model in response to an inflation shock
- **irf_mex_us_r** : IRFs of the unrestricted MEX model in response to an interest-rate shock
- **lag_table_can_rest** : Lag-order selection table for the restricted CAN model 
- **lag_table_mex_rest_monthly** : Lag-order selection table for the restricted monthly MEX model
- **lag_table_mex_rest** : Lag-order selection table for the restricted quarterly MEX model
- **MSE_rest_full** : Comparison of mean squared errors between restricted and unrestricted CAN models
- **plot_mex_var** : Plot of Mexican inflation, unemployment, and interest-rate dynamics

### 2) Extension_2 
- **irf_postcrisis** : Orthogonal IRFs of a FED shock in the post-crisis sample (> 2008)
- **irf_precrisis** : Orthogonal IRFs of a FED shock in the pre-crisis sample (< 2008)
- **lag_selection_postcrisis** : Lag-selection table (AIC, HQ, BIC, FPE) for the post-crisis sample
- **lag_selection_precrisis** : Lag-selection table (AIC, HQ, BIC, FPE) for the pre-crisis sample
- **portmanteau_pre_post** : Portmanteau test comparison table across subsamples
- **roots_compare** : Stability-root comparison table across subsamples
- **stationarity_postcrisis** : PP/DFGLS/KPSS statistics and p-values for the post-crisis sample
- **stationarity_precrisis** : PP/DFGLS/KPSS statistics and p-values for the pre-crisis sample


# Diary log

## Revised 

- **23/04/2026** — Reproduction of Table 1 coefficients > switched from quarterly point values to manually aggregated monthly averages > obtained results much closer to the paper, remaining differences likely due to revised FRED data.

- **23/04/2026** — Replication output organization > exported Table 1 in **.typ**, **.tex**, and consolidated PDF formats > replication tables became fully operational and reusable.

- **25/04/2026** — Missing diagnostics in the original paper > implemented KPSS, PP, DFGLS, lag-order selection, and Portmanteau tests > obtained evidence of strong persistence and justified VAR specifications econometrically.

- **25/04/2026** — Reproduction of Figure 2 IRFs > reused recursive ordering and bootstrap confidence intervals > recovered similar dynamics but different scales and asymmetric confidence bands due to R plotting defaults and bootstrap construction.

- **26/04/2026** — Reproduction of Figure 3 monetary-rule IRFs > replaced actual future observations with VAR-based forecasts > corrected the forward-looking Taylor-rule implementation.

- **15/05/2026** — Revision of Figure 2 and Figure 3 replication > explicitly selected the intended variables and reimplemented the Taylor rule > obtained much more convincing impulse responses.

- **15/05/2026** — First extension on international spillovers > estimated VARs with US and Mexican variables > found significant spillover effects on Mexican unemployment and monetary policy.

- **15/05/2026** — Mexican data-quality issue > identified missing unemployment observations and replaced them with OECD series > obtained more stable and economically plausible VAR estimates.

- **15/05/2026** — Preliminary econometric diagnostics > implemented stationarity and lag-order specification procedures and exported outputs > completed the preliminary validation stage of the replication.

- **19/05/2026** — Investigation of remaining discrepancies with the paper > translated original **.gss** and **.srt** replication files and compared them with the R implementation > identified coefficient and Taylor-rule specification differences and substantially improved replication quality.

- **19/05/2026** — Simplification of the international extension > retained only the US FED rate as US explanatory variable > obtained cleaner and more interpretable VAR results.

- **21/05/2026** — Monthly-data extension for Mexico > proxied inflation with monthly CPI inflation to increase sample size > approximately doubled the number of observations, although results remained somewhat unstable.

- **21/05/2026** — Pre/post financial-crisis subsample extension > constructed monthly CPI-based datasets and implemented stationarity and lag-order diagnostics > retained VAR(2) for the pre-crisis sample and VAR(4) for the post-crisis sample.

- **23/05/2026** — Final documentation phase > revised the README and integrated explicit Taylor-rule coefficients into the methodological description > improved transparency and reproducibility of the project.
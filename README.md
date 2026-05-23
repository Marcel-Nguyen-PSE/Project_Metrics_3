# Project Metrics 3

This repo replicates the paper 'Monetary Policy Forecasts and Shocks Spillovers of U.S on Canada and Mexico: replication and
extension of a structural VAR model'.

# Replication folder:

## Code summary
0) Data prep files: Prep_1.R, Prep_2.R and Prep_3.R have to be fully ran first.
1) figure_1.R: reproduces the Variance Decomposition Tables of the paper (i.e Table 1)
2) figure_2.R: reproduces Impulse Responses in the Inflation-Unemployment Recursive VAR (i.e, Figure 1)
3) figure_3.R: recomputes coefficients of monetary rule and replicates Impulse Responses of Monetary Shocks for Different Taylor Rules (i.e, Figure 2)
4) Prep_2.R: produces preliinaries on lag-order criteria, asymptotic normality, stationarity of VAR tests
4) Prep_3.R: produces a comparison between lag orger specifications 

## Figures summary 
NB: (.typ and .tex only changes the file format, not the content) 

1) Fig_1
- table_1_forecast_err : Forecast Standard Error Decomposition (Panel B, Table 1)
- table_1_panelA_Granger : Granger Causality Tests of p/u/r in US (Panel A, Table 1)
- table_1_var_decomp_comb : Variance Decomposition Table of all variables given an impulse variable (p/u/r) (Panel B, Table 1)

2) Fig_2 
- irf_var_1.jpeg : Impulse Response Functions in the original VAR with backward looking Taylor Rule 

3) Fig_3 
- irf_taylor.jpeg : Impulse Response Functions in both specifications of Taylor Rule (backward looking and forward looking)

4) Preliminaries 
- diagnostics_1.pdf : Portmanteau serial correlation and Multivariate Jarque-Bera normality tests result table
- lag_selection_1.pdf : AIC, HQ, SC, FPE lag selection order tests result table 
- roots_1.pdf : Table of roots modulus given orders 
- stationarity_1.pdf : PP, DFGLS, KPSS statistic and associated p-value for each variable 
- irf_lag_comparison.jpeg : comparison plot of impulse responses functions of the two monetary rule specification to an interest rate (r) shock


# Extensions 

## Data summary 

1) Extension_1 
- Can_GDP_Deflator_Ind2017.csv : quaterly observations from 1961 to 2023 of deflated GDP, index 2017 (source: OECD DATABASE PORTAL)
- LRHUTTTTMXM1562 : monthly observations from 1987 to 2026 of MEX unemployment rate for over 15 years (source: FRED DATABASE)
- Mex_GDP_Deflator_Ind2017.csv : quaterly observations from 1961 to 2023 of deflated GDP, index 2017 (source: OECD DATABASE PORTAL)

## Code summary 

0) Data prep files: Prep_1.R, Prep_2.R, Prep_3.R, Prep_4.R have to be ran first 
1) extension_1.R: produces outputs for the study of spillovers on Canada (1960-2019) and Mexico (2000-2019)
2) extension_2.R: produces output for the study of pre/post 2008 financial crisis for US 

## Figures summary 

1) Extension_1 
- acf_pacf_restricted : ACF and PACF plots of the restricted CAN model 
- FEVD_CAN_US : Forecast Error Variance Decomposition plot of the CAN restricted model 
- FEVD_MEX_monthly_US  : Forecast Error Variance Decomposition of the monthly restricted MEX model
- FEVD_MEX_US_rest : Forecast Error Variance Decomposition of the restricted QUATERLY MEX Model 
- FEVD_MEX_US : Forecast Error Variance Decomposition of the UNRESTRICTED QUATERLY MEX Model
- irf_can_us_r_rest : IRF functions of the CAN Restricted model in response to a FED interest rate shock 
- irf_can_us_r : IRF functions of the CAN UNRESTRICTED model in response to a FED interest rate shock
-  irf_full_vs_restricted : IRF functions of the CAN Restricted model vs. the UNRESTRICTED model in response to a FED interest rate shock 
- irf_mex_monthly : IRF functions of the MEX MONTHLY Restricted model in response to a FED interest rate shock 
- irf_mex_us_p : IRF functions of the MEX UNRESTRICTED model in response to an inflation rate shock
- irf_mex_us_r : IRF functions of the MEX UNRESTRICTED model in response to an interest rate shock
- lag_table_can_rest : lag order selection order table for the CAN RESTRICTED model 
- lag_table_mex_rest_monthly : lag order selection order table for the MEX RESTRICTED MONTHLY model
- lag_table_mex_rest : lag order selection order table for the MEX RESTRICTED QUATERLY model
MSE_rest_full : comparison of Mean Squared Errors of the CAN UNRESTRICTED model and RESTRICTED model table
- plot_mex_var : plot of trends in p, u, and r of Mexico

2) Extension_2 : 
- irf_postcrisis : Orthogonal IRF of a FED shock on post crisis (> 2008) sample
- irf_precris : Orthogonal IRF of a FED shock on pre crisis (< 2008) sample
- lag_selection_postcrisis : lag selection table (AIC, HC, BIC, FQ) of post crisis 
- lag_selection_precrisis : lag selection table (AIC, HC, BIC, FQ) of pre crisis 
- portmanteau_pre_post : Portmanteau test table for pre sample and post sample 
- roots_compare : comparison table of unit roots for pre and post sample
- stationarity_postcrisis : PP/DFGLS/KPSS p value and statistic for post crisis sample
- stationarity_precrisis : PP/DFGLS/KPSS p value and statistic for pre crisis sample


# Diary log

## Revised 

- 23/04/2026 — Reproduction of Table 1 coefficients > switched from quarterly point values to manually aggregated monthly averages > obtained results much closer to the paper, remaining differences likely due to revised FRED data.

- 23/04/2026 — Replication output organization > exported Table 1 in `.typ`, `.tex`, and consolidated PDF formats > replication tables became fully operational and reusable.

- 25/04/2026 — Missing diagnostics in the original paper > implemented KPSS, PP, DFGLS, lag-order selection, and Portmanteau tests > obtained evidence of strong persistence and justified VAR specifications econometrically.

- 25/04/2026 — Reproduction of Figure 2 IRFs > reused recursive ordering and bootstrap confidence intervals > recovered similar dynamics but different scales and asymmetric confidence bands due to R plotting defaults and bootstrap construction.

- 26/04/2026 — Reproduction of Figure 3 monetary-rule IRFs > replaced actual future observations with VAR-based forecasts > corrected the forward-looking Taylor-rule implementation.

- 15/05/2026 — Revision of Figure 2 and Figure 3 replication > explicitly selected the intended variables and reimplemented the Taylor rule > obtained much more convincing impulse responses.

- 15/05/2026 — First extension on international spillovers > estimated VARs with US and Mexican variables > found significant spillover effects on Mexican unemployment and monetary policy.

- 15/05/2026 — Mexican data-quality issue > identified missing unemployment observations and replaced them with OECD series > obtained more stable and economically plausible VAR estimates.

- 15/05/2026 — Preliminary econometric diagnostics > implemented stationarity and lag-order specification procedures and exported outputs > completed the preliminary validation stage of the replication.

- 19/05/2026 — Investigation of remaining discrepancies with the paper > translated original `.gss` and `.srt` replication files and compared them with the R implementation > identified coefficient and Taylor-rule specification differences and substantially improved replication quality.

- 19/05/2026 — Simplification of the international extension > retained only US Fed funds rate as US explanatory variable > obtained cleaner and more interpretable VAR results.

- 21/05/2026 — Monthly-data extension for Mexico > proxied inflation with monthly CPI inflation to increase sample size > approximately doubled the number of observations, although results remained somewhat unstable.

- 21/05/2026 — Pre/post financial-crisis subsample extension > constructed monthly CPI-based datasets and implemented stationarity and lag-order diagnostics > retained VAR(2) for the pre-crisis sample and VAR(4) for the post-crisis sample.

- 23/05/2026 — Final documentation phase > revised the README and integrated explicit Taylor-rule coefficients into the methodological description > improved transparency and reproducibility of the project.

## Original

23/04/26: I reproduced the panel A of Table 1 but coefficients differ significantly -> solution: take montly data then compute manually quaterly averages as in the paper (rather than quarter point values from fred) - > I obtain much similar results, minor significances due to the fred data being revised ?

Panel A of Table 1 is saved in the .typ format and .tex for practicity

25/04/26 : table 1 results are reproduced in a main latex files. pdf is available.
question : in the paper: no stationnarity test: KPSS, PP,... no lag choice justification with AIC/HQ/SC/FPE; should we do it ?

IRFs are computed using the same recursive ordering and horizon, but plotted using R’s default axis. Csq : scaling so it diverges a biut in scales compared to the SW IRFs. Moreover using bootstraps ci 0.66 led to asymmetric confidence bounds. but to use gaussian  approximation, one would need to manually construct bands, (estimate standard errors from simulations/bootstrap standard deviations and plot which is heavy).

26/04/26: I try to reproduce the figure 3 cont. I forgot to use VAR predictions for forward and not actual future observations...

30/04/26 "...

15/05/26: I revised the fig 2 (fig 3) replication in order to take the desired variables instead of present, much more convincing output but still different. I began the first extension by looking at spillover effects of US monetary and macro conjoncture on neighbor countries (e.g, US and Mexico). Results yield a strng spillover effect in unemployment and monetary policy in Mexico, we may focus on this after. However, results are not really stable for mexico, due to missing data range....may need to recompute with more reliable data.

15/05/26: preliminaries (stationarity, lag specification are realised, exported in pdf)

19/05/26: the impulse functions of different monetary rules are different from the paper. I tried to look in the original rep package to see how the authors computed it. I used Claude to translate .gss and .srt files and to compare it with my R code. I used the coefficients in plain value and I used explicitly the taylor rule of the paper. The code yields much better results. 

For the first extension, I kept only US FED Rate as US variable as suggested, and computed IRF and other tests. 

For mexico, I noticed that some values were missing (eg, the u rate that was NA from 2005 to 2017). I retrived proper data of unemployment from the OECD data base. (this may explain the absence of consistency in the prev VAR). I have much more plausible results. 

21/05/2026 : Again for Mexico, I try to use monthly data to have more observations. I proxy monthly inflation rate with monthly Consumer Price Index (CPI), it approx. doubles the sample size. Results are somehow inconsistent. 
21/05 : I started the subsample extension : for the inflation proxy I switched to the CPI as to have monthly data for all periods, realised stationarity and lag order selection: I get VAR 2 and 4 and keep this specification.

23/05/2026: I wrote the final readme. I also include the coefficients of the monetary rule of fig3 inside for comprehensiveness instead of plain values. 
















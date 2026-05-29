# Project Metrics 3

This repo replicates the paper *'Monetary Policy Forecasts and Shocks Spillovers of U.S on Canada and Mexico: replication and extension of a structural VAR model'*.

# Replication 

## Code summary
0) Data prep files: **Prep_1.R**, **Prep_2.R** and **Prep_3.R** have to be fully ran first. The file **run_prep_all.R** does this. 
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
- **LRHUTTTTMXM156S** : monthly observations from 1987 to 2026 of Mexican unemployment rate for individuals above 15 years old (source: FRED DATABASE)
- **Mex_GDP_Deflator_Ind2017.csv** : quarterly observations from 1961 to 2023 of GDP deflator index (base 2017) for Mexico (source: OECD DATABASE PORTAL)

## Code summary 

0) Data prep files: **Prep_1.R**, **Prep_2.R**, **Prep_3.R**, **prep_4.R**, **Prep_5.R** have to be ran first for all extensions 
1) **extension_1.R**: produces outputs for the study of spillovers on Canada (1960–2019) and Mexico (2000–2019) which include: 
    - **fevd.R**: computes only the FEVD decomposition plots of the extension_1
    - **irfs.R**: computes only the IRFs plots of extension_1
    - **lag_table_results.R**: computes only the lag-order selection tables of extension_1
    - **mse.R**: computes only the mean squared errors (MSE) of extension_1
2) **extension_2.R**: produces outputs for the study of pre/post-2008 financial crisis dynamics in the US which include :
    - **irfs.R**: computes only the IRFs plots of extension_2
    - **tests.R**: computes only the stationarity test, lag order tests, and other inference tests of extension_2

## Figures summary 

### 1) Extension_1 
- **FEVD_CAN_US** : Forecast Error Variance Decomposition plot of the restricted CAN model 
- **FEVD_MEX_monthly_US** : Forecast Error Variance Decomposition of the monthly restricted MEX model
- **FEVD_MEX_US_rest** : Forecast Error Variance Decomposition of the restricted quarterly MEX model 
- **irf_can_us_r_rest** : IRFs of the restricted CAN model in response to a FED interest-rate shock 
- **irf_can_us_r** : IRFs of the unrestricted CAN model in response to a FED interest-rate shock
- **irf_full_vs_restricted** : Comparison of restricted and unrestricted CAN IRFs following a FED interest-rate shock 
- **lag_table_can_rest** : Lag-order selection table for the restricted CAN model 
- **lag_table_mex_rest_monthly** : Lag-order selection table for the restricted monthly MEX model
- **lag_table_mex_rest** : Lag-order selection table for the restricted quarterly MEX model
- **MSE_rest_full** : Comparison of mean squared errors between restricted and unrestricted CAN models
- **MSE_mex_rest_full** : Comparison of mean squared errors between restricted and unrestricted MEX models


### 2) Extension_2 
- **irf_pre_post_comparison** : Orthogonal IRFs of a FED shock in the pre-crisis sample (< 2008) and in the post-crisis sample (> 2008)
- **irf_combined** : combination of both IRFs plots (pre and post) 
- **irf_taylor_backward_forward_pre_post**: grid plot of IRFs responses wrt taylor rule specification (backward looking and forward looking)
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

- **23/05/2026** — Subsample IRF extension and structural VAR estimation > refined recursive VAR specifications and tested forward-/backward-looking Taylor rules > identified instability and economically implausible IRFs in forward-looking specifications due to fixed-point correction and persistent monthly dynamics.

- **24/05/2026** — Replication and stationarity diagnostics correction > verified PP/DFGLS test conventions and normalized initial Taylor-rule shocks > reconciled stationarity test interpretation and successfully reproduced the paper’s replication graph.

- **27/06/2026** - Reorganization of the code repository and debug of obsolete code.

- **28/06/2026** - Noticed an inconsistent result about the lag-order selection test of Mexico Quarterly and Monthly > refound plausible specifications.

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

23/05 : I computed better IRFs for extension on subsamples, concentrated analysis on response to monetary shock for recursive VAR. Then I tried to build the structural VAR. i got problems with the IRFS: in figure 3 to compute the best IRFs, authors used an unusual sign convention. Firstly for backward-looking Taylor rule, one can see that post crisis dynamics are not really stable for unemployment. For forward looking taylor rule, I used k = 12 months at the beginning but got weird IRFs so I checked fu_fwd = 13.55 which showed that forward-looking rule is unstable and 1 - fwd_precrisis$fr_raw = -0.0845 so the fixed-point correction is blowing up the coefficients. I try again using k = 4 to have more stable coefficients (because shorter forecast horizon leads to weaker cumulative persistence and smaller fr_raw). But it did not solve the main issue that monthly subsample VAR dynamics are still too persistent/noisy, especially with CPI. In conclusion, forward-looking coefficients remain economically implausible.

24/05/2026 : I reread the table of stationarity results in preliminary section and i realized that p values and statistics were incompatible for PP test. so I tried to find why and it was just because they don't use the usual tau statistic but the Dickey-Fuller Z(alpha) statistic. Similarly for the DFGLS it was not a p value but the critic value at 5%.

24/05/2026 : Alexia noticed that the taylor rule in replication 1 was wrong. I corrected it by normalizing the intitial shocks to 1. Now the Graph is identical to the one on the paper

24/05/2026 : i redo my IRFs following the right taylor rule as well. I try normalizing the initial shocks to 1. I get IRFs with inflation near 1000, real rates near -1200, unstable unemployment oscillations. so given the weak identification, the unstable structural decomposition, I renounce to transfer the quaterly Taylor-rule framework to monthly subsamples. I try to normalize by SD of ra instead ie I scale all IRFs so the contemporaneous nominal-rate response equals 1 percentage point. My IRFs represent a response to a one-standard-deviation monetary-policy shock which solves the scale problem but not the sign reversal i have for unemployment. 

27/05/2026 : reorganized the code so it is easier to go through, deleted some parts that became obsolete and assessed new tests on lag order selection for Mexico as it was unclear in the presentation. 

28/05/2026 : recomputed the lag-order of the monthly and quarterly speicification, found more coherent result esp about the VAR spec of Mexico 
29/05/26 : we finished the redaction of the report taking into account remarks regarding the addition of figures within the main part.

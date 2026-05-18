# Project_Metrics_3


# Replication 
1) Run Prep.R to get the 1960-2000 data of the paper 
2) Figure_X.R files can be ran indepedently, but increasing order yields the most stable outputs.

# Extensions 


# Diary log

23/04/26: I reproduced the panel A of Table 1 but coefficients differ significantly -> solution: take montly data then compute manually quaterly averages as in the paper (rather than quarter point values from fred) - > I obtain much similar results, minor significances due to the fred data being revised ?

Panel A of Table 1 is saved in the .typ format and .tex for practicity

25/04/26 : table 1 results are reproduced in a main latex files. pdf is available.
question : in the paper: no stationnarity test: KPSS, PP,... no lag choice justification with AIC/HQ/SC/FPE; should we do it ?

IRFs are computed using the same recursive ordering and horizon, but plotted using R’s default axis. Csq : scaling so it diverges a biut in scales compared to the SW IRFs. Moreover using bootstraps ci 0.66 led to asymmetric confidence bounds. but to use gaussian  approximation, one would need to manually construct bands, (estimate standard errors from simulations/bootstrap standard deviations and plot which is heavy).

26/04/26: I try to reproduce the figure 3 cont. I forgot to use VAR predictions for forward and not actual future observations...

30/04/26 "...

15/05/26: I revised the fig 2 (fig 3) replication in order to take the desired variables instead of present, much more convincing output but still different. I began the first extension by looking at spillover effects of US monetary and macro conjoncture on neighbor countries (e.g, US and Mexico). Results yield a strng spillover effect in unemployment and monetary policy in Mexico, we may focus on this after. However, results are not really stable for mexico, due to missing data range....may need to recompute with more reliable data.
15/05/26: preliminaries (stationarity, lag specification are realised, exported in pdf)





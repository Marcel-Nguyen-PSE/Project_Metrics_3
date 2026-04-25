# Project_Metrics_3
Project Econometrics 3

# Replication of Table 1 

23/04/26: I reproduced the panel A of Table 1 but coefficients differ significantly -> solution: take montly data then compute manually quaterly averages as in the paper (rather than quarter point values from fred) - > I obtain much similar results, minor significances due to the fred data being revised ?

Panel A of Table 1 is saved in the .typ format and .tex for practicity

25/04/26 : table 1 results are reproduced in a main latex files. pdf is available.
question : in the paper: no stationnarity test: KPSS, PP,... no lag choice justification with AIC/HQ/SC/FPE; should we do it ?
IRFs are computed using the same recursive ordering and horizon, but plotted using R’s default axis. Csq : scaling so it diverges a biut in scales compared to the SW IRFs.

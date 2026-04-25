
#########################################################
# Figure 1: Impulse Response Functions
# Recursive VAR ordered as p, u, r



# Load VAR object estimated in figure_1.R
var_1 <- VAR(
  y = macro_1960_2000,
  p = 4, 
  type = 'const'
)

# Check stability before IRFs
roots_mod <- roots(var_1, modulus = TRUE)
print(max(roots_mod))

# Compute Cholesky orthogonalized IRFs
irf_var_1 <- irf(
  var_1,
  impulse = c("p", "u", "r"),
  response = c("p", "u", "r"),
  n.ahead = 24,
  ortho = TRUE,     #Cholesky orthogonalization
  boot = TRUE,       #bootstrap confidence bands
  runs = 1000,
  ci = 0.66          #ci = 0.66 bec in Stock-Watson ±1 SE bands, ie 66% intervals.
)

jpeg("Replication/Figures/Fig_2/irf_var_1.jpeg",
     width = 1800, height = 1800, res = 150)
par(mfrow = c(3, 3))
plot(irf_var_1, plot.type = "single")
dev.off()









library(vars)

var_1 <- readRDS("var_1_reduced_form.rds")

#########################################################
# Figure 1: Impulse Response Functions
# Recursive VAR ordered as p, u, r

library(vars)

# Load VAR object estimated in figure_1.R
var_1 <- readRDS("var_1_reduced_form.rds")

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

# Save IRF figure as PDF
pdf("figure_1_irf_recursive_var.pdf", width = 10, height = 8)

par(mfrow = c(3,3))
plot(irf_var_1, plot.type = "single")

dev.off()
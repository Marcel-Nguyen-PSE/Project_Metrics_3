
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

plot(irf_var_1, plot.type = "single")



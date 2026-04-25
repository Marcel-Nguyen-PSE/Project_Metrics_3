
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


dir.create("Replication/Figures/Fig_2", recursive = TRUE, showWarnings = FALSE)

impulses  <- c("p", "u", "r")
responses <- c("p", "u", "r")
n_imp     <- length(impulses)
n_resp    <- length(responses)

jpeg("Replication/Figures/Fig_2/irf_var_1.jpeg",
     width  = 1800,
     height = 1800,
     res    = 150)

# 3×3 grid: rows = responses, columns = impulses
layout(matrix(1:(n_imp * n_resp), nrow = n_resp, ncol = n_imp, byrow = TRUE))
par(mar = c(3, 3, 2, 1), mgp = c(1.8, 0.5, 0))

for (resp in responses) {
  for (imp in impulses) {

    irf_vals   <- irf_var_1$irf[[imp]][, resp]
    upper_vals <- irf_var_1$Upper[[imp]][, resp]
    lower_vals <- irf_var_1$Lower[[imp]][, resp]
    horizon    <- 0:(length(irf_vals) - 1)

    y_range <- range(c(irf_vals, upper_vals, lower_vals))

    plot(horizon, irf_vals,
         type  = "l", lwd = 2, col = "black",
         ylim  = y_range,
         xlab  = "Horizon", ylab = "",
         main  = paste("Impulse:", imp, "→ Response:", resp),
         cex.main = 0.85)

    # ±1 SE confidence bands (shaded)
    polygon(c(horizon, rev(horizon)),
            c(upper_vals, rev(lower_vals)),
            col = rgb(0.7, 0.7, 0.7, 0.4), border = NA)

    # Band borders
    lines(horizon, upper_vals, lty = 2, col = "grey40")
    lines(horizon, lower_vals, lty = 2, col = "grey40")

    # Zero line
    abline(h = 0, lty = 1, col = "red", lwd = 0.8)
  }
}

dev.off()


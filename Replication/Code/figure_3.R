### --- MONETARY TAYLOR RULE / IRF FORWARD BACKWARD ---

# --- Coefficients of rules (cf. rep package) ---

fp_back <- 1.5 / 4
fu_back <- (0.5 / 4) * (-2.5)

# --- Forward looking ---
var_1 <- VAR(macro_1960_2000 %>% drop_na() %>% dplyr::select(p, u, r), p = 4, type = "const")

A   <- t(do.call(cbind, lapply(var_1$varresult, function(eq) coef(eq)[1:12])))
Phi <- rbind(A, cbind(diag(9), matrix(0, 9, 3)))

k <- 4; Phi_i <- diag(12); F_k <- matrix(0, 12, 12)
for (i in 1:k) { Phi_i <- Phi %*% Phi_i; F_k <- F_k + Phi_i }
F_k <- F_k / k

fp0 <- 1.5; fu0 <- 0.5 * (-2.5)
fp2 <- fp0 * F_k[1,1] + fu0 * F_k[2,1]
fu2 <- fp0 * F_k[1,2] + fu0 * F_k[2,2]
fr2 <- fp0 * F_k[1,3] + fu0 * F_k[2,3]
fp_fwd <- fp2 / (1 - fr2)
fu_fwd <- fu2 / (1 - fr2)

# --- Taylor residuals ---
taylor_backward <- macro_1960_2000 %>%
  mutate(ra = r - fp_back * p - fu_back * u) %>%
  drop_na() %>% dplyr::select(ra, p, u)

taylor_forward <- macro_1960_2000 %>%
  mutate(ra = r - fp_fwd * p - fu_fwd * u) %>%
  drop_na() %>% dplyr::select(ra, p, u)

# --- VAR ---
var_back <- VAR(taylor_backward, p = 4, type = "const")
var_forw <- VAR(taylor_forward,  p = 4, type = "const")

# --- IRF  ---
irf_back <- irf(var_back, impulse = "ra", response = c("ra","p","u"),
                n.ahead = 24, ortho = TRUE, boot = FALSE)
irf_forw <- irf(var_forw, impulse = "ra", response = c("ra","p","u"),
                n.ahead = 24, ortho = TRUE, boot = FALSE)

# --- Recover r and normalize by r[1] ---
recover_irf <- function(irf_obj, fp, fu) {
  ra  <- irf_obj$irf$ra[, "ra"]
  p   <- irf_obj$irf$ra[, "p"]
  u   <- irf_obj$irf$ra[, "u"]
  r   <- ra + fp * p + fu * u
  std <- r[1]
  list(p      = p       / std,
       u      = u       / std,
       r_real = (r - p) / std)
}

res_back <- recover_irf(irf_back, fp_back, fu_back)
res_forw <- recover_irf(irf_forw, fp_fwd,  fu_fwd)

# --- Plot ---
horizon <- 0:24

jpeg("Replication/Figures/Fig_3/irf_taylor.jpeg", width = 1800, height = 1200, res = 150)
par(mfrow = c(2, 2))

plot(horizon, res_back$p, type="l", lwd=2,
     main="Response of Inflation", xlab="Lag", ylab="Percent", ylim=c(-2.5, 0.5))
lines(horizon, res_forw$p, lty=2, lwd=2)
abline(h=0, col="red")
legend("topright", legend=c("Backward-looking","Forward-looking"), lty=c(1,2), lwd=2, bty="n")

plot(horizon, res_back$u, type="l", lwd=2,
     main="Response of Unemployment", xlab="Lag", ylab="Percent", ylim=c(-1, 1))
lines(horizon, res_forw$u, lty=2, lwd=2)
abline(h=0, col="red")

plot(horizon, res_back$r_real, type="l", lwd=2,
     main="Response of Real Interest Rate", xlab="Lag", ylab="Percent", ylim=c(-2.5, 3.5))
lines(horizon, res_forw$r_real, lty=2, lwd=2)
abline(h=0, col="red")

dev.off()

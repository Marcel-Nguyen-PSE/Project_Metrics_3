### --- IMPULSE RESPONSE FUNCTIONS --- ###

# --- Loading the VAR or 1960-2000 US --- 

var_1 <- VAR(
  y = macro_1960_2000,
  p = 4, 
  type = 'const'
)

# --- Check of roots ---

roots_mod <- roots(var_1, modulus = TRUE)
print(max(roots_mod))

# --- Computation of IRF ---

irf_var_1 <- irf(
  var_1,
  impulse = c("p", "u", "r"),
  response = c("p", "u", "r"),
  n.ahead = 24,
  ortho = TRUE,    
  boot = TRUE,      
  runs = 1000,
  ci = 0.66         
)

# --- Final Output --- 

jpeg("Replication/Figures/Fig_2/irf_var_1.jpeg",
     width = 1800, height = 1800, res = 150)
par(mfrow = c(3, 3))
plot(irf_var_1, plot.type = "single")
dev.off()









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

# --- Bonus ordering 

var_1_data_alt <- VAR(
  y = macro_1960_2000 %>% dplyr::select(p, u, r),
  p = 4, 
  type = 'const'
)

irf_var_1_alt <- irf(
  var_1_data_alt,
  impulse = c("p", "u", "r"),
  response = c("p", "u", "r"),
  n.ahead = 24,
  ortho = TRUE,    
  boot = TRUE,      
  runs = 1000,
  ci = 0.66         
)

# --- Final Output --- 

jpeg("Replication/Figures/Fig_2/irf_var_1_alt.jpeg",
     width = 1800, height = 1800, res = 150)
par(mfrow = c(3, 3))
plot(irf_var_1_alt, plot.type = "single")
dev.off()

# Another ordering

var_1_data_alt2 <- VAR(
  y = macro_1960_2000 %>% dplyr::select(r, p, u),
  p = 4, 
  type = 'const'
)

irf_var_1_alt2 <- irf(
  var_1_data_alt2,
  impulse = c("p", "u", "r"),
  response = c("p", "u", "r"),
  n.ahead = 24,
  ortho = TRUE,    
  boot = TRUE,      
  runs = 1000,
  ci = 0.66         
)

# --- Final Output --- 

jpeg("Replication/Figures/Fig_2/irf_var_1_alt2.jpeg",
     width = 1800, height = 1800, res = 150)
par(mfrow = c(3, 3))
plot(irf_var_1_alt, plot.type = "single")
dev.off()




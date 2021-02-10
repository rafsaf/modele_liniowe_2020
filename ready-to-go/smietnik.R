b1_est = 6.814
alpha <- 0.1
s2_b0 = 16.0
s_b0 <- sqrt(s2_b0)

T_b0 = b1_est / s_b0
T_b0

tc <- qt(1-alpha/2, 27)

abs(T_b0) > tc
# Odrzucamy hipoteze zerowa, gdy |T| > tc
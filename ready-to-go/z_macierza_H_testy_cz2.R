
n <- 72
p <- 3
alpha <- 0.05
MSE <- 1.12 # s^2
beta <- cbind(c(-0.1, 3.03, -1.82))
# Intercept, x1, x2
c <- rbind(c(1, 1, 2))

mu_h <- c %*% beta
mu_h <- mu_h[1,1]

M_x_xprim <- rbind(c(1.7764,  -0.2558,  0.7227), c(-0.2558,  2.5168,  -0.459),
                c(0.7227,  -0.459,  1.7068))  
M_x_xprim
 

s2_mu_h = MSE * c %*% M_x_xprim %*% t(c)
s2_mu_h <- s2_mu_h[1,1]
s_mu_h = sqrt(s2_mu_h)


# przedzialy ufnosci
tc <- qt(1-alpha/2, n-p)

c(mu_h - tc*s_mu_h, mu_h, mu_h + tc*s_mu_h)

# T staty i p-wartosc, wziac c(0,0,1) dla b2 itp
T_stat = abs(mu_h / s_mu_h)
# Odrzucamy hipoteze zerowa, gdy p-value < 0.1
2 * pt(T_stat, n-p, lower.tail = FALSE)










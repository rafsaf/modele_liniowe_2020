
n <- 20
kow_x_y <- 15.0
kow_x_x <- 25.0
s2_b1 <- 0.08
alpha <- 0.05

# H0: b1 = 0  vs H1: b1 != 0

b1 <- kow_x_y / kow_x_x
s_b1 <- sqrt(s2_b1)
T_stat <- b1 / s_b1
tc = qt(1-alpha/2, n - 2)

abs(T_stat) > tc
# Odrzucamy hipoteze zerowa, gdy |T| > tc

2 * pt(T_stat, n-2, lower.tail = FALSE)
# Odrzucamy hipoteze zerowa, gdy p-value < 0.05
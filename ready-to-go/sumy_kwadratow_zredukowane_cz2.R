SSM_Full <- 160.0 + 40.0 + 40.0
SST_Full <- 540.0
SSE_Full <- SST_Full - SSM_Full

SSM <- 40.0
# SSM(X2, X3 | X1) = SSM(F)-SSM = SSE(X1) - SSE(F)
# SSE(X1) = SSM(F) - SSM + SSE(F)
SSE <- SSM_Full - SSM + SSE_Full
n <- 46


dfE <- n - 2
dfT <- n - 1
dfM <- 1

MSM <- SSM / dfM
MSE <- SSE / dfE

# H0 beta1 =  0 przeciwko hipotezie H1  beta_1 != 0

F_stat <- MSM / MSE
alpha  <- 0.05
tf <- qf(1-alpha, 1, n-2)
abs(F_stat) > tf
# Odrzucamy hipoteze zerowa, gdy |T| > tc

2 * pf(F_stat,1, n-2, lower.tail = FALSE)
# Odrzucamy hipoteze zerowa, gdy p-value < 0.1


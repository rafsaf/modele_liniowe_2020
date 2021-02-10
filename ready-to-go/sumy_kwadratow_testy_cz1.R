alpha  <- 0.1
n <- 26

MST <- 76.2


dfE <- 24
dfT <- 25
dfM <- 1

SSM <- 141
SST <- MST * dfT
SSE <- SST - SSM

MSM <- SSM / dfM
MSE <- SSE / dfE

# H0 beta1 =  0 przeciwko hipotezie H1  beta_1 != 0

F_stat <- MSM / MSE
tf <- qf(1-alpha, 1, n-2)
abs(F_stat) > tf
# Odrzucamy hipoteze zerowa, gdy |T| > tc

2 * pf(F_stat,1, n-2, lower.tail = FALSE)
# Odrzucamy hipoteze zerowa, gdy p-value < 0.1


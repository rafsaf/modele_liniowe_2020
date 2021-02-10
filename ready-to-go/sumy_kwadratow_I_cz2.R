SST <- 125
n <- 36
p <- 4
SSM <- 35

dfM <- p - 1
dfE <- n - p
dfT <- n - 1
SSE <- SST - SSM

MSM <- SSM / dfM
MSE <- SSE / dfE

F_stat = MSM / MSE

# F test F* (1 - alpha, dfM, dfE)

# Odrzucamy hipoteze zerowa, gdy p-value < alpha
2 * pf(F_stat,dfM, dfE, lower.tail = FALSE)



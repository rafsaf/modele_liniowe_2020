p <- 4
n <- 48
SST_Full <- 540
SSM_Full <- 240 + 50 + 20
SSE_Full <- SST_Full - SSM_Full
dfE_Full <- n - p
dfM_Full <- p - 1

SSM_Red_od_Full <- SSM_Full - 240
# tutaj SSM(X2, X3 | X1) = SSM_Full - SSM(X1)
SSE_Red_od_Full <- SSM_Red_od_Full

st_licznik <- 2  # roznica miedzy modelem 1 a 2, tu 2

F_stat <- (SSE_Red_od_Full / 2) / (SSE_Full / dfE_Full)

# Odrzucamy hipoteze zerowa, gdy p-value < 0.1
2 * pf(F_stat,1, dfE_Full, lower.tail = FALSE)

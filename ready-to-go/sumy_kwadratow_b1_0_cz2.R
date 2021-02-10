p <- 4
n <- 80
SST_Full <- 870.0
SSM_Full <- 190 + 290 + 90
SSE_Full <- SST_Full - SSM_Full
dfE_Full <- n - p
dfM_Full <- p - 1

SSM_Red_od_Full <- 20
SSE_Red_od_Full <- SSM_Red_od_Full

st_licznik <- 1  # roznica miedzy modelem 1 a 2, tu 1

F_stat <- (SSE_Red_od_Full / 1) / (SSE_Full / dfE_Full)

# Odrzucamy hipoteze zerowa, gdy p-value < 0.1
2 * pf(F_stat,1, dfE_Full, lower.tail = FALSE)

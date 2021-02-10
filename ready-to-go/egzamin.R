#2
calculate_ssx <- function(vector_x) {
  mean_x <- mean(vector_x)
  return(sum((vector_x - mean_x)^2))
}
x = c(-8, -2, 10, 3, 1)
X_mean <- mean(x)
Y_sr <- 2
n <- 5
b1 <- 10.0
s_2 <- 25.0
alpha <- 0.01
b0 <- Y_sr - b1 * X_mean
ssx <- calculate_ssx(x)
s2_b0 <- s_2 * (1/n + ((Y_sr^2) / ssx))

s_b0 <- sqrt(s2_b0)
tc = qt(1-alpha/2, n - 2)
c(b0, b0 + tc * s_b0)

#3

MSM = 41
dfT = 35
s = 9
MSE = s^2
n <- dfT +1
dfM <- 1
dfE <- n - 2
SSM <- MSM * dfM
SST <- MSE * dfE
R_2 <- SSM/SST
R <- sqrt(R_2)
R

## 4

alpha  <- 0.05
n <- 25

MST <- 45.1


dfE <- 23
dfT <- 24
dfM <- 1

SSM <- 121
SST <- MST * dfT
SSE <- SST - SSM

MSM <- SSM / dfM
MSE <- SSE / dfE

# H0 beta1 =  0 przeciwko hipotezie H1  beta_1 != 0

F_stat <- MSM / MSE

2 * pf(F_stat,1, n-2, lower.tail = FALSE)
# Odrzucamy hipoteze zerowa, gdy p-value < alpha

## 5
#Moc testu dla beta_1 = 1.5
x = c(44, -84, -72, 12, -25)

calculate_ssx <- function(vector_x) {
  mean_x <- mean(vector_x)
  return(sum((vector_x - mean_x)^2))
}
ssx <- calculate_ssx(x)

beta1 = 1.8
n<-5
sig2<-1493.0
alpha<-0.05
###
sig2b1<-sig2/ssx
df=n-2
tc<-qt(1-alpha/2,df)
delta<-beta1/sqrt(sig2b1)
prob1<-function(delta){pt(tc,df,delta)}
prob2<-function(delta){pt(-tc,df,delta)}
power<-1-prob1(delta)+prob2(delta)
power


## 7

n <- 5 # przyklad
alpha <- 0.1
b0 <- -12.021
s_2b0 <- 49.0
s_b0 <- sqrt(s_2b0)
T_stat <- b0 / s_b0
tc = qt(1-alpha/2, n - 2)

abs(T_stat) > tc
# Odrzucamy hipoteze zerowa, gdy |T| > tc

for (i in 3:40) {
  tc = qt(1-alpha/2, i - 2)
  print(c(i, abs(T_stat) > tc))
}

## 8

p <- 4
n <- 49
SST_Full <- 750.0
SSM_Full <- 210.0 + 260.0 + 40.0
SSE_Full <- SST_Full - SSM_Full
dfE_Full <- n - p
dfM_Full <- p - 1

SSM_Red_od_Full <- 11
SSE_Red_od_Full <- SSM_Red_od_Full

st_licznik <- 1  # roznica miedzy modelem 1 a 2, tu 1

F_stat <- (SSE_Red_od_Full / st_licznik) / (SSE_Full / dfE_Full)

# Odrzucamy hipoteze zerowa, gdy p-value < 0.1
pf(F_stat,1, dfE_Full, lower.tail = FALSE)







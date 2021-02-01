# Rafal Safin
# list 4

library(data.table)
library(mvtnorm)
library(MASS)
library(reams)
require("car")
## Zad 3

### a)

n = 100
Sigma = matrix(c(1,0.9,0.9,1),2,2)
X = mvrnorm(n,c(0,0),0.01*Sigma)

# anwser a):
Y = 3*X[,1]+rnorm(n,0,1)

  
### b)
  
#Model reduced
reg1 <- lm(Y~X[, 1], x = TRUE)
#Model full
reg2 <- lm(Y~X[, 1]+X[, 2], x = TRUE)


int1 <- confint(reg1, 2)
# interval 1
int1
tc1 <- qt(0.975, n-2)
stat_t1 <- summary(reg1)$coefficients[2, 3]
stat_t1 > tc1
# so we reject H0 hypothesis for reduced model


int2 <- confint(reg2, 2)
# interval 2
int2
tc2 <- qt(0.975, n-3)
stat_t2 <- summary(reg2)$coefficients[2, 3]
stat_t2 > tc2

# so we don't reject H0 hypothesis for full model
    
### c)

modelred <- reg1$x
modelfull <- reg2$x
s2_red <- sum(reg1$residuals^2)/(n-2)
s2_full <- sum(reg2$residuals^2)/(n-3)
s_red_beta1 <- sqrt(s2_red*(solve(t(modelred)%*%modelred))[2, 2])
s_red_beta1
# the standard deviations of the estimate of b1 reduced model
s_full_beta1 <- sqrt(s2_full*(solve(t(modelfull)%*%modelfull))[2, 2])
s_full_beta1
# the standard deviations of the estimate of b1 full model

# the power of the test
calculate_prob1 <- function(delta){pt(tc,df,delta)}
calculate_prob2 <- function(delta){pt(-tc,df,delta)}

alpha <- 0.05
df <- n-2
tc <- qt(1-alpha/2,df)
beta1 <- 3
delta <- beta1/s_red_beta1

(power_red <- 1-calculate_prob1(delta)+calculate_prob2(delta))
# reduced model power of identification of X1
alpha <- 0.05
df <- n-3
tc <- qt(1-alpha/2,df)
beta1 <- 3
delta <- beta1/s_full_beta1

(power_full <- 1-calculate_prob1(delta)+calculate_prob2(delta))
# full model power of identification of X1

### d)

x1 <- 0
x2 <- 0
a <- 1:1000
b <- 1:1000
for(i in 1:1000){
  (eps <- rnorm(n, 0, 1))
  (Y <- 3*X[, 1] + eps)
  reg1 <- lm(Y~X[, 1])
  reg2 <- lm(Y~X[, 1]+X[, 2])
  interval1 <- confint(reg1)[2, ]
  interval2 <- confint(reg2)[2, ]
  if(0>=interval1[1] && 0<=interval1[2])
    x1 <- x1+1
  if(0>=interval2[1] && 0<=interval2[2])
    x2 <- x2+1
  a[i] <- reg1$coefficients[2]
  b[i] <- reg2$coefficients[2]
}
power_red_exp <- 1-x1/1000
power_red_exp
power_full_exp <- 1-x2/1000
power_full_exp

# we may observer that results are similar.

## Zad 4

### a)

n <- 1000
X <- matrix(rnorm(950000, 0, 0.1), n, 950)
B <- c(3, 3, 3, 3, 3, rep(0, 945))
eps <- rnorm(n)
Y <- X%*%B+eps

sub <- c(1, 2, 5, 10, 50, 100, 500, 950)
residuals_vec <- rep(NA, 8)
aic_vec <- rep(NA, 8)
mse_vec <- rep(NA, 8)
pvalues_matrix <- matrix(data = NA, 8, 2)
how_many_false <- rep(0, 8)
for(i in 1:8){
  reg <- lm(Y~X[, 1:sub[i]])
  residuals_vec[i] <- sum(reg$residuals^2)
  aic_vec[i] <- AIC(reg)
  M <- X[, 1:sub[i]]
  v <- matrix(reg$coefficients[-1]-B[1:sub[i]], sub[i], 1)
  mse_vec[i] <- sum((M%*%v)^2)/(n-sub[i]+1)
  if(i == 1)
    pvalues_matrix[1, 1] <- summary(reg)$coefficients[2, 4]
  else{
    pvalues_matrix[i, 1] <- summary(reg)$coefficients[2, 4]
    pvalues_matrix[i, 2] <- summary(reg)$coefficients[3, 4]
  }
  if(i != 1)
    how_many_false[i] <- sum(summary(reg)$coefficients[2:min(sub[i], 6), 4]>0.05)
  if(i >= 4)
    how_many_false[i] <- sum(summary(reg)$coefficients[7:(sub[i]+1), 4]<0.05) + how_many_false[i]
}
# numbers
sub
# mse vectors
mse_vec
# the number of false discoveries
how_many_false
# residual sum of squares
residuals_vec
# value of AIC criterion
aic_vec
# p-values
pvalues_matrix

# AIC will choose last model (n=950)

### b)

reg950 <- lm(Y~X)
cf_vec <- c(rep(TRUE, 5), rep(FALSE, 945))
X2 <- rbind(X, cf_vec)
ordered_coefficients <- order(abs(reg950$coefficients[-1]), decreasing = TRUE)
X2 <- X2[, ordered_coefficients]


sub <- c(1, 2, 5, 10, 50, 100, 500, 950)
residuals_vec <- rep(NA, 8)
aic_vec <- rep(NA, 8)
mse_vec <- rep(NA, 8)
pvalues_matrix <- matrix(data = NA, 8, 2)
how_many_false <- rep(0, 8)

for(i in 1:8){
  reg <- lm(Y~X2[1:1000, 1:sub[i]])
  residuals_vec[i] <- sum(reg$residuals^2)
  aic_vec[i] <- AIC(reg)
  M <- X[, 1:sub[i]]
  v <- matrix(reg$coefficients[-1]-B[1:sub[i]], sub[i], 1)
  mse_vec[i] <- sum((M%*%v)^2)/(n - sub[i]+1)
  if(i == 1)
    pvalues_matrix[1, 1] <- summary(reg)$coefficients[2, 4]
  else{
    pvalues_matrix[i, 1] <- summary(reg)$coefficients[2, 4]
    pvalues_matrix[i, 2] <- summary(reg)$coefficients[3, 4]
  }
  if(i == 1)
    how_many_false[i] <- sum(summary(reg)$coefficients[2, 4]>0.05)
  else{
    a <- which(X2[1001, 1:sub[i]] == 1)
    b <- which(X2[1001, 1:sub[i]] == 0)
    how_many_false[i] <- sum(summary(reg)$coefficients[2:(sub[i]+1), 4][a]>0.05)+how_many_false[i]
    how_many_false[i] <- sum(summary(reg)$coefficients[2:(sub[i]+1), 4][b]<0.05)+how_many_false[i]
  }
}
# numbers
sub
# mse vectors
mse_vec
# the number of false discoveries
how_many_false
# residual sum of squares
residuals_vec
# value of AIC criterion
aic_vec
# p-values
pvalues_matrix
# for n=500 the number of false discoveries has inceresed
# for n=950 MSE has increased
# AIC will again choose last model (n=950)

## zad 5

dt1<-read.table('CH06PR15.txt', col.names=c("age", "severity_of_illnes", "anxiety_level", "satisfaction"))

reg1 <- lm(satisfaction~age+severity_of_illnes+anxiety_level, dt1)
summ_reg <- summary(reg)
summ_reg

# Equation
# Y = 1.053 - 0.006 X1 + 0.002 X2 + 0.030 X3
# R^2 0.5415
# H0 b1=b2=b3=0
# F = 16.54 (3, 42 df)
# p-value = 3.043 * 10^-7 so we reject H0

## zad 6
confint(reg1, level = 0.95)
# H0 are b1=0, b2=0,b3=0
Anova(reg1)
# we can't reject b2=0 (severity) hypothesis 
# becouse p-value for this test is 0.74

## zad 7

res <- reg1$residuals
pred <- predict(reg1)

plot(pred, res)
plot(dt1$age, res)
plot(dt1$severity_of_illnes, res)
plot(dt1$anxiety_level, res)


# we cannot observer any unusuall patterns or outliners, qq-plot looks like 
# normal distribution

## zad 8
shapiro.test(res)
qqnorm(res)

# in our test, the statistic was W = 0.96286 and the p-value was 0.1481.
# this allows us to conclude that the data can come from a normal distribution.
# QQ-plot also points to the normal origin of our data.

## zad 9
dt2<-read.table('csdata.txt', col.names=c("id", "GPA", "HSM", "HSS", "HSE", "SATM", "SATV", "SEX"))
reg1 <- lm(GPA~HSM+HSS+HSE, dt2)
reg2 <- lm(GPA~SATM+SATV+HSM+HSS+HSE, dt2)

# H0: b1=b2=0 (SATM and SATV)
### a)
an1 <- anova(reg1)
an2 <- anova(reg2)
sse1 <- sum(an1$`Sum Sq`[1:3])
sse2 <- sum(an2$`Sum Sq`[1:5])
an2
diffrence <- sse2 - sse1
F_test <- (diffrence / 2) / (106.819 / 218)
F_test
ft = qf(F_test, 2, 218)
ft
F_test > ft
# so we cannot reject H0 hypothesis

### b)

anova(reg1, reg2)

# p-value is 0.3882 so this shows us that not rejection H0 was a good thing.

### zad 10

reg1 <- lm(GPA~SATM+SATV+HSM+HSE+HSS, dt2)
# I sums of squares (second column)
anova(reg1)
# II sums of squares (second column)
Anova(reg1)

# a)
reg1_test <- lm(GPA~SATM+SATV+HSM, dt2)
reg2_test <- lm(GPA~SATM+SATV, dt2)
an1 <- anova(reg1_test)
an2 <- anova(reg2_test)
sse1 <- sum(an1$`Sum Sq`[1:3])
sse2 <- sum(an2$`Sum Sq`[1:2])
type_1_SSE_HSM <- sse1 - sse2
type_1_SSE_HSM
# and it is actually the sum of 1 type for HSM 
# (we can read it from the function anova() )

# b)

# Yes, in this case it is the HSM and in general it is always the last in
# order of the predictors. This is due to the equality between
# sums of 1 and 2 type.


### zad 11

SAT <- dt2$SATM + dt2$SATV
cbind(dt2, SAT)
# added col SAT to dt2
reg1 <- lm(GPA~SATM+SATV+SAT, dt2)
summary(reg1)


# We have already seen in the previous tasks that the SAT variables are weak
# predictors of GPA, so before creating the model, we can conclude that
# nothing interesting will come out here. After analyzing the summary () of our
# model, we can see that it is actually true.
# R left NA our last variable SAT.
# Additionally, R2 = 0.06 so our model explains practically nothing.


### zad 12

reg1 <- lm(GPA~HSM+HSS+HSE+SATM+SATV+SEX, dt2)

reg_hsm_x <- lm(HSM~HSS+HSE+SATM+SATV+SEX, dt2)
reg_hsm_y <- lm(GPA~HSS+HSE+SATM+SATV+SEX, dt2)
plot(reg_hsm_x$residuals, reg_hsm_y$residuals, main = "HSM vs GPA")

reg_hss_x <- lm(HSS~HSM+HSE+SATM+SATV+SEX, dt2)
reg_hss_y <- lm(GPA~HSM+HSE+SATM+SATV+SEX, dt2)
plot(reg_hss_x$residuals, reg_hss_y$residuals, main = "HSS vs GPA")

reg_hse_x <- lm(HSE~HSM+HSS+SATM+SATV+SEX, dt2)
reg_hse_y <- lm(GPA~HSM+HSS+SATM+SATV+SEX, dt2)
plot(reg_hse_x$residuals, reg_hse_y$residuals, main = "HSE vs GPA")

reg_satm_x <- lm(SATM~HSM+HSS+HSE+SATV+SEX, dt2)
reg_satm_y <- lm(GPA~HSM+HSS+HSE+SATV+SEX, dt2)
plot(reg_satm_x$residuals, reg_satm_y$residuals, main = "SATM vs GPA")

reg_satv_x <- lm(SATV~HSM+HSS+HSE+SATM+SEX, dt2)
reg_satv_y <- lm(GPA~HSM+HSS+HSE+SATM+SEX, dt2)
plot(reg_satv_x$residuals, reg_satv_y$residuals, main = "SATV vs GPA")

reg_sex_x <- lm(SEX~HSM+HSS+HSE+SATM+SATV, dt2)
reg_sex_y <- lm(GPA~HSM+HSS+HSE+SATM+SATV, dt2)
plot(reg_sex_x$residuals, reg_sex_y$residuals, main = "SEX vs GPA")


# Here we see that each successive variable loses the structure 
# that is most visible in HSM (first plot). This somewhat agrees with our tests 
# where the former the variables are the most important.

## zad 13

# again
reg1 <- lm(GPA~HSM+HSS+HSE+SATM+SATV+SEX, dt2)

p = 1:224
r  = residuals(reg1)
r1 = rstandard(reg1) # studentized inside
r2 = rstudent(reg1)  # studentized outside

cbind(r, r1, r2)

plot(p, r1)
plot(p, r2)

# Residues behaves calmly and oscillates around 0. 
# Variance seems not to be dependent so everything agrees with 
# the theoretical model.

## zad 14

# again
p = 1:224
reg1 <- lm(GPA~HSM+HSS+HSE+SATM+SATV+SEX, dt2)
y1<-dffits(reg1)
h_value = sqrt(7/224)
plot(p, y1)
abline(h=2*h_value)
abline(h=-2*h_value)

# We know that the DFFITS standard is within 2 * h_value
# We can see that a few observations go beyond, though
# the rest behave correctly


## zad 15



# Toleration is the inverse of the VIF and can help us resolve
# the problem of multicolinearity. When it gets lower than 0.1 it points 
# to this problem. For our data, the tolerance vector
# variables in the order as in model is:
# 0.5188628 0.5088203 0.5429546 0.5745498 0.7310535 0.7742519
v<-vif(reg1)
tolerance = 1/v
tolerance
# so nothing indicates a problem with multicolinearity


## zad 16

ic.min(dt2$GPA, dt2[3:8])

# Here we use the ic.min() function from the reams package which 
# will choose the best models according to AIC and BIC.

# the BIC chooses HSM only
# the AIC chooses HSM and HSE
# $best.aic
# 1     2     3     4     5     6 
# TRUE FALSE  TRUE FALSE FALSE FALSE 
 

# $best.bic
# 1     2     3     4     5     6 
# TRUE FALSE FALSE FALSE FALSE FALSE 

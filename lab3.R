### zad1
alpha = 0.05
deg = 10
# a) two-tailed T test
tc = qt(p=1-alpha/2, df=deg)
# b) F test
Fc = qf(p=1-alpha, df1 = 1, df2 = deg)
# c)
Fc_sqrt = sqrt(Fc)
print(Fc_sqrt == tc)

# Conclusion is that T^2 == F as suspected

### zad 2

dfM = 1
SSM = 100
dfE = 20
SSE = 400
SST = SSM + SSE
dfT = dfM + dfE
n = dfE + 2

# a) We have n observations, n = 22
# b)
MSE = SSE/dfE
estimated_simga = sqrt(MSE)
estimated_simga
# sigma is equal to about 4.47
# c)
# H0: B1 == 0, H1: B1 !== 0
MSM = SSM/dfM
alpha = 0.05

F_test = MSM/MSE
Fc = qf(p = 1 - alpha, df1 = 1, df2 = n-2)

F_test > Fc
# So we reject H0 hypothesis
# d)
R_2 = SSM/SST
R_2
# proportion of the variation
# e)
R = sqrt(R_2)
R

### zad3 
table1_6<-read.table('tabela1_6.txt')

lin_gpa_iq = lm(V2~V3, table1_6)
linear = lm(lin_gpa_iq)
summary(linear)
# a)
coefficients(linear)
# gpa = 0.101 * iq - 3.557
# R_2 = 0.4016

# We test H0: B1==0
# We use summary function, t value for H0 is 7.142, p-value = 4.74* 10^(-10)
# so we reject H0 hypotesis
# b)

predict(linear, newdata=data.frame(V3=c(100)), level = 0.9, interval = "confidence")
# predicted gpa for student with 100 iq

# c)
gpa = table1_6$V2
iq = table1_6$V3

newiq = seq(min(iq),max(iq),by = 0.05)
conf_interval = predict(linear, newdata=data.frame(V3=newiq), interval="confidence",level = 0.95)
plot(iq, gpa,pch=19,col='red', xlab="iq", ylab="gpa", main="gpa~iq")
abline(linear, col="lightblue")
matlines(newiq, conf_interval[,2:3], col = "blue", lty=10)

# Many more observations falls outside this band than stays inside, about 70%

### zad 4

lin_gpa_phs = lm(V2~V5, table1_6)
linear = lm(lin_gpa_phs)
summary(linear)
# a)
coefficients(linear)
# gpa = 0.092 * pht + 2.226
# R_2 = 0.2936

# We test H0: B1==0
# We use summary function, t value for H0 is 5.620, p-value = 3.01* 10^(-7)
# so we reject H0 hypotesis
# b)

predict(linear, newdata=data.frame(V5=c(60)), level = 0.9, interval = "confidence")
# predicted gpa for student with 60 Pierrs-Harris score

# c)
gpa = table1_6$V2
phs = table1_6$V5

newphs = seq(min(phs),max(phs),by = 0.05)
conf_interval = predict(linear, newdata=data.frame(V5=newphs), interval="confidence",level = 0.95)
plot(phs, gpa,pch=19,col='red', xlab="Piers-Harris score", ylab="gpa", main="gpa~phs")
abline(linear, col="lightblue")
matlines(newphs, conf_interval[,2:3], col = "blue", lty=10)

# Many more observations falls outside this band than stays inside, about 80%

# d)
# Result of iq test is a little better prediction of gpa than Pierce-Harris score, 
# there are more observations in
# estimated confidence intervals and p-value for H0 is little lower

### zad 5

table_ch = read.table('CH01PR20.txt')
linear = lm(V2~V1,table_ch)
summary(linear)
# a)
residuals(linear)
# We would like to verify if sum of residuals is equal to 0
sum(residuals(linear))
sum(residuals(linear)) == 0
# 4.58 * 10^(-16) is not equal to 0 but it is very close to 0
# b)
v1 = table_ch$V1
plot(v1, residuals(linear),pch=19,col='red', xlab="V1", ylab="Residual", main="Residuals~V1")
abline(h=0)
# Most of residuals oscillate around the y=0 from both sides, there are only three of them
# a little upper over y=0, but not too upper

# c)
x1 = 1:length(v1)
plot(x1, residuals(linear),pch=19,col='red', xlab="Order", ylab="Residual", main="Residuals~order")
abline(h=0)
# Residuals are similar in order, still like in b) they oscillate around y=0 from both
# sides, with 2-3 of them little upper

# d)
hist(residuals(linear))
qqnorm(residuals(linear))
# We suspect that residuals may come from a normal distibution

### zad 6

v1 = table_ch$V1
v1[1] = 2000
v2 = table_ch$V2
table_ch_deformed = data.frame(V1 = v1, V2 = v2)
linear = lm(V2~V1,table_ch_deformed)
summary(linear)
# a)
# -------- --- standard      --- deformed
# equation --- y=0.06*x+0.25 --- y=-0.003*x+5.14
# t-test   --- 31.123        --- -0.193
# p-value  --- <2*10^(-16)   --- 0.848 (!!! very high)
# R^2      --- 0.5801        --- 0.0009 (!!! very low)

# To sum up, this change strongly (badly) affects this linear model, with this data we can
# accept H0 hypothesis so there is no corellation between variables

# b) b)
v1 = table_ch_deformed$V1
plot(v1, residuals(linear),pch=19,col='red', xlab="V1", ylab="Residual", main="Residuals~V1")
abline(h=0)

# Unusual observation is clearly visible disturbing this plot

# b) c)
x1 = 1:length(v1)
plot(x1, residuals(linear),pch=19,col='red', xlab="Order", ylab="Residual", main="Residuals~order")
abline(h=0)
# Residuals are similar in order, value for order=1 is invisible becouse it is much above other
# results

# b) d)
hist(residuals(linear))
qqnorm(residuals(linear))
# We suspect that residuals may come from a normal distibution, value for order=1 
# is invisible here

### zad 7

table_last = read.table('CH03PR15.txt')
linear = lm(V1~V2, table_last)
summary(linear)
# equation: v1 = -0.324 * v2 + 2.575
# R_2 = 0.474

# We check if this relationship between v1 and v2 could be linear
# To do that, we test H0: slope v2 == 0 vs H1: slope v2 !== 0
# From summary we check that tc = -7.483 and p-value = 4.63 * 10^(-6)
# So we reject H0 and relationship could be linear

### zad 8

v1 = table_last$V1
v2 = table_last$V2

newv2 = seq(min(v2),max(v2),by = 0.05)
conf_interval = predict(linear, newdata=data.frame(V2=newv2), interval="confidence",level = 0.95)
plot(v2, v1,pch=19,col='red', xlab="time", ylab="solution concentration", main="v1~v2")
abline(linear, col="lightblue")
matlines(newv2, conf_interval[,2:3], col = "blue", lty=25)

# This relationship seems not to be linear. There are fiew values very close
# to each other for small values of time. Only two of the values 
# are inside confidence band. For increasing time the values of v1 decrease rapidly,
# but are above zero, which suggests some dependence, but not a linear.

### zad 9

require(MASS)

boxcox(table_last$V1~table_last$V2)
# in 95% conf. interval lambda == 0 
# we should use transformation v1' = log(v1)

### zad 10

table_last = read.table('CH03PR15.txt')
linear = lm(log(V1)~V2, table_last)

# zad 7 repeated for new model
summary(linear)
# equation: v1 = -0.450 * v2 + 1.518
# R_2 = 0.993

# We check if this relationship between v1 and v2 is linear
# (it should be linear as R_2 is close to 1 and v1 is after
# boxcox translation)
# To do that, we test H0: slope v2 == 0 vs H1: slope v2 !== 0
# From summary we check that tc = -42.88 and p-value = 2.19 * 10^(-15)
# So we reject H0 and relationship is indeed linear

# zad 8 repeated for new model

v1 = log(table_last$V1)
v2 = table_last$V2

newv2 = seq(min(v2),max(v2),by = 0.05)
conf_interval = predict(linear, newdata=data.frame(V2=newv2), interval="confidence",level = 0.95)
plot(v2, v1,pch=19,col='red', xlab="time", ylab="solution concentration", main="log(v1)~v2")
abline(linear, col="lightblue")
matlines(newv2, conf_interval[,2:3], col = "blue", lty=25)

# This relationship seems to be linear. 
# Every value lies close to estimated line

# Sumarize
# Before boxcox transformation relationship was not linear, but after
# transformation we have strong linear relationship

### zad 11

par(mfrow = c(1,2))
linear = lm(log(V1)~V2, table_last)
v1 = log(table_last$V1)
v2 = table_last$V2

newv2 = seq(min(v2),max(v2),by = 0.05)
conf_interval = predict(linear, newdata=data.frame(V2=newv2), interval="confidence",level = 0.95)
plot(v2, v1,pch=19,col='red', xlab="time", ylab="solution concentration", main="log(v1)~v2")
abline(linear, col="lightblue")
matlines(newv2, conf_interval[,2:3], col = "blue", lty=25)

linear = lm(V1~V2, table_last)
v1 = table_last$V1
v2 = table_last$V2

newv2 = seq(min(v2),max(v2),by = 0.05)
conf_interval = predict(linear, newdata=data.frame(V2=newv2), interval="confidence",level = 0.95)
plot(v2, v1,pch=19,col='red', xlab="time", ylab="solution concentration", main="v1~v2")
abline(linear, col="lightblue")
matlines(newv2, conf_interval[,2:3], col = "blue", lty=25)


# Becouse from zad10 we have that R_2 = 0.993
R_2 = 0.993
# then corelation coeff. is equal to
sqrt(R_2)
# 0.9965


### zad 12

par(mfrow = c(1,3))
linear = lm(V1~I(V2^(-0.5)), table_last)
summary(linear)
# equation: v1 = 4.196 * v2 - 1.341
# R_2 = 0.9881

# We check if this relationship between v1 and v2 is linear
# (it should be linear as R_2 is close to 1)
# To do that, we test H0: slope v2 == 0 vs H1: slope v2 !== 0
# From summary we check that tc = 32.80 and p-value = 6.9 * 10^(-14)
# So we reject H0 and relationship is indeed linear

v1 = table_last$V1
v2 = I(table_last$V2)^(-0.5)

newv2 = seq(min(v2),max(v2),by = 0.001,)
conf_interval = predict(linear, newdata=data.frame(V2=newv2), interval="confidence",level = 0.95)
plot(v2, v1,pch=19,col='red', xlab="time", ylab="solution concentration", main="v1~v2^(-0.5)")
abline(linear, col="lightblue")
matlines(newv2, conf_interval[,2:3], col = "blue", lty=28)



linear = lm(log(V1)~V2, table_last)
v1 = log(table_last$V1)
v2 = table_last$V2

newv2 = seq(min(v2),max(v2),by = 0.05)
conf_interval = predict(linear, newdata=data.frame(V2=newv2), interval="confidence",level = 0.95)
plot(v2, v1,pch=19,col='red', xlab="time", ylab="solution concentration", main="log(v1)~v2")
abline(linear, col="lightblue")
matlines(newv2, conf_interval[,2:3], col = "blue", lty=25)

linear = lm(V1~V2, table_last)
v1 = table_last$V1
v2 = table_last$V2

newv2 = seq(min(v2),max(v2),by = 0.05)
conf_interval = predict(linear, newdata=data.frame(V2=newv2), interval="confidence",level = 0.95)
plot(v2, v1,pch=19,col='red', xlab="time", ylab="solution concentration", main="v1~v2")
abline(linear, col="lightblue")
matlines(newv2, conf_interval[,2:3], col = "blue", lty=25)


# Model after boxcox translation seems to be the best one, second one could be from zad12
# after v2 = v2^(-0.5) and the worst is first model with standard data without any
# translation

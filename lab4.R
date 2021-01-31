library(MASS)
## Zad 3

### a)


Sigma = matrix(c(1,0.9,0.9,1),2,2)
X = mvrnorm(100,c(0,0),0.01*Sigma)

# anwser a):
Y = 3*X[,1]+rnorm(100,0,1)

  
### b)
  
model_reduced = lm(Y~X[,1])
model_full = lm(Y~X[,1]+X[,2])
confint(model_reduced,2,level = 0.95)
confint(model_full,2,level = 0.95)

# Result from the reduced model is more accurate than from the full model.
# The reason for this may be that we know both X1 and X2 can be linear dependent 
# (they are from normal distribution).
# Looking on confint intervals we can guess the Tests results,
# H0 for the reduced model is rejected, and for full model, for a certain accuracy /alpha
# we can accept it.
    
### c)

plan = cbind(rep(1,100),X)
s2 = sum((model_reduced$residuals)^2)/(100-2)
plan[,1:2]

# Proposal: a model has only one explanatory variable
  
### d)

x1 = 0
x2 = 0
a = c()
b = c()
for (i in 1:1000){
  y = 3*X[,1]+rnorm(100,0,1)
  modelred = lm(y~X[,1])
  modelfull = lm(y~X[,1]+X[,2])
  int1 = confint(modelred)[2,]
  int2 = confint(modelfull)[2,]
  if(0 >= int1[1] && 0 <= int1[2]){
    X[,1] = X[,1]+1
  } else if (0 >= int2[1] && 0<= int2[2]){
    X[,2] = X[,2]+1
  }
  a[i] = modelred$coefficients[2]
  b[i] = modelfull$coefficients[2]
}
mocred = 1-X[,1]/1000
s2 = sum((modelred$residuals)^2)/(100-3)
sqrt(s2*solve(t(m)%%m[2,2]))

## Zad 4

X = matrix(rep(rnorm(1,0,0.1),950*1000),1000,950)
beta = c(3,3,3,3,3,rep(0,945))
epsilon  =  rep(rnorm(1,0,1),1000)
Y  =  X %*% beta + epsilon

### a) 

alfa = 0.05
s = c(1,2,5,10,50,100,500,950)
for (k in s){
  Xs = data.frame(X[,1:k])
  model = lm(Y~.,data = Xs)
  model$coefficients
}

### b) 

## zad 5



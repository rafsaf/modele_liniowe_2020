library(MASS)
############## zad 1
x<-cbind(rnorm(100,0,1),rnorm(100,0,1)) 
# losowe 2 wektory z rozkładu normalnego
plot(x[,1],x[,2],pch=19,col='red',xlab='X',ylab='Y') 

############## zad2
#a
mi1 <- c(4,2) 
# wektor wartości oczekiwanych
sigma1 <- matrix(c(1,0.9,0.9,1),2,2) 
# macierz kowariancji 
a1<-chol(sigma1) 
# rozkład choleskiego sigma1
b1<-t(a1) %*% t(x)+mi1 
# zgodnie ze wzorem po transpozycji

t1<-mvrnorm(100,mi1,sigma1)
# do sprawdzenia wynik z mvrnorm

par(mfrow=c(1,2)) 
plot(b1[1,],b1[2,],pch=19,col='red',xlab='X',ylab='Y',main='rnorm()') 
plot(t1[,1],t1[,2],pch=19,col='blue',xlab='X',ylab='Y',main='mvrnorm()') 
par(mfrow=c(1,1))
# wyniki zbliżone - poprawna metoda, b i c analogicznie

#b
mi2 <- c(4,2)
sigma2 <- matrix(c(1,-0.9,-0.9,1),2,2)
a2<-chol(sigma2)
b2<-t(a2) %*% t(x)+mi2
plot(b2[1,],b2[2,],pch=19,col='red',xlab='o? Y',ylab='o? Y',main='rnorm()')

#c
mi3 <- c(4,2)
sigma3 <- matrix(c(9,0,0,1),2,2)
a3<-chol(sigma3)
b3<-t(a3) %*% t(x)+mi3
plot(b3[1,],b3[2,],pch=19,col='red',xlab='X',ylab='Y',main='rnorm()')

############## zad 3
m<-matrix(0,200,100)
m<-apply(m,2,function(x) x<-rnorm(200,0,1)) 
# losowe wektory w kazdej kolumnie
sigma<-diag(100) 
sigma[sigma==0]<-0.9
# macierz kowariancji 1 na przekątnej i 0.9 reszta miejsc

a<-chol(sigma) 
b<-t(a) %*% t(m)
b<-t(b) 
# rozkład choleskiego
cov_b<-cov(b) 
# macierz kowariancji
diffrence<-cov(b)-sigma 
diffrence[diffrence>0.1] 
# mała różnica

hist(diag(cov_b),pch=19,col='red',xlab='Wartość wariancji',main='Histogram wariancji')
mean(diag(cov_b)) 
# średnia wariancji na przekątnej, około 1 ~ ok
hist(c(cov_b[upper.tri(cov_b)],cov_b[lower.tri(cov_b)]),pch=19,col='red',xlab='Wartość kowariancji',main='Histogram kowariancji')
mean(c(cov_b[upper.tri(cov_b)],cov_b[lower.tri(cov_b)])) 
# średnia kowariancji poza przekątną ~ około 0.9 ok
  
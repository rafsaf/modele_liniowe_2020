# Pasmo ufnosci
x <- c(-70, -68, 1, 59, -72)
n <- 5
s <- 5
b0 <- 5
b1 <- 2
alpha <- 0.01
x_h <- 5

calculate_ssx <- function(vector_x) {
  mean_x <- mean(vector_x)
  return(sum((vector_x - mean_x)^2))
}

calculate_s_pred = function(x, x_h, s, n) {
  mean_x <- mean(x)
  ssx <- calculate_ssx(x)
  counter <- (x_h - mean_x)^2
  expr <- counter / ssx
  s2pred <- s^2 * (1 + 1/n + expr)
  return(sqrt(s2pred))
}

predict_mu_h <- function(x_h, b0, b1) {
  return(b0 + b1 * x_h)
}

s_pred <- calculate_s_pred(x, x_h, s, n)
mu_h <- predict_mu_h(x_h, b0, b1)
###
w<-sqrt(2*qf(1-alpha,2,n-2))
up<- mu_h + w * s
down<- mu_h - w * s
c(down, mu_h, up)




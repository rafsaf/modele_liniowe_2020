#Moc testu dla beta_1 = 1.5
x = c(-70, -68, 1, 59, -72)

calculate_ssx <- function(vector_x) {
  mean_x <- mean(vector_x)
  return(sum((vector_x - mean_x)^2))
}
ssx <-
ssx <- calculate_ssx(x)

beta1 = 1.9
n<-5
sig2<-1729.0
alpha<-0.01
###
sig2b1<-sig2/ssx
df=n-2
tc<-qt(1-alpha/2,df)
delta<-beta1/sqrt(sig2b1)
prob1<-function(delta){pt(tc,df,delta)}
prob2<-function(delta){pt(-tc,df,delta)}
power<-1-prob1(delta)+prob2(delta)
power



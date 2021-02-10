# kol1 
# zad 1

x = c(-70, -68, 1, 59, -72)
t.test(x, mu=1.9, conf.level = 0.01)
mean_x = mean(x, mu=1.9, conf.level=0.01)
x_minus_mean = (x - mean_x)^2
sum_x = sum(x_minus_mean)

sigma2 = 1729.0
sigma = sqrt(sigma2)
b1 = 1.9
TC = b1 / (sigma / sum_x)



# przedzia³ predykcyjny  dla nowej obserwacji  Y_1 


n <- 36
pred <- c(2, 113)

# 95%
alpha1 <- 0.05
alpha2 <- 0.1
s <- 5
y_h <- mean(pred)
diff <- pred[2] - y_h
# tc * s_pred_pred = 113 - 57.5 = 55.5
tc1 <- qt(1-alpha1/2,n - 2)
tc2 <- qt(1-alpha2/2,n - 2)

#s_pred_ufnosci = s_pred_pred - s^2

s_pred_pred = diff / tc1
s_pred_ufnosci = s_pred_pred - s^2

tc_spred <- s_pred_ufnosci * tc2
c(y_h - tc_spred, y_h, y_h+tc_spred)


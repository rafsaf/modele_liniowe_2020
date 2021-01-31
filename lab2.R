
#=================================================================
time<-read.table('CH01TA01.txt', col.names=c("size", "hours"))
# For all exercises here, it will be
# x - size
# y - hours

#=================================================================
# zad 1

plot(hours~size, time)

# Relationship looks like approximately linear.
reg1<-lm(hours~size, time)
summary(reg1)
#=================================================================
# zad 2
# We try to find linear function y= b0 + b1*x + e
# a)
x = time$size
y = time$hours
average_x = mean(time$size)
average_y = mean(time$hours)

opt_x = x - average_x
opt_y = y - average_y
doubled_x = opt_x^2

counter_vector = opt_x * opt_y
frac_counter = sum(counter_vector)
frac_denominator = sum(doubled_x)

estimated_b1 = frac_counter / frac_denominator
estimated_b0 = average_y - average_x * estimated_b1

estimated_s2 = 1 / (length(y) - 2) * (sum((y - estimated_b0 - estimated_b1 * x)^2))

# Values of b1 and b2 are correct with summary from zad1

# b)
s2_b1 = estimated_s2 / sum(doubled_x)
s_b1 = sqrt(s2_b1)
student_quantil = qt(0.025, df=(length(x)-2), lower.tail = TRUE)
t = s_b1 * student_quantil
conf_interval = c(estimated_b1+t, estimated_b1-t)
# Then b1 is in confidence interwal:
conf_interval

# c)

# H0: b1 = 0

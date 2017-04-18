age <- c(46,20,52,30,57,25,28,36,22,43,57,33,22,63,40,48,28,49)
bmi <- c(25.4,20.6,26.2,22.6,25.4,23.1,22.7,24.9,19.8,25.3,23.2,
         21.8,20.9,26.7,26.4,21.2,21.2,22.8)
chol <- c(3.5,1.9,4.0,2.6,4.5,3.0,2.9,3.8,2.1,3.8,4.1,3.0,2.5,
          4.5,3.2,4.2,2.3,4.0)
data <- data.frame(age, bmi, chol)
plot(chol ~ age, pch = 16)

cor.test(age, chol)
cor.test(age, chol, method = "spearman")
cor.test(age, chol, method = "kendall")

lm(chol ~ age)
reg <- lm(chol ~ age)
summary(reg)

op <- par(mfrow = c(2,2))
plot(reg)
dev.off()

plot(chol ~ age, pch = 16)
abline(reg)

new <- data.frame(age = seq(15, 70, 5))
pred.w.plim <- predict.lm(reg, new, interval = "prediction")
pred.w.clim <- predict.lm(reg, new, interval = "confidence")
resc <- cbind(pred.w.clim, new)
resp<- cbind(pred.w.plim, new)
plot(chol ~ age, pch = 16)
lines(resc$fit ~ resc$age)
lines(resc$lwr ~ resc$age, col = 2)
lines(resc$upr ~ resc$age, col = 2)
lines(resp$lwr ~ resp$age, col = 4)
lines(resp$upr ~ resp$age, col = 4)

pairs(data)
summary(lm(chol ~ bmi))
mreg <- lm(chol ~ age + bmi)
summary(mreg)
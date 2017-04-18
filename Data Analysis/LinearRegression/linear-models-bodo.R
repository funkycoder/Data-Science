#Linear model
pitch <- c(233, 204, 242, 130, 112, 142)
sex <- c(rep("female",3), rep("male",3))
my.df <- data.frame(sex, pitch)
fit <- lm(pitch ~ sex, my.df)
summary(fit)

mean(my.df[my.df$sex == "female",]$pitch)
mean(my.df[my.df$sex == "male",]$pitch)

age<- c(14, 23, 35, 48, 52, 67)
pitch <- c(252, 244, 240, 233, 212, 204)
my.df <- data.frame(age, pitch)
fit2 <- lm(pitch ~ age, my.df)
summary(fit2)

plot(pitch ~ age, type = "p", xlab = "Age (years)", 
     ylab = "Voice pitch (Hz)", data = my.df)
abline(fit2)

my.df$age.c <- my.df$age - mean(my.df$age)
fit3 <- lm(pitch ~ age.c, my.df)
summary(fit3)
plot(pitch ~ age.c, type = "p", xlab = "Age (years)", 
     title = "Centering age to the mean",
     ylab = "Voice pitch (Hz)", data = my.df)
abline(fit3)


plot(fitted(fit2), residuals(fit2))
abline(h=0, lty = 2)

plot(rnorm(100), rnorm(100))
abline(h=0, lty = 2)

hist(residuals(fit2))
qqnorm(residuals(fit2))
qqline(residuals(fit2))

dfbeta(fit2)

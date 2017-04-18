score <- c(3, 13, 13, 8, 11, 9, 12, 7, 16, 15, 18, 6, 21, 34, 26, 11, 24, 14, 21, 5, 17, 17, 23, 19, 7)
group <- c(rep("1",10), rep("2",6), rep("3",9))
data <- data.frame(group, score)
data

library(lme4)
fit <- lmer(score ~ 1 + (1 | group), data = data, REML = 0)
summary(fit)

fit1 <- lmer(score ~ 1 + group + (1 | group), data = data, REML = 0)
summary(fit1)

glucose <- c(5.9, 3.9, 3.9, 3.6, 5.3, 4.7, 3.5, 3.2, 4.6, 3.7,
             3.3, 3.2, 6.2, 4.6, 4.3, 3.9, 6.0, 5.4, 5.2, 4.8,
             6.5, 4.7, 4.8, 4.3, 7.6, 4.1, 3.8, 4.1, 5.9, 3.1,
             3.6, 3.3, 7.5, 6.1, 5.4, 4.6, 6.2, 5.3, 4.9, 4.5,
             6.9, 5.6, 5.9, 5.9, 5.6, 4.7, 4.6, 4.0, 5.1, 3.9,
             2.9, 2.9, 5.7, 4.7, 4.3, 4.6, 5.0, 4.0, 3.5, 3.3,
             5.2, 4.2, 4.0, 3.8, 7.7, 6.2, 6.1, 5.7, 8.0, 5.8,
             6.5, 6.0, 7.7, 5.0, 6.3, 6.2)
id <- rep(1:19, each = 4)
time <- rep(c(0, 2, 3, 4), 19)
treatment <- rep(1:2, c(9*4, 10*4))
data2 <- data.frame(id, treatment, time, glucose)
head(data2,10)

library(rms)
library(lattice)
library(lme4)

xyplot(glucose ~ time | id, type = c("p", "r"), as.table = time, xlab = "time", ylab = "Glucose")


time1 <- c(0, 2, 3, 4)
glucose1 <- c(5.9, 3.9, 3.9, 3.6)
summary(lm(glucose1 ~ time1))

time9 <- c(0, 2, 3, 4)
glucose9 <- c(7.5, 6.1, 5.4, 4.6)
summary(lm(glucose9 ~ time9))

fit2 <- by(data2, id, function(data) fitted.values(lm(glucose ~ time, data = data2)))
fit2 <- unlist(fit2)
names(fit2) <- NULL
interaction.plot(time, id, fit2, xlab = "Time", ylab = "Glucose")

glucose <- c(5.9, 5.3, 4.6, 6.2, 6.0, 6.4, 7.6, 5.9, 7.5, 6.2,
             6.9, 5.6, 5.1, 5.7, 5.0, 5.2, 7.7, 8.0, 7.7)
treatment <- rep(c(1, 0), c(9, 10))
t.test(glucose ~ treatment)


fit3 <- lmer(glucose ~ 1 + time + (1 + time | id), data = data2, REML = 0)
summary(fit3)

fit4 <- lmer(glucose ~ 1 + treatment + time + time:treatment + (1 + time | id), data = data2, REML= 0)
summary(fit4)

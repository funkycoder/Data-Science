p <- seq(0, 1, length.out = 100)
p <- p[2 : (length(p)-1)]
logit <- function(t) {
  log(t / (1 - t))
}
plot(logit(p) ~ p, type = "l")

setwd("C:/Users/Quan/OneDrive/R Space/Dataset")
data <- read.csv(file = "fracture.csv", header = TRUE, sep = ",", na.strings = ".", nrows = 500)
names(data)
summary(data)
data <- na.omit(data)
data <- data[, c("fnbmd", "anyfx")]
colnames(data)[1] <- "bmd"
colnames(data)[2] <- "fx"
table(data$fx)
tapply(data$bmd, data$fx, mean)
boxplot(bmd ~ fx, data = data, xlab = "Fracture: 1 = yes, 0 = no", ylab = "BMD")
t.test(bmd ~ fx, data = data)
fit <- glm(fx ~ bmd, data = data, family = "binomial")
summary(fit)
exp(coefficients(fit))
# How much you reduce the odds
(1 - exp(coefficients(fit))[2]) * 100

predict(fit, type = "response")
plot(data$bmd, fitted(fit))

# Create a vector of bmd
fnbmd <- seq(0.5, 1.2, 0.05)
new.data <- data.frame(bmd = fnbmd)
fit2 <- predict(fit, new.data, type = "response")
plot(fit2 ~ fnbmd, type = "l")

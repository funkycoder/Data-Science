library(MASS)
data(menarche)
str(menarche)
summary(menarche)
plot(Menarche/Total ~ Age, data = menarche)
glm.out <- glm(cbind(Menarche, Total - Menarche) ~ Age, family = binomial(logit), data = menarche)
lines(menarche$Age, glm.out$fitted.values, type = "l", col = "red")
title(main = "Menarche Data with Fitted Logistic Regression Line")
summary(glm.out)
1 - pchisq(26.7, df = 23)


gorilla = read.table(header=T, text="
                     seen   W   C CW
                     0 126  86 64
                     0 118  76 54
                     0  61  66 44
                     0  69  48 32
                     0  57  59 42
                     0  78  64 53
                     0 114  61 41
                     0  81  85 47
                     0  73  57 33
                     0  93  50 45
                     0 116  92 49
                     0 156  70 45
                     0  90  66 48
                     0 120  73 49
                     0  99  68 44
                     0 113 110 47
                     0 103  78 52
                     0 123  61 28
                     0  86  65 42
                     0  99  77 51
                     0 102  77 54
                     0 120  74 53
                     0 128 100 56
                     0 100  89 56
                     0  95  61 37
                     0  80  55 36
                     0  98  92 51
                     0 111  90 52
                     0 101  85 45
                     0 102  78 51
                     1 100  66 48
                     1 112  78 55
                     1  82  84 37
                     1  72  63 46
                     1  72  65 47
                     1  89  71 49
                     1 108  46 29
                     1  88  70 49
                     1 116  83 67
                     1 100  69 39
                     1  99  70 43
                     1  93  63 36
                     1 100  93 62
                     1 110  76 56
                     1 100  83 36
                     1 106  71 49
                     1 115 112 66
                     1 120  87 54
                     1  97  82 41")
cor(gorilla)
with(gorilla, tapply(W, seen, mean))
glm.out <- glm(seen ~ W * C * CW, family = binomial(link = "logit"), data = gorilla)
summary(glm.out)
1 - pchisq((65.438 - 57.281), (48 - 41))
1 - pchisq(57.281, 41)
anova(glm.out, test = "Chisq")
plot(glm.out$fitted.values)
abline(v = 30.5, col = "red")
abline(h = 0.3, col = "green")
abline(h = 0.5, col = "green")
text(15, 0.9, "seen = 0")
text(40, 0.9, "seen = 1")
glm.out2 <- glm(seen ~ CW * C * W, family = binomial(link = "logit"), data = gorilla)
anova(glm.out2, test = "Chisq")


              
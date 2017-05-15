# Loading libraries
library(foreign)
library(ggplot2)
library(caret)
library(lmSupport)
library(ggfortify)
library(relaimpo)
library(effects)

# Load dataset ----
df <- read.spss("data/linoutcomeprediction.sav", to.data.frame = TRUE)
head(df)
str(df)
# Convert to factors
df$treatment <-
  factor(df$treatment,
         levels = c(0, 1),
         labels = c("Placebo", "Treatment"))
df$gender <-
  factor(df$gender,
         levels = c(0, 1),
         labels = c("Female", "Male"))
df$comorbidity <-
  factor(df$comorbidity,
         levels = c(0, 1),
         labels = c("No", "Yes"))
head(df)
# Any missing value?
any(is.na(df))

# Data visualization ----

# Hours of Sleep
#*****************
# Left plot
par(mfrow = c(1, 2))
qqnorm(df$hoursofsleep)
qqline(df$hoursofsleep, col = "red")
# Right plot
plot(density(df$hoursofsleep), col = "blue")

# Age
#*****
par(mfrow = c(1, 2))
qqnorm(df$age)
qqline(df$age, col = "red")
plot(density(df$age), col = "blue")

# Hours of sleep * Treament
#***************************
ggplot(df, mapping = aes(x = treatment, y = hoursofsleep, fill = treatment)) +
  geom_boxplot() +
  theme_bw()
# Hours of sleep * Age
#***********************
ggplot(
  df,
  mapping = aes(
    x = age,
    y = hoursofsleep,
    color = treatment,
    fill = treatment
  ),
  alpha = 0.5
) +
  geom_point(size = 3) +
  geom_smooth(method = "lm") +
  theme_bw()

# Training model ----
set.seed(123)
control <-  trainControl(method = "boot",
                         number = 1000,
                         summaryFunction = defaultSummary)
glm1 <-  train(
  hoursofsleep ~ .,
  data = df,
  method = "glmStepAIC",
  trace = FALSE,
  trControl = control,
  metric = "RMSE"
)

# Exploring results ----
glm1$results
summary(glm1)

# Setting up linear model
set.seed(123)
lm <-  train(
  hoursofsleep ~ treatment + age,
  data = df,
  method = "lm",
  trControl = control,
  metric = "RMSE"
)
mod <- lm$finalModel

# Exploring results 2 ----
lm$results
summary(lm)
AIC(mod)
BIC(mod)
anova(mod)

modelEffectSizes(mod)

# Bootstrapping sample ----
re.sample <- as.data.frame(lm$resample)
RMSE=ts(re.sample$RMSE)
Rsquared=ts(re.sample$Rsquared)
# Visualization
autoplot(RMSE,
         ts.colour = "violet",
         size = 0.8,
         alpha = 0.7) +
  theme_bw()
autoplot(Rsquared,
         ts.colour = "red",
         size = 0.8,
         alpha = 0.7) +
  theme_bw()

ggplot(re.sample, aes(x = RMSE)) +
  geom_histogram(color = "black",
                 fill = "violet",
                 alpha = 0.5) +
  theme_bw()

ggplot(re.sample, aes(x = Rsquared)) +
  geom_histogram(color = "black",
                 fill = "red",
                 alpha = 0.5) +
  theme_bw()

# Elements role ----
boot <-
  relimp(
    mod,
    b = 1000,
    type = c("lmg", "last", "first", "pratt"),
    rank = TRUE,
    diff = TRUE,
    rela = TRUE
  )

boot <-
  boot.relimp(
    mod,
    b = 1000,
    type = c("lmg", "last", "first", "pratt"),
    rank = TRUE,
    diff = TRUE,
    rela = TRUE
  )

booteval.relimp(boot)
plot(booteval.relimp(boot,sort=FALSE))

# Marginal effects ----
confint(mod, level = 0.95)
plot(allEffects(mod))

df$pred <- predict(lm,df)
ggplot(df, aes(color = treatment)) +
  geom_point(aes(x = age, y = hoursofsleep), size = 3, alpha = 0.5) +
  geom_smooth(aes(x = age, y = pred), method = "lm") +
  theme_bw()

ggplot(df, aes(fill = treatment)) +
  geom_point(aes(x = treatment, y = hoursofsleep), size = 3, alpha = 0.5) +
  geom_boxplot(aes(x = treatment, y = pred)) +
  theme_bw()

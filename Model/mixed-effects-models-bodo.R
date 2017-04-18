# Load data
politeness <- read.csv("http://www.bodowinter.com/tutorial/politeness_data.csv")
# Overview
head(politeness)
tail(politeness)
summary(politeness)
str(politeness)
colnames(politeness)
# missing data?
which(is.na(politeness$frequency))
which(!complete.cases(politeness))
# Relationship between politeness and pitch by means of a boxplot
boxplot(frequency ~ attitude*gender, col= c("white", "lightgray"), politeness)

library(lme4)
lmer(frequency ~ attitude, data = politeness)
politeness.model <- lmer(frequency ~ attitude + (1| subject) + (1| scenario), data = politeness)
summary(politeness.model)

politeness.model <- lmer(frequency ~ attitude + gender + (1| subject) + (1| scenario), data = politeness)
summary(politeness.model)

#Statistical significant
politeness.null <- lmer(frequency ~ gender + (1| subject) + (1| scenario), data = politeness, REML = FALSE)
politeness.model <- lmer(frequency ~ attitude + gender + (1| subject) + (1| scenario), data = politeness, REML = FALSE)
anova(politeness.null, politeness.model)

coef(politeness.model)

politeness.model <- lmer(frequency ~ attitude + gender + (1 + attitude | subject) + (1 + attitude | scenario), 
                         data = politeness, REML = FALSE)
coef(politeness.model)
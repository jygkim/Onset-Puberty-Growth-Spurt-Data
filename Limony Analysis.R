#install.packages("aod")
#install.packages("ggplot2")
library(aod)
library(ggplot2)

polish.boys <- read.csv("PolishBoys.csv", header = TRUE)
polish.girls <- read.csv("PolishGirls.csv", header = TRUE)

View(polish.boys)
View(polish.girls)

summary(polish.boys$onset.age.of.puberty)
summary(polish.girls$onset.age.of.puberty)

summary(polish.boys$target.height.SDS)
summary(polish.girls$target.height.SDS)

summary(polish.boys$height.SDS.at.onset.age.of.puberty)
summary(polish.girls$height.SDS.at.onset.age.of.puberty)

par(mfrow=c(1,2))
hist(polish.boys$onset.age.of.puberty)
hist(polish.girls$onset.age.of.puberty)

sd(polish.boys$onset.age.of.puberty)
sd(polish.girls$onset.age.of.puberty)

# Linear Model with height.gap-SDS and BMI.percentile
mod.lm.boys.percentile <- lm(onset.age.of.puberty ~ height.gap.SDS + BMI.percentile, data = polish.boys)
summary(mod.lm.boys.percentile)
mod.lm.girls.percentile <- lm(onset.age.of.puberty ~ height.gap.SDS + BMI.percentile, data = polish.girls)
summary(mod.lm.girls.percentile)

# Linear Model with height.gap-SDS and BMI.SDS
mod.lm.boys.sds <- lm(onset.age.of.puberty ~ height.gap.SDS + BMI.SDS, data = polish.boys)
summary(mod.lm.boys.sds)
mod.lm.girls.sds <- lm(onset.age.of.puberty ~ height.gap.SDS + BMI.SDS, data = polish.girls)
summary(mod.lm.girls.sds)

# Logistic Regression
puberty.age.binary.boys <- ifelse(polish.boys$onset.age.of.puberty < 12, 1, 0)
summary(puberty.age.binary.boys)
table (puberty.age.binary.boys)
par(mfrow=c(1,1))
hist(puberty.age.binary.boys)

puberty.age.binary.girls <- ifelse(polish.girls$onset.age.of.puberty < 10, 1, 0)
summary(puberty.age.binary.girls)
table (puberty.age.binary.girls)
par(mfrow=c(1,1))
hist(puberty.age.binary.girls)

# Logistic Model with height.gap-SDS and BMI.percentile
mod.glm.boys.percentile <- glm(puberty.age.binary.boys ~ height.gap.SDS + BMI.percentile, 
                               family = "binomial", data = polish.boys)
summary(mod.glm.boys.percentile)

mod.glm.girls.percentile <- glm(puberty.age.binary.girls ~ height.gap.SDS + BMI.percentile, 
                                family = "binomial", data = polish.girls)
summary(mod.glm.girls.percentile)

# Logistic Model with height.gap-SDS and BMI.SDS
mod.glm.boys.sds <- glm(puberty.age.binary.boys ~ height.gap.SDS + BMI.SDS, 
                        family = "binomial", data = polish.boys)
summary(mod.glm.boys.sds)

mod.glm.girls.sds <- glm(puberty.age.binary.girls ~ height.gap.SDS + BMI.SDS, 
                         family = "binomial", data = polish.girls)
summary(mod.glm.girls.sds)

# Use a confint function to compute confidence intervals.
confint(mod.glm.boys.percentile)
confint(mod.glm.girls.percentile)
confint(mod.glm.boys.sds)
confint(mod.glm.girls.sds)
confint.default(mod.glm.boys.percentile)
confint.default(mod.glm.girls.percentile)
confint.default(mod.glm.boys.sds)
confint.default(mod.glm.girls.sds)

# Use a wald test to check the variables are significant or not.
# Wald test with height.gap-SDS and BMI.percentile
wald.test(b = coef(mod.glm.boys.percentile), Sigma = vcov(mod.glm.boys.percentile), Terms = 2)
# P-value of 0.00041 indicates of height gap is statistically significant.
wald.test(b = coef(mod.glm.boys.percentile), Sigma = vcov(mod.glm.boys.percentile), Terms = 3)
# P-value of 0.17 indicates of BMI percentile is NOT statistically significant.

wald.test(b = coef(mod.glm.girls.percentile), Sigma = vcov(mod.glm.girls.percentile), Terms = 2)
# P-value of 0.00093 indicates of height gap is statistically significant.
wald.test(b = coef(mod.glm.girls.percentile), Sigma = vcov(mod.glm.girls.percentile), Terms = 3)
# P-value of 2.1e-05 indicates of BMI percentile is statistically significant.

# Wald test with height.gap-SDS and BMI.sds
wald.test(b = coef(mod.glm.boys.sds), Sigma = vcov(mod.glm.boys.sds), Terms = 2)
# P-value of 0.00042 indicates of height gap is statistically significant.
wald.test(b = coef(mod.glm.boys.sds), Sigma = vcov(mod.glm.boys.sds), Terms = 3)
# P-value of 0.14 indicates of BMI SDS is NOT statistically significant.

wald.test(b = coef(mod.glm.girls.sds), Sigma = vcov(mod.glm.girls.sds), Terms = 2)
# P-value of 0.00096 indicates of height gap is statistically significant.
wald.test(b = coef(mod.glm.girls.sds), Sigma = vcov(mod.glm.girls.sds), Terms = 3)
# P-value of 1.6e-05 indicates of BMI SDS is statistically significant.

# To get the exponentiated coefficients. The object you want to exponentiate is called coefficients. 
exp(coef(mod.glm.boys.percentile))
exp(coef(mod.glm.girls.percentile))
exp(coef(mod.glm.boys.sds))
exp(coef(mod.glm.girls.sds))

# To get odds ratios and their confidence intervals.
# To put it all in one table, use cbind to bind the coefficients and confidence intervals column-wise.
exp(cbind(OR = coef(mod.glm.boys.percentile), confint(mod.glm.boys.percentile)))
exp(cbind(OR = coef(mod.glm.girls.percentile), confint(mod.glm.girls.percentile)))
exp(cbind(OR = coef(mod.glm.boys.sds), confint(mod.glm.boys.sds)))
exp(cbind(OR = coef(mod.glm.girls.sds), confint(mod.glm.girls.sds)))

### Polish Boys ###
# Predicted probability of early PGS, holding height.gap.SDS and BMI.percentile at their means
newdata1 <- with(polish.boys, 
                 data.frame( height.gap.SDS = mean(height.gap.SDS), BMI.percentile = mean(BMI.percentile)))
newdata1
newdata1$PGS <- predict(mod.glm.boys.percentile, newdata = newdata1, type = "response")
newdata1
# mean(polish.boys$height.gap.SDS)
# mean(polish.boys$BMI.percentile)

# Table of predicted probabilities for various values of height.gap.SDS, holding BMI.percentile at its mean
range(polish.boys$height.gap.SDS)
newdata2 <- with(polish.boys, 
                 data.frame(height.gap.SDS = seq(from = -2, to = 4, length.out = 100), 
                            BMI.percentile = mean(BMI.percentile)))
newdata3 <- cbind(newdata2, predict(mod.glm.boys.percentile, newdata = newdata2, type = "link", se = TRUE))
head(newdata3)
newdata3 <- within(newdata3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})
head(newdata3)

# Table of predicted probabilities for various values of BMI.percentile, holding height.gap.SDS at its mean
range(polish.boys$BMI.percentile)
newdata4 <- with(polish.boys, 
                 data.frame(height.gap.SDS = mean(height.gap.SDS), 
                            BMI.percentile = rep(seq(from = 0, to = 100, length.out = 100),4)))
newdata5 <- cbind(newdata4, predict(mod.glm.boys.percentile, newdata = newdata4, type = "link", se = TRUE))
head(newdata5)
newdata5 <- within(newdata5, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})
head(newdata5)

ggplot(newdata3, aes(x = height.gap.SDS, y = PredictedProb)) + 
  geom_ribbon(aes(ymin = LL, ymax = UL), alpha = 0.2) + geom_line(aes(x = height.gap.SDS), size = 1) 
ggplot(newdata5, aes(x = BMI.percentile, y = PredictedProb)) + 
  geom_ribbon(aes(ymin = LL, ymax = UL), alpha = 0.2) + geom_line(aes(x = BMI.percentile), size = 1) 

### Polish Girls ###
# Predicted probability of early PGS, holding height.gap.SDS and BMI.percentile at their means
newdata1 <- with(polish.girls, 
                 data.frame( height.gap.SDS = mean(height.gap.SDS), BMI.percentile = mean(BMI.percentile)))
newdata1
newdata1$PGS <- predict(mod.glm.girls.percentile, newdata = newdata1, type = "response")
newdata1
# mean(polish.girls$height.gap.SDS)
# mean(polish.girls$BMI.percentile)

# Table of predicted probabilities for various values of height.gap.SDS, holding BMI.percentile at its mean
range(polish.girls$height.gap.SDS)
newdata2 <- with(polish.girls, 
                 data.frame(height.gap.SDS = seq(from = -1.5, to = 3, length.out = 100), 
                            BMI.percentile = mean(BMI.percentile)))
newdata3 <- cbind(newdata2, predict(mod.glm.girls.percentile, newdata = newdata2, type = "link", se = TRUE))
head(newdata3)
newdata3 <- within(newdata3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})
head(newdata3)

# Table of predicted probabilities for various values of BMI.percentile, holding height.gap.SDS at its mean
range(polish.girls$BMI.percentile)
newdata4 <- with(polish.girls, 
                 data.frame(height.gap.SDS = mean(height.gap.SDS), 
                            BMI.percentile = rep(seq(from = 0, to = 100, length.out = 100),4)))
newdata5 <- cbind(newdata4, predict(mod.glm.girls.percentile, newdata = newdata4, type = "link", se = TRUE))
head(newdata5)
newdata5 <- within(newdata5, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})
head(newdata5)

ggplot(newdata3, aes(x = height.gap.SDS, y = PredictedProb)) + 
  geom_ribbon(aes(ymin = LL, ymax = UL), alpha = 0.2) + geom_line(aes(x = height.gap.SDS), size = 1) 
ggplot(newdata5, aes(x = BMI.percentile, y = PredictedProb)) + 
  geom_ribbon(aes(ymin = LL, ymax = UL), alpha = 0.2) + geom_line(aes(x = BMI.percentile), size = 1) 

with(mod.glm.boys.percentile, null.deviance - deviance)
with(mod.glm.boys.percentile, df.null - df.residual)
with(mod.glm.boys.percentile, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
logLik(mod.glm.boys.percentile)

# Plot references: 
# https://stats.idre.ucla.edu/r/dae/logit-regression/                                                                 
# https://thomasleeper.com/Rcourse/Tutorials/binaryglmplots.html
# https://stat.ethz.ch/R-manual/R-devel/library/stats/html/predict.glm.html
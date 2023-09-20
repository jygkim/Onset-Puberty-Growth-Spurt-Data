#install.packages("aod")
#install.packages("ggplot2")
library(aod)
library(ggplot2)

polish.boys <- read.csv("PolishBoys.csv", header = TRUE)
polish.girls <- read.csv("PolishGirls.csv", header = TRUE)

# View(polish.boys)
# View(polish.girls)

# summary(polish.boys$onset.age.of.puberty)
# summary(polish.girls$onset.age.of.puberty)

par(mfrow=c(2,1))
hist(polish.boys$onset.age.of.puberty, main = "Polish Boys", xlab = "onset age of puberty")
hist(polish.girls$onset.age.of.puberty, main = "Polish Girls", xlab = "onset age of puberty")

# sd(polish.boys$onset.age.of.puberty)
# sd(polish.girls$onset.age.of.puberty)

# Linear Model with height.gap-SDS and BMI.SDS
mod.lm.boys.sds <- lm(onset.age.of.puberty ~ height.gap.SDS + BMI.SDS, data = polish.boys)
summary(mod.lm.boys.sds)
mod.lm.girls.sds <- lm(onset.age.of.puberty ~ height.gap.SDS + BMI.SDS, data = polish.girls)
summary(mod.lm.girls.sds)

# Logistic Regression
puberty.age.binary.boys <- ifelse(polish.boys$onset.age.of.puberty < 12, 1, 0)
summary(puberty.age.binary.boys)
# table (puberty.age.binary.boys)
# par(mfrow=c(1,1))
# hist(puberty.age.binary.boys)

puberty.age.binary.girls <- ifelse(polish.girls$onset.age.of.puberty < 10, 1, 0)
summary(puberty.age.binary.girls)
# table (puberty.age.binary.girls)
# par(mfrow=c(1,1))
# hist(puberty.age.binary.girls)

# Logistic Model with height.gap-SDS and BMI.SDS
mod.glm.boys.sds <- glm(puberty.age.binary.boys ~ height.gap.SDS + BMI.SDS, 
                        family = "binomial", data = polish.boys)
summary(mod.glm.boys.sds)

mod.glm.girls.sds <- glm(puberty.age.binary.girls ~ height.gap.SDS + BMI.SDS, 
                         family = "binomial", data = polish.girls)
summary(mod.glm.girls.sds)

# Use a confint function to compute confidence intervals.
confint(mod.glm.boys.sds)
confint(mod.glm.girls.sds)
confint.default(mod.glm.boys.sds)
confint.default(mod.glm.girls.sds)

# Use a wald test to check the variables are significant or not.
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
exp(coef(mod.glm.boys.sds))
exp(coef(mod.glm.girls.sds))

# To get odds ratios and their confidence intervals.
# To put it all in one table, use cbind to bind the coefficients and confidence intervals column-wise.
exp(cbind(OR = coef(mod.glm.boys.sds), confint(mod.glm.boys.sds)))
exp(cbind(OR = coef(mod.glm.girls.sds), confint(mod.glm.girls.sds)))

### Polish Boys ###
# Predicted probability of early PGS, holding height.gap.SDS and BMI.SDS at their means
newdata1 <- with(polish.boys, 
                 data.frame( height.gap.SDS = mean(height.gap.SDS), BMI.SDS = mean(BMI.SDS)))
newdata1
newdata1$PGS <- predict(mod.glm.boys.sds, newdata = newdata1, type = "response")
newdata1
# mean(polish.boys$height.gap.SDS)
# mean(polish.boys$BMI.SDS)

# Table of predicted probabilities for various values of height.gap.SDS, holding BMI.SDS at its mean
range(polish.boys$height.gap.SDS)
newdata2 <- with(polish.boys, 
                 data.frame(height.gap.SDS = seq(from = -2, to = 4, length.out = 100), 
                            BMI.SDS = mean(BMI.SDS)))
newdata3 <- cbind(newdata2, predict(mod.glm.boys.sds, newdata = newdata2, type = "link", se = TRUE))
head(newdata3)
newdata3 <- within(newdata3, {
  PredictedProb <- plogis(fit)
  LL.B <- plogis(fit - (1.96 * se.fit))
  UL.B <- plogis(fit + (1.96 * se.fit))
})
head(newdata3)

# Table of predicted probabilities for various values of BMI.SDS, holding height.gap.SDS at its mean
range(polish.boys$BMI.SDS)
newdata4 <- with(polish.boys, 
                 data.frame(height.gap.SDS = mean(height.gap.SDS), 
                            BMI.SDS = rep(seq(from = -2.5, to = 2.5, length.out = 100),4)))
newdata5 <- cbind(newdata4, predict(mod.glm.boys.sds, newdata = newdata4, type = "link", se = TRUE))
head(newdata5)
newdata5 <- within(newdata5, {
  PredictedProb <- plogis(fit)
  LL.B <- plogis(fit - (1.96 * se.fit))
  UL.B <- plogis(fit + (1.96 * se.fit))
})
head(newdata5)

### Polish Girls ###
# Predicted probability of early PGS, holding height.gap.SDS and BMI.SDS at their means
newdata6 <- with(polish.girls, 
                 data.frame( height.gap.SDS = mean(height.gap.SDS), BMI.SDS = mean(BMI.SDS)))
newdata6
newdata6$PGS <- predict(mod.glm.girls.sds, newdata = newdata6, type = "response")
newdata6
# mean(polish.girls$height.gap.SDS)
# mean(polish.girls$BMI.SDS)

# Table of predicted probabilities for various values of height.gap.SDS, holding BMI.SDS at its mean
range(polish.girls$height.gap.SDS)
newdata7 <- with(polish.girls, 
                 data.frame(height.gap.SDS = seq(from = -2, to = 4, length.out = 100), 
                            BMI.SDS = mean(BMI.SDS)))
newdata8 <- cbind(newdata7, predict(mod.glm.girls.sds, newdata = newdata7, type = "link", se = TRUE))
head(newdata8)
newdata8 <- within(newdata8, {
  PredictedProb <- plogis(fit)
  LL.G <- plogis(fit - (1.96 * se.fit))
  UL.G <- plogis(fit + (1.96 * se.fit))
})
head(newdata8)

# Table of predicted probabilities for various values of BMI.SDS, holding height.gap.SDS at its mean
range(polish.girls$BMI.SDS)
newdata9 <- with(polish.girls, 
                 data.frame(height.gap.SDS = mean(height.gap.SDS), 
                            BMI.SDS = rep(seq(from = -2.5, to = 2.5, length.out = 100),4)))
newdata10 <- cbind(newdata9, predict(mod.glm.girls.sds, newdata = newdata9, type = "link", se = TRUE))
head(newdata10)
newdata10 <- within(newdata10, {
  PredictedProb <- plogis(fit)
  LL.G <- plogis(fit - (1.96 * se.fit))
  UL.G <- plogis(fit + (1.96 * se.fit))
})
head(newdata10)

# Plot of predicted probabilities for values of height.gap.SDS on both boys and girls
# For Boys
ggplot(newdata3, aes(x = height.gap.SDS, y = PredictedProb)) + 
  ggtitle("Predicted Probabilities for Boys Depends on Values of Height Gap") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_ribbon(aes(ymin = LL.B, ymax = UL.B), alpha = 0.2) + 
  geom_line(aes(x = height.gap.SDS), size = 1) 
# For Girls
ggplot(newdata8, aes(x = height.gap.SDS, y = PredictedProb)) + 
  ggtitle("Predicted Probabilities for Girls Depends on Values of Height Gap") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_ribbon(aes(ymin = LL.G, ymax = UL.G), alpha = 0.2) + 
  geom_line(aes(x = height.gap.SDS), size = 1)
# For Both
HeightPlot <- ggplot() + ggtitle("Predicted Probabilities Depends on Values of Height Gap") + 
  theme(plot.title = element_text(size = 15, hjust = 0.5)) +
  geom_ribbon(data=newdata3, aes(x = height.gap.SDS, ymin = LL.B, ymax = UL.B), alpha = 0.2, fill = "blue") + 
  geom_line(data=newdata3, aes(x = height.gap.SDS, y = PredictedProb, color = "blue"), size = 1) +
  geom_ribbon(data=newdata8, aes(x = height.gap.SDS, ymin = LL.G, ymax = UL.G), alpha = 0.2, fill = "red") +
  geom_line(data=newdata8, aes(x = height.gap.SDS, y = PredictedProb, color = "red"), size = 1) +
  labs(y = "Predicted Probabilities", x = "Height Gap Standard Deviation Scores") + 
  theme(plot.title = element_text(face = "bold")) +
  scale_colour_manual("", labels = c("Boys",  "Girls"),
                      values =  c("blue" = "blue", "red" = "red"))
HeightPlot

# Plot of predicted probabilities for values of BMI.SDS on both boys and girls
# For Boys
ggplot(newdata5, aes(x = BMI.SDS, y = PredictedProb)) + 
  ggtitle("Predicted Probabilities for Boys Depends on Values of BMI SDS") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_ribbon(aes(ymin = LL.B, ymax = UL.B), alpha = 0.2) + 
  geom_line(aes(x = BMI.SDS), size = 1) 
# For Girls
ggplot(newdata10, aes(x = BMI.SDS, y = PredictedProb)) + 
  ggtitle("Predicted Probabilities for Girls for of BMI SDS") + 
  theme(plot.title = element_text(size = 15, hjust = 0.5)) + 
  geom_ribbon(aes(ymin = LL.G, ymax = UL.G), alpha = 0.2) + 
  geom_line(aes(x = BMI.SDS), size = 1)
# For Both
BMIPlot <- ggplot() + ggtitle("Predicted Probabilities for BMI SDS") + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.text = element_text(size = rel(1))) +
  geom_ribbon(data=newdata5, aes(x = BMI.SDS, ymin = LL.B, ymax = UL.B), alpha = 0.2, fill = "blue") + 
  geom_line(data=newdata5, aes(x = BMI.SDS, y = PredictedProb, color = "blue"), size = 1) +
  geom_ribbon(data=newdata10, aes(x = BMI.SDS, ymin = LL.G, ymax = UL.G), alpha = 0.2, fill = "red") +
  geom_line(data=newdata10, aes(x = BMI.SDS, y = PredictedProb, color = "red"), size = 1) + 
  labs(y = "Predicted Probabilities", x = "BMI Standard Deviation Scores") + 
  theme(plot.title = element_text(face = "bold")) + 
  scale_colour_manual("", labels = c("Boys",  "Girls"),
                      values =  c("blue" = "blue", "red" = "red"))

BMIPlot

# Evaluate some expressions
# For Boys
with(mod.glm.boys.sds, null.deviance - deviance)
with(mod.glm.boys.sds, df.null - df.residual)
with(mod.glm.boys.sds, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
logLik(mod.glm.boys.sds)
# For Girls
with(mod.glm.girls.sds, null.deviance - deviance)
with(mod.glm.girls.sds, df.null - df.residual)
with(mod.glm.girls.sds, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
logLik(mod.glm.girls.sds)

# Plot references: 
# https://stats.idre.ucla.edu/r/dae/logit-regression/                                                                 
# https://thomasleeper.com/Rcourse/Tutorials/binaryglmplots.html
# https://stat.ethz.ch/R-manual/R-devel/library/stats/html/predict.glm.html
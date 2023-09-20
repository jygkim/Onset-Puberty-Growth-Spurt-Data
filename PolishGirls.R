polish.girls <- read.csv("PolishGirls.csv", header = TRUE)
View(polish.girls)
names(polish.girls)
hist(polish.girls$onset.age.of.puberty)
summary(polish.girls$onset.age.of.puberty)

mod.lm <- lm(onset.age.of.puberty ~ height.gap.SDS + BMI.percentile, data = polish.girls)
summary(mod.lm)

puberty.age.binary <- ifelse(polish.girls$onset.age.of.puberty <= median(polish.girls$onset.age.of.puberty), 1, 0)
hist(puberty.age.binary)
summary(puberty.age.binary)
table (puberty.age.binary)

mod.glm.logistic <- glm(puberty.age.binary ~ height.gap.SDS + BMI.percentile, family = binomial, data = polish.girls)
summary(mod.glm)

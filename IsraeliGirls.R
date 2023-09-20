israeli.girls <- read.csv("IsraeliGirls.csv", header = TRUE)
View(israeli.girls)
names(israeli.girls)
hist(israeli.girls$onset.age.of.puberty)
summary(israeli.girls$onset.age.of.puberty)

mod.lm <- lm(onset.age.of.puberty ~ height.gap.SDS + BMI.percentile, data = israeli.girls)
summary(mod.lm)

puberty.age.binary <- ifelse(israeli.girls$onset.age.of.puberty <= median(israeli.girls$onset.age.of.puberty), 1, 0)
hist(puberty.age.binary)
summary(puberty.age.binary)
table (puberty.age.binary)

mod.glm.logistic <- glm(puberty.age.binary ~ height.gap.SDS + BMI.percentile, family = binomial, data = israeli.girls)
summary(mod.glm)

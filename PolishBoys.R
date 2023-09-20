polish.boys <- read.csv("PolishBoys.csv", header = TRUE)
View(polish.boys)
names(polish.boys)
hist(polish.boys$onset.age.of.puberty)
summary(polish.boys$onset.age.of.puberty)

mod.lm <- lm(onset.age.of.puberty ~ height.gap.SDS + BMI.percentile, data = polish.boys)
summary(mod.lm)

puberty.age.binary <- ifelse(polish.boys$onset.age.of.puberty <= median(polish.boys$onset.age.of.puberty), 1, 0)
hist(puberty.age.binary)
summary(puberty.age.binary)
table (puberty.age.binary)

mod.glm.logistic <- glm(puberty.age.binary ~ height.gap.SDS + BMI.percentile, family = binomial, data = polish.boys)
summary(mod.glm)
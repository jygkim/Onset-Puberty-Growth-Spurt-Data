israeli.boys <- read.csv("IsraeliBoys.csv", header = TRUE)
View(israeli.boys)
names(israeli.boys)
hist(israeli.boys$onset.age.of.puberty)
summary(israeli.boys$onset.age.of.puberty)

mod.lm <- lm(onset.age.of.puberty ~ height.gap.SDS + BMI.percentile, data = israeli.boys)
summary(mod.lm)

puberty.age.binary <- ifelse(israeli.boys$onset.age.of.puberty <= median(israeli.boys$onset.age.of.puberty), 1, 0)
hist(puberty.age.binary)
summary(puberty.age.binary)
table (puberty.age.binary)

mod.glm.logistic <- glm(puberty.age.binary ~ height.gap.SDS + BMI.percentile, family = binomial, data = israeli.boys)
summary(mod.glm)

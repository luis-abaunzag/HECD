library(MASS)
library(nnet)
library(tidyverse)
library(fitdistrplus)
library(nortest)
library(normtest)
data <- read.csv("C:/Users/LuisAlberto/Downloads/insurance_claims.csv")

data <- data %>% filter(incident_severity == "Major Damage" | incident_severity == "Total Loss")
data_recast <- data
data_recast$insured_education_level <- factor(data_recast$insured_education_level,
                                              levels = c("High School", "Associate", "College", "JD", "MD", "Masters", "PhD"))
data_recast$incident_type <- factor(data_recast$incident_type,
                                    levels = c("Single Vehicle Collision", "Multi-vehicle Collision"))
data_recast$incident_severity <- factor(data_recast$incident_severity,
                                        levels = c("Major Damage", "Total Loss"))
claim <- data_recast$total_claim_amount
dist_claim <- fitdist(claim, "norm")
mean_claim <- as.numeric(dist_claim$estimate[1])
sd_claim <- as.numeric(dist_claim$estimate[2])
hist(claim, probability = T, main = "Distribución de los Montos Reclamados", ylab = "Densidad", xlab = "Monto Reclamado" )
curve(dnorm(x, mean = mean_claim, sd = sd_claim), col = "red", lty = 1, lwd = 2, add = T, )
ad.test(claim)
jb.norm.test(claim)

# Modelo
logi <- data_recast
partition <- qnorm(0.25, mean = mean_claim, sd = sd_claim)
logi$category <- cut(logi$total_claim_amount,
                     breaks = c(-Inf, partition, Inf),
                     labels = c("Low", "High"))
logi$category <- relevel(logi$category, ref = "Low")
logi$category <- as.integer(logi$category)-1
logi <- logi[, c(-20)]

reg <- glm(category ~., data = logi, family = binomial)
summary(reg)
stepAIC(reg, direction = "both")

final <- glm(formula = category ~ age + insured_sex + incident_severity + number_of_vehicles_involved ,
            family = binomial, data = logi)

dev <- final$deviance
nullDev <- final$null.deviance
modelChi <- nullDev - dev
chidf <- final$df.null - final$df.residual
chisq.prob <- 1 - pchisq(modelChi, chidf)
chisq.prob

prop.table(table(logi$category))
fit <- stats::predict(final, newdata=logi,type='response')
fit <- ifelse(fit > 0.65,1,0)
logi$pred <- fit
table(logi$pred, logi$category)
1-mean(fit != logi$category)

a <- stats::predict(final, 
                    newdata = data.frame(age = seq(from = 16, to = 80, by = 1), 
                                         insured_sex = rep("FEMALE", 65), incident_severity = rep("Major Damage", 65),
                                         number_of_vehicles_involved = rep(1, 65)), type = "response")

Pred <- a

library(scales)
library(ggplot2)
prima <- data.frame(Edad = seq(from = 16, to = 80, by = 1), Probabilidad = Pred)
ggplot(prima, aes(x = Edad, y = Probabilidad)) + geom_point(color = "red") + theme_minimal() +
  scale_x_continuous(label = comma) + ylim(0,1) + geom_hline(yintercept = 0.65)



fem <- logi %>% filter(insured_sex == "FEMALE")
hom <- logi %>% filter(insured_sex == "MALE")

hist(hom$policy_annual_premium, main = "Primas para Hombres", ylab = "Frecuencia", xlab = "prima")
hist(fem$policy_annual_premium, main = "Primas para Mujeres", ylab = "Frecuencia", xlab = "prima")


library(MASS)
library(nnet)
library(tidyverse)

data <- read.csv("C:/Users/LuisAlberto/Downloads/insurance_claims.csv")

data <- data %>% filter(incident_severity == "Major Damage" | incident_severity == "Total Loss")
data <- data[,c(-3,-12)]

glimpse(data)
chr <- data.frame(select_if(data, is.character))
ncol(chr)

data_recast <- data
data_recast$insured_education_level <- factor(data_recast$insured_education_level,
                                              levels = c("High School", "Associate", "College", "JD", "MD", "Masters", "PhD"))
data_recast$incident_type <- factor(data_recast$incident_type,
                                    levels = c("Single Vehicle Collision", "Multi-vehicle Collision"))
data_recast$incident_severity <- factor(data_recast$incident_severity,
                                        levels = c("Major Damage", "Total Loss"))

graph <- lapply(names(chr),
                function(x) 
                  ggplot(chr, aes(get(x))) +
                  geom_bar() +
                  theme(axis.text.x = element_text(angle = 90)))

fit <- glm(total_claim_amount ~ ., data = data_recast, family = "Gamma")
stepAIC(fit, direction = "both", trace = F)

fit <- glm(total_claim_amount ~ incident_type + incident_severity, data = data_recast, 
           family = "Gamma")
summary(fit)
fit2 <- glm(total_claim_amount ~ incident_type + incident_severity + policy_annual_premium*policy_deductable, 
            data = data_recast,
            family = "Gamma")
summary(fit2)

dev <- fit$deviance
nullDev <- fit$null.deviance
modelChi <- nullDev - dev
modelChi
chidf <- fit$df.null - fit$df.residual
chisq.prob <- 1 - pchisq(modelChi, chidf)
chisq.prob

dev <- fit2$deviance
nullDev <- fit2$null.deviance
modelChi <- nullDev - dev
modelChi
chidf <- fit2$df.null - fit2$df.residual
chisq.prob <- 1 - pchisq(modelChi, chidf)
chisq.prob

library(emmeans)
library(lme4)
library(magrittr)
library(lmerTest)
dat <- read.csv('data.csv')

# 3mm
fit <- glmer.nb(count3 ~ group*cond + run + (1|subjid), dat)
summary(fit)
emmeans(fit, ~ cond|group) %>% pairs()
emmeans(fit, ~ cond) %>% pairs()
emmeans(fit, ~ group|cond) %>% pairs()
emmeans(fit, ~ group) %>% pairs()

# mean
fit <- lmer(meanfd ~ group*cond + run + (1|subjid), dat)
summary(fit)
emmeans(fit, ~ cond|group) %>% pairs()
emmeans(fit, ~ cond) %>% pairs()
emmeans(fit, ~ group|cond) %>% pairs()
emmeans(fit, ~ group) %>% pairs()

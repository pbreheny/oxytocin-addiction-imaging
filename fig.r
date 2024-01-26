library(lme4)
library(ggplot2)
library(visreg)
library(magrittr)
dat <- read.csv('data.csv')

# 3mm
brk <- c(-7, -4, -1)
glmer.nb(count3 ~ group * cond + run + (1|subjid), dat) %>%
  visreg('cond', by='group', gg=TRUE, partial=TRUE, rug=FALSE,
         points.par=list(alpha=0.25)) +
  theme_minimal() +
  scale_y_continuous(breaks=brk, labels=sprintf("%.4f", exp(brk))) + 
  ylab(expression(paste('Count of ', FD >= 3, ' mm'))) +
  xlab('')

# Mean
lmer(meanfd ~ group*cond + run + (1|subjid), dat) %>%
  visreg('cond', by='group', gg=TRUE, partial=TRUE, rug=FALSE,
         points.par=list(alpha=0.25)) +
  theme_minimal() + 
  xlab('') +
  ylab('Mean FD (mm)')

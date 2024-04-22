library(tidyverse)
library(modelsummary)
library(fixest)

gm <- causaldata::gapminder

## Fixed effects der er absorberet. Snit trukket fra variation. Kun "within" variation er tilbage
gm1 <- gm %>% 
  mutate(log_GDPperCap = log(gdpPercap)) %>% 
  group_by(country) %>% 
  mutate(lifeExp_within = lifeExp - mean(lifeExp),
         log_GDPperCap_within = log_GDPperCap - mean(log_GDPperCap)) %>% 
  ungroup()

m1 <- lm(lifeExp_within ~ log_GDPperCap_within, data = gm1)
msummary(m1, stars = c("*" = .1, "**" = .05, "***" = .01))


## Binary control variables for hver individ
m2 <- lm(lifeExp ~ factor(country) + log(gdpPercap), data = gm)
msummary(m2, stars = c("*" = .1, "**" = .05, "***" = .01))

## Both models in one table
msummary(list(m1, m2), stars = c("*" = .1, "**" = .05, "***" = .01))


## TWO WAY FE
twfe <- feols(lifeExp ~ log(gdpPercap) | country + year, data = gm)
m3 <- twfe
msummary(m3, stars = c("*" = .1, "**" = .05, "***" = .01))

msummary(list(m1, m3), stars = c("*" = .1, "**" = .05, "***" = .01))

library(tidyverse)
library(fixest)
library(modelsummary)

od <- causaldata::organ_donations

# Treatment variabel
od <- od %>%
  mutate(California = State == 'California')

# Interager kvartal med treatmentvariabel
clfe <- feols(Rate ~ i(Quarter_Num, California, ref = 3) | State + Quarter_Num, data = od)

coefplot(clfe)

modelsummary(clfe)

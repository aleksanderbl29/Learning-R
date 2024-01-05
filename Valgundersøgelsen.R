library(tidyverse)
library(gtsummary)
library(GGally)
library(RColorBrewer)

import_dnes19 <- read_csv2("Data/DNES 2019.csv")

head(import_dnes19)

### Create summary table for entire dataset
# import_dnes19 %>% 
#   tbl_summary()

# Init and test -----------------------------------------------------------
init_model <- lm(import_dnes19)

dnes19 <- import_dnes19 %>% 
  select(!weight_1) %>% 
  select(!weight_2) %>% 
  select(!weight_3) %>% 
  select(!weight_4) %>% 
  select(!weight_5)

dnes19 <- import_dnes19 %>% 
  # mutate_if(is.character, factor) %>% 
  select_if(~ nlevels(.) < 15)

colnames(dnes19)
summary(dnes19$q3)
# ggpairs(dnes19)

ageudd <- dnes19 %>% 
  lm(udd ~ alder + region, data = .)

summary(ageudd)

dnes19 %>% 
  select(udd, alder, region) %>% 
  filter(!is.na(udd)) %>% 
  ggplot(aes(y = udd, x = alder)) +
  geom_point() +
  geom_smooth(aes(color = region)) +
  facet_wrap(.~region) +
  scale_color_brewer(palette = "Dark2")

RColorBrewer::display.brewer.all()


# Fig. 2.3 Danish Voter's Agenda (1971-2019) ------------------------------

import_cont_dnes <-read_csv("Data/DNES CONT.csv")
colnames(import_cont_dnes)

v194 <-import_cont_dnes %>% 
  select(v194)
view(v194)

glimpse(kontinuitetsfil_1971_2019_27april2021)

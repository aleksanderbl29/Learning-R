library(danstat)
library(tidyverse)

options(scipen = 999)

table_meta <- get_table_metadata("LABY06", variables_only = TRUE)
table_meta

laby06_vars <- list(list(code = "KOMGRP", values = NA),
                    list(code = "ALDER", values = NA),
                    list(code = "Tid", values = NA))

laby06 <- get_data("LABY06", variables = laby06_vars)

overordnede_kategorier <- c("All Denmark", "Capital municipalities", "Metropolitan municipalities",
                            "Provincial municipalities", "Commuter municipalities", "Rural municipalities")

laby06 <- laby06 %>% 
  filter(!KOMGRP %in% overordnede_kategorier) %>% 
  rename(Values = INDHOLD,
         Kommune = KOMGRP,
         Age = ALDER,
         Year = TID)

laby06

laby06 %>% ggplot(aes(x = Age, y = Values)) +
  geom_col() +
  facet_wrap(~Kommune)

laby06 %>% ggplot(aes(x = Values)) +
  geom_histogram() +
  facet_wrap(~Kommune)


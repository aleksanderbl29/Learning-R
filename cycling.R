# Install the development version from GitHub
# devtools::install_github("jenslemb/cyclingdata")
# https://x.com/jenslemb/status/1806616499810947432

library(tidyverse)
library(cyclingdata)
library(aleksandeR)

cyclingdata %>% 
  ggplot(aes(x = year, y = startlist_quality)) +
  geom_point(aes(color = race, shape = avg_speed_winner)) +
  theme_minimal() +
  scale_shape_binned()



## Session 1
library(tidyverse)
sw <- starwars %>% 
  select(name, height, mass) %>% 
  rename(weight = mass)

head(sw)

## Session 2
library(tidyverse)
sw_recode <- starwars %>% 
  select(name, height, mass, gender) %>% 
  rename(weight = mass) %>% 
  na.omit() %>% 
  mutate(height = height/100) %>% 
  filter(gender %in% c("masculine", "feminine")) %>% 
  mutate(gender = recode(gender, 
                         masculine = "m", 
                         feminine = "f")) %>% 
  mutate(size = height > 1 & weight > 75,
         size = if_else(size == TRUE, "big", "small"))
head(sw_recode)
view(sw_recode)

## Session 3
library(tidyverse)
view(msleep)

mammals <- msleep %>% 
  select(name, sleep_total) %>% 
  filter(!sleep_total > 18)
head(mammals)

## Hver af disse filtre skal køres separat (evt comment de andre ud)
large_primates <- msleep %>% 
  select(name, order, bodywt, sleep_total, conservation) %>% 
  # filter(order == "Primates",
  #        bodywt > 20) %>% # Komma fungerer i filter som et &
  # filter(name %in% c("Cow", "Dog", "Cat")) %>%  # filtrerer alle obs der er inkl i c()
  # filter(between(sleep_total, 10, 20)) %>%  # var, nedre, øvre
  # filter(near(sleep_total, 15, tol = 2)) %>% 
  filter(is.na(conservation)) %>% # Finder alle obs, hvor der er NA i conservation kolonnen
  filter(!is.na(conservation)) # Finder alle obs, hvor der IKKE (!) er NA i conservation
head(large_primates)

view(msleep)

data()
head(diamonds)
names(diamonds)
view(diamonds)
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point() +
  geom_smooth(method = lm) +
  ylim(0, max(diamonds$price)) +
  epinion_fill(palette = "main")
  

head(cars)
names(cars)
view(cars)
ggplot(cars, aes(x = carat, y = price)) +
  geom_point() +
  stat_smooth(method = lm) +
  ylim(0, max(cars$price))


head(cars)



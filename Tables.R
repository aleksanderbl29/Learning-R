library(tidyverse)
library(gtsummary)

data()
view(mtcars)

head(mtcars)

data <- mtcars
colnames(data)

data %>% 
  tbl_summary()

library(tidyverse)

head(mpg)


line <- ggplot(mpg, aes(x = year, )) +
        geom_line(aes(color = manufacturer, y = cty))
line

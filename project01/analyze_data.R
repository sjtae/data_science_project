library(tidyverse)

mtcars %>%
  glimpse() %>%
  select(mpg,hp,weight)

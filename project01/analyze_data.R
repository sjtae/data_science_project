library(tidyverse)

mtcars %>%
  glimpse() %>%
  select(mpg,hp,wt)

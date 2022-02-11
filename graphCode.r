library(tidyverse)

ggplot(data = x,
       mapping = aes(x = site,
                     y = now_total,
                     color = trap_type)) +
  geom_point(size = 1)

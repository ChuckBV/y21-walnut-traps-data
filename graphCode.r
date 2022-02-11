library(tidyverse)

x <- read_csv("/Users/#/Desktop/y21-walnut-traps-ssjv.csv")
ggplot(data = x,
       mapping = aes(x = site,
                     y = now_total,
                     color = trap_type))+
  geom_point(size = 1)

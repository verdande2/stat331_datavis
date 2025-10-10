library(ggplot2)
library(tidyverse)
library(magrittr)

# code snippets from https://homepage.divms.uiowa.edu/~luke/classes/STAT4580/ggplot.html

mpg %>% ggplot() + 
  geom_point() +
  aes(x = displ, 
      y=hwy, 
      color=class, 
      shape=factor(cyl),
      size=factor(cyl)
      )




(ms <- mutate(faithful,
              type = ifelse(eruptions < 3,
                            "short",
                            "long")) |>
    group_by(type) |>
    summarize(mean = mean(eruptions),
              sd = sd(eruptions),
              n = n()) |>
    mutate(p = n / sum(n)))
f <- function(x)
  ms$p[1] * dnorm(x, ms$mean[1], ms$sd[1]) +
  ms$p[2] * dnorm(x, ms$mean[2], ms$sd[2])

p + stat_function(fun = f, color = "red")


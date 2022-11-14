library(ggplot2)
library(tidyverse)


x = seq(0,10,by=0.01)
y = sin(x)

df = tibble(
  x,y
) %>%
  mutate(
    across(
      where(is.numeric),
      ~ .x * .5
    )
  )

ggplot(df) +
  geom_path(aes(x, y)) +
  scale_y_continuous(limits = c(-1, 1))


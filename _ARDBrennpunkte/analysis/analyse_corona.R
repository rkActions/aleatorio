library(tidyverse)
library(glue)
library(here)

# -------------------------------------------------------------------------
path = here("output/data_corona.csv")
data = readr::read_csv(path)




# which channel was most often --------------------------------------------
data %>% 
  group_by(speaker, channel) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  gt






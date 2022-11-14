library(tidyverse)
library(glue)
library(here)



# paths --------------------------------------------------------------------
path_all = here("output/df_all.csv")
path_corona = here("output/data_corona.csv")
paths = list(path_all, path_corona)


data = lapply(paths, function(x) readr::read_csv(x))

data = lapply(seq_along(data), function(i) {
  if (i == 1) {
    d = data[[i]] %>%
      select(date, thema)
    return(d)
  } else{
      d = data[[i]] %>%
        select(date, thema = title)
      return(d)
    }
}) %>% bind_rows


readr::write_csv(data, here("output/combined.csv"))






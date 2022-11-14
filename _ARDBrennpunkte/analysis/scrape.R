library(rvest)
library(tidyverse)
library(here)
library(glue)


# url ---------------------------------------------------------------------
url = "https://de.wikipedia.org/wiki/Liste_der_ARD-Brennpunkte"

# scrape ------------------------------------------------------------------
html = read_html(url)

tables = html %>%
  html_elements(".wikitable") %>%
  html_table()

# format ------------------------------------------------------------------
df_all = data.frame()
for (i in seq_along(tables)) {
  print(i)
  
  ## table
  t = tables[[i]]
  
  names_table = names(t)
  
  # if more days tables
  if (str_detect(names_table, "Folge")) {
    table = t %>%
      uncount(1) %>%
      select(c(2, 3)) %>%
      mutate("abweichung" = NA_character_)
  } else if (str_detect(names_table, "geplantes")) {
    next
  } else{
    table = t
  }
  
  names(table) = c("date", "thema", "abweichung")
  print(head(table))
  
  df_all = bind_rows(df_all, table)
  
}

# mutate the dates --------------------------------------------------------
df_all = dput(df_all)

# TODO: THis is the strangest behaviour ever
# The extract like this does not work...
# But if you copy the output of dput(df_all) into a variable df_all
# it works. But you really need to dput(df_all) and mark all the text and 
# cop it into a variable...

df_all %>%
  extract(
    "date",
    into = c("day", "month", "year"),
    regex = ("(^\\d{1,2})\\. ([a-zA-Zä]*)\\.? (\\d{4})"),
    convert = F,
    remove = F
  ) %>%  
  mutate(
    month = case_when(
      str_detect(month, "Jan") ~ "1",
      str_detect(month, "Feb") ~ "2",
      str_detect(month, "Mär") ~ "3",
      str_detect(month, "Apr") ~ "4",
      str_detect(month, "Mai") ~ "5",
      str_detect(month, "Jun") ~ "6",
      str_detect(month, "Jul") ~ "7",
      str_detect(month, "Aug") ~ "8",
      str_detect(month, "Sep") ~ "9",
      str_detect(month, "Okt") ~ "10",
      str_detect(month, "Nov") ~ "11",
      str_detect(month, "Dez") ~ "12",
      TRUE ~ NA_character_
    )) %>%
  mutate(date_n = glue::glue("{day}-{month}-{year}")) %>% 
  mutate(date = as.Date(date_n, format = "%d-%m-%Y")) %>% 
  select(date, thema, abweichung) -> df_normal


readr::write_csv(df_normal, here("output/df_all.csv"))

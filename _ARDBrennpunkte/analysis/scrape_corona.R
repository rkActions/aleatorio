library(rvest)
library(tidyverse)
library(here)
library(glue)


# url ---------------------------------------------------------------------
url = "https://de.wikipedia.org/wiki/Liste_der_ARD-Extra-Sendungen#2020"

# scrape ------------------------------------------------------------------
html = read_html(url)

tables = html %>%
  html_elements(".wikitable") %>%
  html_table() %>%
  lapply(function(x) {
    x %>% rename(
      date = 1,
      title = 2,
      duration  = 3,
      moderation = 4,
      views = 5,
      quote = 6
    )
  })



bind_rows(tables) %>%
  extract(moderation,
          into = c("speaker", "channel"),
          regex = "^(.*)\\s(\\(.*\\))") %>%
  extract(
    "date",
    into = c("day", "month", "year"),
    regex = ("(^\\d{1,2})\\.\\s?([a-zA-Zä]*)\\s?(\\d{4})$"),
    convert = T,
    remove = F
  ) %>%
  mutate(
    month = recode(
      month,
      "Januar" = "01",
      "Februar" = "02",
      "März" = "03",
      "April" = "04",
      "Mai" = "05",
      "Juni" = "06",
      "Juli" = "07",
      "August" = "08",
      "September" = "09",
      "Oktober" = "10",
      "November" = "11",
      "Dezember" = "12"
    )
  ) %>%
  mutate(date = glue::glue("{day}-{month}-{year}")) %>%
  mutate(date = as.Date(date, format="%d-%m-%Y")) %>% 
  select(-c(day, month, year))  -> df_corona

readr::write_csv(df_corona, here("output/data_corona.csv"))  


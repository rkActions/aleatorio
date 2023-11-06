library(tidyverse)
library(here)
library(glue)
library(sf)
library(rajudas)
library(jsonlite)
library(tidygeocoder)



# path data ---------------------------------------------------------------
p = here("analysis/wgs.csv")
d = read_csv(p)
wgs = d %>%
  filter(wg==T)


# no coords ---------------------------------------------------------------
wgs_coords = wgs %>%
  filter(
    if_all(.cols = c(lat, long), ~!is.na(.x))
  )

wgs_no_coords = wgs %>%
  filter(
    if_any(.cols = c(lat, long), ~is.na(.x))
  )

wgs_no_coords_coords = wgs_no_coords %>%
  mutate(
    location = case_when(
      str_detect(location, "[wW]ien|[vV]ienna") ~ str_replace(location, "[bB]ezirk|[dD]istric", ""),
      .default = location
    )
  ) %>%
  tidygeocoder::geocode(address = location)

new_coords = wgs_no_coords_coords %>%
  rename(
    lat = 11,
    long = 12
  ) %>%
  filter(
    if_all(.cols=c(lat, long), ~!is.na(.x))
  ) %>%
  select(-matches("(lat|long)\\.\\.\\."))


# all ---------------------------------------------------------------------
all = bind_rows(wgs_coords, new_coords)
all_sf = all %>% st_as_sf(coords=c("long", "lat"), crs=4326)
mapview::mapview(all_sf, zcol="price", cex="price")


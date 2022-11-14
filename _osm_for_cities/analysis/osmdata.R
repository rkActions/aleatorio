library(osmdata)
library(geodata)
library(sf)
library(tidyverse)

# get the bounding box of a place --------------------------------------------------

# gets the bb of a city, country
hh = getbb("hamburg")
ger = getbb("Germany", featuretype = "country")


# get the outline of a place ----------------------------------------------
get_subdivision = geodata::gadm("Germany", level=3, path=tempdir())
g = st_as_sf(get_subdivision)
hh = g %>% filter(str_detect(NAME_3, "Hamburg"))



library(tidyverse)
library(osmdata)
library(here)
library(glue)
library(GADMTools)


# paths -------------------------------------------------------------------
path_geodata = here("data_raw/geodata/gadm40_AUT_1.shp")
path_output_strees = here("output", "data_streets.gpkg")


# read geodata ------------------------------------------------------------
geodata =sf::read_sf(path_geodata)

## wien
vienna = geodata[geodata$NAME_1 == "Wien", ]

# get osmdata -------------------------------------------------------------
bb_vienna = st_bbox(vienna)

res = opq(bb_vienna) %>% 
  add_osm_feature(key="highway") %>% 
  osmdata_sf()

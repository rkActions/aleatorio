library(tidyverse)
library(osmdata)
library(sf)
library(here)
library(geodata)
library(glue)
library(GADMTools)
library(mapview)
library(scico)
library(showtext)
library(rnaturalearth)


# make data raw dir -------------------------------------------------------
path_data_raw = here("data_raw"); if(!dir.exists(path_data_raw)) dir.create(path_data_raw)
crs = 31287

# get shape of vienna -----------------------------------------------------

au = geodata::gadm("Austria", level=3, path=here("data_raw/"))
au_sf = st_as_sf(au)

wien = au_sf %>%
  filter(stringr::str_detect("Wien", NAME_1)) %>% st_union() %>% 
  st_as_sf()



# reproject ---------------------------------------------------------------
wien_tr = st_transform(wien, crs)


# make grid ---------------------------------------------------------------
grid = st_make_grid(wien_tr, cellsize = 1000, square = F) %>% 
  st_as_sf()


# only get the intersecting cells -----------------------------------------
ints = st_intersects(grid, wien_tr)
w = lengths(ints) > 0
grid_int = grid[w,]



# get osm data ------------------------------------------------------------

# bb needs to be in 4326
x = opq(bbox = st_bbox(wien), timeout = 25) %>% 
  add_osm_feature(key="amenity", value="cafe", value_exact = F) %>% 
  osmdata_sf()

cafes = x$osm_points
cafes_tr = st_transform(cafes, crs)

# only get the intersecting ones ------------------------------------------
cafes_int = cafes_tr[wien_tr, ]


# save them to somewhere --------------------------------------------------
write_sf(cafes_int, here("data_raw/cafes_in_vienna.gpkg"))



# count the cafes per grid cell -------------------------------------------
cafes_per_grid = lengths(st_intersects(grid_int, cafes_int))
grid_int$cafe_count = cafes_per_grid


# get the centroid of each grid cell --------------------------------------
cents = st_centroid(grid_int)


# write out ---------------------------------------------------------------
st_write(cents, here("output/vienna_cafes.gpkg"))

cents %>% 
  arrange(cafe_count) -> cents_ordered

# plot --------------------------------------------------------------------
ggplot() +
  geom_sf(
    data = cents_ordered,
    aes(
      color = cafe_count
    ),
    size = 20,
    alpha=.7
  ) +
  theme_void()


























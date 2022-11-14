library(tidyverse)
library(sf)
library(here)
library(glue)
library(sfarrow)


# path --------------------------------------------------------------------
path_geo=here("data_raw/geodata/densified_30m_streets_points.gpkg")
path_dem = here("data_raw/geodata/dem/vienna_dem.vrt")
path_streets = here("data_raw/vienna_streets_31286.gpkg")


# data --------------------------------------------------------------------
data = read_sf(path_geo)
data_sm = data[,c("osm_id", "name", "maxspeed")]
data_sm[["rn"]] = 1:nrow(data)


# write smaller data ------------------------------------------------------
st_write(data_sm, here("output/data_points_all.gpkg"))



# remove geo --------------------------------------------------------------
data_no_geom = data %>% st_drop_geometry()
write.csv(data_no_geom, here("output/data/points_no_geom.csv"))

# get the first and the last point per street -----------------------------
first_last = data_no_geom %>% 
  group_by(osm_id) %>% 
  filter(
    row_number() %in% c(1, n())
  )

first_last_geo = 
  first_last %>% left_join(data_sm, by="rn") %>% 
  st_as_sf


# write -------------------------------------------------------------------
st_write(first_last_geo, here("output/data/first_last_geo.gpkg"))



# extract height at each point --------------------------------------------
library(stars)
dem = stars::read_stars(path_dem, proxy = T)

# reproject
first_last_geo = st_transform(first_last_geo, st_crs(dem))

# extract
points_with_heigth = stars::st_extract(dem, first_last_geo)

# assign height back to first_last_geo
first_last_geo[["height"]] = points_with_heigth$vienna_dem.vrt


# write it out ------------------------------------------------------------
st_write(first_last_geo, here("output/data/first_last_geo_height.gpkg"))


# get the length of each feature ------------------------------------------
streets = st_read(path_streets)
streets[["length"]] = st_length(streets)
streets = streets[,c("osm_id", "length")]

first_last_geo %>% 
  st_drop_geometry() %>% 
  group_by(osm_id.x) %>% 
  mutate(
    g = ifelse(row_number() == 1, 1, 2)
  ) %>% 
  pivot_wider(
    id_cols =  -c(rn),
    names_from = g, 
    values_from = height
  ) %>% 
  right_join(streets, ., by=c("osm_id" = "osm_id.x")) -> d


# write out ---------------------------------------------------------------
st_write(d, here("output/data/lines_with_height_first_second.gpkg"))



ang = d %>% 
  mutate(
    diff = abs(`1` - `2`),
    length = as.numeric(length),
    angl = asin(diff/length),
    diff_per_m = diff / length
  ) %>% 
  arrange(desc(diff_per_m))





















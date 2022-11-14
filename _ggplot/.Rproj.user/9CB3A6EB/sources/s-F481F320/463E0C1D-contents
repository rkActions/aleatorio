library(rayshader)
library(elevatr)
library(raster)
library(geodata)
library(here)
library(sf)
library(geodata)
library(tidyverse)


# -------------------------------------------------------------------------

# get Country data
# path = here("data_raw/austria_shape")
# geodata::gadm("Austria", level = 1, path = path)
# austria_shape = readRDS(here(path, "gadm36_AUT_1_pk.rds")) %>% terra::vect() %>%
#   st_as_sf()

austria_shape = read_sf("~/projects/geodata/world/world_4236.geojson") %>% 
  filter(
    NAME == "Austria"
  )

austria_raster = elevatr::get_elev_raster(austria_shape, z=6)
au = mask(austria_raster, austria_shape)
AU = as.matrix(au)



# rayshaer ----------------------------------------------------------------

AU %>% 
  rayshader::sphere_shade(texture = "imhof3") %>% 
  plot_3d(AU, windowsize = c(400,400),
          zscale = 20, zoom = 0.75, phi=89, theta = 0, fov=0, background = "black")

out_dir = here("_rayshader_adventures/output")
if(!dir.exists(out_dir)) dir.create(out_dir)
out_file = here(out_dir, "au.png")
render_highquality(out_file, samples=100, width=1000, height=1000)



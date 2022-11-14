library(stars)
library(sf)
library(here)
library(terra)
library(glue)


# path --------------------------------------------------------------------
path_dem = here("data_raw/himalaya0002.vrt")
path_points = here("data_raw/mountains.csv")

# read data ---------------------------------------------------------------
dem = read_stars(path_dem)
points = read_sf(path_points) 
points = st_as_sf(points, coords = c("lon", "lat"), crs=4326)

# create the buffer -------------------------------------------------------
buf = st_buffer(points, 50000, )

# square buffers ----------------------------------------------------------
radius = .2 # degree
polys = vector("list", lengt=nrow(points))
for (i in 1:nrow(points)) {
  
  point = points[i,] 
  coords = st_coordinates(point)
  bb_coords = st_bbox(c("xmin" = coords[1] - radius,
                        "ymin" = coords[2] - radius,
                        "xmax" = coords[1] + radius,
                        "ymax" = coords[2] + radius))
  bb = st_as_sfc(bb_coords) %>% st_as_sf(crs=4326)
  polys[[i]] = bb
  
}

df_sf = do.call("rbind", polys) %>% st_as_sf()
df_sf[["name"]] = points$name



# write out each ----------------------------------------------------------
for (i in 1:nrow(df_sf)) {
 poly = df_sf[i,] 
 name = poly$name %>% tolower() %>% gsub("[.-\\s]", "", .) %>% gsub("\\s*", "", .)
 outfile = here("output", paste0(name, ".shp"))
 st_write(poly, outfile, append=F)
 outfile_dem = here("output", glue("{name}.tif"))
 if(file.exists(outfile_dem)) file.remove(outfile_dem)
 command = glue::glue("gdalwarp -dstnodata 0 -crop_to_cutline -cutline {outfile} {path_dem} {outfile_dem}") 
 system(command) 
  
 # read out file 
 cropped = stars::read_stars(outfile_dem)
 min = min(cropped[[1]])
 max = max(cropped[[1]])
 
 outfile_dem_scaled = here("output", glue("{name}_scaled.tif"))
 
 command_rescale = glue::glue("gdal_translate -scale {min} {max} 0 65000 -ot UInt16 {outfile_dem} {outfile_dem_scaled}")
 system(command_rescale)
}

path_dem = "/media/robink/robinEx/_geodata/_raster/_usa/_eaglenestwildernis/USGS_one_meter_x37y440_CO_Central_Western_2016.tif"

res = prepareDEM(demSource = path_dem, bbSource = here("test/analysis/bb.geojson"), resX=10, resY=10, dsn="~/Desktop/test.tif")


library(sf)
library(stars)
dem = read_stars(path_dem)
sh = read_sf(here("test/analysis/bb.geojson"))
sh = st_transform(sh, st_crs(dem))

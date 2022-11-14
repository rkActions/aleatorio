demSource = system.file("tif/brazil.tif", package = "hsdblendR")
bbSource = system.file("/vec/innerBrazil.gpkg", package = "hsdblendR")
add.x = .2
add.y = .2

library(terra)
rast = rast(demSource)
rast_ex = terra::extend(rast, 200)

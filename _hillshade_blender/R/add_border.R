#' Add padding to the raster
#'
#' @param star An object of class \code{star}
#' @param add.x Percentage of new columns
#' @param add.y Percentage of new rows
#'
#' @return An object of class \code{star} with padding as \code{NA}-values
#'
make_border = function(star, add.x, add.y) {

  grid_vals = star[[1]]
  ncol = ncol(grid_vals)
  nrow = nrow(grid_vals)
  new_cols = floor(ncol * add.x); if(!new_cols %% 2 == 0) new_cols = new_cols -1
  new_rows = floor(nrow * add.y); if(!new_rows %% 2 == 0) new_rows = new_rows -1

  # use terra
  rast = as(star, "SpatRaster")
  rast_extended = terra::extend(rast, c( new_rows, new_cols))
  star = stars::st_as_stars(rast_extended)# as(rast_extended, "star")
  return(star)
}



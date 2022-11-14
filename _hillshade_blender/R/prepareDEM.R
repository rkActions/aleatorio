#' Prepare a raster for the use in Blender
#'
#' @param demSource Path to a raster-file that \code{gdal} can read
#' @param bbSource Path to a vector-file that \code{gdal} can read
#' @param resX The output resolution (X) in units of the DEM's CRS
#' @param resY The output resolution (Y) in units of the DEM's CRS
#' @param add.x Percentage of new border in x-directions (new columns)
#' @param add.y Percentage of new border in y-directions (new rows)
#' @param dsn  The path to the output file
#'
#' @return
#' @export
#'
#' @examples
prepareDEM = function(demSource = NULL,
                      bbSource = NULL,
                      resX = NULL,
                      resY = NULL,
                      add.x = NULL,
                      add.y = NULL,
                      dsn = NULL) {

  # read original dem
  cli::cli_h1("Reading the dem")

  ## TODO: if its big, it will read it as proxy
  # While this might be good data[[1]] will not get the data as matrix
  dem_raw = stars::read_stars(demSource, proxy = F)
  # rast = terra::rast(demSource)

  # if no bounding Source -> use entrire Raster and
  # build the bounding box for potential later use
  cli::cli_h1("Building the bounding box")
  if (!is.null(bbSource)) {
    shape_to_crop = sf::read_sf(bbSource)
    # Making CRS equal
    cli::cli_alert("Making CRSs equal")
    if (!sf::st_crs(dem_raw) == sf::st_crs(shape_to_crop)) {
      shape_to_crop = sf::st_transform(shape_to_crop, sf::st_crs(dem_raw))
    }
    cli::cli_h1("Cropping the Raster")
    #TODO: Cropping not quite working
    dem_raw = sf::st_crop(dem_raw, shape_to_crop)
  }


  # DIMENSIONS OF THE INPUT RASTER
  dims = stars::st_dimensions(dem_raw)

  # 1. resample to desired output resolution
  if (!is.null(resX) | !is.null(resY)) {

    # use a command line call as st_warp crashes R
    cli::cli_h1("Resampling")
    rand = rnorm(1) %>% {sub("-?\\d\\.(.*)$", replacement = "\\1", .)}
    tmpName_resamp = paste(tempdir(),"/", rand, "temp_resamp.tif", sep="")
    cmd = glue::glue("gdalwarp -tr {resX} {resY} {demSource} {tmpName_resamp}")
    system(cmd)
    dem_resamp = stars::read_stars(tmpName_resamp)
    demSource = tmpName_resamp

  } else{
    cli::cli_h1("No resampling necessary")
    dem_resamp = dem_raw
  }

  # 2. Rescale the values from 0 -> 65535
  cli::cli_h1("Rescaling")
  vals = dem_resamp[[1]] %>% as.vector()
  mn = min(vals, na.rm=T)
  mx = max(vals, na.rm=T)
  tmpName_rescale = paste(tempdir(), "/temp_scale.tif", sep="")
  cmd = glue::glue("gdal_translate -of GTiff -ot UInt16 -a_nodata 0.0 -scale {mn} {mx} 0 65535 {demSource} {tmpName_rescale}")
  system(cmd)
  dem_rescale = stars::read_stars(tmpName_rescale)

  # 3. add "border"
  cli::cli_h1("Adding Border")
  if (!is.null(add.x) && !is.null(add.y)) {
    dem_rescale = make_border(dem_rescale, add.x, add.y)
  }

  # 4. write it out as Uint16
  cli::cli_h1("Writing it out")
  if (is.null(dsn)){
    stop("Provide an output filename")
  }
  filename = basename(dsn)
  directory = dirname(dsn)
  if (!dir.exists(directory)){
    dir.create(directory, recursive = T)
  }

  return(dem_rescale)
  stars::write_stars(dem_rescale, dsn, type = "UInt16", NA_value=NA_real_)

  png = sub("\\..*$", ".png", dsn)
  cmd = glue::glue('convert {dsn} -transparent black {png}')
  system(cmd)

}

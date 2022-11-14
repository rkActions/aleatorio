#' Get the osm data for a city
#'
#' @importFrom geodata gadm
#' @importFrom sf st_as_sf st_bbox
#' @importFrom stringdist amatch
#' @importFrom osmdata opq add_osm_feature osmdata_sf
#'
#' @param placename The name of a place (Can be a country, a region or a city...)
#' @param type Either country, region or city
#'
#' @export

getOSMPlace = function(country=NULL, placename=NULL, type=NULL, key=NULL, value=NULL){

  nulls = vapply(list(country, placename, type, key, value), is.null, logical(1))
  if(any(nulls)) stop("all paramters must be provided")

  if(!type %in% c("country", "region", "city")) {
    stop("'Region must be one of: region, city or country'")
  }

  # get geodata
  if(type == "country"){
    res = downloadGeo(country, placename, type)
  }else if(type == "region"){
    res = downloadGeo(country, placename, type)
  }

  # get osmdata
  data_osm = getOSMData(res, key, value)
}

downloadGeo = function(country, placename){

  level = switch(
    type,
    "country" = 0,
    "region" = 1,
    "district" = 2,
    "city" = 3
  )

  gd = geodata::gadm(country, level=level, path=tempdir())
  gd_sf = st_as_sf(gd)

  # filter for the place

  # if exact match....

  # if no extact match


  idx = amatch(placename, gd_sf$NAME_1, maxDist = 5)
  gd_sf_filter = gd_sf[idx, ]




}









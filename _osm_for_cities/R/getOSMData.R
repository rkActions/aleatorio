#' Get the OSM Data, provided a polygon

getOSMData = function(geo_outline, key, value){

  ######
  # get the osm data
  ######

  # if no region was found
  if(nrow(gd_sf_filter) == 0){
    stop(paste0("No Region found for ", region,))
  }

  # otherwise download the data for the bb first
  bb =  st_bbox(gd_sf_filter)

  # build the query
  query = osmdata::opq(bb)

  # add osm features
  cli::cli_inform("Downloading features...")
  data = query %>%
    osmdata::add_osm_feature(key=key,
                             value=value) %>%
    osmdata_sf()

  # crop to outline of region
  attributes = c("osm_points", "osm_lines", "osm_polygons", "osm_multilines", "osm_multipolygons")

  data_intersection = lapply(names(data), function(x){
    print(x)

    if(!x%in% attributes) return(x)
    if(is.null(data[[x]])) return(x)

   st_crs(data[[x]]) = 4326
   st_crs(gd_sf_filter) = 4326

   data_intersection = data[[x]][gd_sf_filter, ]
  }) %>% setNames(names(data))

  ## somehow replace the values in the osmdata-object with this data like
  data$osm_lines = data_intersection$osm_lines
  data$osm_points = data_intersection$osm_points

  return(data)
}

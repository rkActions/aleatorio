library(tidyverse)
library(osmdata)
library(sf)
library(here)
library(glue)
library(GADMTools)
library(mapview)
library(scico)
library(showtext)
library(rnaturalearth)


# where -------------------------------------------------------------------
country = "Austria" # Austria
city = "Wien" # Wienis the region
city_eng = "Vienna" # Vienna

# data dir ----------------------------------------------------------------
data_dir = here("data", city)
regions_dir = here("data/regions")
plot_dir = here("plots", city)

map(c(data_dir, regions_dir, plot_dir), function(x){
  if(!dir.exists(x)){
    dir.create(x, recursive = T)
  }
})



# vienna ------------------------------------------------------------------

# get the isocode of austria
ISOcodes::ISO_3166_1 %>% 
  filter(str_detect(Name, country)) %>% pull(Alpha_3) -> isocode

# get the shape for the level 1 regions
# TODO: better way to find the oultine -> e.g. Hamburg has this island i dont want!
outline = gadm_sf.loadCountries(isocode, level=1, basefile = "./data/regions/")$sf %>% 
  filter(str_detect(NAME_1, city ))


# as with hamburg when there is an island
outline = st_cast(outline, "POLYGON")[1,]

# -------------------------------------------------------------------------

# key value pairs of data to download -------------------------------------
kv_pairs = c("highway:bus_stop", "amenity:cafe", "amenity:bank", "amenity:bar", "amenity:bench")

# get the data for all kv pairs for all regoins ---------------------------

# write  data
# bb = getbb(city_eng, format_out = "sf_polygon")
bb = st_bbox(outline) # this works too!!

dataEachClass = lapply(kv_pairs, function(kv){
  
  print(glue("Loading {kv} data"))
    
    # key and value
    key_value = strsplit(kv, "\\:")
    key = key_value[[1]][[1]]
    value = key_value[[1]][[2]]
    
    # build name for file
    f = here(data_dir, glue("{key}_{value}.gpkg"))
    
    if(!file.exists(f)) {
      data = bb %>% opq(timeout = 60) %>% add_osm_feature(key = key, value=value) %>% osmdata_sf() %>% .[["osm_points"]] 
      data = data %>% select(osm_id, name) %>% st_set_crs(., st_crs(outline))
      data = data[outline, ]
      write_sf(data, f)
      return(data)
    }else{
      data = read_sf(f)
      return(data)
    }
  
}) %>% setNames(., kv_pairs)


# create grid over vienna -------------------------------------------------
grid = st_make_grid(outline, cellsize = c(.01, .01), square = F) %>% st_as_sf()
grid_outline = st_intersects(grid, outline, sparse = F) %>% .[,1,drop=T]
grid = grid[grid_outline, ] %>% mutate(hexid = row_number())



# to eachraster cell add the number of features ---------------------------
joined = lapply(seq_along(dataEachClass), function(d){
  
  print(paste0("Counting ", names(dataEachClass)[[d]]))
  
  join = st_join(grid, dataEachClass[[d]]) %>% 
    group_by(hexid) %>% 
    summarise(c = n())
  
}) %>% setNames(., kv_pairs)


# make maps ---------------------------------------------------------------
font_add_google("Arvo", "av")
showtext_auto()

for(i in seq_along(joined)){
  
  # name for the title 
  name = names(joined)[[i]]
  key = strsplit(name, "\\:")[[1]][[1]]
  value = strsplit(name, "\\:")[[1]][[2]]
  
  # Replace _ with empty Space
  first = gsub("_", " ", value)
  # Capitalize each first letter
  second = str_replace_all(first, "(\\b[a-z])", toupper)
  # add an s to the last word
  words = str_split(second, "\\s")
  words[[1]][length(words[[1]])] = paste0(words[[1]][length(words[[1]])], "s in ", city_eng)
  title = paste(words[[1]], collapse = " ")
  
  # the dataframe 
  df = joined[[i]]
   
  ggplot() +
    geom_sf(data = df,
            aes(fill = c),
            color = NA,
            show.legend = F) +
    scale_fill_scico(palette = "tokyo") +
    theme_void(base_family = "av") +
    labs(
      title = title
    ) +
    theme(
      plot.title = element_text(hjust=.5, size=170)
    ) -> pl
  
  
  # TODO -> Legend as bar plot?!
  
  f = here(plot_dir, glue("{key}_{value}.png") )
  ggsave(f, pl, width=14, height=14, dpi=300)
    
  
}






























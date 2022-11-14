library(osmdata)
library(geodata)
library(tidyverse)
library(sf)
library(here)
library(glue)

# -------------------------------------------------------------------------
dir_out = here("output"); if(!dir.exists(dir_out)) dir.create(dir_out)

# -------------------------------------------------------------------------
# geodata for austria
au = geodata::gadm("Austria", level = 4, path = here("output"))
au = st_as_sf(au)

write_sf(au, here("output/au_cities.gpkg"))

# for each gemeinde get the cafes -----------------------------------------

# for(r in 1:nrow(au)){
lapply(1:nrow(au), function(r) {
  tryCatch({
    cat(r, "\r")
    row = au[r,]
    # name
    name = row$NAME_4
    name_for_output = gsub("(-|\\s)", "_", name) %>% tolower()
    fn = here("output", glue("{r}_{name_for_output}.Rdata"))

    # if there are more than 2 with the same name
    if (file.exists(fn)){
      return()
    }

    # bb
    bb = st_bbox(row)

    # osm data
    data = opq(bb, timeout = 60) %>%
      add_osm_feature(key = "amenity", value = "cafe") %>%
      osmdata_sf()
    saveRDS(data, fn)
  },
  error = function(cond) {
    print(paste0("errror", cond))
  },
  finally = {
    print("finally")
  })
})


# read all the geodata ----------------------------------------------------
files = dir(dir_out, ".*\\.Rdata$", full.names = T)

# which are missing -------------------------------------------------------
names_au = au$NAME_4 %>% tolower() %>% gsub("(-|\\s)", "_", .)
filenames = basename(files) %>% gsub("(.*)\\.Rdata", "\\1", .)
names_au[!names_au %in% filenames]



city_data = lapply(seq_along(files), function(i) {
  print(i)

  file = files[[i]]
  city = gsub(".*\\/\\d*_(.*)\\.Rdata", "\\1", file)
  dataGEO = readRDS(file)

  # points
  points = dataGEO$osm_points
  polys = dataGEO$osm_polygons

  # make all the points that share one polygon one point
  touching_polys = st_touches(polys, points)

  if (length(touching_polys) == 0) {
    if (nrow(points) > 0) {
      points[["city"]] = city
      if (!"name" %in% names(points)) {
        points[["name"]] = NA
      }
    }else{
      points = data.frame(name = NA, city=city)
    }
    return(points)
  }

  # not touching points
  touching_points = unlist(touching_polys)
  non_touching_points = points[-touching_points,]

  for (j in seq_along(touching_polys)) {
    print(j)
    touching_points = points[touching_polys[[j]],]
    touching_point = touching_points[1,]

    # add them
    non_touching_points = bind_rows(non_touching_points, touching_point)
  }

  if (nrow(non_touching_points) > 0){
    non_touching_points[["city"]] = city
    if(!"name" %in% names(points)){
      non_touching_points[["name"]] = NA
    }
  }

  return(non_touching_points)

})



cd = map(seq_along(city_data), function(i){
  d = city_data[[i]]
  d = d[, c("name", "city")]
  if(!"geometry" %in% names(d)){
    d[["geometry"]] = st_sfc(st_point())
    d = st_as_sf(d)
  }
  d
})

res = bind_rows(cd)
st_write(res, here("output/cafes_oe_geo.gpkg"))



# make a ggplot -----------------------------------------------------------
au_union = geodata::gadm("austria", level=0, path=here("output/"))
au_union = st_as_sf(au_union)

library(showtext)
library(ggtext)
font_add_google("Cormorant", "cor")

showtext_auto()
ggplot() +
  geom_sf(
    data = au_union,
    color = "black",
    fill = "#ede5cf",
    size = .05
  ) +
  geom_sf(
    data = res,
    color = "darkred",
    size=.5,
    alpha=.1
  ) +
  annotate(
    "richtext",
    label.color = NA,
    fill = NA,
    color = "black",
    x = 9.5,
    y = 48.5,
    label = "<span style='color: #000000; font-size: 100px;'>Austrias Cafés (# 15.962)</span><br>
    <span style='color: #4a371a; font-size: 60px;'>(As of osm (my osm-research in August 2022...))</span>",
    hjust = 0,
    family = "cor",
  ) +
  theme(
    plot.background = element_rect(color="blue"),
    panel.border = element_rect(color="red")
  ) +
  theme_void() +
  coord_sf(datum = st_crs(3035)) +
  theme(panel.background = element_rect(fill = "#f1f1f1"))

ggsave("../../../website/static/img/graphs/cafes_oe/mapAllCafes.png")











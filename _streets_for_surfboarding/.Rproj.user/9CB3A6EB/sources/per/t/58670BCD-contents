library(here)
library(tidyverse)
library(here)
library(httr)
library(glue)


# endpoint-base -----------------------------------------------------------
make_url = function(x,y){
  # base = glue::glue("https://www.wien.gv.at/ma41datenviewer/downloads/geodaten/dom_tif/{x}_{y}_dom_tif.zip")
  base = glue::glue("https://www.wien.gv.at/ma41datenviewer/downloads/geodaten/dgm_tif/{x}_{y}_dgm_tif.zip")
  
}


urls = vector("list")
i = 1
for (x in 12:60) {
  for (y in 1:5) {
    print(i)
       url  = make_url(x,y)
       dest_file = here("data_raw", "geodata", "dem", glue("{x}_{y}.zip"))
       res = httr::GET(url, write_disk(dest_file, overwrite = T))
       i = i + 1
  } 
   
}


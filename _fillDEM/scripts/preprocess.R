library(sf)
library(tidyverse)
library(here)
library(stars)
library(wes)
library(glue)

# path data
path = here("/media/robink/robinEx/_geodata/_raster/_austria/_10m_laserscan_dem/dhm_at_lamb_2018_100x100.tif")
output_dir = here("output/austria")
name = "austria" # the name for the animation
direction = "backwards"

date = Sys.time() %>% lubridate::date() %>% str_replace_all("-", "")
output_dir = glue("{output_dir}_{date}")

if(!dir.exists(output_dir)){
  dir.create(output_dir)
}

# read data
data = stars::read_stars(path) %>% setNames(., c("dem"))

# values 
vals = data[[1]]
vals[vals == 0] = NA
data[[1]] = vals

mi = min(data[[1]], na.rm=T)
mx = max(data[[1]], na.rm=T)
range = seq(mi, mx, by=40)

for(i in seq_along(range)){
  
  # step 
  if (direction == "forward") {
    s = range[[i]]
  } else{
    s = range[[length(range) - (i - 1)]]
  }
  
  j = formatC(i, 3, flag=0)
  out_path = here(glue("{output_dir}/{j}.png"))
  
  
  # limits
  if(direction == "forward"){
    limits = c(mi, s)
  }else{
    limits = c(s, mx)
  }
  
  ggplot() +
    geom_stars(data = data,
               downsample = 0,
               show.legend = F) +
    scale_fill_continuous(
      low = "white",
      high = "#8f2100",
      limits = limits,
      na.value = "darkgrey"
    ) + theme_void() +
    theme(
      panel.background = element_rect(fill="darkgrey")
    ) -> pl
  
  ggsave(out_path, pl)
}


setwd(here(output_dir))
cmd = glue::glue("ffmpeg -r 15 -i %04d.png -c:v libx264 -s 1920x1080 -r 30 -pix_fmt yuv420p {name}.mp4")
system(cmd)

















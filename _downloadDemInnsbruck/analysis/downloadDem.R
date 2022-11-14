library(tidyverse)
library(here)
library(glue)
library(rajudas)

upper_left =  "2325-43"
lower_left = "2324-59"
lower_right =  "2524-58"
upper_right =  "2525-26"
missing_path =  "2424-26"
missing_path2 =  "2326-11"
hall =  "2425-11"


# paths -------------------------------------------------------------------
output_dir = makePath("~/geodata/innsbruck/demDownload/")



# url ---------------------------------------------------------------------
lines = c(upper_left, upper_right, lower_right, lower_left, missing_path, missing_path2, hall)

missing_tiles_path = "~/geodata/innsbruck/demDownload/missingTiles.txt"
counter = 0
for (i in seq_along(lines)) {
  corner = lines[[i]]
  corner_main_row = gsub("^(\\d{4})-.*", "\\1", corner) %>% as.numeric()


  for (j in 1:100) {
    print(j)
    # output path
    tile = glue("{corner_main_row}-{formatC(j, 2, width=2,flag = '0')}")
    output_file = makePath(here(output_dir, glue("{tile}.zip")))

    if(file.exists(output_file)) next
    if(length(dir(output_dir, tile)) > 0) next



    # url
    url = glue("https://gis.tirol.gv.at/geo/als/mosaik_1m/m28/tif/tif_1m_{tile}.zip")

    # try to download
    res = tryCatch({
      download.file(url, output_file, method = "libcurl")
    },
    warning = function(c) {
      return(c)
    })

    if (is.character(res)) {
      cat(paste0(tile, "\n"), file = missing_tiles_path, append = T)
      unlink(output_file)
    }

    # unzip
    unzip(output_file, exdir = output_dir)

    counter = counter + 1

  }
}



# merge the DOMs ----------------------------------------------------------
all_files = dir(output_dir, full.names = T)
dom_paths = all_files[grep("dom(?!.*shd).*", all_files, perl = T)]
dp = paste0(dom_paths, collapse = " ")




# create the gdal command -------------------------------------------------
command = glue("gdal_merge.py -o ~/geodata/innsbruck/demDownload/innsbruck_merged_31254.tiff -of GTiff {dp}")


# build a vrt -------------------------------------------------------------
command_vrt = glue("gdalbuildvrt /home/robin/geodata/innsbruck/demDownload//innsbruck_merged_31254.vrt {dp}")


# execute -----------------------------------------------------------------
system(command_vrt)
system(command)





















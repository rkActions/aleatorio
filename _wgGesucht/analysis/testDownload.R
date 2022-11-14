library(rvest)
library(glue)
library(dplyr)
library(stringr)


# source ------------------------------------------------------------------
source("_wgGesucht/analysis/utils.R")

# outfile -----------------------------------------------------------------
outpath = "_wgGesucht/analysis/output.csv"

if(!file.exists(outpath)) {
  print("file does not exist")
  df = data.frame(
    price = NA,
    location = NA,
    id = NA,
    size = NA,
    wg = NA
  )
  write.csv(df, outpath)
  start = 9712560
} else{
  df = read.csv(outpath)
  start = df %>% filter(
    if_all(everything(), ~!is.na(.x))
  ) %>% pull(id) %>% max
  print("file exists reading...")
}

print(df)

# get the ids -------------------------------------------------------------
end = start + 50
page_ids = start:end

rows = vector("list", length=length(page_ids))
for(i in seq_along(page_ids)){

  print(i)

  id = page_ids[[i]]
  url = paste0("https://www.wg-gesucht.de/", id, ".html" )
  file = paste0(tempfile(), ".html")

  # download
  download.file(url, file)

  # read
  html = rvest::read_html(file)

  size = getSize(html) %>% as.numeric()
  price = getPrice(html) %>% as.numeric()
  location = getLocation(html) %>% as.character()
  wg = getWG(html)
  print(paste0("WG: ", wg))

  row = list(price = price, size = size, location = location, id = id, wg=wg)
  rows[[i]] = row

}

# create dataframe --------------------------------------------------------
df_new = bind_rows(rows)


# bind them together ------------------------------------------------------
df_final = bind_rows(df, df_new)


# write out ---------------------------------------------------------------
write.csv(df_final, outpath)




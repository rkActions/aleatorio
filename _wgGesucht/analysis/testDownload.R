library(rvest)
library(glue)
library(dplyr)
library(stringr)
library(tidygeocoder)


# source ------------------------------------------------------------------
source("_wgGesucht/analysis/utils.R")

# outfile -----------------------------------------------------------------
outpath = "_wgGesucht/analysis/wgs.csv"

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
} else{
  df = read.csv(outpath)
  print("file exists reading...")
}



# get the urls ------------------------------------------------------------
urls = getUrls()


# find the end id
# end = findId()

df[["date"]] = as.Date(df[["date"]])
df[["id"]] = as.character(df[["id"]])



# date --------------------------------------------------------------------
today = Sys.Date()


# get the ids -------------------------------------------------------------
# page_ids = (end+50):(end-50)

rows = vector("list", length=length(urls))
for(i in seq_along(urls)){

  print(i)

  # id = page_ids[[i]]
  # url = paste0("https://www.wg-gesucht.de/", id, ".html" )
  url = urls[[i]]
  file = paste0(tempfile(), ".html")

  # download
  download.file(url, file)

  # read
  html = rvest::read_html(file)

  size = getSize(html) %>% as.numeric()
  price = getPrice(html) %>% as.numeric()
  ab_bis = freiAb(html)
  ab = ab_bis$ab
  bis = ab_bis$bis

  location = getLocation(html) %>% as.character()
  wg = getWG(file)
  print(paste0("WG: ", wg))

  row = list(price = price, size = size, location = location, id = url, wg=wg, date=today, ab=ab, bis=bis)
  rows[[i]] = row

}

# wgs = vapply(rows, function(x)x[["wg"]] && (length(x[["price"]])!=0), FUN.VALUE=logical(1))
# rows = rows[wgs]

# create dataframe --------------------------------------------------------
df_new = bind_rows(rows)


# geocode -----------------------------------------------------------------
df_new_geo = df_new %>%
  geocode(location)


# bind them together ------------------------------------------------------
df_final = bind_rows(df, df_new_geo)


# write out ---------------------------------------------------------------
write.csv(df_final, outpath, row.names = F)


# calculate how many wgsjj:w ----------------------------------------------
data = read.csv(outpath)

noNa = data %>%
  filter(!is.na(price))

noNaWg = noNa %>%
  filter(wg == T)

df = data.frame(
  date = Sys.time(),
  noNaAndWg = nrow(noNaWg)
)

# output path -------------------------------------------------------------
outpath_n = "_wgGesucht/analysis/nwgs.csv"
outdir = dirname(outpath_n)

if(!file.exists(outpath_n)){
  write.csv(df, outpath_n, row.names = F)
}else{
  df_old = read.csv(outpath_n)
  df_new = rbind(df, df_old)
  write.csv(df_new, outpath_n, row.names = F)
}



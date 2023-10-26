library(rvest)
library(glue)
library(dplyr)
library(stringr)


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

start = findId()
print(paste0("Start: ", start))



df[["date"]] = as.Date(df$date)


# date --------------------------------------------------------------------
today = Sys.Date()


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
  wg = getWG(file)
  print(paste0("WG: ", wg))

  row = list(price = price, size = size, location = location, id = id, wg=wg, date=today)
  rows[[i]] = row

}

# create dataframe --------------------------------------------------------
df_new = bind_rows(rows)


# bind them together ------------------------------------------------------
df_final = bind_rows(df, df_new)


# write out ---------------------------------------------------------------
write.csv(df_final, outpath, row.names = F)


# calculate how many wgsjj:w ----------------------------------------------
data = read.csv("_wgGesucht/analysis/wgs.csv")

noNa = data %>%
  filter(!is.na(price))

noNaWg = noNa %>%
  filter(wg == T)

df = data.frame(
  date = Sys.time(),
  noNaAndWg = nrow(noNaWg)
)

# output path -------------------------------------------------------------
outpath = "_wgGesucht/analysis/nwgs.csv"
outdir = dirname(outpath)

if(!file.exists(outpath)){
  write.csv(df, outpath, row.names = F)
}else{
  df_old = read.csv(outpath)
  df_new = rbind(df, df_old)
  write.csv(df_new, outpath, row.names = F)
}



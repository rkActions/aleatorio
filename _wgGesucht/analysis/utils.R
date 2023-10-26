getSize = function(html){
  size_node = html %>%
    html_elements("h2.headline-key-facts") %>%
    html_text() %>%
    str_subset("m²")

  if (length(size_node) == 0) {
    return(NA_character_)
  } else{
    size = str_extract(size_node, "(\\d*)m") %>% gsub("m", "", .)
    return(size)
  }
}

getPrice = function(html) {
  price_node = html %>%
    html_elements("h2.headline-key-facts")

  if (length(price_node) == 0) {
    return(NA_character_)
  } else{
    price = price_node %>%  html_text() %>% str_subset("€") %>% str_extract("(\\d*)€") %>% gsub("€", "", .)
    return(price)
  }
}

getLocation = function(html){
 location_node = html %>%
    html_elements('a[href="#mapContainer"]')

  if(length(location_node) == 0){
    return(NA_character_)
  } else{
    location = location_node %>% html_text()
    return(location)
  }
}



getWG = function(file){
  rawHtml = readLines(file)
  wg = any(str_detect(rawHtml, "WG-Details"))
  return(wg)
}


findId = function(url="https://www.wg-gesucht.de/wg-zimmer-in-Wien.163.0.1.0.html?"){
 raw_html = read_html(url)
 links = raw_html %>% html_elements("h3 > a") %>% lapply(function(x){
   html_attr(x, "href")
 }) %>% unlist

 wg_gesucht_links = grepl("^\\/.*[0-9]{7,}", links)
 f = links[wg_gesucht_links][[1]]
 id = as.numeric(gsub("[^0-9]", "", f))
 return(id)
}




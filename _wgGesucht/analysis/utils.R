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

getSize = function(html){

  key_fact_values = html %>% html_elements(".key_fact_value") %>%
    html_text(trim=T)

  size = str_subset(key_fact_values, "m") %>% str_extract("[0-9]*")
  return(size)

}

getPrice = function(html) {
  key_fact_values = html %>% html_elements(".key_fact_value") %>%
    html_text(trim=T)

  price = str_subset(key_fact_values, "€") %>% str_extract("[0-9]*")
  return(price)
}

getLocation = function(html){
 location_node = html %>%
    html_elements('a[href="#mapContainer"]')

  if(length(location_node) == 0){
    return(NA_character_)
  } else{
    location = location_node %>% html_text(trim = T) %>% str_replace("\\s{2,}", " ")
    return(location)
  }
}

getWG = function(file){
  rawHtml = readLines(file)
  wg = any(str_detect(rawHtml, "WG-Details"))
  return(wg)
}


findId = function(url="https://www.wg-gesucht.de/wg-zimmer-in-Berlin.8.0.1.0.html"){
 raw_html = read_html(url)
 links = raw_html %>% html_elements("h3 > a") %>% lapply(function(x){
   html_attr(x, "href")
 }) %>% unlist

 wg_gesucht_links = grepl("^\\/.*[0-9]{7,}", links)
 f = links[wg_gesucht_links][[1]]

 id = as.numeric(gsub(".*?([0-9]{4,10}).*", "\\1", f, ))
 return(id)
}


freiAb = function(html){
  sections = html %>%
    html_elements(".col-xs-12.col-sm-6")

  sections_inner = lapply(sections, function(x){
    t = x %>%
      html_elements("h3") %>%
      html_text(trim = T)

    if(length(t) > 0 && t == "Verfügbarkeit"){
      return(x)
    }
    return(NA)
  })

  if (length(sections_inner) == 0) {
    return(list(ab = NA_character_, bis = NA_character_))
  }

  section_verf = tryCatch({
    section_verf = sections_inner[!is.na(sections_inner)][[1]]
  }, error = function(cond) {
    return(NA)
  })

  if(is.na(section_verf)){
    return(list(ab = NA_character_, bis = NA_character_))
  }




  section_panel_details = section_verf %>%
    html_elements(".section_panel_detail") %>%
    html_text(trim = T) %>%
    str_replace("\\s{2,}", " ")

  section_panel_values = section_verf %>%
    html_elements(".section_panel_value") %>%
    html_text(trim = T) %>%
    str_replace("\\s{2,}", " ")

  ab_idx = which(str_detect(section_panel_details, "frei ab"))
  bis_idx = which(str_detect(section_panel_details, "frei bis"))

  ab_val = section_panel_values[ab_idx]
  bis_val = section_panel_values[bis_idx]

  return(list(ab=ab_val, bis=bis_val))

}

getUrls = function(){

  url_all_cities = "https://www.wg-gesucht.de/wohngemeinschaft.html"
  html_all_cities = read_html(url_all_cities)

  # get all the urls to the shared flats
  a_tags = html_all_cities %>% html_elements("a")
  a_tags_share_flats = a_tags[str_detect(tolower(html_text(a_tags, trim = T)), "share")]
  links_locations_1 = html_attr(a_tags_share_flats, "href")

  links_locations_2 = lapply(links_locations_1, function(l){
    if(str_detect(l, "1-zimmer")){
      l = str_replace(l, "1-zimmer-wohnungen", "wg-zimmer")
      l = str_replace(l, "\\.1\\.1\\.0", ".0.1.0")
    }
    return(l)
  }) %>% unlist

  main = c(links_locations[1:24], str_subset(links_locations_2, "Wien"))
  rest = links_locations_2[25:length(links_locations_2)]

  # select 10 random links
  samples_main = sample(main, 3)
  samples_rest = sample(rest, 1)

  sample = c(samples_main, samples_rest)

  # read the html from these locations
  links = lapply(sample, function(u){
    base = "https://www.wg-gesucht.de/"
    url = glue("{base}{u}")
    raw_html = read_html(url)
    links = raw_html %>% html_elements("h3 > a") %>% lapply(function(x) {
      href=html_attr(x, "href")
      link=glue("{base}{href}")
    }) %>% unlist
    return(links)
  }) %>% unlist

  if(length(links) > 50){
    links = links[1:50]
  }


  return(links)

}




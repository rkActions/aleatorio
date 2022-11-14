library(lubridate)
library(tidyverse)
library(glue)
library(stringr)
library(ggthemes)
library(plotly)

github_data = function(from = NULL, user = "RobinKohrs"){
  if(is.null(from)){
    url = sprintf("https://github.com/users/%s/contributions", user)
  }else{
    url = sprintf("https://github.com/users/%s/contributions?from=%s", user, from)
  }

  html = xml2::read_html(url)
  calendar = rvest::html_node(html, ".js-calendar-graph-svg")
  calendar_days = rvest::html_nodes(calendar, "rect")

  gh_data = tibble::tibble(
    date = as.Date(xml2::xml_attr(calendar_days, "data-date")),
    count = xml2::xml_attr(calendar_days, "data-count"),
    type="github",
    tip = sprintf("%s contributions", count),
    link = str_glue("https://github.com/{user}?from={date}&to={date}&tab=overview")
  )

  return(gh_data)
}


calendar_data = function(from=NULL, to = from + years(1),
                         github_user = "RobinKohrs",
                         email = "robin.kohrs@gmx.de"){


  # get the github data
  gh_data  = github_data(from, github_user)

  if(is.null(from)){
    from = today() - years(1)
  }else{
    from = lubridate::as_date(from)
  }

  to = lubridate::as_date(to)

  all_data = bind_rows(
    select(filter(gh_data, count > 0), date, type, tip, link, count),
  )

  empty_days = filter(gh_data, count == 0,
                      !date %in% all_data$date) %>% mutate(
    type = NA
  ) %>% select(date, type)

  df = bind_rows(
    all_data,
    empty_days
  ) %>% filter(date >= from, date <= to)

  return(df)

}

calendar_plot <- function(data = calendar_data()) {

  p_data <- mutate(data,
                   x = floor_date(date, "week"),
                   y = fct_rev(lubridate::wday(date, label = TRUE)),
                   type = factor(type, levels = c("github", "video", "post", "talk", "workshop", "pkg")),
                   on_click = str_glue('window.location.assign("{link}")'),
                   data_id = ifelse(!is.na(type), as.character(seq_along(x)), NA_character_),
                   tip = str_glue("<b>{tip}</b> on {date}")
  )

  counts <- count(p_data, type, .drop = FALSE) %>%
    na.omit() %>%
    mutate(
      n = case_when(
        type == "github" ~ count(na.omit(p_data), type, wt = as.integer(count)) %>% pull(),
        TRUE ~ n),
      emoji = c("💻", "📹", "📋", "📽", "⚒", "📦"),
      nice = c("GitHub", "videos", "posts", "talks", "workshops", "CRAN"),
      label = str_glue("{scales::comma_format(accuracy = 1)(n)} {emoji}\n{nice}")
    )

  my_pal = ggthemes::solarized_pal("green")(6)
  rebase <- ggthemes:::solarized_rebase()

  p <- ggplot() +
    geom_tile(data = filter(p_data, is.na(type)), width = 6, height = .8, aes(x, y, fill = "red"), key_glyph = draw_key_point) +
    # geom_tile_interactive(data = filter(p_data, !is.na(type)), width = 6, height = .8, aes(x, y, fill = type, alpha = log(as.numeric(count)), tooltip = tip, onclick = on_click, data_id = data_id), key_glyph = draw_key_point) +
    geom_tile(data = filter(p_data, !is.na(type)), width = 6, height = .8, aes(x, y, fill = type, alpha = log(as.numeric(count))), key_glyph = draw_key_point) +
    scale_fill_manual(
      values = c("github" = my_pal[[1]], "video" = my_pal[[2]], "post" = my_pal[[3]], "talk" = my_pal[[4]], workshop = my_pal[[5]], pkg = my_pal[[6]]),
      breaks = c("github", "video", "post", "talk", "workshop", "pkg"),
      labels = counts$label,
      na.translate = TRUE, na.value = "#eee8d5"
    ) +
    scale_y_discrete(breaks = c("Mon", "Wed", "Fri")) +
    theme_solarized() +
    theme(rect = element_blank(),
          line = element_blank(),
          plot.background = element_rect(fill = rebase[["rebase03"]]),
          text = element_text(family = "Lato", size = 7),
          legend.position = "bottom",
          aspect.ratio = .8/6
    ) +
    guides(
      alpha = "none",
      fill = guide_legend(
        title = NULL,
        override.aes = list(shape = 'square filled', size = 3),
        keywidth = 3,
        label.theme = element_text(family = "Lato", size = 7),
        label.position = "bottom",
        nrow = 1
      )
    ) +
    labs(x = NULL, y = NULL)

  x <- girafe(code = print(p), width_svg = 6, height_svg = 2)

  tooltip_css <- "background: #002b36; opacity: 1; color: #839496; border-radius: 5px;
  padding: 5px; box-shadow: 3px 3px 5px 0px #888888;
  font-size: 12px; border-width 2px; border-color: #002b36;"

  girafe_options(x, opts_tooltip(css = tooltip_css))
}

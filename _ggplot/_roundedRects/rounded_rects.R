library(tidyverse)
library(here)

# Use read_csv2 because it's an European file
dat = read_csv2(here("data_raw/ratios.csv"))

avgs <- dat %>% 
  pivot_longer(
    cols = -1,
    names_to = 'type',
    values_to = 'ratio'
  ) %>% 
  group_by(type) %>% 
  summarise(ratio = mean(ratio)) %>% 
  mutate(location = 'REGION AVERAGE')

### Combine with data 
dat_longer <- dat %>% 
  pivot_longer(
    cols = -1,
    names_to = 'type',
    values_to = 'ratio'
  ) 

dat_longer_with_avgs <- dat_longer %>% 
  bind_rows(avgs)

color_palette = thematic::okabe_ito(8)

# Make sure that bars are in the same order as in the data set
dat_factored <- dat_longer %>% 
  mutate(location = factor(location, levels = dat$location)) 


dat_with_avgs <- dat_longer_with_avgs %>% 
  pivot_wider(
    names_from = 'type',
    values_from = 'ratio'
  )  

dat_with_avgs %>% 
  ggplot() +
  geom_rect(
    aes(
      xmin = store_lower, 
      xmax = store_upper, 
      ymin = location, 
      ymax = location
    )
  )  



bar_height <- 0.4 
no_highlight_col <- 'grey70'
average_highlight_col <- 'grey40'
below_highlight <- color_palette[2] 

 dat_sorted = dat_with_avgs %>% 
  mutate(num = row_number(inventory_turnover)) %>% 
  # Sort so that everything is in order of rank
  # Important for text labels later on
  arrange(desc(num)) %>% 
  mutate(
    rect_color = case_when(
      inventory_turnover < store_lower ~ below_highlight,
      location == 'REGION AVERAGE' ~ average_highlight_col,
      T ~ no_highlight_col
    ),
    rect_alpha = if_else(
      inventory_turnover < store_lower,
      0.5,
      1
    ),
    point_color = if_else(
      inventory_turnover < store_lower,
      below_highlight,
      'black'
    ),
    point_fill = if_else(
      inventory_turnover < store_lower,
      below_highlight,
      'white'
    ),
    point_size = if_else(
      inventory_turnover < store_lower,
      3,
      2
    )
  )

 
 dat_sorted %>% 
   ggplot() +
   geom_rect(
     aes(
       xmin = store_lower, 
       xmax = store_upper, 
       ymin = num - bar_height, 
       ymax = num + bar_height, 
       fill = rect_color,
       alpha = rect_alpha
     ),
   ) +
   geom_point(
     aes(
       x = inventory_turnover,
       y = num,
       fill = point_fill,
       col = point_color,
       size = point_size
     ),
     shape = 21,
     stroke = 1
   ) +
   scale_fill_identity() +
   scale_color_identity() +
   scale_size_identity() +
   scale_alpha_identity() +
   theme_minimal()

 
 
 dat_sorted %>% 
   ggplot() +
   ggchicklet:::geom_rrect(
     aes(
       xmin = store_lower, 
       xmax = store_upper, 
       ymin = num - bar_height, 
       ymax = num + bar_height, 
       fill = rect_color,
       alpha = rect_alpha
     ),
     # Use relative npc unit (values between 0 and 1)
     # This ensures that radius is not too large for your canvas
     r = unit(0.5, 'npc')
   ) +
   geom_point(
     aes(
       x = inventory_turnover,
       y = num,
       fill = point_fill,
       col = point_color,
       size = point_size
     ),
     shape = 21,
     stroke = 1
   ) +
   scale_fill_identity() +
   scale_color_identity() +
   scale_size_identity() +
   scale_alpha_identity() +
   theme_minimal() -> p

 title_lab <- 'Review stores with turnover ratios that are below their\nforecasted range'
 title_size <- 14
 axis_label_size <- 8
 text_size <- 18
 p +
   scale_x_continuous(
     breaks = seq(0, 25, 5),
     position = 'top'
   ) +
   coord_cartesian(
     xlim = c(-5, 25), 
     ylim = c(0.75, 24.75),  
     expand = F,
     clip = 'off'
   ) +
   annotate(
     'segment',
     x = 0,
     xend = 25,
     y = 24.75,
     yend = 24.75,
     col = no_highlight_col,
     size = 0.25
   ) +
   labs(
     x = 'INVENTORY TURNOVER RATIO',
     y = element_blank(),
     title = title_lab
   ) +
   theme(
     text = element_text(
       size = text_size,
       color = average_highlight_col
     ),
     plot.title.position = 'plot',
     panel.grid = element_blank(),
     axis.title.x = element_text(
       size = axis_label_size,
       hjust = 0.21,
       color = no_highlight_col
     ),
     axis.text.x = element_text(
       size = axis_label_size,
       color = no_highlight_col
     ),
     axis.ticks.x = element_line(color = no_highlight_col, size = 0.25),
     axis.text.y = element_blank(),
     axis.line.x = element_blank()
   )  -> p

 y_axis_text_size <- 3
 p +
   geom_text(
     aes(
       x = 0,
       y = num,
       label = location,
       col = no_highlight_col,
       hjust = 1,
       size = y_axis_text_size
     )
   )
 
 
 
 
 
 
 









  
 
 
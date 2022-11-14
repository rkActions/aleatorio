library(here)
library(tidyverse)
library(glue)
library(sf)
library(gt)
library(gtExtras)
library(stars)

# path --------------------------------------------------------------------
path_data = here("output/cafes_oe_geo.gpkg")
path_pop_dens = here("data_raw/aut_general_2020.tif")

# austria -----------------------------------------------------------------
au_union = geodata::gadm("austria", level=0, path=here("output/"))
au_union = st_as_sf(au_union)
au_union = st_transform(au_union, 3035)

# data --------------------------------------------------------------------
data_geo = read_sf(path_data) %>%
  st_transform(3035)

data_geo %>%
  st_drop_geometry() -> data_no_geo
# gt table of the mos common cities ---------------------------------------
data_no_geo %>%
  count(city) %>%
  arrange(desc(n)) %>%
  slice_head(n=20) %>%
  rename(
   City = 1,
   `#` = 2
  ) %>%
  gt() -> gt_1



# pop dens data -----------------------------------------------------------
grid = st_as_sf(st_make_grid(au_union, 10000))



# plot --------------------------------------------------------------------

only_intersecting_cells = st_within(grid, au_union)
idxs = which(!is.na(as.numeric(only_intersecting_cells)))
cells_intersecting = grid[idxs, ]

# find all the cafes in that cell -----------------------------------------
sel_cell = cells_intersecting[90,]
res = st_intersects(data_geo, sel_cell)
idx_cafes = which(!is.na(as.numeric(res)))
cafes = data_geo[idx_cafes, ]

ggplot() +
  geom_sf(
    data = grid,
    size=.2,
    fill = NA
  ) +
  geom_sf(
    data = cells_intersecting,
    fill = "pink"
  ) +
  geom_sf(
    data = au_union,
    size=.8,
    color = "darkred",
    fill = NA
  ) +
  theme_void() -> pl2

ggsave(here("output/pl_au_grid.png"))


# highlight that cafe -----------------------------------------------------

ggplot() +
  geom_sf(
    data = grid,
    size=.2,
    fill = NA
  ) +
  geom_sf(
    data = cells_intersecting,
    fill = "pink"
  ) +
  geom_sf(
    data = au_union,
    size=.8,
    color = "darkred",
    fill = NA
  ) +
  geom_sf(
    data = sel_cell,
    size = 1,
    fill = "green"
  ) +
  geom_sf(
    data = st_buffer(sel_cell, 20000),
    color = "black",
    size = 1,
    fill = NA
  ) +
  theme_void()

ggsave(here("output/pl_au_one_cell.png"))

# highlight that cell -----------------------------------------------------
ggplot() +
  geom_sf(
    data = grid,
    size=.2,
    fill = NA
  ) +
  geom_sf(
    data = cells_intersecting,
    fill = "pink"
  ) +
  geom_sf(
    data = au_union,
    size=.8,
    color = "darkred",
    fill = NA
  ) +
  geom_sf(
    data = sel_cell,
    size = 1,
    fill = "green"
  ) +
  geom_sf(
    data = st_buffer(sel_cell, 20000),
    color = "black",
    size = 1,
    fill = NA
  ) +
  geom_sf(
    data = cafes,
    color = "darkred"
  ) +
  geom_sf_label(
    data = cafes,
    aes(
    label = name
    )
  ) +
  coord_sf(
    xlim = c(4735480, 4745480),
    ylim = c(2645175, 2655175)
  ) +
  theme_void()


ggsave(here("output/pl_au_one_cell_detail.png"))

# density per cell --------------------------------------------------------

# add idex to cells
cells_intersecting[["id"]] = 1:nrow(cells_intersecting)

# count the cafes per grid cell
cafes_per_cell = map_dbl(st_intersects(cells_intersecting, data_geo), length)
cells_intersecting[["cafes_per_cell"]] = cafes_per_cell


## plot
breaks = seq(0,1800, by=(1800-0)/10)
ggplot() +
  geom_sf(
    data = cells_intersecting,
    aes(
      fill = cafes_per_cell
    ),
    color = NA,
    show.legend = F
  ) +
  scale_fill_continuous(
    low = "black",
    high = "white",
    name = "Cafés per Cell",
    guide = guide_colorbar(
        barwidth = unit(20, "lines"),
        title.position = "top",
        title.hjust = .5
    )
  ) +
  theme_void() +
  theme(
    legend.position = "bottom"
  ) -> pl


ggsave(here("output/cafes_per_cell_austria_no_legend.png"), pl)


# cafes_per_whatever this is ----------------------------------------------
au = geodata::gadm("Austria", level = 4, path = here("output")) %>% st_as_sf()
au_3035 = st_transform(au, 3035)

au_3035[["nr_cafes"]] = map_dbl(st_intersects(au_3035, data_geo), length)
st_write(au_3035, here("outputk/cafes_per_city.gpkg"))

ggplot() +
  geom_sf(
    data = au_3035,
    aes(
      fill = nr_cafes
    ),
    color = NA,
    show.legend = F
  ) +
  scale_fill_continuous(
    low = "black",
    high = "white",
    name = "Cafés per Cell",
    guide = guide_colorbar(
        barwidth = unit(20, "lines"),
        title.position = "top",
        title.hjust = .5
    )
  ) +
  theme_void() +
  theme(
    legend.position = "bottom"
  ) -> pl

ggsave(here("output/cafes_per_city_austria_no_legend.png"), pl, width = 20, height=14)



# bring in the pop density ------------------------------------------------
pop_dens = read_stars(here("data_raw/aut_pd_2020_1km.tif"))
pop_dens = st_transform(pop_dens, 3035)
names(pop_dens) = "pop_dens"

df_geo_pop_dens = st_as_sf(pop_dens)

ggplot() +
  geom_stars(data = pop_dens,
             aes(fill = pop_dens),
             downsample = 0) +
  scico::scale_fill_scico(
    palette = "batlowK",
    guide = guide_colorbar(
      barwidth = unit(20, "lines"),
      title.position = "top",
      title.hjust = .5
    )
  ) +
  coord_sf(datum = st_crs(3035))  +
  theme_void() +
  labs(fill = "Population Density [ppl / km²]") +
  theme(legend.position = "bottom") -> pl_dens

ggsave(here("output/ppl_dens_au_1km.png"), pl_dens)



l = map_dbl(st_intersects(df_geo_pop_dens, data_geo), length)
df_geo_pop_dens[["nr_cafes"]] = l


cafes_pp = df_geo_pop_dens %>%
  rename(
    pop_dens = 1
  ) %>% mutate( cafes_pp = nr_cafes / pop_dens
  ) %>% arrange(desc(cafes_pp))



ggplot(cafes_pp %>% st_drop_geometry()) +
  geom_point(aes(pop_dens,
                 nr_cafes),alpha=.1) +
  geom_smooth(aes(pop_dens,
                  nr_cafes), method = "lm",
              color = "darkred") +
  theme_minimal() +
  labs(
    x = "Population Density [ppl / km^2]",
    y = "# Cafés"
  ) -> pl_dens_scatter


ggsave(here("output/scatter_plot_pop_dens_cafes.png"), pl_dens_scatter)


# plot points and the point with the highest cafes pp ---------------------
cets = st_centroid(cafes_pp)
most_one = cets %>% arrange(desc(cafes_pp)) %>% slice_head(n=10)

library(ggfx)

ggplot() +
  geom_sf(data = cets ,
          aes(color = cafes_pp),alpha=1, pch=20, size=.001, fill=NA) +
  scico::scale_color_scico(palette = "batlowK") +
  # with_blur(geom_sf(data = most_one,
  #                   color = "#990000",
  #                   size = 10) ,
  #           sigma = 10) +
  # geom_sf(data = most_one,
  #                   color = "#990000",
  #                   size = 1) +
  theme_void() -> pl_dens_cents

ggsave(here("output/scatter_plot_pop_dens_cafes_cents.png"), pl_dens_cents, width = 20, height = 14)














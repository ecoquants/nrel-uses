library(tidyverse)
library(raster)
library(sf)
library(fasterize)
library(here)
library(glue)
devtools::load_all(here("../nrelutils"))

depth_r <-  raster(here("data/layers/depth/_depth_epsg4326.tif"))
eez_sf  <- read_sf(here("data/layers/eez/usa_ter_simplify05_wgcs.geojson")) %>%
  mutate(
    id = row_number())

eez_r <- fasterize(eez_sf, depth_r, field = "id") %>%
  raster_trim()

plot(eez_r)
area()
area_r <- area(eez_r, na.rm=T)
plot(area_r)

glue(
  "mean width of {format(w_m_mean, digits=3)} 
  (min: {format(w_m_min, digits=3)}, max: {format(w_m_max, digits=3)})",
  w_m_mean = sqrt(cellStats(area_r, 'mean')) * 1000,
  w_m_min  = sqrt(cellStats(area_r, 'min')) * 1000,
  w_m_max  = sqrt(cellStats(area_r, 'max')) * 1000)

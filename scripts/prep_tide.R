# NOTE: using max of point pwr_wm2 values to define cell vs cable analysis using mean

source(here::here("scripts/setup.R"))
#library(RapidPolygonLookup)
devtools::load_all(here("../nrelutils"))

eez_wgcs_sf  <- read_sf(eez_wgcs_geo)
territories  <- eez_wgcs_sf$territory

tide_csv     <- here("../nrel-cables/data/tide.csv")
tide_tbl     <- read_csv(tide_csv) %>%
  mutate(
    lon = ifelse(lon < 0, lon + 360, lon)) %>%
  select(region, lon, lat, pwr_wm2)
# range(tide_tbl$lon) # 180.3469 295.5679
# range(tide_tbl$pwr_wm2) # 180.3469 295.5679
# hist(tide_tbl %>% filter(pwr_wm2 > 10) %>% .$pwr_wm2)

lyr <- "tide"
lyr_info <- get_lyr_info(lyr)
redo <- F

# reorder from small to big to NA
territories <- c("Atlantic Islands", "East", "Gulf of Mexico", "West", "Alaska", "Great Lakes", "Hawaii", "Pacific Islands")
msg(g("nrow(tide_tbl): {nrow(tide_tbl)}
      territories: {paste(territories, collapse=', ')}"))

for (ter in territories){ # ter="Gulf of Mexico"
  ter_sf           <- eez_wgcs_sf %>% filter(territory == ter)
  ter_tide_r_tif   <- glue("{dir_lyrs}/{lyr}/{ter}_{lyr}_epsg4326.tif")
  ter_tide_pts_geo <- glue("{dir_lyrs}/{lyr}/{ter}_{lyr}_epsg4326_pts.geojson")
  msg(g("{ter}"))

  if ((file.exists(ter_tide_r_tif) & !redo) | (!is.null(lyr_info$territories[[ter]]) && is.na(lyr_info$territories[[ter]]))){
    msg("  skipping")
    next
  }

  msg("  limit tide rows by bounding box of terrritory")
  b      <- sf::st_bbox(ter_sf)
  ter_tide_tbl <- tide_tbl %>%
    filter(
      lon >= b$xmin,
      lon <= b$xmax,
      lat >= b$ymin,
      lat <= b$ymax)
  msg(g("    nrow(ter_tide_tbl): {nrow(ter_tide_tbl)}; range(ter_tide_tbl$pwr_wm2): {paste(range(ter_tide_tbl$pwr_wm2), collapse=', ')}"))
  if (nrow(ter_tide_tbl) == 0 ){
    lyr_info$territories[[ter]] = NA
    set_lyr_info(lyr_info)
    next
  }

  msg("  limit tide points by exact territory polygon")
  if (!file.exists(ter_tide_pts_geo) | redo){
    ter_tide_pts <- st_as_sf(ter_tide_tbl, coords = c('lon','lat'), agr='constant', crs=4326)
    ter_tide_pts <- st_intersection(
      ter_tide_pts, 
      ter_sf %>% select(territory)) %>%
      filter(!is.na(territory))
    msg(g("    nrow(ter_tide_pts): {nrow(ter_tide_tbl)}; range(ter_tide_pts$pwr_wm2): {paste(range(ter_tide_pts$pwr_wm2), collapse=', ')}"))
    if (nrow(ter_tide_pts) == 0 ){
      lyr_info$territories[[ter]] = NA
      set_lyr_info(lyr_info)
      next
    }
    if (file.exists(ter_tide_pts_geo)) unlink(ter_tide_pts_geo)
    write_sf(ter_tide_pts, ter_tide_pts_geo)
  } else {
    ter_tide_pts <- read_sf(ter_tide_pts_geo)
  }

  msg("  rasterize to depth grid, NOTE: using max, not mean or last")
  ter_depth_r <- get_ter_depth_wgcs_r(ter)
  ter_tide_r = rasterize(
    ter_tide_pts %>% as('Spatial'),
    ter_depth_r,
    field='pwr_wm2', fun=max, na.rm=T) # plot(ter_tide_r)

  msg("  writeRaster")
  writeRaster(ter_tide_r, ter_tide_r_tif, overwrite=T)
  lyr_info$territories[[ter]] = ter_tide_r_tif
  set_lyr_info(lyr_info)
}

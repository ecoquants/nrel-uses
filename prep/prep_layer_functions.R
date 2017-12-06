# libraries ----
library(tidyverse)
library(glue)
library(stringr)
library(ncdf4)
library(raster)
library(sf)
library(leaflet) 
library(RColorBrewer)
library(fasterize)
library(rgdal)
#library(mapview)
select <- dplyr::select

# paths ----
depth_nc <- "/Users/bbest/github/obis-lat-time-fig/data/GEBCO_2014_2D.nc"
#eez_wgcs_geo <- "eez/usa__simplify05.geojson" # eez_usa_regions.geojson, usa_rgn.geojson
#file.copy("~/github/nrel-cables/data/usa_rgn_simplify05.geojson", )
#file.copy(, "eez/usa_ter_wgcs.geojson")
eez_wgcs_geo     <- "eez/usa_ter_wgcs.geojson"
eez_gcs_geo      <- "eez/usa_ter_gcs.geojson"
eez_s05_wgcs_geo <- "eez/usa_ter_simplify05_wgcs.geojson"
eez_s05_gcs_geo  <- "eez/usa_ter_simplify05_gcs.geojson"
ter_atl_islands  <- c("Puerto Rico", "US Virgin Islands")
ter_pac_islands  <- c("Guam", "Johnston Atoll", "N Mariana Islands", "Palmyra Atoll", "Wake Island")
ter_pac_plus     <- c("American Samoa", "Jarvis Island", "Howland and Baker islands")

# eez_wgcs ----
if (!file.exists(eez_wgcs_geo)){
  
  # Great Lakes
  # lakes: http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/physical/ne_10m_lakes.zip
  #   name_alt = "Great Lakes"
  # border: http://maritimeboundaries.noaa.gov/downloads/USMaritimeLimitsAndBoundariesSHP.zip
  #   dissolve
  # cut in qgis: https://gis.stackexchange.com/questions/104526/cut-polygon-shapefile-by-line-shapefile
  great_lakes <- read_sf("/Volumes/Best HD/mbon_data_big/technical/boundaries/naturalearth/ne_10m_lakes/ne_10m_greatlakes.geojson") %>%
    sf_wrap() %>%
    mutate(
      territory = "Great Lakes") %>%
    select(territory)
  
  # Plus other Pacific Islands
  pac_plus <- read_sf("/Volumes/Best HD/mbon_data_big/technical/boundaries/eez/eez.shp") %>%
    filter(Territory1 %in% ter_pac_plus) %>%
    sf_wrap() %>%
    mutate(
      territory = "Pacific Islands") %>%
    select(territory)
  
  # Territories from cable analysis and union
  eez_wgcs_sf <- read_sf("~/github/nrel-cables/data/usa_rgn.geojson") %>%
    select(territory) %>%
    rbind(
      great_lakes,
      pac_plus) %>%
    mutate(
      territory = ifelse(
        territory %in% ter_pac_islands, 
        "Pacific Islands",
        ifelse(
          territory %in% ter_atl_islands, 
          "Atlantic Islands",
          territory))) %>%
    group_by(territory) %>%
    summarize() %>%
    mutate(
      area_km2 = st_area(geometry)/(1000*1000))
  
  # simplify
  eez_s05_wgcs_sf <- rmapshaper::ms_simplify(eez_wgcs_sf, keep = 0.5)
  
  # unwrapped versions
  eez_gcs_sf     <- sf_unwrap(eez_wgcs_sf)
  eez_s05_gcs_sf <- sf_unwrap(eez_s05_wgcs_sf)
  
  # write geojson
  write_sf(eez_wgcs_sf    , eez_wgcs_geo    , delete_dsn = T)
  write_sf(eez_gcs_sf     , eez_gcs_geo     , delete_dsn = T)
  write_sf(eez_s05_wgcs_sf, eez_s05_wgcs_geo, delete_dsn = T)
  write_sf(eez_s05_gcs_sf , eez_s05_gcs_geo , delete_dsn = T)
}

# ter_depth_wgcs_sf ----
get_ter_eez_wgcs_sf <- function(ter){
  read_sf(eez_wgcs_geo) %>% 
    filter(territory==ter)
}

# ter_depth_wgcs_r ----
get_ter_depth_wgcs_r <- function(ter){
  
  dir.create("./depth", showWarnings=F)
  
  ter_depth_wgcs_tif <- sprintf("./depth/%s_depth_epsg4326.tif", ter)
  if (!file.exists(ter_depth_wgcs_tif)){
    cat("    creating", ter_depth_wgcs_tif, "\n")
    
    depth_wgcs_tif <- "depth/depth_epsg4326.tif"
    if (!file.exists(depth_wgcs_tif)){
      cat("    creating", depth_wgcs_tif, "\n")
      
      depth_gcs  <- raster(depth_nc, layer = "elevation")
      depth_wgcs <- raster_wrap(depth_gcs) # 4.2 min for Alaska
      depth_wgcs <- depth_wgcs * -1
      writeRaster(depth_wgcs, depth_wgcs_tif)
    } else {
      depth_wgcs <- raster(depth_wgcs_tif)
    }
    
    ter_eez_wgcs_r <- fasterize(ter_eez_wgcs_sf, depth_wgcs) %>% trim()

    ter_depth_wgcs <- raster_clip(depth_wgcs, ter_eez_wgcs_r) # plot(ter_depth_wgcs)
    ter_depth_wgcs[ter_depth_wgcs < 0] <- NA
    ter_depth_wgcs <- trim(ter_depth_wgcs)
    
    # hist(depth_wgcs[depth_wgcs < 0])
    # sum(getValues(depth_wgcs < 0), na.rm=T) / sum(getValues(depth_wgcs >= 0), na.rm=T)
    # plot(depth_wgcs < 0)
    
    #a <- area(ter_depth_wgcs)
    #res_km <- mean(sqrt(na.omit(getValues(a))))
    ter_depth_wgcs_tif <- sprintf("./depth/%s_depth_epsg4326.tif", ter)
    writeRaster(ter_depth_wgcs, ter_depth_wgcs_tif)
    
    #depth <- projectRaster(depth_gcs, crs=leaflet:::epsg3857, res=res_km*1000)
  }
  ter_depth_wgcs <- raster(ter_depth_wgcs_tif)
}

# efh ----
efh_ply_mod <- function(ply){
  # habitat areas of particular concern (HAPC)
  # http://www.habitat.noaa.gov/protection/efh/newInv/index.html
  # National > Information & GIS Data Download
  # - `HAPC.gdb.zip`: Habitat Areas of Particular Concern (HAPC)

  if (ter=="West"){ 
    ply <- ply %>%
      mutate(
        layer = ifelse(Species == "Groundfish", "Groundfish", HAPC_Sitename))
  }
  if (ter=="Alaska"){
    v <- ply$HAPC_Sitename
    habs <- c("Seamount", "Coral", "Slope", "Skate", "Bowers")
    for (h in habs){
      v <- ifelse(str_detect(v, h), h, v)
    }
    ply$layer <- v
  }
  ter_described = c("Alaska","West")
  if (!ter %in% ter_described){
    csv <- glue("./{p$key}/{ter}_{p$key}_data.csv")
    ply %>% st_set_geometry(NULL) %>% write_csv(csv)
    cat(glue("    ter not in ter_described. Determine criteria in {csv}."), "\n")
    cat("    For now defaulting to all MPA.\n")
    if (nrow(ply) > 0) ply$layer <- "MPA"
    #browser() # ply %>% st_set_geometry(NULL) %>% View()
  }
  
  ply
  
}

# mpa ----
mpa_ply_mod = function(ply){
  ply <- ply %>%
    mutate(
      layer = ifelse(State %in% state.name, "State MPA", State))
  ply
}
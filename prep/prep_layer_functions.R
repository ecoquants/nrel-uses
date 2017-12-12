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
devtools::load_all("~/github/nrelutils") # devtools::install_github("ecoquants/nrelutils"); library(nrelutils) 

# paths ----
setwd("~/github/nrel-uses/prep/data")
depth_nc         <- "/Users/bbest/github/obis-lat-time-fig/data/GEBCO_2014_2D.nc"
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

# depth & eez functions for regional analysis ----

get_ter_eez_wgcs_sf <- function(ter){
  read_sf(eez_wgcs_geo) %>% 
    filter(territory==ter)
}

if (F){
  # TODO: cleanup this quick fix, genericize fxn for digest_txt with nrelutils::ply_to_tifs()
  p <- list(key = "depth")
  for (ter in eez_wgcs_sf$territory){
    digest_txt <- glue("./{p$key}/{ter}_{p$key}_epsg4326.txt")
    tif        <- glue("{ter}_{p$key}_epsg4326.tif")
    write_lines(glue("depth:{tif}"), digest_txt)
  }
}

get_ter_depth_wgcs_r <- function(ter){
  
  dir.create("./depth", showWarnings=F)
  
  ter_depth_wgcs_tif <- sprintf("./depth/%s_depth_epsg4326.tif", ter)
  if (!file.exists(ter_depth_wgcs_tif)){
    cat("    creating", ter_depth_wgcs_tif, "\n")
    
    depth_wgcs_tif <- "depth/_depth_epsg4326.tif"
    if (!file.exists(depth_wgcs_tif)){
      cat("    creating", depth_wgcs_tif, "\n")
      
      depth_gcs  <- raster(depth_nc, layer = "elevation")
      depth_wgcs <- raster_wrap(depth_gcs) # 4.2 min for Alaska
      depth_wgcs <- depth_wgcs * -1
      writeRaster(depth_wgcs, depth_wgcs_tif)
    } else {
      depth_wgcs <- raster(depth_wgcs_tif)
    }
    
    ter_eez_wgcs_sf <- get_ter_eez_wgcs_sf(ter) # plot(ter_eez_wgcs_sf["territory"])
    ter_eez_wgcs_r <- fasterize(ter_eez_wgcs_sf, depth_wgcs) %>% trim() # plot(ter_eez_wgcs_r); plot(ter_eez_wgcs_sf["territory"], add=T) 
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
  #browser()
  
  ter_depth_wgcs # plot(ter_depth_wgcs)
}

# layer polygon modification functions ----

efh_ply_mod <- function(ply){
  # habitat areas of particular concern (HAPC)
  # http://www.habitat.noaa.gov/protection/efh/newInv/index.html
  # National > Information & GIS Data Download
  # - `HAPC.gdb.zip`: Habitat Areas of Particular Concern (HAPC)

  if (ter=="West"){ 
    ply <- ply %>%
      mutate(
        layer = ifelse(species == "Groundfish", "Groundfish", hapc_sitename))
  }
  if (ter=="Alaska"){
    #browser()
    v <- ply$hapc_sitename
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

mpa_ply_mod <- function(ply){
  ply <- ply %>%
    mutate(
      layer = ifelse(state %in% state.name, "State MPA", state))
  ply
}

shippinglanes_ply_mod <- function(ply){

  #cat(paste(sort(unique(ply$themelayer)), collapse="", ""))
  vals_excluded <- c("Area to be Avoided", "Particularly Sensitive Sea Area", "Precautionary Areas", "Speed Restrictions/Right Whales","Traffic Separation Schemes")
  # TODO: use vals_excluded, esp sensitive / precautionary / right whale areas, as alternate ocean use critera?
  vals_included <- c("Recommended Routes", "Shipping Fairways Lanes and Zones", "Traffic Separation Schemes/Traffic Lanes")
  # check geometries: table(st_geometry_type(ply$geometry)) # POLYGON
  
  if (nrow(ply) == 0) return(ply)
  
  #if (ter == "Hawaii") browser()
  
  # check future datasets for new values not in set of actively excluded or included to flag
  ply_unaccounted_themelayer <- ply %>%
    filter(!ply$themelayer %in% c(vals_included, vals_excluded)) %>%
    .$themelayer %>% unique()
  if (length(ply_unaccounted_themelayer) > 0) stop(glue("MISSING themelayer in shippinlanes: {paste(ply_unaccounted_themelayer, collapse=', '}"))
  
  ply <- ply %>%
    filter(themelayer %in% vals_included)
  
  if (nrow(ply) == 0) return(ply)
  
  ply <- ply %>%
    mutate(
      one = 1) %>%
    group_by(one) %>%
    summarize(
      n = n())
  ply
}


# wind ---
"/Volumes/Best HD/nrel_data_big/nrel.gov/wind/pac/pacific_coast_90mwindspeed_off.shp
/Volumes/Best HD/nrel_data_big/nrel.gov/wind/hi/HI_90mwindspeed_off.shp
/Volumes/Best HD/nrel_data_big/nrel.gov/wind/gom/gulf_of_mexico_90mwindspeed_off.shp
/Volumes/Best HD/nrel_data_big/nrel.gov/wind/gl/great_lakes_90mwindspeed_off.shp
/Volumes/Best HD/nrel_data_big/nrel.gov/wind/atl/atlantic_coast_90mwindspeed_off.shp"

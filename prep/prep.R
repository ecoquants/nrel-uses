# libraries ----
library(tidyverse)
library(stringr)
library(ncdf4)
library(raster)
library(sf)
library(leaflet) 
library(RColorBrewer)
library(fasterize)
library(rgdal)
library(mssimp)
#library(mapview)
select <- dplyr::select
devtools::load_all("~/github/nrelutils") # devtools::install_github("ecoquants/nrelutils"); library(nrelutils) 

# paths ----
setwd("~/github/nrel-uses/prep/data")
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

if (!file.exists(eez_wgcs_geo)){
  
  # Great Lakes
  # lakes: http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/physical/ne_10m_lakes.zip
  #   name_alt = "Great Lakes"
  # border: http://maritimeboundaries.noaa.gov/downloads/USMaritimeLimitsAndBoundariesSHP.zip
  #   dissolve
  # cut in qgis: https://gis.stackexchange.com/questions/104526/cut-polygon-shapefile-by-line-shapefile
  great_lakes <- read_sf("/Volumes/Best HD/mbon_data_big/technical/boundaries/naturalearth/ne_10m_lakes/ne_10m_greatlakes.geojson") %>%
    mutate(
      territory = "Great Lakes",
      geometry  = sf_wrap(geometry)) %>%
    select(territory) %>%
    st_set_crs(leaflet:::epsg4326)
  
  # Plus other Pacific Islands
  pac_plus <- read_sf("/Volumes/Best HD/mbon_data_big/technical/boundaries/eez/eez.shp") %>%
    filter(Territory1 %in% ter_pac_plus) %>%
    mutate(
      territory = "Pacific Islands",
      geometry = sf_wrap(geometry)) %>%
    select(territory) %>% 
    st_set_crs(leaflet:::epsg4326)
  
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
  eez_gcs_sf <- eez_wgcs_sf %>%
    mutate(
      geometry = sf_unwrap(geometry))
  eez_s05_gcs_sf = eez_s05_wgcs_sf %>%
    mutate(
      geometry = sf_unwrap(geometry))
  
  # write geojson
  write_sf(eez_wgcs_sf    , eez_wgcs_geo    , delete_dsn = T)
  write_sf(eez_gcs_sf     , eez_gcs_geo     , delete_dsn = T)
  write_sf(eez_s05_wgcs_sf, eez_s05_wgcs_geo, delete_dsn = T)
  write_sf(eez_s05_gcs_sf , eez_s05_gcs_geo , delete_dsn = T)
}
eez_wgcs_sf     <- read_sf(eez_wgcs_geo)
eez_s05_wgcs_sf <- read_sf(eez_s05_wgcs_geo)
# plot(eez_s05_wgcs_sf["territory"])
  
# vars ----
ter <- "Alaska"
# TODO: create digests for West
#prj <- "epsg3857"
#res_km <- 1

# territory eez ----
ter_eez_wgcs_sf <- read_sf(eez_wgcs_geo) %>%
  filter(territory==ter) # %>% st_set_crs(leaflet:::epsg4326)
ter_eez_wgcs_sp <- ter_eez_wgcs_sf %>% as("Spatial")
# eez_mer_sf <- eez_gcs_sf %>% st_transform(leaflet:::epsg3857)
# eez_mer_sp <- spTransform(eez_gcs_sp, projection(depth))

# depth ----
dir.create("./depth", showWarnings=F)

#depth_tif <- sprintf("./depth/%s_depth_%s_%dkm.tif", ter, prj, res_km)
ter_depth_wgcs_tif <- sprintf("./depth/%s_depth_epsg4326.tif", ter)
if (!file.exists(ter_depth_wgcs_tif)){

  depth_wgcs_tif <- "depth/depth_epsg4326.tif"
  if (!file.exists(depth_wgcs_tif)){
    depth_gcs  <- raster(depth_nc, layer = "elevation")
    depth_wgcs <- raster_wrap(depth_gcs) # 4.2 min for Alaska
    depth_wgcs <- depth_wgcs * -1
    writeRaster(depth_wgcs, depth_wgcs_tif)
  } else {
    depth_wgcs <- raster(depth_wgcs_tif)
  }
  
  ter_eez_wgcs_r <- fasterize(ter_eez_wgcs_sf, depth_wgcs) %>%
    trim() # 3.7 min # plot(ter_eez_wgcs_sf["territory"])
  
  ter_depth_wgcs <- raster_clip(depth_wgcs, ter_eez_wgcs_r) # plot(ter_depth_wgcs)
  
  # hist(depth_wgcs[depth_wgcs < 0])
  # sum(getValues(depth_wgcs < 0), na.rm=T) / sum(getValues(depth_wgcs >= 0), na.rm=T)
  # plot(depth_wgcs < 0)
  
  a <- area(ter_depth_wgcs)
  #res_km <- mean(sqrt(na.omit(getValues(a))))
  ter_depth_wgcs_tif <- sprintf("./depth/%s_depth_epsg4326.tif", ter)
  writeRaster(ter_depth_wgcs, ter_depth_wgcs_tif)

  #depth <- projectRaster(depth_gcs, crs=leaflet:::epsg3857, res=res_km*1000)
  #depth[depth < 0] <- NA

  writeRaster(depth, depth_tif, overwrite=T)
}
#depth <- raster(depth_tif)
ter_depth_wgcs <-  raster(ter_depth_wgcs_tif)

# mpa ----
mpa_shp <- "/Volumes/Best HD/nrel_data_big/marinecadastre.gov/MPA Inventory - MPAs by Government Level/mpa_inventory_2014_public_shp_dir/mpa_inventory_2014_public_shp.shp"
key <- "mpa"
digest_txt <- sprintf("./%s/%s_%s_%s_%dkm.txt", key, ter, key, prj, res_km)
if (!file.exists(digest)){
  mpa_sf <- read_sf(mpa_shp) %>%
    sf_intersects(eez_gcs_sf) %>%
    st_make_valid() %>%
    st_intersection(eez_gcs_sf) %>%
    st_transform(leaflet:::epsg3857) %>%
    mutate(
      type = ifelse(State %in% state.name, "State MPA", State),
      one  = 1,
      geometry = st_cast(geometry, "MULTIPOLYGON"))
  
  mpa_b <- fasterize(mpa_sf, depth, field="one", fun="first", by="type")
  
  dir.create("./mpa", showWarnings=F)
  for (lyr in names(mpa_b)){
    mpa_lyr_tif <- sprintf("./mpa/%s_%s_%s_%dkm.tif", ter, lyr, prj, res_km)
    writeRaster(mpa_b[[lyr]], mpa_lyr_tif, overwrite=T)
  }
  
  file.create(digest_txt)
}

# efh ----
# habitat areas of particular concern (HAPC)
# http://www.habitat.noaa.gov/protection/efh/newInv/index.html
# National > Information & GIS Data Download
# - `HAPC.gdb.zip`: Habitat Areas of Particular Concern (HAPC)
hapc_gdb <- "/Volumes/Best HD/nrel_data_big/marinecadastre.gov/Essential Fish Habitat (EFH)/habitat-areas-of-particular-concern/NationalHAPC.gdb"
key <- "efh"
digest_txt <- sprintf("./%s/%s_%s_%s_%dkm.txt", key, ter, key, prj, res_km)
if (!file.exists(digest)){
  hapc <- read_sf(hapc_gdb) %>%
    st_intersection(eez_gcs_sf) %>%
    st_transform(leaflet:::epsg3857) %>%
    mutate(
      one = 1,
      Shape = st_cast(Shape, "MULTIPOLYGON"))

  if (ter=="West"){ 
    hapc <- hapc %>%
      mutate(
        habitat = ifelse(Species == "Groundfish", "Groundfish", HAPC_Sitename))
  }
  if (ter=="Alaska"){
    v <- hapc$HAPC_Sitename
    habs <- c("Seamount", "Coral", "Slope", "Skate", "Bowers")
    for (h in habs){
      v <- ifelse(str_detect(v, h), h, v)
    }
    hapc$habitat <- v
  }

  hapc_b <- fasterize(hapc, depth, field="one", fun="first", by="habitat")
  
  dir.create("./efh", showWarnings=F)
  for (lyr in names(hapc_b)){
    hapc_lyr_tif <- sprintf("./efh/%s_hapc_%s_%s_%dkm.tif", ter, lyr, prj, res_km)
    writeRaster(hapc_b[[lyr]], hapc_lyr_tif, overwrite=T)
  } 
  
  file.create(digest_txt)
}

# ais ----
# TODO: redo from raw data or gdb since only 255 values in exported vesseldensity2013.tif
# vessel density: had to manually export raster from gdb
#ais_gdb <- "/Volumes/Best HD/nrel_data_big/marinecadastre.gov/2013 Vessel Density/VesselDensity2013_dir/VesselDensity2013.gdb"
#ais_gdb <- "/Volumes/Best HD/nrel_data_big/marinecadastre.gov/2013 Vessel Density/VesselDensity2013/VesselDensity2013.gdb"
#ais_gdb <- "/Volumes/Best HD/nrel_data_big/marinecadastre.gov/2013 Vessel Density/VesselDensity2013_b/VesselDensity2013.gdb"
#ogrListLayers(ais_gdb)
#ogrInfo(ais_gdb)
#st_layers(ais_gdb)
ais_orig_tif <- "/Users/bbest/Documents/VesselDensity2013_dir/vesseldensity2013.tif"
ais_out_tif  <- sprintf("./ais/%s_vesseldensity2013_%s_%dkm.tif", ter, prj, res_km)

if (!file.exists(ais_out_tif)){
  ais <- raster(ais_orig_tif)
  
  eez_ais <- st_transform(eez_gcs_sf, crs=proj4string(ais)) %>% as("Spatial")
  ais <- crop(ais, eez_ais) # plot(ais) # hist(ais)
  #ais[ais==0] <- NA; hist(ais); table(na.omit(getValues(ais))) # Doh! only 255: 545,826
  
  ais <- projectRaster(ais, depth)
  ais <- mask(ais, eez_mer_sp) # plot(ais)
  # ais0 <- ais # ais <- ais0
  ais <- ais/maxValue(ais)
  ais[ais == 0] <- NA # plot(ais)
  
  dir.create("./ais", showWarnings=F)
  writeRaster(ais, ais_out_tif, overwrite=T)
}
ais <- raster(ais_orig_tif)
  
# oil ----
#oil_shp <- "/Volumes/Best HD/nrel_data_big/marinecadastre.gov/Active Oil and Gas Leases/actlease_dir/ActiveLeases_APR2017.shp"
oil_shps <- c(
  "West"           <- "/Volumes/Best HD/nrel_data_big/marinecadastre.gov/Active Oil and Gas Leases/pac_lease(3)_dir/BOEM_Pacific_Leases.shp",
  "Gulf of Mexico" <- "/Volumes/Best HD/nrel_data_big/marinecadastre.gov/Active Oil and Gas Leases/actlease_dir/ActiveLeases_APR2017.shp",
  "Alaska"         <- "/Volumes/Best HD/nrel_data_big/marinecadastre.gov/Active Oil and Gas Leases/AKUpdate_dir/AK_Lease.shp")

if (ter %in% names(oil_shps)){
  oil_shp <- oil_shps[ter]
  oil_tif <- sprintf("./oil/%s_active_%s_%dkm.tif", ter, prj, res_km)

  if (!file.exists(oil_tif)){
    oil <- read_sf(oil_shp) %>%
      mutate(
        one = 1) %>% #,
      st_transform(leaflet:::epsg3857)
  
    oil_r <- fasterize(oil, depth, field="one", fun="first") # plot(oil_r)
    
    dir.create("./oil", showWarnings=F)
    writeRaster(oil_r, oil_tif, overwrite=T)
  }
  oil <- raster(oil_tif)
} else {
  rm(oil)
}

# wind ----
wind_shp <- "/Volumes/Best HD/nrel_data_big/nrel.gov/wind/pac/pacific_coast_90mwindspeed_off.shp"
wind_tif <- sprintf("./wind/%s_90mwindspeed_%s_%dkm.tif", ter, prj, res_km)

if (!file.exists(wind_tif)){
  wind <- read_sf(wind_shp) %>%
    st_transform(leaflet:::epsg3857)

  wind_r <- fasterize(wind, depth, field="Speed_90", fun="first") # plot(wind_r)
  
  dir.create("./wind", showWarnings=F)
  writeRaster(wind_r, wind_tif, overwrite=T)
}
wind <- raster(wind_tif)

# plot depth ----

system.time({

  # aggregate: 0.6 sec fact=10, vs 6 sec without
  r_depth <- aggregate(depth, fact=10)
  
  pal_depth <- colorNumeric(
    palette <- c("cyan3","blue","darkblue"),
    domain  <- getValues(r_depth), na.color="#00000000")
  
  leaflet() %>%
    addProviderTiles("Stamen.TonerLite", group = "B&W") %>%
    #addPolygons(data=eez_ter) %>%
    addRasterImage(
      r_depth, project=T,
      colors=pal_depth, opacity=0.7)
  
})

# shippinglanes ----
shp <- "/Volumes/Best HD/nrel_data_big/marinecadastre.gov/Shipping Lanes and Regulations/shippinglanes_dir/shippinglanes.shp"
ply <- read_sf(shp)
cat(paste(sort(unique(ply$THEMELAYER)), collapse="", ""))

vals_excluded <- c("Area to be Avoided", "Particularly Sensitive Sea Area", "Precautionary Areas", "Speed Restrictions/Right Whales","Traffic Separation Schemes")
# TODO: use vals_excluded, esp sensitive / precautionary / right whale areas, as alternate ocean use critera?
# TODO: check future datasets for new values not in set of actively excluded or included to flag
vals_included <- c("Recommended Routes", "Shipping Fairways Lanes and Zones", "Traffic Separation Schemes/Traffic Lanes")

# THEMELAYER %in% c()
# TODO: THEMELAYER %in% c()


# TODO: constraints & uses ----



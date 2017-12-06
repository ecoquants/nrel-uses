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



# TODO: constraints & uses ----
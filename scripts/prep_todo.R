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

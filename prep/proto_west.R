# by region, eg east

# libraries ----
library(tidyverse)
library(raster)
library(sf)
library(leaflet)
library(RColorBrewer)
library(fasterize)
library(rgdal)
library(mapview)
select = dplyr::select

# paths ----
setwd('~/github/nrel-uses/prep/data')
depth_w_grd = '/Volumes/Best HD/nrel_data_big/data/depth.grd' # wrapped and cropped
eez_w_geo   = '~/github/nrel-cables/data/usa_rgn_simplify05.geojson' # eez_usa_regions.geojson, usa_rgn.geojson
# TODO: use non-simplified EEZ

# vars ----
ter = 'West'
prj = 'epsg3857'
res_km = 2

# territory eez ----
eez_gcs_sf = read_sf(eez_w_geo) %>%
  filter(territory==ter) %>%
  mutate(geometry = (geometry + c(360,90)) %% c(-360) - c(0,-360+90)) %>%
  st_set_crs(leaflet:::epsg4326)
eez_gcs_sp = eez_gcs_sf %>% as('Spatial')
eez_mer_sf = eez_gcs_sf %>% st_transform(leaflet:::epsg3857)

# depth ----
depth_tif = sprintf('./depth/%s_depth_%s_%dkm.tif', ter, prj, res_km)
if (!file.exists(depth_tif)){

  depth_gcs = raster(depth_w_grd) %>%
    shift(-360) %>%
    crop(eez_gcs_sp) * -1
  depth_gcs = mask(depth_gcs, eez_gcs_sp) # plot(depth)
  
  depth = projectRaster(depth_gcs, crs=leaflet:::epsg3857, res=res_km*1000)
  depth[depth < 0] = NA
  
  dir.create('./depth', showWarnings=F)
  writeRaster(depth, depth_tif, overwrite=T)
}
depth = raster(depth_tif)

eez_mer_sp = spTransform(eez_gcs_sp, projection(depth))

# mpa ----
mpa_shp = '/Volumes/Best HD/nrel_data_big/marinecadastre.gov/MPA Inventory - MPAs by Government Level/mpa_inventory_2014_public_shp_dir/mpa_inventory_2014_public_shp.shp'

if (T){
  mpa_sf = read_sf(mpa_shp) %>%
    st_intersection(eez_gcs_sf) %>%
    st_transform(leaflet:::epsg3857) %>%
    mutate(
      type     = recode(
        State, 
        'California' = 'State MPA',
        'Oregon'     = 'State MPA',
        'Washington' = 'State MPA'),
      one      = 1,
      geometry = st_cast(geometry, 'MULTIPOLYGON'))
  
  mpa_b = fasterize(mpa_sf, depth, field='one', fun='first', by='type')
  
  dir.create('./mpa', showWarnings=F)
  for (lyr in names(mpa_b)){
    mpa_lyr_tif = sprintf('./mpa/%s_%s_%s_%dkm.tif', ter, lyr, prj, res_km)
    writeRaster(mpa_b[[lyr]], mpa_lyr_tif, overwrite=T)
  }
}

# efh ----
# habitat areas of particular concern (HAPC)
# http://www.habitat.noaa.gov/protection/efh/newInv/index.html
# National > Information & GIS Data Download
# - `HAPC.gdb.zip`: Habitat Areas of Particular Concern (HAPC)
hapc_gdb = '/Volumes/Best HD/nrel_data_big/marinecadastre.gov/Essential Fish Habitat (EFH)/habitat-areas-of-particular-concern/NationalHAPC.gdb'

if (T){
  hapc = read_sf(hapc_gdb) %>%
    st_intersection(eez_gcs_sf) %>%
    st_transform(leaflet:::epsg3857) %>%
    mutate(
      habitat = ifelse(Species == 'Groundfish', 'Groundfish', HAPC_Sitename),
      one = 1,
      Shape = st_cast(Shape, 'MULTIPOLYGON'))
  
  hapc_b = fasterize(hapc, depth, field='one', fun='first', by='habitat')
  
  dir.create('./efh', showWarnings=F)
  for (lyr in names(hapc_b)){
    hapc_lyr_tif = sprintf('./efh/%s_hapc_%s_%s_%dkm.tif', ter, lyr, prj, res_km)
    writeRaster(hapc_b[[lyr]], hapc_lyr_tif, overwrite=T)
  } 
}

# ais ----
# TODO: redo from raw data or gdb since only 255 values in exported vesseldensity2013.tif
# vessel density: had to manually export raster from gdb
#ais_gdb = '/Volumes/Best HD/nrel_data_big/marinecadastre.gov/2013 Vessel Density/VesselDensity2013_dir/VesselDensity2013.gdb'
#ais_gdb = '/Volumes/Best HD/nrel_data_big/marinecadastre.gov/2013 Vessel Density/VesselDensity2013/VesselDensity2013.gdb'
#ais_gdb = '/Volumes/Best HD/nrel_data_big/marinecadastre.gov/2013 Vessel Density/VesselDensity2013_b/VesselDensity2013.gdb'
#ogrListLayers(ais_gdb)
#ogrInfo(ais_gdb)
#st_layers(ais_gdb)
ais_orig_tif = '/Users/bbest/Documents/VesselDensity2013_dir/vesseldensity2013.tif'
ais_out_tif  = sprintf('./ais/%s_vesseldensity2013_%s_%dkm.tif', ter, prj, res_km)

if (!file.exists(ais_out_tif)){
  ais = raster(ais_orig_tif)
  
  eez_ais = st_transform(eez_gcs_sf, crs=proj4string(ais)) %>% as('Spatial')
  ais = crop(ais, eez_ais) # plot(ais) # hist(ais)
  #ais[ais==0] = NA; hist(ais); table(na.omit(getValues(ais))) # Doh! only 255: 545,826
  
  ais = projectRaster(ais, depth)
  ais = mask(ais, eez_mer_sp) # plot(ais)
  # ais0 = ais # ais = ais0
  ais = ais/maxValue(ais)
  ais[ais == 0] = NA # plot(ais)
  
  dir.create('./ais', showWarnings=F)
  writeRaster(ais, ais_out_tif, overwrite=T)
}
ais = raster(ais_orig_tif)
  
# oil ----
#oil_shp = '/Volumes/Best HD/nrel_data_big/marinecadastre.gov/Active Oil and Gas Leases/actlease_dir/ActiveLeases_APR2017.shp'
oil_shp = '/Volumes/Best HD/nrel_data_big/marinecadastre.gov/Active Oil and Gas Leases/pac_lease(3)_dir/BOEM_Pacific_Leases.shp'
oil_tif = sprintf('./oil/%s_active_%s_%dkm.tif', ter, prj, res_km)

if (!file.exists(oil_tif)){
  oil = read_sf(oil_shp) %>%
    mutate(
      one = 1) %>% #,
    st_transform(leaflet:::epsg3857)

  oil_r = fasterize(oil, depth, field='one', fun='first') # plot(oil_r)
  
  dir.create('./oil', showWarnings=F)
  writeRaster(oil_r, oil_tif, overwrite=T)
}
oil = raster(oil_tif)

# wind ----
wind_shp = '/Volumes/Best HD/nrel_data_big/nrel.gov/wind/pac/pacific_coast_90mwindspeed_off.shp'
wind_tif = sprintf('./wind/%s_90mwindspeed_%s_%dkm.tif', ter, prj, res_km)

if (!file.exists(wind_tif)){
  wind = read_sf(wind_shp) %>%
    st_transform(leaflet:::epsg3857)

  wind_r = fasterize(wind, depth, field='Speed_90', fun='first') # plot(wind_r)
  
  dir.create('./wind', showWarnings=F)
  writeRaster(wind_r, wind_tif, overwrite=T)
}
wind = raster(wind_tif)

# plot depth ----

system.time({

  # aggregate: 0.6 sec fact=10, vs 6 sec without
  r_depth = aggregate(depth, fact=10)
  
  pal_depth = colorNumeric(
    palette = c('cyan3','blue','darkblue'),
    domain  = getValues(r_depth), na.color="#00000000")
  
  leaflet() %>%
    addProviderTiles('Stamen.TonerLite', group = 'B&W') %>%
    #addPolygons(data=eez_ter) %>%
    addRasterImage(
      r_depth, project=T,
      colors=pal_depth, opacity=0.7)
  
})

# TODO: constraints & uses ----


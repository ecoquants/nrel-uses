# by region, eg east
library(tidyverse)
library(raster)
library(sf)
library(leaflet)
library(RColorBrewer)
library(fasterize)
library(rgdal)
library(mapview)
select = dplyr::select

depth_w_grd = '/Volumes/Best HD/nrel_data_big/data/depth.grd' # wrapped and cropped
eez_w_geo   = '~/github/nrel-cables/data/usa_rgn_simplify05.geojson' # eez_usa_regions.geojson, usa_rgn.geojson
# TODO: use non-simplified EEZ

# prep territory depth ----
ter = 'West'

eez = read_sf(eez_w_geo) %>%
  filter(territory==ter) %>%
  mutate(geometry = (geometry + c(360,90)) %% c(-360) - c(0,-360+90)) %>%
  st_set_crs(leaflet:::epsg4326)
eez_sp = eez %>% as('Spatial')

depth = raster(depth_grd) %>%
  shift(-360) %>%
  crop(eez_sp) * -1
depth = mask(depth, eez_sp) # plot(depth)

# depth_0preagg = depth # depth = depth_0preagg
depth = aggregate(depth, fact=4)
depth = mask(depth, eez_sp)
# depth_1postagg = depth

depth_km2 = area(depth)
cell_km2 = c(minValue(depth_km2), maxValue(depth_km2))
cell_km2
sqrt(cell_km2) # cell width

writeRaster(depth, sprintf('./data/%s_depth.tif', ter))

# mpa ----

mpa_shp = '/Volumes/Best HD/nrel_data_big/marinecadastre.gov/MPA Inventory - MPAs by Government Level/mpa_inventory_2014_public_shp_dir/mpa_inventory_2014_public_shp.shp'

mpa = read_sf(mpa_shp) %>%
  st_intersection(eez) %>%
  mutate(
    type     = recode(
      State, 
      'California' = 'State MPA',
      'Oregon'     = 'State MPA',
      'Washington' = 'State MPA'),
    one      = 1,
    geometry = st_cast(geometry, 'MULTIPOLYGON'))

mpa_b = fasterize(mpa, depth, field='one', fun='first', by='type')

dir.create('./data/mpa')
for (lyr in names(mpa_b)){
  writeRaster(mpa_b[[lyr]], sprintf('./data/mpa/%s_%s.tif', ter, lyr))
}

# efh ----

# habitat areas of particular concern (HAPC)
# http://www.habitat.noaa.gov/protection/efh/newInv/index.html
# National > Information & GIS Data Download
# - `HAPC.gdb.zip`: Habitat Areas of Particular Concern (HAPC)

hapc_gdb = '/Volumes/Best HD/nrel_data_big/marinecadastre.gov/Essential Fish Habitat (EFH)/habitat-areas-of-particular-concern/NationalHAPC.gdb'

hapc = read_sf(hapc_gdb) %>%
  st_intersection(eez) %>%
  mutate(
    habitat = ifelse(Species == 'Groundfish', 'Groundfish', HAPC_Sitename),
    one = 1,
    Shape = st_cast(Shape, 'MULTIPOLYGON'))

hapc_b = fasterize(hapc, depth, field='one', fun='first', by='habitat')

dir.create('./data/efh')
for (lyr in names(hapc_b)){
  writeRaster(hapc_b[[lyr]], sprintf('./data/efh/%s_hapc_%s.tif', ter, lyr))
}

# ais ----
# vessel density

ais_gdb = '/Volumes/Best HD/nrel_data_big/marinecadastre.gov/2013 Vessel Density/VesselDensity2013_dir/VesselDensity2013.gdb'
ais_gdb = '/Volumes/Best HD/nrel_data_big/marinecadastre.gov/2013 Vessel Density/VesselDensity2013/VesselDensity2013.gdb'
ais_gdb = '/Volumes/Best HD/nrel_data_big/marinecadastre.gov/2013 Vessel Density/VesselDensity2013_b/VesselDensity2013.gdb'
ogrListLayers(ais_gdb)
ogrInfo(ais_gdb)
st_layers(ais_gdb)

ais = raster('/Users/bbest/Documents/VesselDensity2013_dir/vesseldensity2013.tif')

eez_ais = st_transform(eez_sp %>% st_as_sf(), proj4string(ais)) %>% as('Spatial')
ais = crop(ais, eez_ais)
ais = projectRaster(ais, depth)
ais = mask(ais, eez_sp) # plot(ais)
# ais0 = ais # ais = ais0
ais = ais/maxValue(ais)
ais[ais == 0] = NA # plot(ais)

dir.create('./data/ais')
writeRaster(ais, sprintf('./data/ais/%s_vesseldensity2013.tif', ter), overwrite=T)

# oil ----

oil_shp = '/Volumes/Best HD/nrel_data_big/marinecadastre.gov/Active Oil and Gas Leases/actlease_dir/ActiveLeases_APR2017.shp'
oil_shp = '/Volumes/Best HD/nrel_data_big/marinecadastre.gov/Active Oil and Gas Leases/pac_lease(3)_dir/BOEM_Pacific_Leases.shp'

oil = read_sf(oil_shp) %>%
  mutate(
    one = 1) %>% #,
  st_transform(leaflet:::epsg4326)
    #Shape = st_cast(Shape, 'MULTIPOLYGON'))

# mapview(oil) # 
#oil %>% st_set_geometry(NULL) %>% View()

oil_r = fasterize(oil, depth, field='one', fun='first') # plot(oil_r)

dir.create('./data/oil')
writeRaster(oil_r, sprintf('./data/oil/%s_active.tif', ter), overwrite=T)

# wind ----

wind_shp = '/Volumes/Best HD/nrel_data_big/nrel.gov/wind/pac/pacific_coast_90mwindspeed_off.shp'

wind = read_sf(wind_shp) # %>% #,
  #st_transform(leaflet:::epsg4326)

# mapview(wind) # wind

wind_r = fasterize(wind, depth, field='Speed_90', fun='first') # plot(wind_r)

dir.create('./data/wind')
writeRaster(wind_r, sprintf('./data/wind/%s_90mwindspeed.tif', ter), overwrite=T)


# plot depth ----

depth_mer = projectRasterForLeaflet(depth)

pal_depth = colorNumeric(
  palette = c('cyan3','blue','darkblue'),
  #palette = c('lightblue','blue','darkblue'),
  domain  = getValues(depth), na.color="#00000000")

leaflet() %>%
  addProviderTiles('Stamen.TonerLite', group = 'B&W') %>%
  #addPolygons(data=eez_ter) %>%
  addRasterImage(
    depth_mer, project=T,
    colors=pal_depth, opacity=0.7)
  
# 


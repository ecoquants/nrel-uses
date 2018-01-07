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

# layer polygon modification functions ----
aquaculture_explore <- function(){
  ply <- read_sf("/Volumes/Best HD/nrel_data_big/marinecadastre.gov/Aquaculture/Aquaculture_dir/Aquaculture.gdb") 
  names(ply)
  
  ply %>%
    st_set_geometry(NULL) %>%
    View()
  
  table(ply$type)
  # Finfish     Other Shellfish 
  #     107      1414      8433
  plot(ply["type"])
}

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
    cat("    For now defaulting to all EFH.\n")
    if (nrow(ply) > 0) ply$layer <- "EFH"
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
  if (length(ply_unaccounted_themelayer) > 0) stop(glue("MISSING themelayer in shippinglanes: {paste(ply_unaccounted_themelayer, collapse=', '}"))
  
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

explore_oceanuseatlases <- function(ply){
  
}


  

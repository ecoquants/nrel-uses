library(tidyverse)
library(glue)
library(sf)

# vars ----
pfx  <- "/Volumes/Best HD/nrel_data_big/marineprotectedareas.noaa.gov_ocean-use-atlases"
gdbs <- c(
  CA   = "CA_ocean_uses_atlas_March2010/CA_ocean_uses_atlas.gdb",
  NH   = "NewHampshireMaineOceanUses/NewHampshireMaineOceanUses.gdb",
  HI   = "Pacific-Regional-Ocean-Use-Atlas/OceanUsesHawaiiPROUA/OceanUsesHawaiiPROUA.gdb",
  ORWA = "Pacific-Regional-Ocean-Use-Atlas/OceanUsesOregonWashingtonPROUA/OceanUsesOregonWashingtonPROUA.gdb",
  ST   = "St. Thomas East End Reserve Coastal Use Mapping Project/STEER_coastal_use_mapping_2012_1130.gdb")
gdbs <- set_names(file.path(pfx, gdbs), names(gdbs))
gdb_lyrs_csv <- file.path(pfx, "gdb_layers.csv")
dir_lyrs <- here::here("data/layers")
  
# gdb layers summary ----
do_gdb_lyrs <- F
if (do_gdb_lyrs){
  tbl <- tibble(rgn = character(0), gdb = character(0), layer = character(0))
  for (i in 1:length(gdbs)){
    rgn <- names(gdbs)[i]
    gdb <- gdbs[i]
    lyrs <- st_layers(gdb) %>% .$name
    tbl_i <- tibble(
      rgn   = rgn,
      gdb   = str_replace(gdb, pfx,""),
      layer = lyrs)
    tbl <- bind_rows(tbl, tbl_i)
  }
  write_csv(tbl, gdb_lyrs_csv)
}

# CA many layers in gdb ----
# score: 1 = dominant, 0.5 = future|footprint
do_ca <- F
if (do_ca){
  gdb <- gdbs[["CA"]]
  lyrs <- st_layers(gdb) %>% .$name
  lyrs_tbl <- tibble(
    lyr = lyrs) %>%
    mutate(
     dominant = ifelse(
       str_detect(lyr, "_dominant$"), 
       str_replace(lyr, "_dominant$", ""),
       NA),
     footprint = ifelse(
       str_detect(lyr, "_footprint$"), 
       str_replace(lyr, "_footprint$", ""),
       NA),
     future = ifelse(
       str_detect(lyr, "_future$"), 
       str_replace(lyr, "_future$", ""),
       NA))  %>%
    gather(key, value, -lyr) %>%
    filter(!is.na(value)) %>%
    spread(key, lyr)
  # View(lyrs_tbl)
  
  dataset <- "oceanuseatlas.ca"
  dir_lyr_prep <- file.path(dir_lyrs, dataset, "prep")
  dir.create(dir_lyr_prep, showWarnings = F)
  dataset_geo <- glue("{dir_lyr_prep}/{dataset}.geojson")
  dataset_epsg26910_geo <- glue("{dir_lyr_prep}/{dataset}_epsg26910.geojson")
  for (i in 1:nrow(lyrs_tbl)){ # i <- 1
    lyr <- lyrs_tbl$value[i]
    cat(glue("{sprintf('%02d',i)} of {nrow(lyrs_tbl)}: {lyr}"),"\n")
    
    dom_sf <- read_sf(gdb, lyrs_tbl$dominant[i]) %>%
      mutate(score = 1) %>%
      select(score)
    oth_sf <- read_sf(gdb, lyrs_tbl$footprint[i]) %>%
      select()
    fut_lyr <- lyrs_tbl$future[i]
    if (!is.na(fut_lyr)){
      fut_sf <- read_sf(gdb, fut_lyr) %>%
        select()
      oth_sf <- st_union(oth_sf, fut_sf)
    }
    oth_sf <- oth_sf %>%
      mutate(score = 0.5) %>%
      select(score)
    dif_sf <- st_difference(oth_sf, dom_sf) %>%
      select(score) # plot(dif_sf["score"])
    lyr_new <- lyr %>%
      str_replace_all("_", " ") %>%
      str_to_title()
    cat(glue("  -> {lyr_new}"),"\n")
    fin_sf <- rbind(dom_sf, dif_sf) %>%
      mutate(layer = lyr_new) # plot(fin_sf["score"])
    
    if (i==1){
      lyrs_sf <- fin_sf
    } else {
      lyrs_sf <- rbind(lyrs_sf, fin_sf)
    }
  }

  idx_invalid <- which(!st_is_valid(lyrs_sf))
  if (length(idx_invalid) > 0){
    # 1: Ring Self-intersection at or near point 503986.52819999959 4149455.2171
    # lyrs_sf <- lwgeom::st_make_valid(lyrs_sf) # blows up RStudio
    lyrs_sf[idx_invalid,] <- st_buffer(lyrs_sf[idx_invalid,], dist=0)
  }
  lyrs_sf_gcs <- st_transform(lyrs_sf, 4326)
  idx_invalid <- which(!st_is_valid(lyrs_sf_gcs))
  if (length(idx_invalid) > 0){
    lyrs_sf_gcs[idx_invalid,] <- st_buffer(lyrs_sf_gcs[idx_invalid,], dist=0)
  }
  if (file.exists(dataset_geo)) unlink(dataset_geo)
  write_sf(lyrs_sf_gcs, dataset_geo)
}

# ORWA gdb
# *[island|state], *includes, *excludes, 
#gdb <- gdbs[["HI"]]
do_orwa <- F
if (do_orwa){
  #rgn <- "ORWA"; layer_name <- "OceanUsesOregonWashingtonPROUA"
  rgn <- "HI"; layer_name <- "OceanUsesHawaiiPROUA"
  
  gdb <- gdbs[[rgn]]
  dataset <- glue("oceanuseatlas.{str_to_lower(rgn)}")
  dir_lyr_prep <- file.path(dir_lyrs, dataset, "prep")
  dir.create(dir_lyr_prep, recursive=T, showWarnings = F)
  dataset_geo <- glue("{dir_lyr_prep}/{dataset}.geojson")
  
  st_layers(gdb)
  lyrs_sf <- st_read(gdb, layer_name) # dominant = 1, footprint = 2 use (not footprint|future)
  # lyrs_sf %>%
  #   st_set_geometry(NULL) %>%
  #   View()
  names(lyrs_sf)
  summary(lyrs_sf)
  # ORWA -- 4 columns: lyr, lyrExcludes, lyrIncludes, lyrOregon, lyrWashington

  if (rgn=="ORWA"){
    lyrs_rm <- c(
      "dominantUseSummary", "SHAPE", "SHAPE_Area", "SHAPE_Length",
      names(lyrs_sf)[str_detect(names(lyrs_sf), ".*(Excludes|Includes|Oregon|Washington)$")])
    lyrs_sel <- sort(setdiff(names(lyrs_sf), lyrs_rm))
  } else { # rgn=="HI"
    lyrs_sel <- sort(setdiff(names(lyrs_sf)[1:30], "dominantUseSummary"))
  }  
  
  lyrs_sf <- lyrs_sf %>%
    select_at(lyrs_sel)
  
  if (exists('new_lyrs_sf')) rm(new_lyrs_sf)
  for (i in 1:length(lyrs_sel)){ # lyr = names(lyrs_sf)[1]
    lyr     <- lyrs_sel[i]
    lyr_new <- snakecase::to_any_case(lyr, case="upper_camel", postprocess='.')
    cat(glue("{sprintf('%02d', i)} of {length(lyrs_sel)}: {lyr} -> {lyr_new}"), "\n")
    
    lyr_sf <- lyrs_sf %>%
      select(var=!!lyr) %>%
      filter(var > 0) %>%
      mutate(
        score <- var/2,
        layer <- lyr_new) %>%
      select(-var)
    
    if (i==1){
      new_lyrs_sf <- lyr_sf
    } else {
      new_lyrs_sf <- rbind(new_lyrs_sf, lyr_sf)
    }
  }

  idx_invalid <- which(!st_is_valid(new_lyrs_sf))
  if (length(idx_invalid) > 0){
    new_lyrs_sf[idx_invalid,] <- st_buffer(new_lyrs_sf[idx_invalid,], dist=0)
  }
  new_lyrs_sf_gcs <- st_transform(new_lyrs_sf, 4326)
  idx_invalid <- which(!st_is_valid(new_lyrs_sf_gcs))
  if (length(idx_invalid) > 0){
    new_lyrs_sf_gcs[idx_invalid,] <- st_buffer(new_lyrs_sf_gcs[idx_invalid,], dist=0)
  }
  if (file.exists(dataset_geo)) unlink(dataset_geo)
  write_sf(new_lyrs_sf_gcs, dataset_geo)
  
  # /Users/bbest/github/nrel-uses/data/layers/oceanuseatlas.orwa/prep/oceanuseatlas.orwa.geojson
}
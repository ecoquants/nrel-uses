setwd("~/github/nrel-uses/prep/data")
source("../prep_layer_functions.R")
devtools::load_all("~/github/nrelutils") # devtools::install_github("ecoquants/nrelutils"); library(nrelutils) 

eez_wgcs_sf     <- read_sf(eez_wgcs_geo)
eez_s05_wgcs_sf <- read_sf(eez_s05_wgcs_geo)
# ply_map(eez_s05_wgcs_sf, "territory") # plot(eez_s05_wgcs_sf["territory"])

# vars ----
params <- read_csv('~/github/nrel-uses/prep/prep_params.csv') %>%
  filter(!is.na(run))

# loop params ----
for (i in 1:nrow(params)){ # i <- 2
  p <- params[i,]
  if (!p$run) next
  cat(glue("{i}: {p$key}"),"\n")

  for (j in 1:length(eez_wgcs_sf$territory)){ # j <- 1
    ter <- eez_wgcs_sf$territory[j]
    cat(glue("  {j}: {ter}"),"\n")
    
    digest_txt <- glue("./{p$key}/{ter}_{p$key}_epsg4326.txt")
    if (!file.exists(digest_txt)){
      
      # get eez and depth
      ter_eez_wgcs_sf  <- get_ter_eez_wgcs_sf(ter)  # ply_map(ter_eez_wgcs_sf, "territory")
      ter_depth_wgcs_r <- get_ter_depth_wgcs_r(ter) # r_map(ter_depth_wgcs_r)
      
      # read path
      ply <- read_sf(p$path)
      
      # wrap and intersect with eez
      ply <- sf_wrap_intersection(ply, ter_eez_wgcs_sf)
      
      # modify as needed
      if (!is.na(p$mod_eval)){
        ply <- eval(parse(text=p$mod_eval))
      }
      # ply_map(ply)
      
      # convert to raster tifs
      ply_to_tifs(ply, ter_depth_wgcs_r, ter, p$key, field=p$field, by=p$by)
      # TODO: clip by reasonable depth > 0 & st_intersection(eez) 
    }
  }
}

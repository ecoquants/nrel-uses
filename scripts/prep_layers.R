source(here::here("prep/setup.R"))
# devtools::load_all("~/github/nrelutils") 

eez_wgcs_sf     <- read_sf(eez_wgcs_geo)
territories     <- eez_wgcs_sf$territory
#territories     <- "Great Lakes"

# TODO: cleanup this quick fix, genericize fxn for digest_txt with nrelutils::ply_to_tifs()
if (F){
  p <- list(key = "depth")
  for (ter in eez_wgcs_sf$territory){
    digest_txt <- glue("./{p$key}/{ter}_{p$key}_epsg4326.txt")
    tif        <- glue("{ter}_{p$key}_epsg4326.tif")
    write_lines(glue("depth:{tif}"), digest_txt)
  }
}

# loop layers ----
for (i in 1:nrow(lyr_params)){ # i <- 7
  lyr_p <- lyr_params[i,]
  lyr <- lyr_p$key
  #if (!lyr_p$run | lyr == "depth") next
  if (!lyr_p$run) next
  cat(glue("{i}: {lyr}"),"\n")

  lyr_info <- get_lyr_info(lyr)

  if (any(map_lgl(lyr_info$territories, is.null)) | lyr_p$redo){
    cat("  NAs found, loading lyr_ply\n")
    lyr_ply <- sf_lyr_ply(lyr_p)
  #   # TODO: if raster input
  }
  
  for (j in 1:length(territories)){ # j <- 1
    ter <- territories[j]
    cat(glue("  {j}: {ter}"),"\n")
    # devtools::load_all("~/github/nrelutils") 
    prep_lyr_ter(lyr_p, lyr_ply, ter)
  }
}

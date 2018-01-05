source(here::here("scripts/setup.R"))
devtools::load_all("~/github/nrelutils") 

eez_wgcs_sf     <- read_sf(eez_wgcs_geo)
territories     <- eez_wgcs_sf$territory
#territories     <- "Great Lakes"

# loop layers ----
for (i in 1:nrow(lyr_params)){ # i <- 8
  lyr_p <- lyr_params[i,]
  lyr <- lyr_p$key
  
  if (!lyr_p$run) next
  cat(glue("{i}: {lyr}"),"\n")

  lyr_info <- get_lyr_info(lyr)

  if (any(map_lgl(lyr_info$territories, is.null)) | lyr_p$redo){
    cat("  territories with NULLs found, loading lyr_ply\n")
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

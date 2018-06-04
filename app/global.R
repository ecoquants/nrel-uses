# load packages
library(tidyverse)
library(stringr)
library(rgdal)
library(raster)
library(sp)
library(leaflet)
library(shiny)
library(shinydashboard)
library(here)
select = dplyr::select

# debug ----
# https://shiny.rstudio.com/reference/shiny/latest/shiny-options.html
# defaults:
options(
  shiny.sanitize.errors = F, shiny.deprecation.messages=F, 
  shiny.autoreload=F,
  shiny.fullstacktrace=F, shiny.stacktraceoffset=F,
  shiny.trace=F, shiny.testmode=F, shiny.minified=T,
  shiny.reactlog=F)
# debug:
# options(
#   shiny.sanitize.errors = F, shiny.deprecation.messages=T, 
#   shiny.autoreload=T, shiny.autoreload.pattern = glob2rx("*.R"),
#   shiny.fullstacktrace=T, shiny.stacktraceoffset=T,
#   shiny.trace=T, shiny.testmode=F, shiny.minified=T,
#   shiny.reactlog=F)

msg = function(txt, debug=F){
  if (debug)
      cat(sprintf('%s ~ %s\n', txt, Sys.time()), file=stderr())
}

# paths ----
# dir_wd = 'app'; if (basename(getwd())!=dir_wd) setwd(dir_wd)

dir_data <- switch(
  here(),
  # BB's Mac development environments
  "/Users/bbest/github/nrel-uses" = "/Users/bbest/github/nrel-uses/data/layers",
  "/Users/bbest/docker-nrel/github/nrel-uses" = "/Users/bbest/docker-nrel/data",
  # NREL server environment
  "/data")
constraints_grd <- file.path(dir_data, "constraints_epsg3857_2km.grd")
uses_grd        <- file.path(dir_data, "uses_epsg3857_2km.grd")
depth_gcs_grd   <- file.path(dir_data, "depth_epsg4326.grd")

ter = 'West'

# project
# raster(file.path(dir_data, 'depth/West_depth_epsg4326.tif')) %>%
#   projectRaster(crs=leaflet:::epsg3857, res=2000, method='bilinear') %>%
#   writeRaster(file.path(dir_data, 'depth/West_depth_epsg3857_2km.tif'))

list_constraints      = list(
  `Depth`             = c(
    `Depth (m)`         = 'depth/West_depth_epsg3857_2km.tif'),
  `Energy`            = c(
    `Wind (m/s)`      = 'wind/West_90mwindspeed_epsg3857_2km.tif'),
  `Competing Uses`        = c(
    `Competing Uses Score` = 'score'))

list_uses = list(
  `Infrastructure`            = c(
    `Vessel Traffic (2013)`   = 'ais/West_vesseldensity2013_epsg3857_2km.tif'), 
  `Legal`                     = c(
    `Active Oil & Gas Leases` = 'oil/West_active_epsg3857_2km.tif'), 
  `Environmental`             = c(
    `EFH HAPC - Estuaries`    = 'efh/West_hapc_Estuaries_epsg3857_2km.tif',
    `EFH HAPC - Groundfish`   = 'efh/West_hapc_Groundfish_epsg3857_2km.tif',
    `EFH HAPC - Kelp`         = 'efh/West_hapc_Canopy.Kelp_epsg3857_2km.tif',
    `EFH HAPC - Rocky Reefs`  = 'efh/West_hapc_Rocky.Reefs_epsg3857_2km.tif',
    `EFH HAPC - Seagrass`     = 'efh/West_hapc_Seagrass_epsg3857_2km.tif',
    `MPA - BOEM`                                = 'mpa/West_Bureau.of.Ocean.Energy.Management_epsg3857_2km.tif',
    `MPA - Marine National Monuments`           = 'mpa/West_Marine.National.Monuments_epsg3857_2km.tif',
    `MPA - National Estuarine Research Reserve` = 'mpa/West_National.Estuarine.Research.Reserve.System_epsg3857_2km.tif',
    `MPA - National Marine Fisheries Service`   = 'mpa/West_National.Marine.Fisheries.Service_epsg3857_2km.tif',
    `MPA - National Marine Sanctuaries`         = 'mpa/West_National.Marine.Sanctuaries_epsg3857_2km.tif',
    `MPA - National Park Service`               = 'mpa/West_National.Park.Service_epsg3857_2km.tif',
    `MPA - National Wildlife Refuge`            = 'mpa/West_National.Wildlife.Refuge.System_epsg3857_2km.tif',
    `MPA - State MPA`                           = 'mpa/West_State.MPA_epsg3857_2km.tif'))

list_layers = c(list_constraints, list_uses)
n_uses = length(unlist(list_uses))
max_weight = 1

#msg(sprintf('pryr::mem_used(): %0.2f MB', pryr::mem_used() / (1000*1000)))

if (F){
  # 1st: create from tifs
  #setwd('~/github/nrel-uses/app')
  s_constraints = stack(file.path(dir_data, unlist(list_constraints))[1:2])
  s_uses        = stack(file.path(dir_data, unlist(list_uses)))
  #s_constraints = projectRaster(s_constraints, crs=leaflet:::epsg3857)
  #s_uses        = projectRaster(s_uses, crs=leaflet:::epsg3857)
  writeRaster(s_constraints, constraints_grd, overwrite=T)
  writeRaster(s_uses, uses_grd, overwrite=T)
  
  # 2nd: as raster stacks
  s_constraints = stack(constraints_grd)
  s_uses        = stack(uses_grd)

  # 3rd: as serialized R objects
  write_rds(s_constraints, file.path(dir_data, "s_constraints.rds"))
  write_rds(s_uses, file.path(dir_data, "s_uses.rds"))
  
  projectRaster(raster(s_constraints, 1), crs=leaflet:::epsg4326) %>%
    writeRaster(depth_gcs_grd)
}

# s_constraints <- read_rds(file.path(dir_data, "s_constraints.rds"))
# s_uses        <- read_rds(file.path(dir_data, "s_uses.rds"))
s_constraints <- stack(constraints_grd)
s_uses        <- stack(uses_grd)
s_layers      <- stack(s_constraints, s_uses)

r_depth_gcs <- raster(depth_gcs_grd)
r_depth     <- raster(s_constraints, 1)
bb          <- bbox(r_depth_gcs)

# for (i in n_layers(list_uses)){ # i=1
#   r = s_uses[[i]]
#   cat(sprintf('%02d: %s %g to %g\n', i, names(s_uses)[i], minValue(r), maxValue(r)))
#   print(r)
# }

# pal_depth = colorNumeric(
#   palette = c('cyan3','blue','darkblue'),
#   domain  = getValues(depth), na.color="#00000000")

score = sum(s_uses * max_weight, na.rm=T)
max_score = maxValue(score)
aggregate_factor = 1

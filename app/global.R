# load packages
library(tidyverse)
library(stringr)
library(raster)
library(leaflet)
library(shiny)
library(shinydashboard)
select = dplyr::select

# debug ----
# https://shiny.rstudio.com/reference/shiny/latest/shiny-options.html
options(
  shiny.sanitize.errors = F, shiny.autoreload=F,
  shiny.fullstacktrace=F, shiny.stacktraceoffset=T,
  shiny.trace=F, shiny.testmode=F, shiny.minified=T,
  shiny.deprecation.messages=T,
  shiny.reactlog=F)

msg = function(txt, debug=F){
  if (debug)
    cat(sprintf('%s ~ %s\n', txt, Sys.time()), file=stderr())
}

# paths ----
# dir_wd = 'app'; if (basename(getwd())!=dir_wd) setwd(dir_wd)
constraints_grd = 'data/constraints_epsg3857.grd'
uses_grd        = 'data/uses_epsg3857.grd'

list_constraints      = list(
  `Depth`             = c(
    `Depth_m`         = 'depth/West_depth.tif'),
  `Energy`            = c(
    `Wind (m/s)`      = 'wind/West_90mwindspeed.tif'),
  `Ocean Uses`        = c(
    `Ocean Use Score` = 'score'))

list_uses = list(
  `Infrastructure`            = c(
    `Vessel Traffic (2013)`   = 'ais/West_vesseldensity2013.tif'), 
  `Legal`                     = c(
    `Active Oil & Gas Leases` = 'oil/West_active.tif'), 
  `Environmental`             = c(
    `EFH HAPC - Estuaries`    = 'efh/West_hapc_Estuaries.tif',
    `EFH HAPC - Groundfish`   = 'efh/West_hapc_Groundfish.tif',
    `EFH HAPC - Kelp`         = 'efh/West_hapc_Canopy.Kelp.tif',
    `EFH HAPC - Rocky Reefs`  = 'efh/West_hapc_Rocky.Reefs.tif',
    `EFH HAPC - Seagrass`     = 'efh/West_hapc_Seagrass.tif',
    `MPA - BOEM`                                = 'mpa/West_Bureau.of.Ocean.Energy.Management.tif',
    `MPA - Marine National Monuments`           = 'mpa/West_Marine.National.Monuments.tif',
    `MPA - National Estuarine Research Reserve` = 'mpa/West_National.Estuarine.Research.Reserve.System.tif',
    `MPA - National Marine Fisheries Service`   = 'mpa/West_National.Marine.Fisheries.Service.tif',
    `MPA - National Marine Sanctuaries`         = 'mpa/West_National.Marine.Sanctuaries.tif',
    `MPA - National Park Service`               = 'mpa/West_National.Park.Service.tif',
    `MPA - National Wildlife Refuge`            = 'mpa/West_National.Wildlife.Refuge.System.tif',
    `MPA - State MPA`                           = 'mpa/West_State.MPA.tif'))

list_layers = c(list_constraints, list_uses)
n_uses = length(unlist(list_uses))
max_weight = 10

if (F){
  s_constraints = stack(sprintf('data/%s', unlist(list_constraints))[1:2])
  s_uses        = stack(sprintf('data/%s', unlist(list_uses)))
  s_constraints = projectRaster(s_constraints, crs=leaflet:::epsg3857)
  s_uses        = projectRaster(s_uses, crs=leaflet:::epsg3857)
  writeRaster(s_constraints, constraints_grd, overwrite=T)
  writeRaster(s_uses, uses_grd, overwrite=T)
}
s_constraints = stack(constraints_grd)
s_uses        = stack(uses_grd)
s_layers      = stack(s_constraints, s_uses)
  
# for (i in n_layers(list_uses)){ # i=1
#   r = s_uses[[i]]
#   cat(sprintf('%02d: %s %g to %g\n', i, names(s_uses)[i], minValue(r), maxValue(r)))
#   print(r)
# }

# pal_depth = colorNumeric(
#   palette = c('cyan3','blue','darkblue'),
#   domain  = getValues(depth), na.color="#00000000")

score = sum(s_uses*max_weight, na.rm=T)
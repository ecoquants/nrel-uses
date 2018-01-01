# libraries ----
library(tidyverse)
library(yaml)
library(glue)
library(stringr)
library(rgdal)
library(sf)
library(ncdf4)
library(raster)
library(fasterize)
library(leaflet) 
library(RColorBrewer)
library(knitr)
library(rmarkdown)
library(htmltools)
library(DT)
library(here)
select <- dplyr::select
devtools::load_all(here("../nrelutils")) # devtools::install_github("ecoquants/nrelutils"); library(nrelutils) 

# paths ----
dir_data            <- here("data")
dir_prep            <- here("prep")
dir_prep_data       <- here("prep/data")
depth_nc            <- "~/github/obis-lat-time-fig/data/GEBCO_2014_2D.nc"
eez_wgcs_geo        <- here("prep/data/eez/usa_ter_wgcs.geojson")
eez_gcs_geo         <- here("prep/data/eez/usa_ter_gcs.geojson")
eez_s05_wgcs_geo    <- here("prep/data/eez/usa_ter_simplify05_wgcs.geojson")
eez_s05_gcs_geo     <- here("prep/data/eez/usa_ter_simplify05_gcs.geojson")
lyr_params_csv      <- here("prep/layer_params.csv")
lyr_categories_xlsx <- here("data/categories_and_datasets.xlsx")

# variables ----
ter_atl_islands  <- c("Puerto Rico", "US Virgin Islands")
ter_pac_islands  <- c("Guam", "Johnston Atoll", "N Mariana Islands", "Palmyra Atoll", "Wake Island")
ter_pac_plus     <- c("American Samoa", "Jarvis Island", "Howland and Baker islands")

# data ----
lyr_categories  <- readxl::read_xlsx(lyr_categories_xlsx, sheet="datasets")
lyr_params      <- suppressMessages(readr::read_csv(lyr_params_csv)) %>%
  filter(!is.na(key))
limits          <- nrelutils::nrel_limits
eez_s05_wgcs_sf <- read_sf(eez_s05_wgcs_geo)

# extra functions ----
source(here("prep/layer_functions.R"))

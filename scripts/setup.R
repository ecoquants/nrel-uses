# libraries ----
if (!require("pacman")) install.packages("pacman")
library(pacman)   # for p_load() to install package if not present vs just library()
p_load(tidyverse) # ggplot2,tibble,tidyr,readr,purrr,dplyr,stringr,forcats
p_load(yaml, glue, here)                                   # text
p_load(rgdal, sf, ncdf4, raster, velox, leaflet, lwgeom)  # spatial
p_load(knitr, rmarkdown, htmltools, DT, htmltools)         # reporting
p_load(RColorBrewer)                                       # graphics
p_load_gh("ecohealthalliance/fasterize")                   # raster
#p_update()
p_load_gh("ecoquants/nrelutils")                           # custom
# devtools::load_all(here("../nrelutils"))
select <- dplyr::select

# paths ----
dir_data            <- here("data")
dir_lyrs            <- here("data/layers")
dir_ters            <- here("data/territories")
dir_scripts         <- here("scripts")
depth_nc            <- "~/github/obis-lat-time-fig/data/GEBCO_2014_2D.nc"
eez_wgcs_geo        <- file.path(dir_lyrs, "eez/usa_ter_wgcs.geojson")
eez_gcs_geo         <- file.path(dir_lyrs, "eez/usa_ter_gcs.geojson")
eez_s05_wgcs_geo    <- file.path(dir_lyrs, "eez/usa_ter_simplify05_wgcs.geojson")
eez_s05_gcs_geo     <- file.path(dir_lyrs, "eez/usa_ter_simplify05_gcs.geojson")
lyr_params_csv      <- file.path(dir_data, "layer_params.csv")
lyr_categories_xlsx <- file.path(dir_data, "categories_and_datasets.xlsx")
lyr_fxns_r          <- file.path(dir_scripts, "layer_functions.R")
lyr_res_areas_csv   <- file.path(dir_data, "layer_resource_areas.csv")

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
source(lyr_fxns_r)

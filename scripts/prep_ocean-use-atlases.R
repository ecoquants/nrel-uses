library(tidyverse)
library(sf)

# vars ----
gdbs         <- c(
  CA   = "/Volumes/Best HD/nrel_data_big/marineprotectedareas.noaa.gov_ocean-use-atlases/CA_ocean_uses_atlas_March2010/CA_ocean_uses_atlas.gdb",
  NH   = "/Volumes/Best HD/nrel_data_big/marineprotectedareas.noaa.gov_ocean-use-atlases/NewHampshireMaineOceanUses/NewHampshireMaineOceanUses.gdb",
  HI   = "/Volumes/Best HD/nrel_data_big/marineprotectedareas.noaa.gov_ocean-use-atlases/Pacific-Regional-Ocean-Use-Atlas/OceanUsesHawaiiPROUA/OceanUsesHawaiiPROUA.gdb",
  ORWA = "/Volumes/Best HD/nrel_data_big/marineprotectedareas.noaa.gov_ocean-use-atlases/Pacific-Regional-Ocean-Use-Atlas/OceanUsesOregonWashingtonPROUA/OceanUsesOregonWashingtonPROUA.gdb",
  ST   = "/Volumes/Best HD/nrel_data_big/marineprotectedareas.noaa.gov_ocean-use-atlases/St. Thomas East End Reserve Coastal Use Mapping Project/STEER_coastal_use_mapping_2012_1130.gdb")
pfx          <- "/Volumes/Best HD/nrel_data_big/marineprotectedareas.noaa.gov_ocean-use-atlases/"
gdb_lyrs_csv <- file.path(pfx, "gdb_layers.csv")

# gdb layers summary ----
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

# CA many layers in gdb
gdb <- gdbs[["CA"]]
st_layers(gdb)



dplyr::mutate_all()

# *[island|state], *includes, *excludes, 
#gdb <- gdbs[["HI"]]
gdb <- gdbs[["ORWA"]]
rgdal::ogrListLayers(gdb)
#sf <- st_read(gdb, "OceanUsesHawaiiPROUA")
sf <- st_read(gdb, "OceanUsesOregonWashingtonPROUA")
tbl <- sf %>% 
  st_set_geometry(NULL) %>%
  as.tibble() %>%
  gather(layer, value) %>%
  group_by(layer) %>%
  summarize(
    val_min = min(value, na.rm=T),
    val_max = min(value, na.rm=T))

View(tbl)

head() %>% View()
apply(hi_lyrs, )
dplyr::row

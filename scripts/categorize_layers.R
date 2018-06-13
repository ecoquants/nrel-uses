library(tidyverse)
library(yaml)
library(here)
library(glue)

dir_layers     <- here("data/layers") 
lyr_cats_csv   <- here("data/layer_categories.csv") 
yml_exceptions <- c()

ymls <- list.files(dir_layers, ".*\\.yml$", recursive = T, full.names = T)

get_yml_components <- function(yml){
  # yml <- ymls[1] # _aquaculture.yml
  # yml <- ymls[4] # _mpa.yml
  
  read_yaml(yml) %>%
    .$territories %>%
    keep(
      function(x) "components" %in% names(x)) %>%
    map_df(
      function(x) tibble(components = x$components %>% names()), 
      .id = "territory") %>%
    distinct(components) %>%
    pull(components)
}

lyrs <- tibble(
  yml=ymls) %>%
  mutate(
    dataset = yml %>% dirname() %>% basename(),
    layer   = map(ymls, get_yml_components)) %>%
  unnest(layer) %>%
  select(-yml)

if (!file.exists(lyr_cats_csv)){
  lyr_cats <- lyrs %>%
    mutate(
      category = "")
  write_csv(lyr_cats, lyr_cats_csv)
} else {
  lyr_cats <- read_csv(lyr_cats_csv)
  
  miss_layers <- anti_join(lyrs, lyr_cats, by=c("dataset","layer"))
  miss_layers
  lyr_cats <- bind_rows(lyr_cats, miss_layers)
  
  write_csv(lyr_cats, lyr_cats_csv)
}

miss_cats <- lyr_cats %>%
  group_by(dataset) %>%
  summarise(
    n_layers = n())

miss_cats
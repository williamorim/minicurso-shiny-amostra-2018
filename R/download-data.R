# Download and format the data

library(tidyverse)
library(rvest)
library(xml2)

path <- function(x) {
  paste0(
    "https://raw.githubusercontent.com/phalt/pokeapi/master/data/v2/csv/", 
    x
  )
}

df_pkmn <- read_csv(path("pokemon.csv")) %>% 
  select(-order, -is_default) %>% 
  rename(pokemon = identifier)

df_stats <- read_csv(path("stats.csv")) %>% 
  rename(stat_id = id) %>% 
  right_join(
    read_csv(path("pokemon_stats.csv")),
    by = "stat_id"
  ) %>% 
  mutate(identifier = str_replace(identifier, "-", "_")) %>% 
  select(pokemon_id, identifier, base_stat) %>% 
  spread(identifier, base_stat) %>% 
  rename(id = pokemon_id)

df_type <- read_csv(path("types.csv")) %>% 
  rename(type_id = id) %>% 
  right_join(read_csv(path("pokemon_types.csv")), by = "type_id") %>% 
  select(pokemon_id, identifier, slot) %>% 
  mutate(slot = paste0("type_", slot)) %>% 
  spread(slot, identifier) %>% 
  rename(id = pokemon_id)

df_egg <- read_csv(path("egg_groups.csv")) %>% 
  rename(egg_group_id = id) %>% 
  right_join(read_csv(path("pokemon_egg_groups.csv")), by = "egg_group_id") %>% 
  group_by(species_id) %>% 
  mutate(ranking = row_number(),
         ranking = paste0("egg_group_", ranking)) %>% 
  select(species_id, ranking, identifier) %>% 
  spread(ranking, identifier) 

df_img <- 
  "https://github.com/phalt/pokeapi/tree/master/data/Pokemon_XY_Sprites" %>% 
  read_html() %>% 
  html_nodes("tr.js-navigation-item > .content > .css-truncate a") %>% 
  map_df(function(x) {
    url <- x %>% html_attr("href")
    data_frame(
      id = str_extract(basename(url), "\\d+"),
      url_image = basename(url)
    )
  }) %>%
  mutate(id = as.numeric(id))

url_bulbapedia_list <- 
  "http://bulbapedia.bulbagarden.net/wiki/List_of_Pok%C3%A9mon_by_base_stats_(Generation_VI-present)" 

id <- url_bulbapedia_list %>% 
  read_html(encoding = "UTF-8") %>% 
  html_node("table.sortable") %>% 
  html_table() %>% 
  .[[1]] %>% 
  as.numeric()

url_icon <-  url_bulbapedia_list %>% 
  read_html() %>%
  html_nodes("table.sortable img") %>% 
  html_attr("src")

df_icon <- data_frame(id, url_icon) %>% 
  filter(!is.na(id)) %>% 
  distinct(id)

df_color <- map_df(
  na.omit(unique(c(df_type$type_1, df_type$type_2))), 
  function(t) {
    # t <- "bug"
    col <- "http://pokemon-uranium.wikia.com/wiki/Template:%s_color" %>% 
      sprintf(t) %>%
      read_html() %>% 
      html_nodes("span > b") %>% 
      html_text()
    data_frame(type = t, color = paste0("#", col))
  }
)

df_color_f <- 
  expand.grid(
    color_1 = df_color$color, 
    color_2 = df_color$color,
    stringsAsFactors = FALSE
  ) %>% 
  tbl_df() %>% 
  group_by(color_1, color_2) %>% 
  do({
    n = 100; p = 0.25
    data_frame(
      color_f = colorRampPalette(c(.$color_1, .$color_2))(n)[round(n*p)]
    )
  })

# THE join
df <- df_pkmn %>% 
  left_join(df_type, by = "id") %>% 
  left_join(df_stats, by = "id") %>% 
  left_join(rename(df_color, type_1 = type, color_1 = color), by = "type_1") %>% 
  left_join(rename(df_color, type_2 = type, color_2 = color), by = "type_2") %>% 
  left_join(df_color_f, by =  c("color_1", "color_2")) %>% 
  left_join(df_egg, by = "species_id") %>% 
  left_join(df_img, by = "id") %>% 
  left_join(df_icon, by = "id")

write_rds(df, path = "data-raw/df_pkmn.rds")
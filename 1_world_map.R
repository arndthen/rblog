# Post 1: World Map Visualization in ggplot2
# Henriette L. Arndt, December 7 2022
# https://www.henriettearndt.com/blog

library(tidyverse)
library(ggthemes)
library(viridis)

## Geographical data --------------------------------------
world_map <- map_data("world") %>%
  # remove Antarctica
  filter(! region == "Antarctica") %>%
  # recode Hong Kong as region
  mutate(region = 
           case_when(
             # if subregion is HK, change region
             subregion == "Hong Kong" ~ "Hong Kong",
             # else retain original string
             TRUE ~ region
           )
  )


## Data by country ----------------------------------------
# List of countries
countries <- world_map %>%
  distinct(region)

# Data by country
data <- tibble(
  region = 
    c("Armenia", "Australia", "Austria", "Belgium", 
      "Brazil", "Chile", "China", "Croatia", "Denmark", 
      "Finland", "France", "Germany", "Greece", 
      "Hong Kong", "Indonesia", "Iran", "Japan", 
      "Malaysia", "Mexico", "Morocco", "Netherlands", 
      "Norway", "Poland", "Russia", "Saudi Arabia", 
      "Slovakia", "Slovenia", "South Korea", "Spain", 
      "Sweden", "Taiwan", "Thailand", "Turkey", "UK", 
      "USA", "Vietnam"),
  study.count =
    c(1, 6, 3, 7, 2, 2, 3, 3, 3, 10, 19, 5, 4, 17,
      5, 3, 6,4, 2, 1, 4, 3, 2, 2, 6, 1, 2,
      13, 9, 14, 3, 1, 2, 4, 7, 1)
  )

# Join
data <- full_join(countries, data, .by = region)


## Plot ---------------------------------------------------
data %>% 
  ggplot(aes(
    # variable to visualize
    fill = study.count, 
    map_id = region)) +
  geom_map(map = world_map) +
  expand_limits(x = world_map$long, y = world_map$lat) +
  theme_map() +
  # legend position
  theme(legend.position = c(0.1, 0.3)) +
  # colour-blind friendly palette
  scale_fill_viridis(option = "plasma", direction = -1)

# Save plot as .png
ggsave("1_mapprojection.png",
       # image size
       width = 10,
       # background colour
       bg='#ffffff')

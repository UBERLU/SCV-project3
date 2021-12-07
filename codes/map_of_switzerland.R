pacman::p_load(tidyverse, pacman, ggmap, janitor, sf, mapview, leaflet, rgdal, RColorBrewer, 
               lubridate, boot, broom)



library(tidyverse)
library(sf)
library(rcartocolor)
library(readxl)
library(plotly)


swiss_cantons <- st_read("resources/G1K09.shp")
head(swiss_cantons)



swiss_cantons <- swiss_cantons %>% 
  left_join(total_by_canton_2011, c("KURZ" = "Canton"))


ggplot()+
  geom_sf(data = swiss_cantons, aes(fill = total), size = 0.3) + 
  theme_void() + 
  ggrepel::geom_label_repel(
    data = swiss_cantons,
    aes(label = paste0(KURZ,":",round(total, digits = 0)), 
        geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0.2,
    colour = "#541f3f",
    size = 3,
    segment.alpha = 0.5
  ) +
  scale_fill_continuous(low="white", high="#2166AC", limits=c(0,8000)) +
  labs(title = "Total Naturalisation by Canton in 2011") +
  theme(legend.title = element_blank())


population_canton <- read_csv("population.csv")

swiss_cantons <- swiss_cantons %>% 
  left_join(population_canton, c("KURZ" = "Canton"))


swiss_cantons <- swiss_cantons %>%
  mutate(relative_nat = total/(Population_in_1000.x))


ggplot()+
  geom_sf(data = swiss_cantons, aes(fill = relative_nat), size = 0.3) + 
  theme_void() + 
  ggrepel::geom_label_repel(
    data = swiss_cantons,
    aes(label = paste0(KURZ,":",round(relative_nat, digits = 3)), 
        geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0.2,
    colour = "#541f3f",
    size = 3,
    segment.alpha = 0.5
  ) +
  scale_fill_continuous(low="white", high="#2166AC", limits=c(0,10)) +
  labs(title = "TRelative Naturalisation (devided by the number of citizens in canton) by Canton in 2011") +
  theme(legend.title = element_blank())




pacman::p_load(tidyverse, pacman, ggmap, janitor, sf, mapview, leaflet, rgdal, RColorBrewer, 
               lubridate, boot, broom)
library(tidyverse)
library(sf)
library(rcartocolor)
library(readxl)
library(plotly)

# Load the whole data containing
load("my_data_French.RData")

new_age <- my_data %>%
  group_by(Year, Age, Genre, Type.of.acquisition) %>%
  summarise(Value = sum(Value))

new_age_2011 <- subset(new_age, Year == 2011 )

new_age_2011 <- new_age_2011 %>%
  mutate(Value = ifelse(Genre == "Female",
                        Value, -Value))

new_age_2011 <- new_age_2011 %>% 
  mutate(category_age=cut(Age, breaks=c(-Inf, 9, 19, 29, 39, 49, 59, 69, 79, Inf), 
                          labels=c("0 - 9","10 - 19","20 - 29","30 - 39","40 - 49","50 - 59", "60 - 69", "70 - 79", "80 +")))



new_age_2011 <- new_age_2011 %>%
  select(Genre, category_age, Value, Type.of.acquisition) %>%
  mutate(Genre = as.factor(Genre), Cat_age = as.factor((category_age)),
         Type.of.acquisition = as.factor(Type.of.acquisition)) %>%
  group_by(Genre, category_age, Type.of.acquisition) %>%
  summarise(Value = sum(Value))


library(ggplot2)
library(gganimate)

p <- ggplot(new_age_2011,
            aes(x = category_age, y = Value,
                fill = interaction(Genre,Type.of.acquisition))) +
  geom_col() +
  theme_bw() +
  theme(#legend.position = c(0.7,0.7),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "Age", y = "Count", fill = "Gender",
       title = "Confirmation of the Swiss nationality") +
  scale_y_continuous(breaks = seq(-10000, 10000, by = 1000), labels = abs) +
  coord_flip() 
  
p
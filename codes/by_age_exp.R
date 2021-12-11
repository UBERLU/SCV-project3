pacman::p_load(tidyverse, pacman, ggmap, janitor, sf, mapview, leaflet, rgdal, RColorBrewer, 
               lubridate, boot, broom)
library(tidyverse)
library(sf)
library(rcartocolor)
library(readxl)
library(plotly)

# Load the whole data containing
load("/Users/darynabilodid/Documents/GitHub/SCV-project3/codes/my_data_French.RData")

new_age <- my_data %>%
  group_by(Year, Age, Genre, Type.of.acquisition) %>%
  summarise(total = sum(Value))

new_age_2011 <- subset(new_age, Year == 2011 )

new_age_2011 <- new_age_2011 %>%
  mutate(total = ifelse(Genre == "Female",
                        total, -total))

new_age_2011 <- new_age_2011 %>% 
  mutate(category_age=cut(Age, breaks=c(-Inf, 9, 19, 29, 39, 49, 59, 69, 79, Inf), 
                          labels=c("0 - 9","10 - 19","20 - 29","30 - 39","40 - 49","50 - 59", "60 - 69", "70 - 79", "80 +")))



new_age_2011 <- new_age_2011 %>%
  select(Genre, category_age, Value) %>%
  mutate(Genre = as.factor(Genre), Cat_age = as.factor((category_age))) %>%
  group_by(Genre, category_age) %>%
  summarise(Value = sum(Value))


library(ggplot2)
library(gganimate)

p <- ggplot(new_age_2011,
            aes(x = category_age, y = total,
                fill = Genre)) +
  geom_col() +
  theme_bw() +
  theme(legend.position = c(0.7,0.7),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "Age", y = "Count", fill = "Gender",
       title = "Confirmation of the Swiss nationality") +
  scale_y_continuous(breaks = seq(-10000, 10000, by = 1000), labels = abs) +
  coord_flip() 
  
p






### ANIMATED
new_age1 <- new_age %>%
  mutate(total = ifelse(Genre == "Female",
                        total, -total))


new_age1 <- new_age1 %>% 
  mutate(category_age=cut(Age, breaks=c(-Inf, 9, 19, 29, 39, 49, 59, 69, 79, Inf), 
                          labels=c("0 - 9","10 - 19","20 - 29","30 - 39","40 - 49","50 - 59", "60 - 69", "70 - 79", "80 +")))


new_age1 <- new_age1 %>%
  select(Year, Genre, category_age, total) %>%
  mutate(Genre = as.factor(Genre), Year = as.integer(Year), category_age = as.factor((category_age))) %>%
  group_by(Year, Genre, category_age) %>%
  summarise(Value = sum(total))

new_age1<-new_age1[!(new_age1$Genre=="Sex - total"),]



ggplot(new_age1,
            aes(x = category_age, y = Value,
                fill = Genre, )) +
  geom_col() +
  theme_bw() +
  theme(legend.position = c(0.7,0.7),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "Age", y = "Count", fill = "Gender",
       title = "Confirmation of the Swiss nationality") +
  scale_y_continuous(breaks = seq(-10000, 10000, by = 1000), labels = abs) +
  coord_flip() +
  transition_time(as.integer(Year)) +
  labs(title = 'Year: {frame_time}', x = 'Naturalisations per year', y = 'Age category') +
  ease_aes('linear') 




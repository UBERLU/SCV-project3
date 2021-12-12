# Load the required packages
pacman::p_load(tidyverse, pacman, ggmap, janitor, sf, mapview, leaflet, rgdal, RColorBrewer, 
               lubridate, boot, broom)
library(tidyverse)
library(sf)
library(rcartocolor)
library(readxl)
library(plotly)

# Load the whole data containing
load("/Users/darynabilodid/Documents/GitHub/SCV-project3/codes/my_data_French.RData")

### 2 additional map plots

data_ordinary  = subset(my_data, Type.of.acquisition == "Ordinary naturalisation")
data_simplified  = subset(my_data, Type.of.acquisition == "Simplified naturalisation")

data_ordinary_map <- data_ordinary %>%
  group_by(Canton) %>%
  summarise(total_13 = sum(Value))


data_simplified_map <- data_simplified %>%
  group_by(Canton) %>%
  summarise(total_13 = sum(Value))


swiss_cantons <- st_read("~/Documents/GitHub/SCV-project3/data/for map plot/G1K09.shp")



levels(data_ordinary_map$Canton)  <- c("CH", 
                          "ZH", "BE", "LU", "UR", "SZ", "OW", 
                          "NW", "GL", "ZG", "FR", "SO", "BS", 
                          "BL", "SH", "AR", "AI", "SG", "GR", 
                          "AG", "TG", "TI", "VD", "VS", "NE", 
                          "GE", "JU", 
                          "NA")

levels(data_simplified_map$Canton)  <- c("CH", 
                                       "ZH", "BE", "LU", "UR", "SZ", "OW", 
                                       "NW", "GL", "ZG", "FR", "SO", "BS", 
                                       "BL", "SH", "AR", "AI", "SG", "GR", 
                                       "AG", "TG", "TI", "VD", "VS", "NE", 
                                       "GE", "JU", 
                                       "NA")

data_ordinary_map <- swiss_cantons %>% 
  left_join(data_ordinary_map, c("KURZ" = "Canton"))


data_simplified_map <- swiss_cantons %>% 
  left_join(data_simplified_map, c("KURZ" = "Canton"))


##### ORDINARY
ggplot()+
  geom_sf(data = data_ordinary_map, aes(fill = total_13), size = 0.3) + 
  theme_void() + 
  ggrepel::geom_label_repel(
    data = data_ordinary_map,
    aes(label = paste0(KURZ,":",round(total_13, digits = 0)), 
        geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0.2,
    colour = "#541f3f",
    size = 3,
    segment.alpha = 0.5
  ) +
  scale_fill_continuous(low="white", high="#2166AC") +
  labs(title = "Ordinary Naturalisations between 2011 to 2020") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = c(0.9,0.9)) + labs(fill='Number of Naturalisations') 


##### SIMPLIFIED
ggplot()+
  geom_sf(data = data_simplified_map, aes(fill = total_13), size = 0.3) + 
  theme_void() + 
  ggrepel::geom_label_repel(
    data = data_simplified_map,
    aes(label = paste0(KURZ,":",round(total_13, digits = 0)), 
        geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0.2,
    colour = "#541f3f",
    size = 3,
    segment.alpha = 0.5
  ) +
  scale_fill_continuous(low="white", high="#2166AC") +
  labs(title = "Simplified Naturalisations between 2011 to 2020") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = c(0.9,0.9)) + labs(fill='Number of Naturalisations') 


################## RELATIVE
## Now with relative data
population <- read_csv("/Users/darynabilodid/Documents/GitHub/SCV-project3/data/population_by_year/pop_data.csv")

# Get the sum of population over the years
population$all_pop_y <- rowSums( population[,2:11] )

# Join the table
data_ordinary_map <- data_ordinary_map %>% 
  left_join(population, c("KURZ" = "canton"))

data_simplified_map <- data_simplified_map %>% 
  left_join(population, c("KURZ" = "canton"))


data_ordinary_map$all_pop_y <- as.numeric(data_ordinary_map$all_pop_y)
data_simplified_map$all_pop_y <- as.numeric(data_simplified_map$all_pop_y)


data_ordinary_map <- data_ordinary_map %>%
  mutate(relative_total = (total_13/(all_pop_y))*100)

data_simplified_map <- data_simplified_map %>%
  mutate(relative_total = (total_13/(all_pop_y))*100)


## Relative map
ggplot()+
  geom_sf(data = data_ordinary_map, aes(fill = relative_total), size = 0.3) + 
  theme_void() + 
  ggrepel::geom_label_repel(
    data = data_ordinary_map,
    aes(label = paste0(KURZ,":",round(relative_total, digits = 3)), 
        geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0.2,
    colour = "#541f3f",
    size = 3,
    segment.alpha = 0.5
  ) +
  scale_fill_continuous(low="white", high="#2166AC") +
  labs(title = "Relative number of Ordinary Naturalisations between 2011 and 2020") +
  theme(legend.title = element_blank()) +  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = c(0.9,0.9)) + labs(fill='Relative Number') 


ggplot()+
  geom_sf(data = data_simplified_map, aes(fill = relative_total), size = 0.3) + 
  theme_void() + 
  ggrepel::geom_label_repel(
    data = data_simplified_map,
    aes(label = paste0(KURZ,":",round(relative_total, digits = 3)), 
        geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0.2,
    colour = "#541f3f",
    size = 3,
    segment.alpha = 0.5
  ) +
  scale_fill_continuous(low="white", high="#2166AC") +
  labs(title = "Relative number of Simplified Naturalisations between 2011 and 2020") +
  theme(legend.title = element_blank()) +  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = c(0.9,0.9)) + labs(fill='Relative Number') 

#########




data_2011  = subset(my_data, Year == 2011)
data_2012  = subset(my_data, Year == 2012)
data_2013  = subset(my_data, Year == 2013)
data_2014  = subset(my_data, Year == 2014)
data_2015  = subset(my_data, Year == 2015)
data_2016  = subset(my_data, Year == 2016)
data_2017  = subset(my_data, Year == 2017)
data_2018  = subset(my_data, Year == 2018)
data_2019  = subset(my_data, Year == 2019)
data_2020  = subset(my_data, Year == 2020)

# Obtain the total number by canton for each year
# 2011
total_by_canton_2011 <- data_2011 %>%
  group_by(Canton) %>%
  summarise(total_11 = sum(Value))

# 2012
total_by_canton_2012 <- data_2012 %>%
  group_by(Canton) %>%
  summarise(total_12 = sum(Value))

# 2013
total_by_canton_2013 <- data_2013 %>%
  group_by(Canton) %>%
  summarise(total_13 = sum(Value))

# 2014
total_by_canton_2014 <- data_2014 %>%
  group_by(Canton) %>%
  summarise(total_14 = sum(Value))

# 2015
total_by_canton_2015 <- data_2015 %>%
  group_by(Canton) %>%
  summarise(total_15 = sum(Value))

# 2016
total_by_canton_2016 <- data_2016 %>%
  group_by(Canton) %>%
  summarise(total_16 = sum(Value))

# 2017
total_by_canton_2017 <- data_2017 %>%
  group_by(Canton) %>%
  summarise(total_17 = sum(Value))

# 2018
total_by_canton_2018 <- data_2018 %>%
  group_by(Canton) %>%
  summarise(total_18 = sum(Value))

# 2019
total_by_canton_2019 <- data_2019 %>%
  group_by(Canton) %>%
  summarise(total_19 = sum(Value))

# 2020
total_by_canton_2020 <- data_2020 %>%
  group_by(Canton) %>%
  summarise(total_20 = sum(Value))


## Merge dataframes
data <- merge(total_by_canton_2011, total_by_canton_2012, by = "Canton")
data2 <- merge(total_by_canton_2013, total_by_canton_2014, by = "Canton")
data <- merge(data, data2, by = "Canton")
data3 <- merge(total_by_canton_2015, total_by_canton_2016, by = "Canton")
data4 <- merge(total_by_canton_2017, total_by_canton_2018, by = "Canton")
data5 <- merge(data3, data4, by = "Canton")
data <- merge(data, data5, by = "Canton")
data6 <- merge(total_by_canton_2019, total_by_canton_2020, by = "Canton")
data <- merge(data, data6, by = "Canton")

levels(data$Canton)  <- c("CH", 
                         "ZH", "BE", "LU", "UR", "SZ", "OW", 
                         "NW", "GL", "ZG", "FR", "SO", "BS", 
                         "BL", "SH", "AR", "AI", "SG", "GR", 
                         "AG", "TG", "TI", "VD", "VS", "NE", 
                         "GE", "JU", 
                         "NA")

data$all_years <- rowSums( data[,2:11] )

# Read the geographic data
swiss_cantons <- st_read("~/Documents/GitHub/SCV-project3/data/for map plot/G1K09.shp")


full_data <- swiss_cantons %>% 
  left_join(data, c("KURZ" = "Canton"))


## Plot with row data
ggplot()+
  geom_sf(data = full_data, aes(fill = all_years), size = 0.3) + 
  theme_void() + 
  ggrepel::geom_label_repel(
    data = full_data,
    aes(label = paste0(KURZ,":",round(all_years, digits = 0)), 
        geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0.2,
    colour = "#541f3f",
    size = 3,
    segment.alpha = 0.5
  ) +
  scale_fill_continuous(low="white", high="#2166AC", limits=c(0,90000)) +
  labs(title = "Total Number of Naturalisations between 2011 to 2020") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = c(0.9,0.9)) + labs(fill='Number of Naturalisations') 


## Now with relative data
population <- read_csv("/Users/darynabilodid/Documents/GitHub/SCV-project3/data/population_by_year/pop_data.csv")

# Get the sum of population over the years
population$all_pop_y <- rowSums( population[,2:11] )

# Join the table
full_data <- full_data %>% 
  left_join(population, c("KURZ" = "canton"))


# To numeric values 
full_data$all_pop_y <- as.numeric(full_data$all_pop_y)

full_data <- full_data %>%
  mutate(relative_total = (all_years/(all_pop_y))*100)

subset(my_data, Year == 2011)
## Relative map
ggplot()+
  geom_sf(data = full_data, aes(fill = relative_total), size = 0.3) + 
  theme_void() + 
  ggrepel::geom_label_repel(
    data = full_data,
    aes(label = paste0(KURZ,":",round(relative_total, digits = 3)), 
        geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0.2,
    colour = "#541f3f",
    size = 3,
    segment.alpha = 0.5
  ) +
  scale_fill_continuous(low="white", high="#2166AC", limits=c(0,1)) +
  labs(title = "Relative number of Naturalisations between 2011 and 2020") +
  theme(legend.title = element_blank()) +  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = c(0.9,0.9)) + labs(fill='Relative Number') 



#### Gif for time varibale 
# Create a category year
total_by_canton_2011['year'] <- 2011
total_by_canton_2012['year'] <- 2012
total_by_canton_2013['year'] <- 2013
total_by_canton_2014['year'] <- 2014
total_by_canton_2015['year'] <- 2015
total_by_canton_2016['year'] <- 2016
total_by_canton_2017['year'] <- 2017
total_by_canton_2018['year'] <- 2018
total_by_canton_2019['year'] <- 2019
total_by_canton_2020['year'] <- 2020

# Rename column of total

names(total_by_canton_2011)[2] <- "total"
names(total_by_canton_2012)[2] <- "total"
names(total_by_canton_2013)[2] <- "total"
names(total_by_canton_2014)[2] <- "total"
names(total_by_canton_2015)[2] <- "total"
names(total_by_canton_2016)[2] <- "total"
names(total_by_canton_2017)[2] <- "total"
names(total_by_canton_2018)[2] <- "total"
names(total_by_canton_2019)[2] <- "total"
names(total_by_canton_2020)[2] <- "total"


total_by_canton <- bind_rows(total_by_canton_2011, total_by_canton_2012, total_by_canton_2013,
          total_by_canton_2014,total_by_canton_2015,total_by_canton_2016,
          total_by_canton_2017,total_by_canton_2018,total_by_canton_2019,total_by_canton_2020)

levels(total_by_canton$Canton) <-  c("CH", 
                                     "ZH", "BE", "LU", "UR", "SZ", "OW", 
                                     "NW", "GL", "ZG", "FR", "SO", "BS", 
                                     "BL", "SH", "AR", "AI", "SG", "GR", 
                                     "AG", "TG", "TI", "VD", "VS", "NE", 
                                     "GE", "JU", 
                                     "NA")

########## Work on population
View(population)

pop_2011 <- population[c("canton","year_2011")]
names(pop_2011)[2] <- "pop"
pop_2011['year'] <- 2011

pop_2012 <- population[c("canton","year_2012")]
names(pop_2012)[2] <- 'pop'
pop_2012['year'] <- 2012

pop_2013 <- population[c("canton","year_2013")]
names(pop_2013)[2] <- 'pop'
pop_2013['year'] <- 2013

pop_2014 <- population[c("canton","year_2014")]
names(pop_2014)[2] <- 'pop'
pop_2014['year'] <- 2014

pop_2015 <- population[c("canton","year_2015")]
names(pop_2015)[2] <- 'pop'
pop_2015['year'] <- 2015

pop_2016 <- population[c("canton","year_2016")]
names(pop_2016)[2] <- 'pop'
pop_2016['year'] <- 2016

pop_2017 <- population[c("canton","year_2017")]
names(pop_2017)[2] <- 'pop'
pop_2017['year'] <- 2017

pop_2018 <- population[c("canton","year_2018")]
names(pop_2018)[2] <- 'pop'
pop_2018['year'] <- 2018

pop_2019 <- population[c("canton","year_2019")]
names(pop_2019)[2] <- 'pop'
pop_2019['year'] <- 2019

pop_2020 <- population[c("canton","year_2020")]
names(pop_2020)[2] <- 'pop'
pop_2020['year'] <- 2020


pop_total <- bind_rows(pop_2011,pop_2012,pop_2013,pop_2014,pop_2015,
                       pop_2016,pop_2017,pop_2018,pop_2019,pop_2020)

### Gif map for row
levels(total_by_canton$Canton) <- levels(data$Canton)  <- c("CH", 
                                                            "ZH", "BE", "LU", "UR", "SZ", "OW", 
                                                            "NW", "GL", "ZG", "FR", "SO", "BS", 
                                                            "BL", "SH", "AR", "AI", "SG", "GR", 
                                                            "AG", "TG", "TI", "VD", "VS", "NE", 
                                                            "GE", "JU", 
                                                            "NA")


total_by_canton <- swiss_cantons %>% 
  left_join(total_by_canton, c("KURZ" = "Canton"))




ggplot(total_by_canton) +
  geom_sf(aes(fill = total), size = 0.3) +
  theme_void() + 
  theme(legend.title = element_blank(),
        legend.position = "bottom") +
  transition_manual(as.integer(year)) +
  scale_fill_continuous(low="white", high="#2166AC", limits=c(0,11000)) +
  labs(title = "Number of naturalisations by canton in year: {current_frame}") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = c(0.9,0.9)) + labs(fill='Number of Naturalisations') 




################Relative gif
names(total_by_canton)[3] <- "canton"
gif_rel <- merge(total_by_canton, pop_total, by = c("year", "canton")) gif_rel

gif_rel <- gif_rel %>%
  mutate(relative = (total/pop)*100)


ggplot(gif_rel) +
  geom_sf(aes(fill = relative), size = 0.3) +
  theme_void() + 
  theme(legend.title = element_blank(),
        legend.position = "bottom") +
  transition_manual(as.integer(year)) +
  scale_fill_continuous(low="white", high="#2166AC", limits=c(0,1.6)) +
  labs(title = "Relative number of naturalisations by canton in year: {current_frame}") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = c(0.9,0.9)) + labs(fill='Relative number of Naturalisations') 




############################

# names(total_by_canton)[1] <- "KZ"
pop_total$canton <- as.factor(pop_total$canton)

total_by_canton


data_ready <- merge(total_by_canton,pop_total, by = c("year", "canton")) 

data_ready <- data_ready %>%
  mutate(relative = (total/pop)*100)
data_ready$relative <- round(data_ready$relative ,digit=3) 

data_ready$year <- as.numeric(as.character(data_ready$year))
### Stacked plot 
library(plotly)
library(pals)
library(RColorBrewer)
coul <- brewer.pal(4, "PuOr") 
# Add more colors to this palette :
coul <- colorRampPalette(coul)(27)

p <- ggplot(data_ready, aes(x = year,
                     y = total,
                     fill = canton)) +
  geom_area(color = "black") +
  labs(title = "Total number of naturalisations by canton over time",
       subtitle = "2011 - 2020",
       x = "Year",
       y = "",
       fill = "Canton") +
  scale_x_continuous(breaks=seq(2011, 2020, 1)) +
  theme_minimal() + theme(axis.text.x=element_text(angle=45, hjust=1)) + theme(plot.title = element_text(hjust = 0.5))
ggplotly(p)






## Relative to the population 
p <- ggplot(data_ready, aes(x = year,
                            y = relative,
                            fill = canton)) +
  geom_area(color = "black") +
  labs(title = "Relative number of naturalisations by Canton over time",
       subtitle = "2011 - 2020",
       x = "Year",
       y = "",
       fill = "Canton") +
  scale_x_continuous(breaks=seq(2011, 2020, 1)) +
  theme_minimal() + theme(axis.text.x=element_text(angle=45, hjust=1)) + theme(plot.title = element_text(hjust = 0.5))
ggplotly(p)







  

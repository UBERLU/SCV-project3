# load packages 
library(dplyr)
library(tidyverse)
library(ggplot2)
# library(ggswissmaps)
# library(sf)
library(tmap)
library(plotly)
library("readxl")
library(data.table)

# Working on a subset data (Year 2011)
data_2011  = subset(my_data, Year == 2011)

# Obtain the total number of 
total_by_canton_2011 <- data_2011 %>%
  group_by(Canton) %>%
  summarise(total = sum(Value))

# Delete row oof unknown
total_by_canton_2011 <-subset(total_by_canton_2011, Canton != "Unknown")

# Change for abbreviations of cantons
levels(total_by_canton_2011$Canton) <- c("CH", 
         "ZH", "BE", "LU", "UR", "SZ", "OW", 
         "NW", "GL", "ZG", "FR", "SO", "BS", 
         "BL", "SH", "AR", "AI", "SG", "GR", 
         "AG", "TG", "TI", "VD", "VS", "NE", 
         "GE", "JU", 
         "NA")

levels(total_by_canton_2011$Canton)

p <-ggplot(data=total_by_canton_2011, aes(x=total, y=Canton)) +
  geom_bar(stat="identity")
p

# Data by population
pop <- read_excel("je-e-21.03.02.xlsx")
header.true <- function(df) {
  names(df) <- as.character(unlist(df[1,]))
  df[-1,]
}
pop_2019 <- header.true(pop_2019)

pop_2019_in_1000 <- as.data.frame(t(as.matrix(pop_2019)))

library(tibble)

pop_2019_in_1000 <- tibble::rownames_to_column(pop_2019_in_1000, "Canton")
pop_2019_in_1000 <- pop_2019_in_1000[-c(1, 2), ]


pop_2019_in_1000 <- pop_2019_in_1000 %>% 
  rename(
    Population_in_1000 = V1
  )


rel_2011 <- merge(total_by_canton_2011, pop_2019_in_1000, by.x = "Canton")
rel_2011$Population_in_1000 <- as.numeric(as.character(rel_2011$Population_in_1000))



rel_2011 <- rel_2011 %>%
  mutate(relative = total/(Population_in_1000 *1000))

p <-ggplot(data=rel_2011, aes(x=relative, y=Canton)) +
  geom_bar(stat="identity")
p



# By Age
# We define categories by age and gender 
data_2011 <- data_2011 %>% 
  mutate(category_age=cut(Age, breaks=c(-Inf, 9, 19, 29, 39, 49, 59, 69, 79, Inf), 
                      labels=c("0 - 9","10 - 19","20 - 29","30 - 39","40 - 49","50 - 59", "60 - 69", "70 - 79", "80 +")))

# New dataframe
age_gender_2011 <- data_2011 %>%
  group_by(category_age, Genre) %>%
  summarise(total_by_age = sum(Value))




# Plot 
library(ggthemes)
options(scipen = 999)
brks <- seq(-5000, 5000, 1000)
lbls = paste0(as.character(c(seq(-5000, 0, 1000), seq(1000, 5000, 1000))))
p <- ggplot(age_gender_2011, aes(x = category_age, y = total_by_age, fill = Genre)) +   # Fill column
  geom_bar(stat = "identity", width = .6) + scale_y_continuous(breaks = brks,   # Breaks
                                                               labels = lbls) + coord_flip() +
  labs(title = "Age") +
  theme_tufte() +  # Tufte theme from ggfortify
  theme(plot.title = element_text(hjust = 0), 
        axis.ticks = element_blank()) +   # Centre plot title
  scale_fill_brewer(palette = "Dark2")# Labels

ggplotly(p)

data_2018  = subset(my_data, Year == 2018)

total_by_canton_2018 <- data_2018 %>%
  group_by(Canton) %>%
  summarise(total = sum(Value))

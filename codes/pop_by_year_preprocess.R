library("readxl")
library(tidyverse)
library(dplyr)

setwd("~/Documents/GitHub/SCV-project3/data/population_by_year")

# Get the data by year and select the correct columns
## 2011
data_2011 <- read_excel("je-e-01.02.03.04.xlsx", sheet = "2011")
data_2011 <- data_2011 %>% select(1:2)
## Rename the columns
names(data_2011)[1] <- "Canton"
names(data_2011)[2] <- "Year_2011"

## 2012
data_2012 <- read_excel("je-e-01.02.03.04.xlsx", sheet = "2012")
data_2012 <- data_2012 %>% select(1:2)
## Rename the columns
names(data_2012)[1] <- "Canton"
names(data_2012)[2] <- "Year_2012"


## 2013
data_2013 <- read_excel("je-e-01.02.03.04.xlsx", sheet = "2013")
data_2013 <- data_2013 %>% select(1:2)
## Rename the columns
names(data_2013)[1] <- "Canton"
names(data_2013)[2] <- "Year_2013"

## 2014
data_2014 <- read_excel("je-e-01.02.03.04.xlsx", sheet = "2014")
data_2014 <- data_2014 %>% select(1:2)
## Rename the columns
names(data_2014)[1] <- "Canton"
names(data_2014)[2] <- "Year_2014"

## 2015
data_2015 <- read_excel("je-e-01.02.03.04.xlsx", sheet = "2015")
data_2015 <- data_2015 %>% select(1:2)
## Rename the columns
names(data_2015)[1] <- "Canton"
names(data_2015)[2] <- "Year_2015"

## 2016
data_2016 <- read_excel("je-e-01.02.03.04.xlsx", sheet = "2016")
data_2016 <- data_2016 %>% select(1:2)
## Rename the columns
names(data_2016)[1] <- "Canton"
names(data_2016)[2] <- "Year_2016"

## 2017
data_2017 <- read_excel("je-e-01.02.03.04.xlsx", sheet = "2017")
data_2017 <- data_2017 %>% select(1:2)
## Rename the columns
names(data_2017)[1] <- "Canton"
names(data_2017)[2] <- "Year_2017"

## 2018
data_2018 <- read_excel("je-e-01.02.03.04.xlsx", sheet = "2018")
data_2018 <- data_2018 %>% select(1:2)
## Rename the columns
names(data_2018)[1] <- "Canton"
names(data_2018)[2] <- "Year_2018"

## 2019
data_2019 <- read_excel("je-e-01.02.03.04.xlsx", sheet = "2019")
data_2019 <- data_2019 %>% select(1:2)
## Rename the columns
names(data_2019)[1] <- "Canton"
names(data_2019)[2] <- "Year_2019"

## 2020
data_2020 <- read_excel("je-e-01.02.03.04.xlsx", sheet = "2020")
data_2020 <- data_2020 %>% select(1:2)
## Rename the columns
names(data_2020)[1] <- "Canton"
names(data_2020)[2] <- "Year_2020"

# Merge the data by canton 
data <- merge(data_2011, data_2012, by = "Canton")
data2 <- merge(data_2013, data_2014, by = "Canton")
data <-  merge(data, data2, by = "Canton")
data3 <- merge(data_2015, data_2016, by = "Canton")
data <-  merge(data, data3, by = "Canton")
data4 <- merge(data_2017, data_2018, by = "Canton")
data5 <- merge(data_2019, data_2020, by = "Canton")
data6 <- merge(data4, data5)
data <- merge(data, data6)


### Clean the table
population_yearly <- data %>% filter(
  Canton == "Vaud" | Canton == "Valais" | Canton == "Geneva"| 
    Canton == "Bern"| Canton == "Fribourg" | Canton == "Solothurn"| Canton == "Neuchâtel"| Canton == "Jura"|
    Canton == "Basel-Stadt" | Canton == "Basel-Landschaft" | Canton == "Aargau"| 
    Canton == "Glarus"| Canton == "Schaffhausen" | Canton == "Appenzell A. Rh."| Canton == "Appenzell I. Rh."| Canton == "St. Gallen"| Canton == "Graubünden"| Canton == "Thurgau"|
    Canton == "Lucerne"| Canton == "Uri" | Canton == "Schwyz"| Canton == "Obwalden"| Canton == "Nidwalden"| Canton == "Zug"|
    Canton =="Ticino"| Canton =="Zurich"
)

### Clean the column names 
population_yearly <- population_yearly %>%
  clean_names()


### Canton names to codes 
population_yearly$canton[population_yearly$canton == "Zurich"]  <- "ZH"
population_yearly$canton[population_yearly$canton == "Bern"] <- "BE"
population_yearly$canton[population_yearly$canton == "Lucerne"]  <- "LU"
population_yearly$canton[population_yearly$canton == "Uri"]  <- "UR"
population_yearly$canton[population_yearly$canton == "Schwyz"] <- "SZ"
population_yearly$canton[population_yearly$canton == "Obwalden"]  <- "OW"
population_yearly$canton[population_yearly$canton == "Nidwalden"]  <- "NW"
population_yearly$canton[population_yearly$canton == "Glarus"]  <- "GL"
population_yearly$canton[population_yearly$canton == "Zug"] <- "ZG"
population_yearly$canton[population_yearly$canton == "Fribourg"]  <- "FR"
population_yearly$canton[population_yearly$canton == "Solothurn"]  <- "SO"
population_yearly$canton[population_yearly$canton == "Basel-Stadt"] <- "BS"
population_yearly$canton[population_yearly$canton == "Basel-Landschaft"]  <- "BL"
population_yearly$canton[population_yearly$canton == "Schaffhausen"]  <- "SH"
population_yearly$canton[population_yearly$canton == "Appenzell A. Rh."] <- "AR"
population_yearly$canton[population_yearly$canton == "Appenzell I. Rh."]  <- "AI"
population_yearly$canton[population_yearly$canton == "St. Gallen"]  <- "SG"
population_yearly$canton[population_yearly$canton == "Graubünden"] <- "GR"
population_yearly$canton[population_yearly$canton == "Aargau"]  <- "AG"
population_yearly$canton[population_yearly$canton == "Thurgau"]  <- "TG"
population_yearly$canton[population_yearly$canton == "Ticino"] <- "TI"
population_yearly$canton[population_yearly$canton == "Vaud"]  <- "VD"
population_yearly$canton[population_yearly$canton == "Valais"]  <- "VS"
population_yearly$canton[population_yearly$canton == "Neuchâtel"] <- "NE"
population_yearly$canton[population_yearly$canton == "Geneva"]  <- "GE"
population_yearly$canton[population_yearly$canton == "Jura"]  <- "JU"

write_csv(population_yearly, "~/Documents/GitHub/SCV-project3/data/population_by_year/pop_data.csv")




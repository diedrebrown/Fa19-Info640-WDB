#Diedre Brown; dbrow207@pratt.edu
#INFO 640 - Fall 2019
#Lab 9 - Mapping
#

####install packages and load libraries####
install.packages("ggmap")
install.packages("maps")
install.packages("mapdata")
install.packages("sf")
install.packages("sp")
install.packages("ggthemes")
install.packages("tigris")
install.packages("tmap")
install.packages("leaflet")
install.packages("rgdal", dependencies=TRUE)

install.packages("devtools")
devtools::install_github("dkahle/ggmap")
library(ggmap)
library(devtools)

library(dplyr)
library(ggplot2)
library(broom) 
library(GGally)

library(tidyverse)
library(lubridate)
library(leaflet)
library(ggmap)
library(tmap)
library(tigris)
library(sp)
library(ggthemes)
library(maps)
library(mapdata)
library(sf)
library(stringr)
library(rgdal)

register_google(key = "APIKey", write=TRUE)

#https://www.google.com/maps/@40.6971494,-74.2598655,10z
nyc_map <- get_map(location = c(lon=-74.259, lat=40.6971), zoom = 18, scale=1)
ggmap(nyc_map)

??get_map

noise_311 <- read.csv("Desktop/Pratt/Fall2019/DataAnalysis_Info640_McSweeney/INFO640-WDB/info640-Lab Info and DataSets/DataSets/311_Service_Requests_from_2010_to_present.csv")
glimpse(noise_311)
dim(noise_311)

#clean dataset
not_all_na <- function(x) any(!is.na(x))
noise_311_clean <- noise_311 %>%
  select_if(not_all_na)
glimpse(noise_311_clean)
#change date format
noise_311_clean$Created.Date <- mdy_hms(noise_311_clean$Created.Date)
noise_311_clean$Closed.Date <- mdy_hms(noise_311_clean$Closed.Date)
glimpse(noise_311_clean)

#plot of noise complaints
ggmap(nyc_map) +
  geom_point(aes(Longitude, Latitude), data = noise_311_clean)

#where are ice cream truck complaints the most prominent
unique(noice_311_clean311$Descriptor)
as.character(noise_311_clean$Descriptor)
icecream_311 <- noise_311_clean %>%
  filter(str_detect(Descriptor, "Ice Cream"))
dim(icecream_311)
unique(icecream_311$Descriptor)
glimpse(icecream_311)
#visualize complaints
ggmap(nyc_map) +
  geom_point(aes(Longitude, Latitude), data = icecream_311)

#increase the zoom level and the scale to improve the resolution.
nyc_map <- get_map(location = nyc, zoom = 11, scale=2)
ggmap(nyc_map) +
  geom_point(aes(Longitude, Latitude), data = icecream_311)

#use color to respreent address, intersection, or a blockface
ggmap(nyc_map) +
  geom_point(aes(Longitude, Latitude, color=Address.Type), data = icecream_311)

#basemap to specify things that are common to every plot
#specify the dataset in the base_layer and the Address Type in the geometry
ggmap(nyc_map, base_layer = ggplot(icecream_311, aes(Longitude, Latitude))) +
  geom_point(aes(color=Address.Type))
#facet by address type
ggmap(nyc_map, base_layer = ggplot(icecream_311, aes(Longitude, Latitude))) +
  geom_point(aes(color=Address.Type)) +
  facet_wrap(~Address.Type)
#remove scales, add legend, title, labels, and a caption
####THIS IS NOT WORKING BECAUSE OF THE RGDAL PACKAGE ERRORS###
ggmap(nyc_map, base_layer = ggplot(icecream_311, aes(Longitude, Latitude))) +
  geom_point(aes(color=Borough)) +
  theme_void()+
  theme(legend.position='bottom', plot.title = element_text(hjust = 0.5)) +
  labs(title="Ice Cream Noise Complaints by Address Type", caption="Source: NYC Open Data") facet_wrap(~Address.Type)

####THEMATIC MAPS####
####THESE ARE ALSO NOT WORKING BECAUSE OF RDGAL PACKAGE ERRORS###
#visualizing census data for median income in NYC with a shape file (the spatial container) and a datafile (with some sort of key to line them up)
nyc_counties <- c("New York", "Kings", "Queens", "Bronx", "Richmond")
nyc_tracts <- tracts(state = "NY", nyc_counties, cb = TRUE)
summary(nyc_tracts)
plot(nyc_tracts)
head(nyc_tracts, n=1)














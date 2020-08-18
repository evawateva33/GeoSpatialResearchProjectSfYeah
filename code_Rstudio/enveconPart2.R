library(spdep)
library(rgdal)
library(pacman) 
library(lm.beta) 
library(dplyr)
library(haven) 
library(readr)
library(measurements)
library(readstata13)
library(data.table)
library(dplyr)
options(stringsAsFactors = F)
library(sp)
library(tibble)
library(tidyr)
library(tidyverse)
# Load new packages
library(pacman)
p_load(ggmap, leaflet)
library(mapview)
library(rgdal)
library(raster)
library(rgeos)
library(maps)
options(stringsAsFactors = FALSE)
# Load old packages
p_load(lubridate, rgdal, raster, broom, rgeos, GISTools)
p_load(dplyr, ggplot2, ggthemes, fields, magrittr, viridis)
setwd("/Users/eva/Desktop/class/")
getwd()
dir.class <- "/Users/eva/Desktop/class/"

crime_df = read_csv(paste0(dir.class, "SFYEAH_geo_f.csv"))
crime_df_unsafe = subset(crime_df, pl_unsafe ==1)
crime_df_safe = subset(crime_df, pl_safe ==1)
chi_beats <- readOGR(".", "geo_export_e21612ee-1948-49ab-a925-adaad6bfb384.shp", layer="geo_export_e21612ee-1948-49ab-a925-adaad6bfb384",  verbose = TRUE) 
chi_lightz <- raster(dsn= ".", "popz.tiff")
med_income <- raster(dsn= ".", "income.tiff")
chi_lights <- raster(dsn= ".", "night.jpg")

crs(med_income) <- CRS("+init=epsg:4326")

crs(chi_lights) <- CRS("+init=epsg:4326")
crs(chi_lightz) <- CRS("+init=epsg:4326")

bb <- extent(-122.56495, -122.3279, 37.69, 37.83243)
extent(med_income) <- bb
med_income <- setExtent(med_income, bb)
med_income<- crop(med_income, extent(chi_beats))
med_income <- mask(med_income, chi_beats)

bb <- extent(-122.56495, -122.3279, 37.69, 37.83243)
extent(chi_lights) <- bb
chi_lights <- setExtent(chi_lights, bb)
chi_lights<- crop(chi_lights, extent(chi_beats))
chi_lights <- mask(chi_lights, chi_beats)

bb <- extent(-122.56495, -122.3279, 37.69, 37.83243)
extent(chi_lightz) <- bb
chi_lightz <- setExtent(chi_lightz, bb)
chi_lightz<- crop(chi_lightz, extent(chi_beats))
chi_lightz <- mask(chi_lightz, chi_beats)

pal <- colorFactor(c("navy", "red"), domain = c("pl_safe", "pl_unsafe"))

leaflet(df) %>% addTiles() %>%
  addCircleMarkers(
    radius = ~ifelse(type == "ship", 6, 10),
    color = ~pal(type),
    stroke = FALSE, fillOpacity = 0.5
  )
m <- leaflet() %>%
  addTiles() %>% 
  addRasterImage(med_income, 
                 opacity = .8, project = TRUE) %>%
  addCircleMarkers(lng = crime_df_unsafe$lon, lat = crime_df_unsafe$lat, color = 'red',popup = "Unsafe")%>%
  addCircleMarkers(lng = crime_df_safe$lon, lat = crime_df_safe$lat, color = 'navy',popup = "Safe")%>%
  addLegend(pal = pal, values = values(med_income))%>%
  setView(lng = -122.45, lat = 37.76, zoom = 12)
m
m2 <- leaflet() %>%
  addTiles() %>% 
  addRasterImage(chi_lightz, 
                 opacity = .8, project = TRUE) %>%
  addCircleMarkers(lng = crime_df_unsafe$lon, lat = crime_df_unsafe$lat, color = 'red',popup = "Unsafe")%>%
  addCircleMarkers(lng = crime_df_safe$lon, lat = crime_df_safe$lat, color = 'navy',popup = "Safe")%>%
  addLegend(pal = pal, values = values(chi_lightz))%>%
  setView(lng = -122.45, lat = 37.76, zoom = 12)
m2
m3<- leaflet() %>%
  addTiles() %>% 
  addRasterImage(chi_lights, 
                 opacity = .8, project = TRUE) %>%
  addCircleMarkers(lng = crime_df_unsafe$lon, lat = crime_df_unsafe$lat, color = 'red',popup = "Unsafe")%>%
  addCircleMarkers(lng = crime_df_safe$lon, lat = crime_df_safe$lat, color = 'navy',popup = "Safe")%>%
  addLegend(pal = pal, values = values(chi_lights))%>%
  setView(lng = -122.45, lat = 37.76, zoom = 12)
m3

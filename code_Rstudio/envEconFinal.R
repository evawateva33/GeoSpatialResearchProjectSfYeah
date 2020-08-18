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
# Load old packages
p_load(lubridate, rgdal, raster, broom, rgeos, GISTools)
p_load(dplyr, ggplot2, ggthemes, fields, magrittr, viridis)
# My ggplot2 theme
theme_ed <- theme(
  legend.position = "bottom",
  panel.background = element_rect(fill = NA),
  panel.border = element_rect(fill = NA, color = "grey75"),
  axis.ticks = element_line(color = "grey95", size = 0.3),
  panel.grid.major = element_line(color = "grey95", size = 0.3),
  panel.grid.minor = element_line(color = "grey95", size = 0.3),
  legend.key = element_blank())
library(htmltools)
library(tidyverse)
library(leaflet)
library(stringr)
library(sf)
library(here)
library(widgetframe)


setwd("/Users/eva/Desktop/class/")
getwd()
dir.class <- "/Users/eva/Desktop/class/"

#crime_df= read_csv(paste0(dir.class, "homeless.csv"))
crime_df = read_csv(paste0(dir.class, "SFYEAH_geo_f.csv"))

chi_lights <- raster(dsn= ".", "night.jpg")
chi_lightz <- raster(dsn= ".", "popz.tiff")
med_income <- raster(dsn= ".", "income.tiff")
chi_lights

bb <- extent(-122.56495, -122.3279, 37.69, 37.83243)
extent(chi_lights) <- bb
chi_lights <- setExtent(chi_lights, bb)
chi_beats <- readOGR(".", "geo_export_e21612ee-1948-49ab-a925-adaad6bfb384.shp", layer="geo_export_e21612ee-1948-49ab-a925-adaad6bfb384",  verbose = TRUE) 
chi_lights<- crop(chi_lights, extent(chi_beats))
chi_lights <- mask(chi_lights, chi_beats)

bb <- extent(-122.56495, -122.3279, 37.69, 37.83243 )
extent(chi_lightz) <- bb
chi_lightz<- crop(chi_lightz, extent(chi_beats))
chi_lightz <- mask(chi_lightz, chi_beats)

bb <- extent(-122.56495, -122.3279, 37.69, 37.83243)
extent(med_income) <- bb
med_income <- setExtent(med_income, bb)
chi_beats <- readOGR(".", "geo_export_e21612ee-1948-49ab-a925-adaad6bfb384.shp", layer="geo_export_e21612ee-1948-49ab-a925-adaad6bfb384",  verbose = TRUE) 
med_income<- crop(med_income, extent(chi_beats))
med_income <- mask(med_income, chi_beats)

plot(chi_lights, col = viridis(1e3), axes = T, box = T)
lines(chi_beats, col = "red", lwd = 0.8)
 
plot(chi_lightz, col =terrain.colors(255), axes = T, box = T)
lines(chi_beats, col = "red", lwd = 0.8)


plot(med_income, col =terrain.colors(255), axes = T, box = T)
lines(chi_beats, col = "red", lwd = 0.8)


ggplot(data = data.frame(light = getValues(chi_lights)),
       aes(x = log(light))) +
  geom_histogram(bins = 20, fill = viridis(20)) +
  ylab("Count") +
  xlab("Log(light") +
  ggtitle("Distribution of Light Radiance in SF",
          subtitle = "") +
  theme_ed

ggplot(data = data.frame(med_incomes = getValues(med_income)),
       aes(x = (med_incomes))) +
  geom_histogram(bins = 20, fill = viridis(20)) +
  ylab("Count") +
  xlab("Medium Income Per Household in SF") +
  ggtitle("Distribution of Income per Household In SF",
          subtitle = "July 2020") +
  theme_ed

coordinates(crime_df) <- ~ lon + lat
# Assing a CRS to the crime points data
proj4string(crime_df) <- crs(chi_beats)
# Take the union of the beats polygons
chicago_union <- gUnaryUnion(chi_beats)
# Check which crime points are inside of Chicago
test_points <- over(crime_df, chicago_union)
# From 1 and NA to T and F (F means not in Chicago)
test_points <- !is.na(test_points)
crime_df <- crime_df[which(test_points == T), ]
crime_extract <- raster::extract(
  x = chi_lights,
  y = crime_df,
  fun = mean,
  na.rm = T,
  sp = T)
crime_extractz <- raster::extract(
  x = chi_lightz,
  y = crime_df,
  fun = mean,
  na.rm = T,
  sp = T)
crime_extracto <- raster::extract(
  x = med_income,
  y = crime_df,
  fun = mean,
  na.rm = T,
  sp = T)
lights_df <- chi_lights %>% rasterToPoints() %>% tbl_df()
lights_dff <- chi_lightz %>% rasterToPoints() %>% tbl_df()
lights_dfff <- med_income %>% rasterToPoints() %>% tbl_df()
# Plot with ggplot

lights_masked <- mask(x = chi_lights, mask = chicago_union)
lights_maskedd <- mask(x = chi_lightz, mask = chicago_union)
income_masked <- mask(x = med_income, mask = chicago_union)
masked_df <- lights_masked %>% rasterToPoints() %>% tbl_df()
masked_dff <- lights_maskedd %>% rasterToPoints() %>% tbl_df()
# Plot with
masked_dfff <- income_masked %>% rasterToPoints() %>% tbl_df()




crime_extracto%<>% tbl_df()
crime_extract %<>% tbl_df()
crime_extractz %<>% tbl_df()
# Density plots


names(crime_df)

graph = ggplot() +
  geom_density(
    data = masked_df,
    aes(x = log(night), color = "All of Sf", fill = "All of Sf"),
    alpha = 0.6) +
  geom_density(
    data = filter(crime_extract, pl_unsafe  == 1),
    aes(x = log(night), color = "Unsafe", fill = "Unsafe"),
    alpha = 0.6) +
  geom_density(
    data = filter(crime_extract, pl_safe  == 1),
    aes(x = log(night), color = "Safe", fill = "Safe"),
    alpha = 0.6) +
  
  ylab("Density") +
  xlab("Log(Light radiance)") +
  theme_ed +
  scale_color_viridis("", discrete = T) +
  scale_fill_viridis("", discrete = T)

graph = graph +geom_vline(xintercept = log(120.1) , linetype="dotted", 
                color = "dark green", size=1.5)+ 
geom_vline(xintercept = log(130.2) , linetype="dotted", 
           color = "yellow", size=1.5) + 
geom_vline(xintercept = log(71.88) , linetype="dotted", 
           color = "purple", size=1.5)

graph
graph2 = ggplot() +
  geom_density(
    data = masked_dff,
    aes(x = (popz), color = "All of Sf", fill = "All of Sf"),
    alpha = 0.6) +
  geom_density(
    data = filter(crime_extractz, pl_unsafe  == 1),
    aes(x = (popz), color = "Unsafe", fill = "Unsafe"),
    alpha = 0.6) +
  geom_density(
    data = filter(crime_extractz, pl_safe  == 1),
    aes(x = (popz), color = "Safe", fill = "Safe"),
    alpha = 0.6) +
  ylab("Density") +
  xlab("Pop Density") +
  theme_ed +
  scale_color_viridis("", discrete = T) +
  scale_fill_viridis("", discrete = T)

graph3 = ggplot() +
  geom_density(
    data = masked_dfff,
    aes(x = (income), color = "All of Sf", fill = "All of Sf"),
    alpha = 0.6) +
  geom_density(
    data = filter(crime_extracto, pl_unsafe  == 1),
    aes(x = (income), color = "Unsafe", fill = "Unsafe"),
    alpha = 0.6) +
  geom_density(
    data = filter(crime_extracto, time_night  == 1),
    aes(x = (income), color = "night", fill = "night"),
    alpha = 0.6) +
  geom_density(
    data = filter(crime_extracto, time_day  == 1),
    aes(x = (income), color = "day", fill = "day"),
    alpha = 0.6) +
  ylab("Density") +
  xlab("Log(Light radiance)") +
  theme_ed +
  scale_color_viridis("", discrete = T) +
  scale_fill_viridis("", discrete = T)

graph3
crime_extractz %>%
  filter(re_sleep == 1) %>%
  dplyr::select(popz) %>%
  summary()
p_load(gridExtra)
library(gridExtra)
masked_dff %>%
  dplyr::select(popz) %>%
  summary()
graph2 = graph2 + geom_vline(xintercept = (134.4) , linetype="dotted", 
                   color = "dark green", size=1.5)+ 
  geom_vline(xintercept = (143) , linetype="dotted", 
             color = "yellow", size=1.5) + 
  geom_vline(xintercept = (96.67) , linetype="dotted", 
             color = "purple", size=1.5) 

grid.arrange(graph, graph2, graph3, ncol = 1)

mapview(med_income)
values(med_income) <- matrix(1:900, nrow(med_income), ncol(med_income, byrow = TRUE)
crs(med_income) <- CRS("+init=epsg:4326")
m <- leaflet() %>%
  addMarkers(lng = crime_df$lon, lat = crime_df$lat, popup = "Google") %>%
  addRasterImage(med_income)%>%
  setView(lng = 122.45, lat = 37.76, zoom = 12)
m

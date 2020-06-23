library(tidyverse)
library(leaflet)
library(lubridate)
library(sf)

# Load processed geojson of data
points <- read_sf("data/heli_points.geojson")

# Adjust knots to miles per hour

points$speed <- points$speed * 1.15

# Decide which aircraft to focus on
points_dc <- points %>% 
  #filter(code.x=="AE1F45" | code.x=="AE1FE3") %>% 
  #filter(timestamp < ymd_hms("2020-06-02 04:00:00"))
  #filter(code.x=="AE1FE3") %>% 
  #filter(code.x=="AE1F45")
  #filter(code.x=="AE0BED") %>% 
  mutate(day=day(timestamp)) %>% 
  mutate(hour=hour(timestamp)) %>% 
  mutate(minute=minute(timestamp)) #%>% 
  #filter(altitude_adjusted<=250) # if you want to focus only on the low altitude spots

# filtering to June 1
points_dc1 <- points_dc %>% 
  filter(day==1)

# filtering to June 2 before 4 am
points_dc2 <- points_dc %>% 
  filter(day==2) %>% 
  filter(hour<4) 

# joining June 1 and June 2
points_dc <- rbind(points_dc1, points_dc2)

# Set up popup window
points_dc$pop <- paste0("<b>", points_dc$code.x, "</b><br />",
                        points_dc$timestamp, "<br />Altitude: ",
                        points_dc$altitude_adjusted, "<br />Speed: ",
                        points_dc$speed)

# Some custom colors
cof <- colorFactor(c("#e41a1c", "#377eb8",
                     "#4daf4a", "#984ea3",
                     "#ff7f00", "#ffff33", "#a65628", "#f781bf",
                     "#6a3d9a"), 
                   domain=c("A1DFFB", "AC9AB0",
                            "AC9BAE", "ADA908",
                            "ADD817", "AE1F45",
                            "A4D827", "AE1FE3", "AE0BED"))

# generating a leaflet map

m <- leaflet(points_dc) %>% 
  addProviderTiles(providers$CartoDB.DarkMatter) %>% 
  setView( -77.033768, 38.899123, zoom = 7) %>% 
  addCircleMarkers(popup=points_dc$pop, weight = 3, radius=2, 
                   color=~cof(code.x), stroke = F, fillOpacity = 0.7) 
m


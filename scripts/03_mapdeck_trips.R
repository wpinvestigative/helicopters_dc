library(tidyverse)
library(sf)
library(mapdeck)
library(lubridate)
library(data.table)

mb_key <-'your_key_goes_here'

# my key is in this file that has not been uploaded to Github
source("scripts/mapbox_key.R")

# reading in the geojson file
heli_big2 <- read_sf("data/heli_points.geojson")

# Can choose which helicopters to focus on and what time
# This code below will filter only the two National Guard Lakotas
# And show where they traveled between 9 and 10 p.m. on June 1
# AE0BED is the Black Hawk, if you'd like to include that

heli_one9 <- heli_big2 %>% 
  #filter(code.x=="AE1F45" | code.x=="AE1FE3" | code.x=="AE0BED") %>% 
  filter(code.x=="AE1F45" | code.x=="AE1FE3") %>% 
  mutate(day=day(timestamp)) %>% 
  mutate(hour=hour(timestamp)) %>% 
  mutate(minute=minute(timestamp)) %>% 
  filter(day==1) %>% 
  filter(hour==21) %>% 
  filter(minute>00 & minute <=59) 

heli_one10 <- heli_big2 %>% 
  #filter(code.x=="AE1F45" | code.x=="AE1FE3" | code.x=="AE0BED") %>% 
  filter(code.x=="AE1F45" | code.x=="AE1FE3") %>% 
  mutate(day=day(timestamp)) %>% 
  mutate(hour=hour(timestamp)) %>% 
  mutate(minute=minute(timestamp)) %>% 
  filter(day==1) %>% 
  filter(hour==22) %>% 
  filter(minute>00 & minute <=59) 

heli_one <- rbind(heli_one9, heli_one10) %>% 
  arrange(code.x,timestamp)

## Quick chart for altitude

heli_one <- heli_one %>% 
  mutate(heli=case_when(
    code.x=="AE1FE3" ~ "NG Lakota",
    code.x=="AE1F45" ~ "NG Lakota Red Cross",
    code.x=="AE0BED" ~ "Black Hawk"
  ))

ggplot(heli_one, aes(x=timestamp, y=altitude_adjusted, colour=heli)) +
  geom_line() +
  theme_minimal()

## Mapping the path with Mapbox/Mapdeck
## We need to take the flight data data frame and
## transform the coordinate sinto a sfc_LINESTRING geometry 
## so it will be compatible with Mapbox/Mapdeck code

heli_path <- heli_one 


## grab the helicopter path coordinates only
dt <- sf::st_coordinates( heli_path )

## transform the matrix into a data table
dt <- as.data.table( dt )


# Extracting the altitude into an array
hel_alt <- heli_one %>% 
  mutate(day=day(timestamp)) %>% 
  pull(altitude)

# Extracting the timestamps into an array
hel_time <- heli_one %>% 
  mutate(day=day(timestamp)) %>% 
  pull(timestamp)

# Extracting a numeric value for the helicopter ID (1 and 2)
hel_id <- heli_one %>% 
  mutate(day=day(timestamp)) %>% 
  mutate(id=case_when(
    code.x=="AE1F45" ~ 1,
    code.x=="AE1FE3" ~ 2,
   # code.x=="AE0BED" ~ 3, # in case there was interest in mapping the Black Hawk
    TRUE ~ 0
  )) %>% 
  pull(id)


## This is a lot of work just to structure our 
## lat, lon, altitude, timestamp, and helicopter id
## juuust right

## Need Z and M attributes
dt[, Z := hel_alt ]   ## setting Z as our altitude
dt[, M := hel_time]   ## setting M as our timestamp
dt[, L1 := hel_id]    ## setting L1 as our helicopter id number

## now we can convert back to sf LINESTRING
heli_trips <- sfheaders::sf_linestring(
  obj = dt
  , x = "X"
  , y = "Y"
  , z = "Z"
  , m = "M"
  , linestring_id = "L1"
)

mapdeck(
  token=mb_key # if you don't have an mb_key you won't see any street tiles
  , location = c(-77.024058, 38.904742)
  , zoom = 12
  , style = mapdeck_style("dark")
) %>%
  add_trips(
    data = heli_trips
    , stroke_colour = "L1"
    , stroke_width = 30
    , animation_speed = 50
    , trail_length = 1000
  )




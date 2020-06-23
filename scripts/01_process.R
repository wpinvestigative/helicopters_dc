library(tidyverse)
library(sf)
library(sp)
library(lubridate)
library(mosaic)
library(raster)

# Add breaks where there is a gap of more than 5 minutes
# Convert timestamps to POSIXct
heli_processed <- 
  read_csv("data/heli.csv") %>% 
  mutate(breaks = case_when(
    timestamp - lag(timestamp) > 300 ~ 1, 
    TRUE ~ 0)) %>% 
  mutate(flightnum = cumsum(breaks)) %>% 
  dplyr::select(-breaks) %>% 
  mutate(
    timestamp = as_datetime(start_time + as.numeric(timestamp), tz = "America/New_York"),
    timestr = format_ISO8601(timestamp))

# model the relationship between pressure and geometric altitude for helicopters between 9pm and 11pm
# note that this method of adjusting the pressure altitude is only intended for aircraft flying at low altitude
altitude_model <- lm(alt_geom~altitude, data=filter(heli_processed, timestamp > ymd_h("2020-06-02 01") & timestamp < ymd_h("2020-06-02 03")))
altitude_model_fun <- makeFun(altitude_model)
# y-intercept is 152.2 feet

# alternate (simpler) approach yields 154 feet as the mean difference between pressure and geometric altitude
# this gives us pretty much the same result as above and is easier to explain
heli_processed %>% 
  filter(timestamp > ymd_h("2020-06-02 01") & timestamp < ymd_h("2020-06-02 03") & !is.na(alt_geom)) %>% 
  mutate(pressure_geometric_altitude_difference = as.numeric(alt_geom) - as.numeric(altitude)) %>%
  summarise(mean(pressure_geometric_altitude_difference))

# The ADS-B geometric altitudes are relative to the WGS84 ellipsoid 
# Our ground elevation data uses the NAVD88 vertical datum 
# So we need to add an offset to correct for the height of the geoid
# I used this NOAA tool https://vdatum.noaa.gov/vdatumweb/vdatumweb?a=171402120200617
# to convert WGS84(transit) to NAVD88
# I used the coordinates -77.029395, 38.895809 
# This results in a datum shift of 32.059 m (these offsets don't vary much over a small area)
# For a sanity check, the National Geodetic Survey has a data sheet for the Jefferson Pier survey point https://www.ngs.noaa.gov/cgi-bin/ds_mark.prl?PidBox=UA0024
datum_shift <- 32.059 * 3.28084

heli_processed_altitude <-
  heli_processed %>% 
  mutate(
    altitude_modeled = altitude_model_fun(altitude) + datum_shift,
    altitude_adjusted = altitude + 154 + datum_shift,
    altitude_modeled_meters = altitude_modeled / 3.28084,
    altitude_adjusted_meters = altitude_adjusted / 3.28084
  )
  
# convert to 2D spatial data (points)
heli_points <- st_as_sf(heli_processed_altitude, coords = c("lon", "lat")) %>% 
  st_set_crs(4326)

# find the ground elevation in meters at each point
bare_earth <- raster("~/Downloads/BARE_EARTH_2015/BareEarth2015.tif")
heli_points$bare_earth_elevation <- raster::extract(bare_earth, heli_points, method='simple')

# convert to feet for comparison with altitude
heli_points <- mutate(
  heli_points,
  bare_earth_elevation_feet = bare_earth_elevation * 3.28084,
  dsm_elevation_feet = dsm_elevation * 3.28084
)

# convert to 3D spatial data (points)                   
heli_points3d <- st_as_sf(heli_processed_altitude, coords = c("lon", "lat", "altitude_adjusted_meters")) %>% 
  st_set_crs(4326)

# exports as geojson
st_write(heli_points, "data/heli_points.geojson", delete_dsn=TRUE)

# convert to 3D spatial data (lines)
heli_lines <- group_by(heli_points3d, code.x, flightnum) %>% 
  summarise(do_union=FALSE) %>% 
  st_cast("LINESTRING")

st_write(heli_lines, "data/heli_lines.geojson", delete_dsn=TRUE)

# create a KML with 3D lines for the time period we are focusing on 
# Note: this can be opened in Google Earth and with some modifications you can view the lines in 3D
filter(heli_points3d, code.x == "AE1F45" & 
         timestamp > ymd_hm("2020-06-02 01:50") &
         timestamp <  ymd_hm("2020-6-02 02:11")) %>% 
  group_by(code.x, flightnum) %>% 
  summarise(do_union=FALSE) %>% 
  st_cast("LINESTRING") %>% 
  st_write("data/heli_lines_select.kml", delete_dsn=TRUE)

# creates the geojson we are using for the maps in the article
filter(heli_points, code.x == "AE1F45" & timestamp > ymd_hm("2020-06-02 01:45") &
         timestamp <  ymd_hm("2020-6-02 02:11")) %>% 
  select(timestamp, code.x) %>% 
  mutate(timestamp = paste0(format_ISO8601(with_tz(timestamp, "UTC"), precision=NULL), ".000Z")) %>% 
  st_write("data/heli_points_prod.geojson", delete_dsn=TRUE)

# create a 3D KML for the entire flight of this Lakota
filter(heli_points3d, code.x == "AE1F45") %>% 
  group_by(code.x, flightnum) %>% 
  summarise(do_union=FALSE) %>% 
  st_cast("LINESTRING") %>% 
  st_write("data/heli_lines_AE1F45.kml", delete_dsn=TRUE)

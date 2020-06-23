library(tidyverse)
#library(whitebox)
library(rayshader)
#library(rgdal)
library(raster)
#library(leaflet)
library(lubridate)
library(sf)


#Load 3D surface
#rayshader::raster_to_matrix("tif/dc_dsm_patched.tif") -> dc_mat2
dc_mat2 <- rayshader::raster_to_matrix("tif/mosaicfilled.tif") 
dc_mat2[is.na(dc_mat2)] = 0

reduced_dc_matrix = rayshader::reduce_matrix_size(dc_mat2,0.3)
reduced_dc_matrix %>% 
  sphere_shade() %>%
  add_shadow(ray_shade(reduced_dc_matrix),0.5) ->
  hillshade

#Plot hillshade
plot_map(hillshade)

# Load processed geojson of data
points <- read_sf("data/heli_points.geojson")

# Decide which aircraft to focus on
points_dc <- points %>% 
  filter(code.x=="AE1F45") %>% 
  mutate(day=day(timestamp)) %>% 
  mutate(hour=hour(timestamp)) %>% 
  mutate(minute=minute(timestamp)) 

# filtering to June 1 9:50 p.m.
points_dc1 <- points_dc %>% 
  filter(day==1) %>% 
  filter(hour==21) %>% 
  filter(minute>=50 & minute <=59)

# filtering to June 1 before 10:10 p.m.
points_dc2 <- points_dc %>% 
  filter(day==1) %>% 
  filter(hour==22) %>% 
  filter(minute>=00 & minute <=59)

# joining June 1 and June 2
points_dc <- rbind(points_dc1, points_dc2)

lon_lat_array <- st_coordinates(points_dc)
# creating an extent of the raster

dc_extent <- raster::raster("tif/mosaicfilled.tif")
  

sputm <- SpatialPoints(dc_extent, proj4string=CRS("+init=epsg:26985")) 
spgeo <- spTransform(sputm, CRS("+proj=longlat +datum=WGS84"))

dc_extent <- raster::extent(spgeo)

#Plot in 3D
plot_3d(hillshade,reduced_dc_matrix, triangulate = TRUE, 
        max_error = 1, zoom=.4, zscale=1, verbose=TRUE,
        shadow=TRUE)

# add path of helicopter to 3D plot
render_path(extent = dc_extent,
            lat=lon_lat_array[,2], lon=lon_lat_array[,1],
            altitude=points_dc$altitude_adjusted_meters,
            linewidth=1, 
            color="orange", antialias=TRUE)


render_movie(filename = "lakota.gif", type = "oscillate", 
             frames = 360,  fps=30, phi = 30, zoom = 0.4, theta = -90,
             title_text = "Lakota path on June 1 from 9:50 to 11 p.m.")

#render_camera(theta = -90, phi = 30,zoom = 0.4)



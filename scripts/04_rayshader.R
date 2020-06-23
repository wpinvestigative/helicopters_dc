library(whitebox)
library(rayshader)
library(rgdal)
library(raster)

library(leaflet)
library(lubridate)
library(sf)


#Load 3D surface
rayshader::raster_to_matrix("dc_dsm_patched.tif.tif") -> dc_mat2
dc_mat2[is.na(dc_mat2)] = 0
reduced_dc_matrix = rayshader::reduce_matrix_size(dc_mat2,0.3)
reduced_dc_matrix %>% 
  sphere_shade() %>%
  add_shadow(ray_shade(reduced_dc_matrix),0.5) ->
  hillshade

#Plot hillshade
plot_map(hillshade)

#Plot in 3D
plot_3d(hillshade,reduced_dc_matrix, triangulate = TRUE, max_error = 1, zscale=2,verbose=TRUE)





# Load processed geojson of data
points <- read_sf("data/heli_points.geojson")

# Adjust knots to miles per hour

points$speed <- points$speed * 1.15

# Decide which aircraft to focus on
points_dc <- points %>% 
  filter(code.x=="AE1F45")
  mutate(day=day(timestamp)) %>% 
  mutate(hour=hour(timestamp)) %>% 
  mutate(minute=minute(timestamp)) 

# filtering to June 1
points_dc1 <- points_dc %>% 
  filter(day==1)

# filtering to June 2 before 4 am
points_dc2 <- points_dc %>% 
  filter(day==2) %>% 
  filter(hour<4) 

# joining June 1 and June 2
points_dc <- rbind(points_dc1, points_dc2)
### SACOG Mobility Hubs
### Extract and process OSM data (to develop walkability index)
### 13-Feb-2024
### v1 - on Git

#----------------------00 Setup-----------------

#load required packages
require(tidyverse)
require(sf)
require(osmextract)
require(mapview)

require(sfnetworks)
require(tidygraph)

#----------------------01 Read Tigris-----------------

yolo <- tigris::roads(state = 'CA', county = 'Yolo')

#see road types
mapview(yolo, zcol = 'RTTYP')

net$tiger <- yolo %>% 
  filter(RTTYP %in% c('M', 'C')) %>% #only common names and county roads (exclude NA, US, Interstate, State)
  as_sfnetwork(directed = F) %>% 
  #connect relevant pseudonodes
  convert(to_spatial_subdivision) %>% 
  #simplify duplicates
  convert(to_spatial_simple) %>% 
  activate('nodes') %>% 
  #calc edges per node
  mutate(deg = centrality_degree()) %>% 
  #exclude curve nodes, etc.
  filter(deg>2) %>% 
  #make legible
  as_tibble() %>% 
  st_as_sf() %>% 
  st_transform(2226)
  
mapview(net$tiger, zcol = 'deg')


#TIGER intersections plus Class 1 trail access points (indicating intersecton of road/path)
net$tiger_class1 <- net$tiger %>% 
  bind_rows(lyr$Class_1Trails_Access_Points %>% 
              st_transform(2226)) %>% 
  mutate(source = if_else(is.na(.tidygraph_node_index),
                          'Class 1 Trails Access Points',
                          'TIGER Roads'))

mapview(net$tiger_class1, zcol = 'source')

#intersection density
lyr$walkability <- lyr$block_groups %>%
  st_transform(2226) %>% 
  filter(Countyname == 'Yolo County') %>% 
  mutate(num_intersections = lengths(st_intersects(geometry, net$tiger_class1)),
         intersection_density = num_intersections/area,
         intersection_density_unitless = as.numeric(intersection_density))


lyr$walkability %>% 
  mapview(layer.name = 'walkability (intersections/mi^2)',
          zcol = 'intersection_density_unitless',
          col.regions = RColorBrewer::brewer.pal(5, 'YlGn'),
          at = quantile(.$intersection_density_unitless))

#next steps, add multi-use path entry points, and bang - list of points for pedestrian connectivity calculation. 
# can we now join x = tracts to y = points, no need for spatial join just count number of intersections (ie like lenghts(st_intersection))

# divide num intersections by tract area for unit intersection density

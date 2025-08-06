### SACOG Mobility Hubs
### Process SACOG Open Data
### 13-Feb-2024
### v1 - on Git

#----------------------00 Setup-----------------

#load required packages
require(tidyverse)
require(sf)
require(tigris)
require(mapview)

#----------------------01 Input-----------------

sacog_data_path <- 'input/sacog_open_data/'

input <- vector('list')

#read from disk
input$open <- list.files(sacog_data_path) %>% 
  set_names(\(x) str_split_i(x, '\\.', 1)) %>% 
  map(\(x) st_read(str_c(sacog_data_path, x))) %>% 
  map(\(x) if (any(class(x) == 'sf')) st_make_valid(x) else x)

#ev data table as sf object
input$open$ev <- input$open[['alt_fuel_stations (Feb 9 2024)']] %>% 
  mutate(lon = as.numeric(Longitude),
         lat = as.numeric(Latitude)) %>% 
  st_as_sf(coords = c('lon', 'lat'),
           crs = st_crs(input$open$Counties)) %>% 
  st_filter(input$open$Counties) #restrict to only ev charging stations in SACOG counties

#----------------------02 Process-----------------

#----------------------|- Initial exploration-----------------
#remove data table to retain only spatial layers
lyr <- input$open %>% 
  within(rm('alt_fuel_stations (Feb 9 2024)')) #WOW COOL LINE OF CODEEE

#create list of attributes to symobologize
zcol_list <- c('STA_NAME', 'DESCRIPTION', 'TrailName', 'COUNTY_NAM', 'ACTIVE', 'CLASS', 'BIKE_CLASS', 'BIKE_CLASS', 'Access.Code') %>% 
  set_names(names(lyr))

maps <- lyr[1:length(zcol_list)] %>% 
  imap(\(x, idx) mapview(x, 
                         layer.name = idx,
                         zcol = zcol_list[[idx]])) %>% 
  reduce(`+`)

maps

##filter out local roads from major roads
##filter or classify "safe" bike routes (which class corresponds to safe/accessibile to all riders)
# see if SACOG have plans to include major transit corridors
#incorporate high injury / priority vision zero data
#osm major destinations


# Include layer below:
# https://data.sacog.org/datasets/SACOG::high-frequency-transit-area-mtp-scs-2020/explore?location=38.615703%2C-121.350025%2C10.77

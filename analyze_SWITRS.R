### SACOG Mobility Zones ###
### Identify Focus Areas ###
### Replica OD Analysis ###
### NDL 1-Apr-2025 ###

#-------00 Packages---------

library(tidyverse)
library(sf)
library(mapview)
library(tigris)

#-------01 Inputs---------

#SACOG counties
counties <- c('Yuba', 
              'Placer',
              'Sutter',
              'El Dorado',
              'Yolo',
              'Sacramento')

#input working dir
iwd <- 'input/SWITRS/' 

#SWITRS crash data
crashes <- list.files(iwd, pattern = '.csv') %>% 
  set_names(\(x) str_remove(x, '.csv')) %>% 
  map(\(x)
      read_csv(str_c(iwd, x))
  )

#-------02 Process---------

crashes_lyr <- crashes %>% 
  map(\(x) x %>% 
        select(CASE_ID, 
               ACCIDENT_YEAR, 
               COLLISION_DATE, 
               COLLISION_TIME, 
               PRIMARY_RD, 
               SECONDARY_RD, 
               INTERSECTION, 
               WEATHER_1,
               COLLISION_SEVERITY,
               NUMBER_KILLED,
               NUMBER_INJURED,
               PEDESTRIAN_ACCIDENT,
               BICYCLE_ACCIDENT,
               MOTORCYCLE_ACCIDENT,
               TRUCK_ACCIDENT,
               POINT_X,
               POINT_Y)
      )%>% 
  bind_rows(.id = 'county') %>% 
  mutate(COLLISION_SEVERITY_TXT =
           case_match(COLLISION_SEVERITY,
                      1 ~ 'Fatal',
                      2 ~ 'Injury (Severe)',
                      3 ~ 'Injury (Other Visible)',
                      4 ~ 'Injury (Complaint of Pain)'),
         COLLISION_SEVERITY_GRP =
           case_match(COLLISION_SEVERITY,
                      c(1,2) ~ 'severe',
                      c(3,4) ~ 'other')
  ) %>%
  st_as_sf(crs = st_crs(4326),
           coords = c('POINT_X', 'POINT_Y'))

mapview(crashes_lyr, zcol = 'COLLISION_SEVERITY_GRP')

#-------03 Output---------

st_write(crashes_lyr, 'output/crashes_2020-2024.shp')

write_csv(st_drop_geometry(crashes_lyr),
          'output/crashes_2020-2024.csv')
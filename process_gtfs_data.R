### SACOG Mobility Hubs
### Process GTFS data from SACOG operators
### 13-Feb-2024
### v1 - on Git

#----------------------00 Setup-----------------

#load required packages
require(tidyverse)
require(sf)
require(tigris)
require(tidytransit)
require(mapview)

#----------------------01 Input-----------------

gtfs_path <- 'input/gtfs/gtfs_downloads/'

input$gtfs <- list.files(gtfs_path, pattern = 'schedule') %>% 
  set_names(\(x) str_split_i(x, '\\.', 1)) %>% 
  map(\(x) read_gtfs(str_c(gtfs_path, x)))
#issue with placer flex. use placer schedules

## STOP

##----------------------|-Fix Auburn Schedules----------------

#Update Auburn caldendar dates to replace duplicated vals
input$gtfs$auburn_schedule$calendar_dates <- input$gtfs$auburn_schedule$calendar_dates %>% 
  mutate(date = if_else(duplicated(date),
                 '20241129',
                 date))

input$gtfs$auburn_schedule <- input$gtfs$auburn_schedule %>% 
  as_tidygtfs()

#Update Auburn schedules so that necessary cols are numeric
input$gtfs$auburn_schedule$shapes <- input$gtfs$auburn_schedule$shapes %>% 
  mutate(across(c(shape_pt_lat, shape_pt_lon, shape_dist_traveled),
                as.numeric),
         across(shape_pt_sequence,
                as.integer))

input$gtfs$auburn_schedule$stops <- input$gtfs$auburn_schedule$stops %>% 
  mutate(across(c(stop_lat, stop_lon),
                as.numeric),
         across(c(location_type, wheelchair_boarding),
                as.integer))

test <- input$gtfs$unitrans_davis_schedule %>% 
  filter_feed_by_trips()

##----------------------02 GTFS Processing-----------------

gtfs <- vector('list')

#build master gtfs dataset as sf, interpolate stop times from schedules/shape dist
gtfs$raw <- input$gtfs %>%
  map(gtfs_as_sf) %>% 
  map(interpolate_stop_times)
  
#summarize operator by service pattern, review sensibility of n_[dates/routes/trips]
gtfs$patterns <- gtfs$raw %>%
  map(set_servicepattern) %>% 
  map(\(x) x$.$dates_servicepatterns %>% 
        left_join(x$.$servicepatterns,
                  join_by(servicepattern_id),
                  relationship = 'many-to-many') %>% 
        left_join(x$trips,
                  join_by(service_id),
                  relationship = 'many-to-many') %>% 
        summarise(n_trips = n_distinct(trip_id),
                  n_routes = n_distinct(route_id),
                  n_dates = n_distinct(date),
                  .by = servicepattern_id))

#before stop frequency analysis, select representative service pattern (weekday?)  
gtfs$weekday <- gtfs$raw %>% 
  map(\(x) pluck(x$.$dates_services) %>% 
        filter(date == '2024-03-05') %>% pull(service_id))

#functions to clean
bind_gtfs_records <- \(x) bind_rows(x, .id = 'operator')

lyr$gtfs_routes <- gtfs$raw %>% 
  map(\(x) pluck(x, 'shapes')) %>% 
  bind_gtfs_records() %>% 
  st_filter(lyr$Counties)

lyr$gtfs_stops <- gtfs$raw %>% 
  map(\(x) pluck(x, 'stops')) %>% 
  bind_gtfs_records() %>% 
  st_filter(lyr$Counties)

##----------------------|-Get Route Frequency----------
# BELOW - NEED TO CONNECT SHAPES TO ROUTES. SUMMARISE MULTIPLE LINESTRINGS BY ROUTE ID? NECESSARy
lyr$gtfs_route_freq <- gtfs$raw %>%
  imap(\(x, idx) get_route_frequency(x,
                               start_time = '06:00:00',
                               end_time = '10:00:00', 
                               service_ids = pluck(gtfs$weekday, idx)) %>% 
         left_join(pluck(gtfs, 'raw', idx, 'trips') %>% 
                     filter(service_id %in% pluck(gtfs$weekday, idx)),
                   join_by(route_id))) %>%
  map(\(x) mutate(x, 
                  across(any_of(c('direction_id', 'bikes_allowed', 'wheelchair_accessible')),
                         as.integer))) %>% 
  bind_gtfs_records() %>%
  mutate(mean_headways_mins = mean_headways/60) %>%
  #joining spatial data
  inner_join(lyr$gtfs_routes,
            join_by(operator, shape_id)) %>%
  st_as_sf() %>%
  #count number of trips in route. maybe best to do before line 108? (before joining routes - so that only most freq route is joined...)
  mutate(shape_count = n(),
         .by = c(operator, route_id, shape_id)) %>% 
  arrange(operator, route_id, shape_count) %>% #to get most frequent shape up top
  select(-trip_id) %>%  #only needed trips to count trips per shape
  slice_head(by = c(operator, route_id, shape_id)) %>% #unitrans routes span multiple shapes, check others
  mutate(services_per_hour = 60/mean_headways_mins) %>% #update 4 to be dynamic based on user input above
  summarise(across(geometry,
                   st_union),
            across(everything(),
                   first),
            .by = c(operator, route_id))
  

table(lyr$gtfs_route_freq$operator) #could be nice to summarise routes by operator, to show which has most am service
#how to visually show "trunk" routes on map

##----------------------|-Get Stop Frequency----------------

#need to determine appraoch for grouping near stations. might not be relevant if we are just connecting to census tracts later
lyr$gtfs_stop_freq <- gtfs$raw %>%
  imap(\(x, idx) get_stop_frequency(x, 
                                    start_time = '06:00:00',
                                    end_time = '10:00:00', 
                                    service_ids = pluck(gtfs$weekday, idx),
                                    by_route = FALSE)) %>% #use stop clusters, 6am:10pm
  bind_gtfs_records() %>% 
  mutate(mean_headway_mins = mean_headway/60,
         services_per_hour = n_departures/4, #4 hr time window, update to be dynamic
         size = case_match(operator,
                           'flixbus_schedule' ~ 0.5,
                           'amtrak_schedule' ~3,
                           .default = 1)) %>% 
  #joining spatial data
  inner_join(lyr$gtfs_stops,
            join_by(operator, stop_id)) %>% 
  st_as_sf() 
#unitrans ends up with funky estimates because loop routes have end-of-route coded into middle of route (when out/inbound signs switch)
  
#consider adding buses/trains per hour (multiply headway in minutes by (1/60))
#missing stops? unitrans as example
#perhaps best to count "trips" in time period and divide by time period unit. attach to stops
#consider delting flixbus (amtrak already deleted)

mapview(lyr$gtfs_stop_freq, zcol = 'services_per_hour', cex = 'size', layer.name = 'mean services per hour, Tues AM peak', at = c(0, .5, 1, 2, 4, 8, 16, Inf))

mapview(lyr$gtfs_route_freq, zcol = 'services_per_hour', layer.name = 'mean services per hour, Tues AM peak', at = c(0, .5, 1, 2, 4, 8, 16, Inf)) #clean up

#considering creating function that adjusts "stop times" to be NA (arrival) at Seq 1 and NA (departure) at last Seq (using min and max stop_sequence)

#remove certain low-frequency stations if they are surrounded by high-fgrequency stations (set our defined threshold) - these weigh signigicantly less. if they are more isolated from other stations, keep them.
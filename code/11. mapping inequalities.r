# Mapping inequalities

# Summary tables
# Probs one for patient journey related/demographic
# Another with a deep dive into dental data

library(tidyverse)
library(gtsummary)
library(lubridate)
library(osrm)
library(httr)
library(jsonlite)
library(PostcodesioR)
library(openrouteservice)
library(sf)

source('999-DONOTSHARE.r')

event_df <- readRDS('data/event_log_df.rds')
dental_df <- readRDS('data/all_iuc_dental_with_LSOA_df.rds')

# Convert LSOA and treatment postcode to Lat/Lon -----------


trt_pc <- dental_df %>% distinct(TRT_LOCATION_POSTCODE) %>% pull(TRT_LOCATION_POSTCODE)


lat_lon_pc_fn <- function(postcode) {
  print(glue::glue("Procssing {postcode}"))
  if(!is.na(postcode)) {
    tryCatch(
      {
        df <- postcode_lookup(postcode)
        df %>% write_csv('data/lat_lon_pc.csv', append = T)
      },
      error = function(e) {
        print(glue::glue("Error Procssing {postcode} {e$message"))
      }
    )
    
    Sys.sleep(5)
  }
  
}

#map(trt_pc, lat_lon_pc_fn)

treatment_centre_df <- read_csv('data/lat_lon_pc.csv') %>%
  distinct(postcode, latitude, longitude)


# LSOA centroids -------------


lsoa_centroid_lu_df <- read_csv("data/LLSOA_Dec_2021_PWC_for_England_and_Wales_2022_1028145039677403461.csv")
dental_lsoa <- dental_df %>% distinct(LSOA)
dental_lsoa %>% count() # 642

lsoa_lat_lon <- st_as_sf(lsoa_centroid_lu_df, coords = c("x", "y"), crs = 27700) %>%
  st_transform(crs = 4326) 

lsoa_coords <- st_coordinates(lsoa_lat_lon)

final_centroid_df <- lsoa_lat_lon %>%
  mutate(
    lat = lsoa_coords[,2],
    lon = lsoa_coords[,1]
  )


# Every unique journey between centroid and treatment

unique_journey_df <- dental_df %>%
  filter(dental_contact == 'yes') %>%
  count(LSOA, TRT_LOCATION_POSTCODE, sort = T) %>%
  na.omit() %>%
  filter(LSOA != "")

uj_df1 <- unique_journey_df %>%
  left_join(final_centroid_df, by=c("LSOA"="LSOA21CD")) %>%
  rename(
    start_lat = lat,
    start_lon = lon
  ) %>% left_join(treatment_centre_df, by=c("TRT_LOCATION_POSTCODE"="postcode")) %>%
  rename(
    end_lat = latitude,
    end_lon = longitude
  ) %>% select(-FID, -GlobalID, -geometry)

#uj_df1 %>% saveRDS('data/uj_df1.rds')
uj_df1 <- readRDS('data/uj_df1.rds')


get_dist_fn <- function(start_lat, start_lon, end_lat, end_lon, append = T) {
  
  tryCatch({
    if(!is.na(start_lat) & !is.na(start_lon) & !is.na(end_lat) & !is.na(end_lon)) {
      
      coords <- list(
        c(start_lon, start_lat),
        c(end_lon, end_lat)
      )
      
      car <- ors_directions(coordinates = coords, profile = "driving-car")
      Sys.sleep(1)
      walk <- ors_directions(coordinates = coords, profile = 'foot-walking')
      
      result <- tibble(
        start_lat = start_lat,
        start_lon = start_lon,
        end_lat = end_lat,
        end_lon = end_lon,
        car_dist = car$features[[1]]$properties$summary$distance,
        car_time = car$features[[1]]$properties$summary$duration,
        walk_dist = walk$features[[1]]$properties$summary$distance,
        walk_time = walk$features[[1]]$properties$summary$duration
      )
      
      result %>% write_csv('data/dist_time.csv', append = append)
      
      Sys.sleep(1)
    }
  }, error = function(e) {
    print('Error')
  }
  )
  
}

# Get first result to set up the CSV file
#get_dist_fn(uj_df1$start_lat[1], uj_df1$start_lon[1], uj_df1$end_lat[1], uj_df1$end_lon[1], F)

# Iterate through all of the remaining lat/lon start/end points



dist_time_df <- read_csv('data/dist_time.csv')
dist_time_df %>% count()

# There is a daily limit on requests, so filter by queries that have already been run.
uj_df2 <- uj_df1 %>%
  anti_join(dist_time_df, by=c('start_lat', 'end_lat', 'start_lon', 'end_lon'))
uj_df2 %>% count()


pwalk(
  .l = list(
    start_lat = uj_df2$start_lat,
    end_lat   = uj_df2$end_lat,
    start_lon = uj_df2$start_lon,
    end_lon   = uj_df2$end_lon
  ),
  .f = get_dist_fn
)

# Map distance -----------

dist_df <- read_csv('data/dist_time.csv')

combo_df <- uj_df1 %>%
  left_join(dist_df, by=c("start_lat", "end_lat", "start_lon", "end_lon"))

dental_df %>% count() # 7050'

final_dental_df <- dental_df %>%
  left_join(combo_df, by=c("LSOA", "TRT_LOCATION_POSTCODE")) %>%
  distinct() %>%
  mutate(
    across(ends_with('_time'), ~.x/60),
    across(ends_with('_dist'), ~ .x/1000)
  )


final_dental_df %>% count() # 7050

final_dental_df %>% glimpse()


final_dental_df %>% saveRDS('data/final_dental_df.rds')

final_dental_df <- readRDS('data/final_dental_df.rds')


# Quick calculation for IMD vs dist -----------------

imd_df <- final_dental_df %>%
  filter(!is.na(dental)) %>%
  select(index_call_ref, index_call_date, index_imd_decile, ends_with('_dist'), ends_with('_time'), index_ethnicity) %>%
  rename(IMD = index_imd_decile, ethnicity = index_ethnicity) %>%
  mutate(
    IMD = factor(IMD, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")),
    ethnicity = 
  ) 

imd_df %>% saveRDS('data/imd_df.rds')

imd_df %>%
  group_by(IMD) %>%
  summarise(
    n = n(),
    across(everything(), \(x) median(x, na.rm = T))
  )

imd_df %>%
  na.omit() %>%
  pivot_longer(-IMD) %>%
  ggplot(aes(x = IMD, y = value, fill = IMD)) +
  geom_boxplot(outlier.shape = NA) +
  ylim(c(0, 100)) +
  facet_wrap(~name, scales = "free_y") +
  theme_minimal()



# Grab dental practices for typing ----------

dental_df %>% count(TRT_LOCATION_POSTCODE, sort = T) %>%
  filter(!is.na(TRT_LOCATION_POSTCODE)) %>%
  write_csv('data/dental_pc.csv')

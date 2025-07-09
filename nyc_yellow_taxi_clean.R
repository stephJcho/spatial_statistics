
##########################################################
## Data Cleaning File for NYC Yellow Taxi Trip Data ######
##########################################################

# install.packages("arrow")
library(arrow)
library(dplyr)

## downloaded from NYC Trip Record Data
df <- read_parquet("yellow_tripdata_2024-08.parquet")
zone <- read.csv("taxi_zone_lookup.csv")

## downloaded from https://data.cityofnewyork.us/Transportation/NYC-Taxi-Zones/d3c5-ddgc
zone_coord <- read.csv("taxi_zones.csv")

##########################################################
###### 1. Make sure the data from another source (zone_coord) matches the zone look up

consistency_check <- zone %>%
  inner_join(zone_coord, by = "LocationID") %>%
  filter(Borough != borough | Zone != zone)
consistency_check #length is zero, zone and zone_coord are consistent. 

##########################################################
###### 2. Removing Duplicates in zone_coord and 
######  & handling missing data (out of NYC bounds)

# View(zone_coord[which(zone_coord$OBJECTID != zone_coord$LocationID),])

## fixing the mislabeled rows 
zone_coord <- zone_coord %>%
  mutate(LocationID = ifelse(OBJECTID != LocationID, OBJECTID, LocationID))

## In both PU and DO, 265 264 LocationIDs are included, but these are outside of NYC 
## remove the entries with these location IDs from temp
out_of_nyc_ids <- c(264, 265)

# Filter out rows with PULocationID or DOLocationID as 264 or 265 from df before sampling
df_filtered <- df %>%
  filter(!(PULocationID %in% out_of_nyc_ids | DOLocationID %in% out_of_nyc_ids))


##########################################################
###### 3. Merging the zone data withe yellow taxi data 

## only keep the location ID and geom information from the zone_coord
zone_coord_unique <- zone_coord %>% select(LocationID, the_geom)

temp <- df_filtered %>%
  left_join(zone_coord_unique, by = c("PULocationID" = "LocationID")) %>%
  rename(PU_Location = the_geom) %>%
  left_join(zone_coord_unique, by = c("DOLocationID" = "LocationID")) %>%
  rename(DO_Location = the_geom)



##########################################################
###### 4. turning the spatial data into an sf object and save as csv

library(sf)
index <- sample(1:nrow(temp), 1000000, replace = FALSE)
temp <- temp[index,]
temp$PU_Location <- st_centroid(st_as_sfc(temp$PU_Location))
temp$DO_Location <- st_centroid(st_as_sfc(temp$DO_Location))

# Extract coordinates from centroids
pickup_coords <- st_coordinates(temp$PU_Location) %>%
  as.data.frame() %>%
  rename(PU_Latitude = Y, PU_Longitude = X)

dropoff_coords <- st_coordinates(temp$DO_Location) %>%
  as.data.frame() %>%
  rename(DO_Latitude = Y, DO_Longitude = X)

# Bind coordinates and remove original geometry columns
temp <- temp %>%
  bind_cols(pickup_coords, dropoff_coords) %>%
  select(-PU_Location, -DO_Location)


write.csv(temp, "cleaned_NYC_Yellow_Cab_2024-08.csv", row.names = FALSE)










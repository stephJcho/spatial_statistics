library(wavelets)
library(reshape2)
library(tidyverse)
library(lubridate)
library(basemapR)
library(sf)
library(ggspatial)
library(prettymapr)
library(ggplot2)
library(igraph)
library(geosphere)
library(ggraph)

# This file is a compilation of codes from other files to produce plots for the final report
# Refer to other files from each steps of the project for details

data <- read.csv("clustered_NYC_Yellow_Cab_with_clusters_fin.csv")
head(data)
#summary(data$trip_distance)

### setup for weekday plot

Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "C") #to get weekdays in English
#Sys.setlocale("LC_TIME", "Korean_Korea.utf8") #to reset
weekdays(Sys.Date()+0:6) #for double check

### Create pickup_data df with the final dataset

pickup_data <- data %>% select(PU_Longitude, PU_Latitude, tpep_pickup_datetime, trip_id)

pickup_data <- pickup_data %>%
  mutate(
    pickup_hour = hour(tpep_pickup_datetime),
    pickup_day = wday(tpep_pickup_datetime, label = TRUE)
  )

### Plots for EDA
zoneshp <- st_read("taxi_zones.shp")
zoneshp <- st_as_sf(zoneshp)
PUcounts <- table(data$PULocationID)
PUcounts <- as.data.frame(PUcounts)
colnames(PUcounts) <- c("LocationID", "PUCounts")
zoneshp_2 <- merge(zoneshp, PUcounts, by = "LocationID", all = TRUE)

ggplot()+
  annotation_map_tile(type = "osm", zoom = 12) +
  geom_sf(data = zoneshp_2, aes(fill = PUCounts))+
  scale_fill_distiller(palette = "Spectral")+
  labs(title = "NYC Yellow Cab Pickup Counts by Taxi Zone",
       x = "Longitude", y = "Latitude") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

topPU <- as.data.frame(top_n(zoneshp_2, 10, PUCounts)[,5:7])
topPU

zoneshp_2 %>% filter(is.na(PUCounts))

DOcounts <- table(data$DOLocationID)
DOcounts <- as.data.frame(DOcounts)
colnames(DOcounts) <- c("LocationID", "DOCounts")
zoneshp_2 <- merge(zoneshp_2, DOcounts, by = "LocationID", all = TRUE)

ggplot()+
  annotation_map_tile(type = "osm", zoom = 12) +
  geom_sf(data = zoneshp_2, aes(fill = DOCounts))+
  scale_fill_distiller(palette = "Spectral")+
  labs(title = "NYC Yellow Cab Dropoff Counts by Taxi zone",
       x = "Longitude", y = "Latitude") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

topDO <- top_n(zoneshp_2, 10, DOCounts)[,5:8]
topDO

zoneshp_2 %>% filter(is.na(DOCounts))

ggplot(pickup_data, aes(x=as.factor(pickup_hour)))+
  geom_bar(color="black", fill="lightblue", alpha = 0.5)+
  labs(title = "Distribution of Yellow Taxi Pickup Counts by Hour")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(y="Pickup Counts", x="Hour(24h scale)")+
  geom_label(aes(label = after_stat(count)),stat = "count", nudge_y = 0.25)

ggplot(pickup_data, aes(x=as.factor(pickup_day)))+
  geom_bar(color="black", fill="darkblue", alpha = 0.5)+
  labs(title = "Distribution of Yellow Taxi Pickup Counts by Weekday")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(y="Pickup Counts", x="Day")+
  geom_label(aes(label = after_stat(count)),stat = "count", nudge_y = 0.25)

ggplot(pickup_data, aes(x=as.Date(tpep_pickup_datetime)))+
  geom_bar(color="darkred", fill="red", alpha = 0.3)+
  labs(title = "Daily Yellow Taxi Pickup Counts of August 2024")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

###

hourly_demand <- pickup_data %>%
  group_by(PU_Longitude, PU_Latitude, pickup_hour, pickup_day) %>%
  summarize(demand = n(), .groups = "drop")

demand_matrix <- hourly_demand %>%
  unite("location", PU_Longitude, PU_Latitude, remove = FALSE) %>% # Combine coordinates for unique location ID
  pivot_wider(names_from = c(pickup_hour, pickup_day), values_from = demand, values_fill = 0)

### temporal 
temporal_data <- as.matrix(demand_matrix[, -c(1:3)])

wavelet_coeffs <- apply(temporal_data, 1, function(row) {
  dwt(as.numeric(row), filter = "haar")  # Haar wavelet as an example
})

wavelet_matrix <- do.call(rbind, lapply(wavelet_coeffs, function(x) x@W))  # Extract wavelet coefficients
distances <- distm(cbind(demand_matrix$PU_Longitude, demand_matrix$PU_Latitude))
# edge list
edge_list <- which(lower.tri(distances), arr.ind = TRUE)  # Get row and column indices
edge_weights <- distances[lower.tri(distances)]  # Corresponding weights

# creating the graph 
spatial_graph <- graph_from_edgelist(edge_list, directed = FALSE)
E(spatial_graph)$weight <- edge_weights

# creating minimum spanning tree
mst_pickup <- mst(spatial_graph, weights = E(spatial_graph)$weight)
shortest_path_distances <- distances(mst_pickup, weights = E(mst_pickup)$weight)
dist_matrix <- as.dist(shortest_path_distances)

png("Pickup_spatial_heatmap_plot.png", width = 800, height = 600)
heatmap(as.matrix(dist_matrix), main = "Pickup Location Pairwise Distances in MST")
dev.off()


hc <- hclust(dist_matrix, method = "ward.D2")
num_clusters <- 5
spatial_clusters <- cutree(hc, k = num_clusters)

demand_matrix$spatial_cluster <- spatial_clusters
merged_data <- inner_join(pickup_data, demand_matrix, by = c("PU_Longitude", "PU_Latitude"))

png("Pickup_spatial_clustered.png", width = 500, height = 500)
ggplot(demand_matrix, aes(x = PU_Longitude, y = PU_Latitude, color = factor(spatial_cluster))) +
  geom_point(size = 1) +
  labs(
    title = "Spatial Clustering of Pickup Locations",
    x = "Longitude",
    y = "Latitude",
    color = "Cluster"
  ) +
  theme_minimal()
dev.off()

png("Pickup_time_clustered.png", width = 700, height = 400)
ggplot(merged_data, aes(x = pickup_hour, fill = factor(spatial_cluster))) +
  geom_bar(position = "dodge") +
  labs(
    title = "Demand by Hour and Cluster",
    x = "Hour of the Day",
    y = "Number of Pickups",
    fill = "Cluster"
  ) +
  theme_minimal()
dev.off()

################################################################################################################################################################
### Part 2: drop off demand ################################################################################################################################################################
################################################################################################################################################################

dropoff_data <- data %>% select(DO_Longitude, DO_Latitude, tpep_dropoff_datetime, trip_id)

dropoff_data <- dropoff_data %>%
  mutate(
    dropoff_hour = hour(tpep_dropoff_datetime),
    dropoff_day = wday(tpep_dropoff_datetime, label = TRUE)
  )

hourly_demand_dropoff <- dropoff_data %>%
  group_by(DO_Longitude, DO_Latitude, dropoff_hour, dropoff_day) %>%
  summarize(demand = n(), .groups = "drop")

demand_matrix_dropoff <- hourly_demand_dropoff %>%
  unite("location", DO_Longitude, DO_Latitude, remove = FALSE) %>%
  pivot_wider(names_from = c(dropoff_hour, dropoff_day), values_from = demand, values_fill = 0)

temporal_data_dropoff <- as.matrix(demand_matrix_dropoff[, -c(1:3)])

wavelet_coeffs_dropoff <- apply(temporal_data_dropoff, 1, function(row) {
  dwt(as.numeric(row), filter = "haar")
})

wavelet_matrix_dropoff <- do.call(rbind, lapply(wavelet_coeffs_dropoff, function(x) x@W))

distances_dropoff <- distm(cbind(demand_matrix_dropoff$DO_Longitude, demand_matrix_dropoff$DO_Latitude))
edge_list_dropoff <- which(lower.tri(distances_dropoff), arr.ind = TRUE)
edge_weights_dropoff <- distances_dropoff[lower.tri(distances_dropoff)]

spatial_graph_dropoff <- graph_from_edgelist(edge_list_dropoff, directed = FALSE)
E(spatial_graph_dropoff)$weight <- edge_weights_dropoff

mst_dropoff <- mst(spatial_graph_dropoff, weights = E(spatial_graph_dropoff)$weight)
shortest_path_distances_dropoff <- distances(mst_dropoff, weights = E(mst_dropoff)$weight)
dist_matrix_dropoff <- as.dist(shortest_path_distances_dropoff)

png("Dropoff_spatial_heatmap_plot.png", width = 800, height = 600)
heatmap(as.matrix(dist_matrix_dropoff))
dev.off()

hc_dropoff <- hclust(dist_matrix_dropoff, method = "ward.D2")
num_clusters <- 5
spatial_clusters_dropoff <- cutree(hc_dropoff, k = num_clusters)

demand_matrix_dropoff$spatial_cluster <- spatial_clusters_dropoff
merged_data_dropoff <- inner_join(dropoff_data, demand_matrix_dropoff, by = c("DO_Longitude", "DO_Latitude"))

png("Dropoff_spatial_clustered.png", width = 500, height = 500)
ggplot(demand_matrix_dropoff, aes(x = DO_Longitude, y = DO_Latitude, color = factor(spatial_cluster))) +
  geom_point(size = 1) +
  labs(
    title = "Spatial Clustering of Drop-off Locations",
    x = "Longitude",
    y = "Latitude",
    color = "Cluster"
  ) +
  theme_minimal()
dev.off()

png("Dropoff_time_clustered.png", width = 700, height = 400)
ggplot(merged_data_dropoff, aes(x = dropoff_hour, fill = factor(spatial_cluster))) +
  geom_bar(position = "dodge") +
  labs(
    title = "Demand by Hour and Drop-off Cluster",
    x = "Hour of the Day",
    y = "Number of Drop-offs",
    fill = "Cluster"
  ) +
  theme_minimal()
dev.off()

################################################################################################################################################################
### Part 3: match clusters ################################################################################################################################################################
################################################################################################################################################################
traffic_data <- data %>%
  left_join(merged_data, by = c("trip_id")) %>%
  left_join(merged_data_dropoff, by = c("trip_id"))

traffic_flow <- traffic_data %>%
  group_by(spatial_cluster.x, spatial_cluster.y) %>%
  summarize(trip_count = n(), .groups = "drop") %>%
  arrange(desc(trip_count))

library(ggraph)
traffic_flow$trip_count <- round(traffic_flow$trip_count)
flow_graph <- graph_from_data_frame(d = traffic_flow, directed = TRUE)

png("Flow_graph.png", width = 700, height = 400)
ggraph(flow_graph, layout = "fr") +
  geom_edge_link(aes(width = trip_count, color = trip_count), alpha = 0.8) +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  scale_edge_color_gradient(
    low = "lightblue",
    high = "darkblue",
    trans = "log",
    labels = scales::comma  # Keep original counts formatted with commas
  ) +
  scale_edge_width(range = c(0.5, 2), guide = "none") +
  labs(
    title = "Directed Traffic Flow Between Clusters",
    edge_color = "Trip Count"
  ) +
  theme_minimal()
dev.off()
library(sf)
library(tidyverse)
library(spatmap)
library(tidymodels)
library(dbscan)

df_bus <- st_read('combined_bus_data.geojson')

df_bus_coords <- bind_cols(df_bus, st_coordinates(df_bus)) %>%
  as_tibble() %>%
  select(X, Y, reliability)

df_bus_coords_norm <- df_bus_coords %>%
  mutate(
    across(
      everything(), ~scale(.) %>% as.vector
    )
  )
kclusts <- 
  tibble(k = 1:9) %>%
  mutate(
    kclust = map(k, ~kmeans(df_bus_coords_norm, .x)),
    tidied = map(kclust, tidy),
    glanced = map(kclust, glance),
    augmented = map(kclust, augment, df_bus_coords)
  )

assignments <- kclusts %>%
  unnest(cols = c(augmented))
p1 <- ggplot(assignments, aes(x = X, y = Y)) +
  geom_point(aes(color = .cluster), alpha = 0.8) +
  facet_wrap(~k)

clusterings <- 
  kclusts %>%
  unnest(cols = c(glanced))

ggplot(clusterings, aes(k, tot.withinss)) +
  geom_line() +
  geom_point()


col_pal <- colorFactor(
  palette = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99'),
  domain = c(1:5)
)

df_five <- clusterings %>% filter(k == 5) %>% unnest(cols = c(augmented)) %>%
  st_as_sf(coords = c('X', 'Y'))

df_five %>% group_by(.cluster) %>%
  summarize(mean = mean(reliability))

df_five %>%  
  leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(
      radius = 1,
      color = ~col_pal(.cluster),
      fillOpacity = .75
    ) %>%
    addLegend("bottomright", col_pal, values = ~.cluster,
              title = "5 Means Clustering")

df_five %>% 
  group_by(.cluster) %>%
  summarize(
    mean = mean(reliability)
  ) %>%
  as_tibble() %>%
  select(
  Cluster = .cluster,
  `Mean reliability` = mean
) %>%
  gt::gt() 
  # gt::tab_header(
  #   title = "Clusters and mean reliability"
  # )

cl <- hdbscan(df_bus_coords_norm %>% select(X,Y), minPts = 10)

plot(df_bus_coords_norm %>% select(X,Y), col = cl$cluster+1, pch = 20)

dbscan::kNNdistplot(df_bus_coords_norm, k =  5)

abline(h = 0.2, lty = 2)


centroid <- st_centroid(st_union(df_bus$geometry))
library(leaflet)
leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    data = centroid
  )


pt_dense <- data.frame(
  X = c(-87.627817),
  Y = c(41.882086)
) %>%
  st_as_sf(coords = c('X', 'Y'), crs = 4326)


dist_list <- c()
for (i in (1:nrow(df_bus))) {
  print(i)
  stop <- df_bus[i, 'Name']
  df_nn <- df_bus %>%
    filter(
      Name != stop$Name
    )
  
  nearest <- st_nearest_feature(stop, df_nn)
  
  dist <- st_distance(df_bus[i,], df_bus[nearest,])
  dist_list <- c(dist_list, dist)
  
}

df_dist <- data.frame(distance = dist_list) %>%
  filter(
    distance != max(distance)
  )

ggplot(df_dist, aes(x = distance)) +
  geom_density() +
  bbplot::bbc_style() +
  labs(
    title = "Density of bus stops' distance to nearest neighbor",
    
  )

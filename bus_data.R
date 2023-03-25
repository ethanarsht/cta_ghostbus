library(tidyverse)
library(sf)
library(spatialEco)
library(leaflet)

df_ghosts <- st_read('ghostbus_data/all_routes_2022-05-20_to_2023-02-15_wk_ratio_2023-02-17.json')

df_combined <- read_csv('ghostbus_data/combined_long_df_2022-11-06.csv')

df_stops <- st_read('ghostbus_data/stops/CTA_busStops-point.shp') %>%
  select(Name, routes = ROUTESSTPG, cta_id = SYSTEMSTOP) %>%
  distinct(
    Name, .keep_all = T
  ) %>%
  separate_rows(
    routes
  ) %>%
  as.data.frame()

df_combined <- df_ghosts %>%
  as_data_frame() %>%
  select(-geometry) %>%
  left_join(df_stops, by = c('route_id' = 'routes')) %>%
  st_as_sf() %>%
  drop_na(ratio) %>%
  drop_na(cta_id) %>%
  distinct(route_id, Name, .keep_all = T) %>%
  group_by(Name, geometry) %>%
  summarize(reliability = mean(ratio),
            count = n(),
            lines = paste(route_id, collapse = ",")
            ) %>%
  st_as_sf()

col_pal <- colorNumeric(c('#ef8a62','#67a9cf'),domain = df_combined$reliability)
df_combined %>%
  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(
    radius = 1,
    color = ~col_pal(reliability),
    fillOpacity = .75,
    popup = ~paste("Count: ", count, "<br>",
                   "Name: ", Name, "<br>",
                   "Mean reliability: ", round(reliability, 2), "<br>",
                   "Lines: ", lines)
  ) %>%
  addLegend("bottomright", col_pal, values = ~reliability,
            title = "Bus reliability by stop")

st_write(df_combined, 'combined_bus_data.geojson')

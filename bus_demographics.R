library(tidyverse)
library(sf)
library(tidymodels)
library(spatmap)
library(httr)
library(jsonlite)
library(leaflet)
library(spdep)
library(readxl)

### helper function

pif <- function(x, p, true, false = identity){
  if(!requireNamespace("purrr")) 
    stop("Package 'purrr' needs to be installed to use function 'pif'")
  
  if(inherits(p,     "formula"))
    p     <- purrr::as_mapper(
      if(!is.list(x)) p else update(p,~with(...,.)))
  if(inherits(true,  "formula"))
    true  <- purrr::as_mapper(
      if(!is.list(x)) true else update(true,~with(...,.)))
  if(inherits(false, "formula"))
    false <- purrr::as_mapper(
      if(!is.list(x)) false else update(false,~with(...,.)))
  
  if ( (is.function(p) && p(x)) || (!is.function(p) && p)){
    if(is.function(true)) true(x) else true
  }  else {
    if(is.function(false)) false(x) else false
  }
}

### data preparation
df_demo <- tibble()
for (s in excel_sheets("demographics/combined_demos.xlsx")[1:7]) {
  print(s)
  temp <- read_excel('demographics/combined_demos.xlsx', skip = 1, sheet = s) %>%
    select(`...1`, `2016-2020`) %>%
    slice(1:which(`...1` == 'Chicago')) %>%
    mutate(
      neigh = if_else(str_detect(`...1`, "[0-9]\\."), `...1`, NA)
    ) %>%
    fill(neigh) %>%
    drop_na(`2016-2020`) %>%
    pivot_wider(
      id_cols = neigh,
      names_from = `...1`,
      values_from = `2016-2020`
    ) %>%
    mutate(
      neigh = str_remove(neigh, "[0-9]*\\.") %>% str_trim() %>% str_to_upper(),
      across(2:ncol(.), function(x) {x = as.numeric(x)})
    )
  
  if (s != 'Income' & s != "Poverty" & s != 'Education') {
    temp <- temp %>%
      mutate(
        across(3:ncol(.), function(x) {x = x/Total})
      )
  }
  temp <- temp %>%
    select(-Total)
  if (ncol(df_demo > 1)) {
    if (s != "Income") {
      df_demo <- df_demo %>% left_join(temp)
    } else {
      df_demo <- df_demo %>% left_join(temp)
    }
  } else {
    df_demo <- temp
  }
}

df_demo <- df_demo %>% 
  mutate(
    `citizen` = coalesce(`U.S. citizen by birth`, `U.S. citizen`)
  )

df_population <- read_csv('demographics/neighborhood_populations.csv')

df_crime <- read_csv('demographics/crimes_neighborhood_year.csv') %>%
  mutate(
    `Community Areas` = as.character(`Community Areas`)
  )

geo_neigh <- st_read('demographics/geo_export_458a04a1-5369-4342-9247-aad47c881467.shp')

df_crime_nabe <- geo_neigh %>%
  left_join(df_crime, by = c('area_numbe' = 'Community Areas')) %>%
  st_transform(4326)


df_bus_neigh <- st_read('combined_bus_data.geojson')

df_combined <- df_crime_nabe %>%
  st_join(
    df_bus_neigh,
    left = T
  ) %>%
  left_join(
    df_population %>% select(area_id, population),
    by = c('area_numbe' = 'area_id')
  ) %>%
  mutate(
    crime_percap = ID/population
  ) %>%
  group_by(
    area_numbe
  ) %>%
  summarize(
    reliability = mean(reliability),
    community = first(community),
    crime = first(ID),
    population = first(population),
    crime_percap = first(crime_percap)
  ) %>%
  ungroup() %>%
  left_join(
    df_demo, by = c('community' = 'neigh')
  ) %>%
  filter(
    community != 'FULLER PARK'
  )


### regressions

df_race_reg <- df_combined %>% select(asian = Asian, black = `Black or African-American`,
                                      spanish = `Persons of Spanish Language*`,
                                      white = White, other = Other, reliability)

#race
lr_race <- lm(reliability ~ black+white, data = df_race_reg)

ggplot(df_race_reg, aes(x = black, y = reliability)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  bbplot::bbc_style() +
  theme(axis.title = element_text(size = 14)) +
  labs(
    title = "",
    subtitle = "Bivariate regression of bus reliability \nvs. neighborhood black population share",
    y = 'Reliability score',
    x = "Black population share"
  ) +
  scale_x_continuous(labels = percent_format())

#crime
lr_bus_crime <- lm(reliability~crime, data = df_combined)

#income
lr_income <- lm(reliability~`Median household income`, data = df_combined)

#poverty line
lr_poverty <- lm(reliability~`Percent income below poverty level`, data = df_combined)

#education
df_education <- df_combined %>% select(BA = `Percent with a BA or Higher`,
                                       HS = `Percent HS Grad or Higher`,
                                       reliability) %>%
  drop_na(BA)
lr_education <- lm(reliability ~HS, data = df_education)

#combined
df_bigreg <- df_combined %>% select(community, asian = Asian, black = `Black or African-American`,
                                    spanish = `Persons of Spanish Language*`,
                                    white = White, other = Other, reliability,
                                    income = `Median household income`,
                                    BA = `Percent with a BA or Higher`,
                                    `poverty` = `Percent income below poverty level`,
                                    crime_percap
                                    ) %>%
  mutate(
    across(
      is.numeric, ~scale(.) %>% as.vector
    )
  )

cor(df_bigreg %>% as_tibble() %>% select(-community, -geometry) %>% drop_na(white))

lr_combined <- lm(reliability~asian+black+spanish+
                    income+crime_percap, data = df_bigreg)

df_reg <- summary(lr_combined)$coefficients %>% as_tibble(rownames = NA) %>%
  mutate(
    Variable = rownames(.)
  ) %>%
  relocate(Variable) %>%
  mutate(
    across(
      c(2:5), function(x) {x = round(x, 6)}
    )
  )
  

df_reg %>% gt::gt()

plot_summs(lr_combined)


### LISA
library(fastmap)
library(rgeos)
nb <- poly2nb(df_combined, queen = TRUE)
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

rel_lag <- lag.listw(lw, df_combined$reliability)
global_moran <- moran.test(df_combined$reliability, lw)
local_m <- localmoran(df_combined$reliability, lw)

df_lisa <- cbind(df_combined, local_m) %>%
  mutate(
    Ii = Ii - mean(Ii, na.rm = T),
    lag_rel = lag.listw(lw, reliability, NAOK = T),
    
    rel_norm = reliability - mean(reliability, na.rm = T),
    lag_rel = lag_rel - mean(lag_rel, na.rm = T),
    
    quadrant = case_when(
      rel_norm > 0 & lag_rel > 0 ~ 1,
      rel_norm < 0 & lag_rel < 0 ~ 2,
      rel_norm < 0 & lag_rel > 0 ~ 3,
      rel_norm > 0 & lag_rel < 0 ~ 4
    ),
    quadrant = if_else(`Pr.z....E.Ii..` > .1, 0, quadrant)
  )

breaks <- c(0,1,2,3,4,5)

tmap::tm_shape(df_lisa) + 
  tmap::tm_fill(col = "quadrant", breaks = breaks, 
         palette = c("white","red","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4)),
         labels = c("Not significant", "High-High", "Low-Low", "Low-High", "High-Low"), title = "") +
  tmap::tm_legend(text.size = 1) +
  tmap::tm_borders(alpha = .5)

### visualizations
library(leaflet)

df_neigh_viz <- geo_neigh %>% st_transform(4326) %>%
  st_join(df_bus_neigh %>% st_transform(4326)) %>%
  group_by(community) %>%
  summarize(
    reliability = mean(reliability)
  ) %>%
  ungroup()

col_pal <- colorNumeric(c('#ef8a62','#67a9cf'),domain = df_neigh_viz$reliability)
col_pal2 <- colorNumeric(c('#ef8a62','#67a9cf'),domain = df_bus_neigh$reliability)

df_neigh_viz %>%
  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    weight = 1,
    fillOpacity = .75,
    fillColor = ~col_pal(reliability),
    label = ~community
  ) %>%
  addLegend("topright", col_pal, values = ~reliability,
            title = "Bus reliability by <br> community area/neighborhood")

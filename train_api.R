library(tidyverse)
library(httr)
library(xml2)

api_key <- '9c796ad85cbe40398abf505aed2f93df'

test <- GET('http://lapi.transitchicago.com/api/1.0/ttarrivals.aspx?key=9c796ad85cbe40398abf505aed2f93df&mapid=40380')

c <- content(test, as = 'text')

cta_xml <- read_xml(test)

unnest_longer(as_tibble(as_list(cta_xml)))


xml_address = "http://lapi.transitchicago.com/api/1.0/ttarrivals.aspx?key=9c796ad85cbe40398abf505aed2f93df&mapid=40380"

restaurant_license_xml = as_list(read_xml(xml_address))

xml_df = tibble::as_tibble(restaurant_license_xml) %>%
  unnest_longer(ctatt) %>%
  drop_na(ctatt_id) %>%
  unnest_longer(ctatt)
%>%
  pivot_wider(
    names_from = ctatt_id,
    values_from = ctatt
  )

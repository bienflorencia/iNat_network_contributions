#| title: Temporal data
#| date: 2026-05-26
#| author: Florencia Grattarola

library(httr)
library(jsonlite)
library(knitr)
library(sf)
sf_use_s2(FALSE)
library(tidyverse) 

source('R/variables_per_country.R')

inat_network <- read_csv('data/inat_nodes.csv')
data_variables <- readRDS('data/Global_data_variables.rds')

country_list <- countrycode::codelist_panel %>% 
  distinct(country.name.en, iso2c, region, unicode.symbol) %>% 
  filter(!is.na(iso2c)) %>% 
  rename(country_name = country.name.en,
         country_code = iso2c,
         region = region,
         flag = unicode.symbol) %>% 
  # merge with those that have node
  left_join(inat_network %>% rename(country_name=node_country))

data_temporal_records_network <- 
  getiNatRecordsPerCountryYear(list_of_country_names = data_variables$country_name,
                               verbose = TRUE,
                               years = 2011:2025, 
                               sleep_time = 10)

saveRDS(data_temporal_records_network, 'data/data_temporal_records_network.csv')

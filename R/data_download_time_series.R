#| title: Temporal data
#| date: 2026-07-26
#| author: Florencia Grattarola

library(httr)
library(jsonlite)
library(knitr)
library(sf)
sf_use_s2(FALSE)
library(tidyverse) 

source('R/funs_download_data.R')

inat_network <- read_csv('data/inat_nodes.csv')
data_variables <- read_csv('data/data_global_variables.csv', na='')

country_list <- countrycode::codelist_panel %>% 
  distinct(country.name.en, iso2c, region, unicode.symbol) %>% 
  filter(!is.na(iso2c)) %>% 
  rename(country_name = country.name.en,
         country_code = iso2c,
         region = region,
         flag = unicode.symbol) %>% 
  # merge with those that have node
  left_join(inat_network %>% rename(country_name=node_country))

#######################################################################

data_temporal_records <- 
  getiNatRecordsPerCountryYear(list_of_country_names = data_variables$country_name,
                               verbose = TRUE,
                               years = 2011:2025, 
                               sleep_time = 10)

# saveRDS(data_temporal_records, 'data/data_temporal_records.rds')

#######################################################################

data_temporal_literature <- 
  getGBIFcitationsPerCountryYear(list_of_country_codes = data_variables$country_code,
                               verbose = TRUE,
                               years = 2011:2025, 
                               sleep_time = 2)

# saveRDS(data_temporal_literature, 'data/data_temporal_literature.rds')

#######################################################################

data_temporal_species <- 
  getiNatSpeciesPerCountryYear(list_of_country_names = data_variables$country_name,
                               verbose = TRUE,
                               years = 2011:2025,
                               sleep_time = 20)

# saveRDS(data_temporal_species, 'data/data_temporal_species.rds')

#######################################################################

data_temporal_users <- 
  getiNatUsersPerCountryYear(list_of_country_names = data_variables$country_name,
                                    verbose = TRUE,
                                    years = 2011:2025, 
                                    sleep_time = 20)

# saveRDS(data_temporal_users, 'data/data_temporal_users.rds')

#######################################################################

data_temporal_projects <- 
  getiNatProjectsPerCountryYear(list_of_country_names = data_variables$country_name,
                                verbose = TRUE,
                                years = 2011:2025,
                                sleep_time = 20)

# saveRDS(data_temporal_projects, 'data/data_temporal_projects.rds')


#######################################################################

start <- Sys.time()

data_temporal_prop_research_grade <- 
  getiNatResearchPropPerCountryYear(
    list_of_country_names = data_variables$country_name,
    verbose = TRUE,
    years = 2011:2025,
    sleep_time = 1)

end <- Sys.time()
saveRDS(data_temporal_prop_research_grade, 
        'data/data_temporal_prop_research_grade.rds')

end-start

# Time difference of 8.222129 hours
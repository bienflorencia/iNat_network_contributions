#| title: Data download
#| date: 2026-07-03
#| author: Florencia Grattarola

# library(httr)
# library(httr2)
# library(jsonlite)
# library(knitr)
# library(sf)
# sf_use_s2(FALSE)
# library(tidyverse)  

library(httr2)     # request(), req_url_query(), req_user_agent(), req_throttle(), req_timeout(), req_perform(), resp_body_json()  -> iNaturalist + World Bank functions
library(httr)       # GET(), HEAD(), content(), status_code(), add_headers()  -> GBIF, IUCN, and getCountryiNatPlaceID
library(jsonlite)   # fromJSON()  -> GBIF/IUCN functions and getCountryiNatPlaceID
library(dplyr)      # filter(), mutate(), select(), the %>% pipe
library(tibble)     # tibble(), add_row(), as_tibble()
library(stringr)    # str_glue()
library(rgbif)      # occ_count()  -> getGBIFrecordsPerCountry
library(countrycode) # countrycode()  -> getLatitudePerCountry, getIfNeighboursHaveSite
library(rnaturalearth)     # ne_countries()  -> getIfNeighboursHaveSite
library(rnaturalearthdata) # countries50  -> getLatitudePerCountry
library(sf)         # st_touches()  -> getIfNeighboursHaveSite
library(tidyverse)

token <- Sys.getenv('IUCN_REDLIST_KEY')
source('R/funs_download_data_new.R')

inat_network <- read_csv('data/inat_nodes.csv')

country_list <- countrycode::codelist_panel %>% 
  distinct(country.name.en, iso2c, region, unicode.symbol) %>% 
  filter(!is.na(iso2c)) %>% 
  rename(country_name = country.name.en,
         country_code = iso2c,
         region = region,
         flag = unicode.symbol) %>% 
  # merge with those that have node
  left_join(inat_network %>% rename(country_name=node_country))



# variables_global
start <- Sys.time()
variables_global <- getCountryVariables(df = country_list,
                            IUCN_token = token,
                            inat_nodes_names = inat_network$node_country)

end <- Sys.time()
end-start


variables_global <- variables_global %>% 
  mutate(has_node = ifelse(!is.na(node_name), 1, 0)) %>% 
  relocate(has_node, .before = neighbour_has_node)

saveRDS(variables_global, 'data/archive/Global_data_variables.rds')


########################################################################
# 1) Download the data per region, to avoid API overflows
########################################################################

EastAsiaPacific <- country_list %>% 
  filter(region == 'East Asia & Pacific')

data_variables_EastAsiaPacific <- getCountryVariables(
  df = EastAsiaPacific,
  IUCN_token = token,
  inat_nodes_names = inat_network$node_country)

saveRDS(data_variables_EastAsiaPacific, 
        'data/archive/EastAsiaPacific_data_variable.rds')

########################################################################

EuropeCentralAsia <- country_list %>% 
  filter(region == 'Europe & Central Asia')

data_variables_EuropeCentralAsia <- getCountryVariables(
  df = EuropeCentralAsia,
  IUCN_token = token,
  inat_nodes_names = inat_network$node_country)

saveRDS(data_variables_EuropeCentralAsia, 
        'data/archive/EuropeCentralAsia_data_variables.rds')

# Source - https://stackoverflow.com/a/36618034
# Posted by Anup Tirpude, modified by community. See post 'Timeline' for change history
# Retrieved 2026-07-04, License - CC BY-SA 3.0

rm(list = ls())
.rs.restartR()


########################################################################

LatinAmericaCaribbean <- country_list %>% 
  filter(region == 'Latin America & Caribbean')

data_variables_LatinAmericaCaribbean <- getCountryVariables(
  df = LatinAmericaCaribbean,
  IUCN_token = token,
  inat_nodes_names = inat_network$node_country)

saveRDS(data_variables_LatinAmericaCaribbean, 
        'data/archive/LatinAmericaCaribbean_data_variables.rds')

########################################################################

MiddleEastNorthAfrica <- country_list %>% 
  filter(region == 'Middle East & North Africa')

data_variables_MiddleEastNorthAfrica <- getCountryVariables(
  df = MiddleEastNorthAfrica,
  IUCN_token = token,
  inat_nodes_names = inat_network$node_country)

saveRDS(data_variables_MiddleEastNorthAfrica, 
        'data/archive/MiddleEastNorthAfrica_data_variables.rds')


########################################################################

NorthAmerica <- country_list %>% 
  filter(region == 'North America')

data_variables_NorthAmerica <- getCountryVariables(
  df = NorthAmerica,
  IUCN_token = token,
  inat_nodes_names = inat_network$node_country)

# saveRDS(data_variables_NorthAmerica, 
#         'data/archive/NorthAmerica_data_variables.rds')

gc()

########################################################################

SouthAsia <- country_list %>% 
  filter(region == 'South Asia')

data_variables_SouthAsia <- getCountryVariables(
  df = SouthAsia,
  IUCN_token = token,
  inat_nodes_names = inat_network$node_country)

saveRDS(data_variables_SouthAsia, 
        'data/archive/SouthAsia_data_variables.rds')
gc()
########################################################################

SubSaharanAfrica <- country_list %>% 
  filter(region == 'Sub-Saharan Africa')

data_variables_SubSaharanAfrica <- getCountryVariables(
  df = SubSaharanAfrica,
  IUCN_token = token,
  inat_nodes_names = inat_network$node_country)

saveRDS(data_variables_SubSaharanAfrica, 
        'data/archive/SubSaharanAfrica_data_variables.rds')

########################################################################
#######################################################################

variables_global <- bind_rows(data_variables_EastAsiaPacific,
                              data_variables_EuropeCentralAsia, 
                              data_variables_LatinAmericaCaribbean,
                              data_variables_MiddleEastNorthAfrica,
                              data_variables_NorthAmerica, 
                              data_variables_SouthAsia,
                              data_variables_SubSaharanAfrica) %>% 
  mutate(has_node = ifelse(!is.na(node_name), 1, 0)) %>% 
  relocate(has_node, .before = neighbour_has_node)

saveRDS(variables_global, 'data/archive/Global_data_variables.rds')

########################################################################
# 2) Check for individual cases in which the iNat download may have failed
########################################################################
# n_records, p_research_grade, n_users,n_species
variables_global <- readRDS('data/archive/Global_data_variables.rds')

# check if country names are found in iNat
variables_global <- variables_global %>%
  mutate(place_id = map_int(country_name, getCountryiNatPlaceID, verbose = T))

# get a list of the unmatched
unmatched <- variables_global %>% 
  filter(is.na(place_id)) %>% select(country_name) 

unmatched %>% 
  print.data.frame()

#                country_name
# 1       Congo - Brazzaville
# 2          Congo - Kinshasa
# 3                    Cyprus
# 4                  Dominica
# 5                   Georgia
# 6                    Guinea
# 7       Hong Kong SAR China
# 8           Myanmar (Burma)
# 9                     Niger
# 10  Palestinian Territories
# 11        St. Kitts & Nevis
# 12                St. Lucia
# 13 St. Vincent & Grenadines
# 14                    Sudan
# 15              Timor-Leste
# 16                   Turkey
# 17            United States


# get a list of all place_id on iNat (from: http://www.inaturalist.org/places/inaturalist-places.csv.zip)
inat_place_id <- read_csv('data/inat_places.csv') %>% 
  filter(admin_level == 0)

# go one by one and find the place_id for the unmatched country names
# iNat_place_id %>% filter(grepl('Palest', name)) %>% pull(id)

# modify manually some country names
variables_global <- variables_global %>% 
  mutate(place_id = case_when(
    country_name == 'Congo - Brazzaville' ~ 7046,
    country_name == 'Congo - Kinshasa' ~ 7054,
    country_name == 'Cyprus' ~ 10289,
    country_name == 'Dominica' ~ 9184,
    country_name == 'Georgia' ~ 8857,
    country_name == 'Guinea' ~ 8512,
    country_name == 'Hong Kong SAR China' ~ 7613,
    country_name == 'Myanmar (Burma)' ~ 6992,
    country_name == 'Niger' ~ 8515,
    country_name == 'Palestinian Territories' ~ 9753,
    country_name == 'St. Kitts & Nevis' ~ 10297,
    country_name == 'St. Lucia' ~ 10300,
    country_name == 'St. Vincent & Grenadines' ~ 10317,
    country_name == 'Sudan' ~ 7064,
    country_name == 'Timor-Leste' ~ 10314,
    country_name == 'Turkey' ~ 7183,
    country_name == 'United States' ~ 1,
    TRUE ~ place_id))

unmatched_country_list <- variables_global %>% 
  filter(country_name %in% unmatched$country_name) 

unmatched_country_list <- variables_global %>% 
  filter(place_id %in% c(7046, 7054, 10289, 9184, 8857, 8512, 7613, 6992, 8515, 9753, 10297, 10300, 10317, 7064, 10314, 7183, 1)) 

n_records <- getiNatRecordsPerCountry(place_ids = unmatched_country_list$place_id, 
                                      sleep_time = 1,
                                      verbose = T,
                                      return_vector = T)
Sys.sleep(20)
p_research_grade <- getiNatResearchPropPerCountry(place_ids = unmatched_country_list$place_id, 
                                                  sleep_time = 1,
                                                  verbose = T,
                                                  return_vector = T)
Sys.sleep(20)
n_users <- getiNatUsersPerCountry(place_ids = unmatched_country_list$place_id, 
                                  sleep_time = 1,
                                  verbose = T,
                                  return_vector = T)
Sys.sleep(20)
n_species <-  getiNatSpeciesPerCountry(place_ids = unmatched_country_list$place_id, 
                                 sleep_time = 1,
                                 verbose = T,
                                 return_vector = T)

Sys.sleep(20)
n_projects <- getiNatProjectsPerCountry(place_ids = unmatched_country_list$place_id, 
                                        sleep_time = 1,
                                        verbose = T,
                                        return_vector = T)

unmatched_country_list <- unmatched_country_list %>% 
  mutate(n_records = !!n_records,
         p_research_grade = !!p_research_grade,
         n_users = !!n_users,
         n_species = !!n_species,
         n_projects = !!n_projects)

########################################################################

variables_global <- left_join(variables_global,
  unmatched_country_list %>% 
    select(country_code,
           n_records,
           p_research_grade,
           n_users,
           n_species,
           n_projects), 
  by = "country_code",  suffix = c("", "_new")) %>% 
  mutate(n_records = coalesce(n_records_new, n_records),
         p_research_grade = coalesce(p_research_grade_new, p_research_grade),
         n_users = coalesce(n_users_new, n_users),
         n_species = coalesce(n_species_new, n_species),
         n_projects = coalesce(n_projects_new, n_projects)) %>%
  select(-ends_with("_new")) 


########################################################################
# 3) Check for individual cases when the WDI data failed
########################################################################
# area, population, gdp_per_capita, gdp_in_research, latitude, n_species

variables_global %>% 
  filter(is.na(latitude)) %>% 
  # drop_na(area, population, gdp_per_capita, gdp_in_research, n_species) %>% 
  select(country_name, country_code, n_records) %>% 
  print.data.frame()

#   country_name country_code n_records
# 1       France           FR   7265947
# 2       Norway           NO    515398
# 3       Taiwan           TW   4930701

rnaturalearth::ne_countries() %>% filter(grepl('France', name)) %>% pull(label_y)
# [1] 46.69611
rnaturalearth::ne_countries() %>% filter(grepl('Taiwan', name)) %>% pull(label_y)
# [1] 23.65241
rnaturalearth::ne_countries() %>% filter(grepl('Norway', name)) %>% pull(label_y)
# [1] 61.35709

# Sources
# area, population, gdp_per_capita: Wikipedia
# gdp_in_research: https://power.lowyinstitute.org/data/economic-capability/technology/rnd-spending-of-gdp/

variables_global <- variables_global %>% 
  mutate(latitude = case_when(country_name == 'Taiwan' ~ 23.65241,
                              country_name == 'France' ~ 46.69611,
                              country_name == 'Norway' ~ 61.35709,
                              TRUE ~ latitude)) %>% 
  mutate(area = case_when(country_name == 'Taiwan' ~ 36197,
                          TRUE ~ area)) %>% 
  mutate(population = case_when(country_name == 'Taiwan' ~  23396049,
                                TRUE ~ population)) %>% 
  mutate(gdp_per_capita = case_when(country_name == 'Taiwan' ~ 34426,
                                    TRUE ~ gdp_per_capita)) %>% 
  mutate(gdp_in_research = case_when(country_name == 'Taiwan' ~ 4,
                                     country_name == 'Bangladesh' ~ 0.4,
                                     country_name == 'North Korea' ~ 3.5,
                                     TRUE ~ gdp_in_research)) %>% 
  mutate(neighbour_has_node = case_when(country_name == 'Taiwan' ~ 0,
                                        country_name == 'France' ~ 1,
                                        country_name == 'Norway' ~ 1,
                                        TRUE ~ neighbour_has_node))

variables_global <- variables_global %>% 
  relocate(place_id, .after = 'flag') 

variables_global %>% 
  drop_na(area, population, gdp_per_capita, gdp_in_research, iucn_species) %>% 
  nrow()


########################################################################
# Store the final dataset
########################################################################

saveRDS(variables_global, 'data/archive/Global_data_variables.rds')
write_csv(variables_global, 'data/data_global_variables.csv', na = '')

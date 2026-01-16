#| title: Data download
#| date: 2025-12-18
#| author: Florencia Grattarola

library(httr)
library(jsonlite)
library(knitr)
library(sf)
sf_use_s2(FALSE)
library(tidyverse)  

token <- Sys.getenv('IUCN_REDLIST_KEY')
source('R/variables_per_country.R')

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

########################################################################
# 1) Download the data per region, to avoid API overflows
########################################################################

EastAsiaPacific <- country_list %>% 
  filter(region == 'East Asia & Pacific')

data_variables_EastAsiaPacific <- getCountryVariables(
  df = EastAsiaPacific,
  IUCN_token = token,
  inat_nodes = inat_network$node_country)

saveRDS(data_variables_EastAsiaPacific, 
        'data/data_variables_EastAsiaPacific.rds')

########################################################################

EuropeCentralAsia <- country_list %>% 
  filter(region == 'Europe & Central Asia')

data_variables_EuropeCentralAsia <- getCountryVariables(
  df = EuropeCentralAsia,
  IUCN_token = token,
  inat_nodes = inat_network$node_country)

saveRDS(data_variables_EuropeCentralAsia, 
        'data/EuropeCentralAsia_data_variables.rds')

########################################################################

LatinAmericaCaribbean <- country_list %>% 
  filter(region == 'Latin America & Caribbean')

data_variables_LatinAmericaCaribbean <- getCountryVariables(
  df = LatinAmericaCaribbean,
  IUCN_token = token,
  inat_nodes = inat_network$node_country)

saveRDS(data_variables_LatinAmericaCaribbean, 
        'data/LatinAmericaCaribbean_data_variables.rds')

########################################################################

MiddleEastNorthAfrica <- country_list %>% 
  filter(region == 'Middle East & North Africa')

data_variables_MiddleEastNorthAfrica <- getCountryVariables(
  df = MiddleEastNorthAfrica,
  IUCN_token = token,
  inat_nodes = inat_network$node_country)

saveRDS(data_variables_MiddleEastNorthAfrica, 
        'data/MiddleEastNorthAfrica_data_variables.rds')


########################################################################

NorthAmerica <- country_list %>% 
  filter(region == 'North America')

data_variables_NorthAmerica <- getCountryVariables(
  df = NorthAmerica,
  IUCN_token = token,
  inat_nodes = inat_network$node_country)

saveRDS(data_variables_NorthAmerica, 
        'data/NorthAmerica_data_variables.rds')

########################################################################

SouthAsia <- country_list %>% 
  filter(region == 'South Asia')

data_variables_SouthAsia <- getCountryVariables(
  df = SouthAsia,
  IUCN_token = token,
  inat_nodes = inat_network$node_country)

saveRDS(data_variables_SouthAsia, 
        'data/SouthAsia_data_variables.rds')

########################################################################

SubSaharanAfrica <- country_list %>% 
  filter(region == 'Sub-Saharan Africa')

data_variables_SubSaharanAfrica <- getCountryVariables(
  df = SubSaharanAfrica,
  IUCN_token = token,
  inat_nodes = inat_network$node_country)

saveRDS(data_variables_SubSaharanAfrica, 
        'data/SubSaharanAfrica_data_variables.rds')

########################################################################
########################################################################

variables_global <- bind_rows(data_variables_EastAsiaPacific,
                              data_variables_EuropeCentralAsia, 
                              data_variables_LatinAmericaCaribbean,
                              data_variables_MiddleEastNorthAfrica,
                              data_variables_NorthAmerica, 
                              data_variables_SouthAsia,
                              data_variables_SubSaharanAfrica) %>% 
  mutate(has_node = ifelse(!is.na(node_name), 1, 0)) %>% 
  relocate(has_node, .before = neighbour_has_node)

saveRDS(variables_global, 'data/Global_data_variables.rds')

########################################################################
# 2) Check for individual cases in which the iNat download may have failed
########################################################################
# n_records, p_research_grade, n_users,n_taxa

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
inat_place_id <- read_csv('data/inaturalist-places.csv') %>% 
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

n_records <- getiNatRecordsPerPlaceID(unmatched_country_list$place_id, 
                                      sleep_time = 10,
                                      verbose = T,
                                      return_vector = T)
Sys.sleep(20)
cat('\n')
cat(' *** Getting the proportion of records that reach Research Grade for a country\n')
p_research_grade <- getiNatResearchPropPerPlaceID(unmatched_country_list$place_id, 
                                                  sleep_time = 20,
                                                  verbose = T,
                                                  return_vector = T)
Sys.sleep(20)
cat('\n')
cat(' *** Getting number of users on iNat for a country\n')
n_users <- getiNatUsersPerPlaceID(unmatched_country_list$place_id, 
                                  sleep_time = 20,
                                  verbose = T,
                                  return_vector = T)
Sys.sleep(20)
cat('\n')
cat(' *** Getting number of taxa on iNat for a country\n')
n_taxa <-  getiNatTaxaPerPlaceID(unmatched_country_list$place_id, 
                                 sleep_time = 20,
                                 verbose = T,
                                 return_vector = T)

unmatched_country_list <- unmatched_country_list %>% 
  mutate(n_records = !!n_records,
         p_research_grade = !!p_research_grade,
         n_users = !!n_users,
         n_taxa = !!n_taxa)

########################################################################

variables_global <- left_join(variables_global,
  unmatched_country_list %>% 
    select(country_code,
           n_records,
           p_research_grade,
           n_users,
           n_taxa), 
  by = "country_code",  suffix = c("", "_new")) %>% 
  mutate(n_records = coalesce(n_records_new, n_records),
         p_research_grade = coalesce(p_research_grade_new, p_research_grade),
         n_users = coalesce(n_users_new, n_users),
         n_taxa = coalesce(n_taxa_new, n_taxa)) %>%
  select(-ends_with("_new")) 


########################################################################
# 3) Check for individual cases when the WDI data failed
########################################################################
# area, population, gdp_per_capita, gdp_in_research, latitude, n_species

variables_global %>% 
  filter(is.na(latitude)) %>% 
  drop_na(area, population, gdp_per_capita, gdp_in_research, n_species) %>% 
  select(country_name, country_code) %>% 
  print.data.frame()

# rnaturalearth::ne_countries() %>% filter(grepl('Tan', formal_en)) %>% pull(sovereignt)
# 
# latitude <- rnaturalearth::ne_countries(country = 'United Republic of Tanzania',
#                                         returnclass = 'sf') %>% st_make_valid()
# sf::st_coordinates(sf::st_centroid(latitude$geometry))[1,2]


# Sources
# area, population, gdp_per_capita: Wikipedia
# gdp_in_research: https://power.lowyinstitute.org/data/economic-capability/technology/rnd-spending-of-gdp/
# latitude: rnaturaleart with different names

variables_global <- variables_global %>% 
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
  mutate(latitude = case_when(country_name == 'Myanmar' ~ 21.017,
                              country_name == 'Bosnia & Herzegovina' ~ 44.18077,
                              country_name == 'Serbia' ~ 44.23304,
                              country_name == 'Trinidad & Tobago' ~ 10.42824,
                              country_name == 'United States' ~ 45.70563,
                              country_name == 'Congo - Brazzaville' ~ -0.8378011,
                              country_name == 'Congo - Kinshasa' ~ -2.850276,
                              country_name == 'Côte d’Ivoire' ~ 7.553755,
                              country_name == 'Tanzania' ~ -6.257732,
                              TRUE ~ latitude)) %>% 
  mutate(neighbour_has_node = case_when(country_name == 'Taiwan' ~ 0,
                                        country_name == 'France' ~ 1,
                                        country_name == 'Norway' ~ 1,
                                        TRUE ~ neighbour_has_node))

########################################################################
# Store the final dataset
########################################################################

saveRDS(variables_global, 'data/Global_data_variables.rds')

#' Country's `place_id` on iNaturalist
#' Get the `place_id` on iNaturalist for a country
#'
#' @param country_name A country name (in English)
#' 
#' @returns A numeric value
#' @export
#'
#' @examples
#' getCountryiNatPlaceID('Uruguay)
getCountryiNatPlaceID <- function(country_name){
  
  api <- 'https://api.inaturalist.org/v1'
  page <- '&page=1&per_page=10'
  
  call_url_place <- str_glue('{api}/places/autocomplete?q={country_name}{page}')
  get_json_call_place <- GET(url = URLencode(call_url_place)) %>%
    content(as = "text") %>% fromJSON(flatten = TRUE)
  results_place <- as_tibble(get_json_call_place$results)
  if(nrow(results_place) != 0){
    results_place <- results_place %>% filter(admin_level == 0 & name == country_name)
    place_id <- results_place$id[results_place$admin_level==0]
  } else {
    place_id <- NA
  }
  return(place_id)
}

##################################################################

#' Country's records in GBIF
#' Get the number of records in GBIF and the number or records from iNaturalist on GBIF for a country 
#'
#' @param list_of_country_codes A country ISO two-letters code, or a list of codes 
#'
#' @returns A tibble with the country ISO code (`country_code`), the number of records in GBIF in the country (`n_records_gbif`), and the number of records from iNaturalist in GBIF in the country (`n_records_gbif_inat`)
#' @export
#'
#' @examples
#' getGBIFrecordsPerCountry('UY')
#' getGBIFrecordsPerCountry(c('AR', 'BR', 'PY'))
getGBIFrecordsPerCountry <- function(list_of_country_codes){
  
  iNatKey <- '50c9509d-22c7-4a22-a47d-8c48425ef4a7'
  
  records_per_country <- tibble(country_code = character(),
                                n_records_gbif = numeric(),
                                n_records_gbif_inat = numeric())
  
  for(code in list_of_country_codes){

    n_records_country <- occ_count(country=code,
                                   hasCoordinate=TRUE, hasGeospatialIssue=FALSE)
    
    n_records_gbif_inat <- occ_count(country=code,
                                        datasetKey=iNatKey,
                                        hasCoordinate=TRUE, hasGeospatialIssue=FALSE)
    
    records_per_country_i <- tibble(country_code = code,
                                    n_records_gbif = n_records_country,
                                    n_records_gbif_inat = n_records_gbif_inat)
    
    records_per_country <- rbind(records_per_country, records_per_country_i)
  }
  return(records_per_country)
}

##################################################################

#' Country's records on iNat
#' Get the number of records on iNaturalist for a country 
#'
#' @param list_of_country_names A country name in English, or a list of country names 
#'
#' @returns A tibble with the country name (`country_name`), and the number of records on iNaturalist in the country (`n_records_inat`)
#' @export
#'
#' @examples
#' getiNatRecordsPerCountry('Uruguay')
#' getiNatRecordsPerCountry(c('Brazil', 'Argentina'))
getiNatRecordsPerCountry <- function(list_of_country_names){
  
  countries_iNat_records <- tibble(country_name = character(),
                                   n_records_inat = numeric())
  
  api <- 'https://api.inaturalist.org/v1'
  page <- '&page=1&per_page=1'
  
  for(country_name in list_of_country_names){

    place_id <- getCountryiNatPlaceID(country_name)
    
    if(!is.na(place_id)){
      call_url_observations <- str_glue('{api}/observations?verifiable=true&place_id={place_id}')
      get_json_call_observations <- GET(url = call_url_observations) %>%
        content(as = "text") %>% fromJSON(flatten = TRUE)
      results_observations <- as_tibble(get_json_call_observations$total_results) 
      
      countries_iNat_records_i <- tibble(country_name = country_name,
                                         n_records_inat = results_observations$value)
      countries_iNat_records <- rbind(countries_iNat_records, countries_iNat_records_i)
    } else {
      countries_iNat_records_i <- tibble(country_name = country_name,
                                         n_records_inat = NA)
      countries_iNat_records <- rbind(countries_iNat_records, countries_iNat_records_i)
    }
  }
  return(countries_iNat_records)
}

##################################################################

#' Country's number of users on iNaturalist
#' Get the number of users recording on iNaturalist for a country 
#'
#' @param list_of_country_names A country name in English, or a list of country names 
#'
#' @returns A tibble with the country name (`country_name`), and the number of users recording on iNaturalist in the country (`n_users`)
#' @export
#'
#' @examples
#' getiNatUsersPerCountry('Uruguay')
#' getiNatUsersPerCountry(c('Brazil', 'Argentina'))
getiNatUsersPerCountry <- function(list_of_country_names){
  
  users_iNat_records <- tibble(country_name = character(),
                               n_users = numeric())
  
  api <- 'https://api.inaturalist.org/v1/observations/observers'
  page <- '&page=1&per_page=1'
  
  for(country_name in list_of_country_names){
  
      place_id <- getCountryiNatPlaceID(country_name)
        
      if(!is.na(place_id)){
        call_url_observations <- str_glue('{api}?verifiable=true&place_id={place_id}')
        get_json_call_observations <- GET(url = call_url_observations) %>%
          content(as = "text") %>% fromJSON(flatten = TRUE)
        n_users <- as_tibble(get_json_call_observations$total_results) 
        
        users_iNat_records_i <- tibble(country_name = country_name,
                                       n_users = n_users$value)
        users_iNat_records <- rbind(users_iNat_records, users_iNat_records_i)
      } else {
        countries_iNat_records_i <- tibble(country_name = country_name,
                                           n_users = NA)
        users_iNat_records <- rbind(users_iNat_records, users_iNat_records_i)
      }
    }
  return(users_iNat_records)
}

##################################################################

#' Country's number of taxa on iNaturalist
#' Get the number of taxa recorded on iNaturalist for a country 
#'
#' @param list_of_country_names A country name in English, or a list of country names 
#'
#' @return A tibble with the country name (`country_name`), and the number of taxa recorded on iNaturalist in the country (`n_taxa`)
#' @export
#'
#' @examples
getiNatTaxaPerCountry <- function(list_of_country_names){
  
  taxa_iNat_records <- tibble(country_name = character(),
                              n_taxa = numeric())
  
  api <- 'https://api.inaturalist.org/v1/observations/species_counts'
  page <- '&page=1&per_page=1'
  
  for(country_name in list_of_country_names){
    
    place_id <- getCountryiNatPlaceID(country_name)
    
    if(!is.na(place_id)){
      call_url_observations <- str_glue('{api}?verifiable=true&place_id={place_id}')
      get_json_call_observations <- GET(url = call_url_observations) %>%
        content(as = "text") %>% fromJSON(flatten = TRUE)
      n_taxa <- as_tibble(get_json_call_observations$total_results) 
      
      taxa_iNat_records_i <- tibble(country_name = country_name,
                                     n_taxa = n_taxa$value)
      taxa_iNat_records <- rbind(taxa_iNat_records, taxa_iNat_records_i)
    } else {
      taxa_iNat_records_i <- tibble(country_name = country_name,
                                         n_taxa = NA)
      taxa_iNat_records <- rbind(taxa_iNat_records, taxa_iNat_records_i)
    }
  }
  return(taxa_iNat_records)
}

##################################################################

#' Country's area
#' Get the area of a country in km^2^ (according to World Bank 'AG.SRF.TOTL.K2' indicator)
#'
#' @param list_of_country_codes A country ISO two-letters code, or a list of codes 
#'
#' @return A tibble with the country name (`country_code`), and the area of the country in km^2^ (`area`)
#' @export
#'
#' @examples
#' getAreaPerCountry('UY')
#' getAreaPerCountry(c('AR', 'BR', 'PY'))
getAreaPerCountry <- function(list_of_country_codes){
  
  area_country <- tibble(country_code = character(),
                         area = numeric())
  
  for(country_code in list_of_country_codes){
    
    surface <- try(WDI::WDI(country = country_code, 
                            indicator = 'AG.SRF.TOTL.K2',
                            latest = 1), silent=T)
    
    if(class(surface)=='try-error'){
      area_country_i <- tibble(country_code = country_code,
                               area = NA)
      area_country <- rbind(area_country, area_country_i)
    } else {
      surface <- surface %>% pull(AG.SRF.TOTL.K2)
      area_country_i <- tibble(country_code = country_code,
                               area = surface)
      area_country <- rbind(area_country, area_country_i) 
    }
  }
  return(area_country)
}

######################################################################

#' Country's population
#' Get the population of a country (according to World Bank 'SP.POP.TOTL' indicator)
#'
#' @param list_of_country_codes A country ISO two-letters code, or a list of codes 
#'
#' @return A tibble with the country ISO code (`country_code`) and the population of the country (`pop`)
#' @export
#'
#' @examples
#' getPopulationPerCountry('UY')
#' getPopulationPerCountry(c('AR', 'BR', 'PY'))
getPopulationPerCountry <- function(list_of_country_codes){
  pop_country <- tibble(country_code = character(),
                        population = numeric())
  
  for(country_code in list_of_country_codes){
    population <- try(WDI::WDI(country = country_code, 
                               indicator = 'SP.POP.TOTL',
                               latest = 1), silent = T)
    if(class(population)=='try-error'){ 
      pop_country_i <- tibble(country_code = country_code,
                              pop = NA)
      pop_country <- rbind(pop_country, pop_country_i) 
    } else {
      population <- population %>% pull(SP.POP.TOTL)
      pop_country_i <- tibble(country_code = country_code,
                              population = population)
      pop_country <- rbind(pop_country, pop_country_i) 
      }

  }
  return(pop_country)
}

######################################################################

#' Country's GDP per capita
#' Get the most recent value of the GDP per capita (US$) for a country (according to World Bank 'NY.GDP.PCAP.CD' indicator)
#'
#' @param list_of_country_codes A country ISO two-letters code, or a list of codes 
#'
#' @return A tibble with the country ISO code (`country_code`) and the GDP per capita for a country (`gdp_per_capita`)
#' @export
#'
#' @examples
#' getGDPperCapitaPerCountry('UY')
#' getGDPperCapitaPerCountry(c('AR', 'BR', 'PY'))
getGDPperCapitaPerCountry <- function(list_of_country_codes){
  gdp_per_capita_country <- tibble(country_code = character(),
                                   gdp_per_capita = numeric())
  
  for(country_code in list_of_country_codes){
    gdp <- try(WDI::WDI(country = country_code,
                        indicator = 'NY.GDP.PCAP.CD',
                        latest = 1), silent = T)
    if(class(gdp)=='try-error'){ 
      gdp_country_i <- tibble(country_code = country_code,
                              gdp_per_capita = NA)
      gdp_per_capita_country <- rbind(gdp_per_capita_country, gdp_country_i) 
    } else {
      gdp <- gdp %>% pull(NY.GDP.PCAP.CD)
      gdp_country_i <- tibble(country_code = country_code,
                              gdp_per_capita = gdp)
      gdp_per_capita_country <- rbind(gdp_per_capita_country, gdp_country_i)
    }
    
  }
  return(gdp_per_capita_country)
}

######################################################################

#' Country's % of GDP in research
#' Get the research and development expenditure (% of GDP) for a country (according to World Bank 'GB.XPD.RSDV.GD.ZS' indicator)
#'
#' @param list_of_country_codes A country ISO two-letters code, or a list of codes 
#'
#' @return A tibble with the country ISO code (`country_code`) and the % of GDP invested in research for a country (`gdp_in_research`)
#' @export
#'
#' @examples
#' getGDPinResearchPerCountry('UY')
#' getGDPinResearchPerCountry(c('AR', 'BR', 'PY'))
getGDPinResearchPerCountry <- function(list_of_country_codes){
  gdp_r_country <- tibble(country_code = character(),
                          gdp_in_research = numeric())
  
  for(country_code in list_of_country_codes){
    gdp_research <- try(WDI::WDI(country = country_code,
                                 indicator = 'GB.XPD.RSDV.GD.ZS',
                                 latest = 1), silent = T)
    if(class(gdp_research)=='try-error'){ 
      gdp_r_country_i <- tibble(country_code = country_code,
                                gdp_in_research = NA)
      gdp_r_country <- rbind(gdp_r_country, gdp_r_country_i) 
    } else {
      gdp_research <- gdp_research %>% pull(GB.XPD.RSDV.GD.ZS)
      gdp_r_country_i <- tibble(country_code = country_code,
                                gdp_in_research = gdp_research)
      gdp_r_country <- rbind(gdp_r_country, gdp_r_country_i)
    }
    
  }
  return(gdp_r_country)
}

########################################################################
# Latitud media del país (como proxy de biodiversidad esperada): `latitude`.

#' Latitude of the country
#' Get the latitude of a country's geographic centroid (as a proxy of biodiversity richness)
#' @param list_of_country_codes A country ISO two-letters code, or a list of codes 
#'
#' @return A tibble with the country ISO code (`country_code`) and the latitude of a country's geographic centroid (`latitude`)
#' @export
#'
#' @examples
#' getLatitudePerCountry('UY')
#' getLatitudePerCountry(c('AR', 'BR', 'PY'))
getLatitudePerCountry <- function(list_of_country_codes){
  
  country_lat <- tibble(country_code = character(),
                        latitude = numeric())
  
  for(country_code in list_of_country_codes){
    country_name <- countrycode::countrycode(country_code, 
                                             'iso2c',
                                             'country.name')
    
    latitude <- try(rnaturalearth::ne_countries(country = country_name, 
                                                returnclass = 'sf'),
                    silent = T)
    
    if(class(latitude)[1]=='try-error'){ 
      country_lat_i <- tibble(country_code = country_code,
                              latitude = NA)
      country_lat <- rbind(country_lat, country_lat_i) 
    } else {
      latitude <- sf::st_coordinates(sf::st_centroid(latitude$geometry))[1,2]
      
      country_lat_i <- tibble(country_code = country_code,
                              latitude = latitude)
      country_lat <- rbind(country_lat, country_lat_i) 
    }
  }
  return(country_lat)
}

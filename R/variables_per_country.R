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
getCountryiNatPlaceID <- function(country_name, 
                                  verbose=FALSE){
  
  api <- 'https://api.inaturalist.org/v1'
  page <- '&page=1&per_page=10'
  
  if(verbose){
    cat('Fetching place_id for:', country_name, '\n')
  }
  
  call_url_place <- str_glue('{api}/places/autocomplete?q={country_name}{page}')
  get_json_call_place <- GET(url = URLencode(call_url_place)) %>%
    content(as = "text") %>% fromJSON(flatten = TRUE)
  
  results_place <- as_tibble(get_json_call_place$results)
  
  if(nrow(results_place) != 0){
    results_place <- results_place %>% filter(admin_level == 0)
    if(nrow(results_place)>1){
      results_place <- results_place %>% filter(place_type == 12)
    } 
    if(nrow(results_place)>1 | nrow(results_place)==0){
      place_id <- NA
    } else {
      place_id <- results_place$id[results_place$admin_level==0]
    }
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
getGBIFrecordsPerCountry <- function(list_of_country_codes, 
                                     verbose=FALSE, 
                                     return_vectors=FALSE){
  
  iNatKey <- '50c9509d-22c7-4a22-a47d-8c48425ef4a7'
  
  results <- tibble(country_code = character(),
                    n_records_gbif = numeric(),
                    n_records_gbif_inat = numeric())
  
  for(code in list_of_country_codes){
    
    if(verbose){
      cat('Fetching data for country code: ', country_code, '\n')
    }

    n_records_country <- occ_count(country=code,
                                   hasCoordinate=TRUE, 
                                   hasGeospatialIssue=FALSE)
    
    n_records_gbif_inat <- occ_count(country=code,
                                     datasetKey=iNatKey,
                                     hasCoordinate=TRUE, 
                                     hasGeospatialIssue=FALSE)
    
    results <- add_row(results,
                       country_code = code,
                       n_records_gbif = n_records_country,
                       n_records_gbif_inat = n_records_gbif_inat
    )
  }

    if (return_vectors) {
    return(list(
      n_records_gbif      = results$n_records_gbif,
      n_records_gbif_inat = results$n_records_gbif_inat
    ))
  }
  return(results)
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
getiNatRecordsPerCountry <- function(list_of_country_names, 
                                     verbose=FALSE,
                                     return_vector = FALSE){
  
  results <- tibble(country_name = character(),
                    n_records_inat = numeric())
  
  api <- 'https://api.inaturalist.org/v1'
  page <- '&page=1&per_page=1'
  
  for(country_name in list_of_country_names){
    
    if(verbose==TRUE){
      cat('Fetching data for: ', country_name, '\n')
    }
    
    place_id <- getCountryiNatPlaceID(country_name)
    
    if(!is.na(place_id)){
      call_url_observations <- str_glue('{api}/observations?verifiable=true&place_id={place_id}')
      
      get_json_call_observations <- GET(url = call_url_observations) %>%
        content(as = "text") %>% fromJSON(flatten = TRUE)
      
      n_obs <- get_json_call_observations$total_results
    } else {
      n_obs <- NA
    }
    
    results <- add_row(results,
                       country_name = country_name,
                       n_records_inat = n_obs)
  }
  
  # If user wants a numeric vector for mutate()
  if (return_vector) {
    return(results$n_records_inat)
  }
  
  return(results)
}

##################################################################

#' Proportion of records that reach "Research Grade"
#' Get the proportion of records on iNaturalist for a country that reach the "Research Grade" quality
#'
#' @param list_of_country_names A country name in English, or a list of country names 
#'
#' @returns A tibble with the country name (`country_name`), and the proportion of records on iNaturalist in the country tha reach "Research Grade" (`p_research_grade`)
#' @export
#'
#' @examples
#' getiNatResearchPropPerCountry('Uruguay')
#' getiNatResearchPropPerCountry(c('Brazil', 'Argentina'))
getiNatResearchPropPerCountry <- function(list_of_country_names,
                                          verbose=FALSE,
                                          return_vector = FALSE){
  
  results <- tibble(country_name = character(),
                    p_research_grade = numeric())
  
  api <- 'https://api.inaturalist.org/v1'
  page <- '&page=1&per_page=1'
  
  for(country_name in list_of_country_names){
    
    if(verbose==TRUE){
      cat('Fetching data for: ', country_name, '\n')
    }
    
    place_id <- getCountryiNatPlaceID(country_name)
    
    if(!is.na(place_id)){
      call_url_observations <- str_glue('{api}/observations?verifiable=true&place_id={place_id}')
      
      call_url_research_grade <- str_glue('{api}/observations?verifiable=true&quality_grade=research&place_id={place_id}')
      
      get_json_call_observations <- GET(url = call_url_observations) %>%
        content(as = "text") %>% fromJSON(flatten = TRUE)
      
      get_json_call_research_grade <- GET(url = call_url_research_grade) %>%
        content(as = "text") %>% fromJSON(flatten = TRUE)
      
      n_obs <- get_json_call_observations$total_results
      n_obs_research <- get_json_call_research_grade$total_results
    } else {
      n_obs <- NA
      n_obs_research <- NA
    }
    
    results <- add_row(results,
                       country_name = country_name,
                       p_research_grade = n_obs_research/n_obs)
  }
  
  # If user wants a numeric vector for mutate()
  if (return_vector) {
    return(results$n_records_inat)
  }
  
  return(results)
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
getiNatUsersPerCountry <- function(list_of_country_names, 
                                   verbose=FALSE,
                                   return_vector=FALSE){
  
  results <- tibble(country_name = character(),
                    n_users = numeric())
  
  api <- 'https://api.inaturalist.org/v1/observations/observers'
  page <- '&page=1&per_page=1'
  
  for(country_name in list_of_country_names){
  
    if(verbose==TRUE){
      cat('Fetching data for: ', country_name, '\n')
    }
    
    place_id <- getCountryiNatPlaceID(country_name)
    
    if(!is.na(place_id)){
      call_url_observations <- str_glue('{api}?verifiable=true&place_id={place_id}')
      get_json_call_observations <- GET(url = call_url_observations) %>%
        content(as = "text") %>% fromJSON(flatten = TRUE)
      
      n_users <- get_json_call_observations$total_results
    } else {
      n_users <- NA
    }
    
    results <- add_row(results,
                       country_name = country_name,
                       n_users = n_users)
      
  }
  # If user wants a numeric vector for mutate()
  if (return_vector) {
    return(results$n_users)
  }
  return(results)
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
getiNatTaxaPerCountry <- function(list_of_country_names, 
                                  verbose=FALSE,
                                  return_vector = FALSE){
  
  results <- tibble(country_name = character(),
                    n_taxa = numeric())
  
  api <- 'https://api.inaturalist.org/v1/observations/species_counts'
  page <- '&page=1&per_page=1'
  
  for(country_name in list_of_country_names){
    
    if(verbose==TRUE){
      cat('Fetching data for: ', country_name, '\n')
    }
    
    place_id <- getCountryiNatPlaceID(country_name)
    
    if(!is.na(place_id)){
      call_url_observations <- str_glue('{api}?verifiable=true&place_id={place_id}')
      get_json_call_observations <- GET(url = call_url_observations) %>%
        content(as = "text") %>% fromJSON(flatten = TRUE)
      
      n_taxa <- get_json_call_observations$total_results
      
    } else{
      n_taxa <- NA
    }
    
    results <- add_row(results,
                       country_name = country_name,
                       n_taxa = n_taxa)
    
  }
  # If user wants a numeric vector for mutate()
  if (return_vector) {
    return(results$n_taxa)
  }
  return(results)
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
getAreaPerCountry <- function(list_of_country_codes, 
                              verbose=FALSE,
                              return_vector = FALSE){
  
  results <- tibble(country_code = character(),
                    area = numeric())
  
  for(country_code in list_of_country_codes){
    
    if(verbose==TRUE){
      cat('Fetching data for:', country_code, '\n')
    }
    
    area <- try(WDI::WDI(country = country_code, 
                            indicator = 'AG.SRF.TOTL.K2',
                            latest = 1), silent=T)
    
    if(inherits(area, "try-error")){
      area_value <- NA
    } else {
      area_value <- suppressWarnings(area %>% 
                                     pull(AG.SRF.TOTL.K2))
      if(length(area_value) == 0) area_value <- NA
    }
    results <- add_row(results,
                       country_code = country_code,
                       area = area_value)
  }
  # return a single vector if requested
  if (return_vector) {
    return(results$area)
  }
  return(results)
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
getPopulationPerCountry <- function(list_of_country_codes, 
                                    verbose=FALSE,
                                    return_vector = FALSE){
  
  results <- tibble(country_code = character(),
                   population = numeric())
  
  for(country_code in list_of_country_codes){
    
    if(verbose==TRUE){
      cat('Fetching data for: ', country_code, '\n')
    }
    
    population <- try(WDI::WDI(country = country_code, 
                               indicator = 'SP.POP.TOTL',
                               latest = 1), silent = T)
    
    if(inherits(population, 'try-error')){ 
      pop_value <- NA
    } else {
      pop_value <- suppressWarnings(population %>% 
                                      pull(SP.POP.TOTL))
      # In case WDI returns no rows
      if (length(pop_value) == 0) pop_value <- NA
      }
    
    results <- add_row(results,
                      country_code = country_code,
                      population = pop_value)
  }
  
  # return a single vector if requested
  if (return_vector) {
    return(results$population)
  }
  
  return(results)
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
getGDPperCapitaPerCountry <- function(list_of_country_codes,
                                      verbose=FALSE,
                                      return_vector = FALSE){
  
  results <- tibble(country_code = character(),
                    gdp_per_capita = numeric())
  
  for(country_code in list_of_country_codes){
    
    if(verbose==TRUE){
      cat('Fetching data for: ', country_code, '\n')
    }
    
    gdp <- try(WDI::WDI(country = country_code,
                        indicator = 'NY.GDP.PCAP.CD',
                        latest = 1), silent = T)
    
    if(inherits(gdp, "try-error")){ 
      gdp_value <-  NA
    } else {
      gdp_value <- suppressWarnings(gdp %>% 
                                      pull(NY.GDP.PCAP.CD))
      # In case WDI returns no rows
      if (length(gdp_value) == 0) gdp_value <- NA
    }
    
    results <- add_row(results,
                       country_code = country_code,
                       gdp_per_capita = gdp_value)
  }
  # return a single vector if requested
  if (return_vector) {
    return(results$gdp_per_capita)
  }
  return(results)
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
getGDPinResearchPerCountry <- function(list_of_country_codes,
                                       verbose=FALSE,
                                       return_vector = FALSE){
  
  results <- tibble(country_code = character(),
                    gdp_in_research = numeric())
  
  for(country_code in list_of_country_codes){
    
    if(verbose==TRUE){
      cat('Fetching data for: ', country_code, '\n')
    }
    
    gdp_research <- try(WDI::WDI(country = country_code,
                                 indicator = 'GB.XPD.RSDV.GD.ZS',
                                 latest = 1), silent = T)
    
    if(inherits(gdp_research, "try-error")){ 
      gdp_research_value <- NA
    } else {
      gdp_research_value <- suppressWarnings(gdp_research %>%
                                               pull(GB.XPD.RSDV.GD.ZS))
      if (length(gdp_research_value) == 0) gdp_research_value <- NA
    }
    
    results <- add_row(results,
                       country_code = country_code,
                       gdp_in_research = gdp_research_value)
    
  }
  if (return_vector) {
    return(results$gdp_in_research)
  }
  return(results)
}

########################################################################

#' Mean latitude of the country
#' Get the latitude of a country's geographic centroid (as a proxy of biodiversity richness)
#' @param list_of_country_codes A country ISO two-letters code, or a list of codes 
#'
#' @return A tibble with the country ISO code (`country_code`) and the latitude of a country's geographic centroid (`latitude`)
#' @export
#'
#' @examples
#' getLatitudePerCountry('UY')
#' getLatitudePerCountry(c('AR', 'BR', 'PY'))
getLatitudePerCountry <- function(list_of_country_codes, 
                                  verbose=FALSE,
                                  return_vector = FALSE){
  
  results <- tibble(country_code = character(),
                    latitude = numeric())
  
  for(country_code in list_of_country_codes){
    
    if(verbose==TRUE){
      cat('Fetching data for: ', country_code, '\n')
    }
    
    country_name <- countrycode::countrycode(country_code, 
                                             'iso2c',
                                             'country.name')
    
    latitude <- try(rnaturalearth::ne_countries(country = country_name, 
                                                returnclass = 'sf'),
                    silent = T)
    
    if(inherits(latitude, "try-error")){ 
      latitude_value <- NA
    } else {
      latitude_value <- sf::st_coordinates(sf::st_centroid(latitude$geometry))[1,2]
    }
    
    results <- add_row(results,
                       country_code = country_code,
                       latitude = latitude_value)
  }
  if (return_vector) {
    return(results$latitude)
  }
  return(results)
}

########################################################################

#' Number of species in the country
#' Get the latitude of a country's geographic centroid (as a proxy of biodiversity richness)
#' @param list_of_country_codes A country ISO two-letters code, or a list of codes 
#' @param api_key A  IUCN Red List API key
#'
#' @return A tibble with the country ISO code (`country_code`) and the number of species in the country (`n_species`)
#' @export
#'
#' @examples
#' getNspeciesPerCountry(api_key=Sys.getenv('IUCN_REDLIST_KEY'), 'UY')
#' getNspeciesPerCountry(api_key, c('AR', 'BR', 'PY'))
getNspeciesPerCountry <- function(api_key, 
                                  list_of_country_codes, 
                                  verbose=FALSE,
                                  return_vector = FALSE) {
  
  # Define the base URL for the IUCN Red List API
  base_url <- "https://api.iucnredlist.org/api/v4"
  endpoint <- "/countries"
  
  # Authorization token
  headers <- add_headers(Authorization = paste("Bearer", api_key))
  
  results <- tibble(country_code = character(),
                    n_species = numeric())
  
  for (country_code in list_of_country_codes) {
    
    if(verbose==TRUE){
      cat('Fetching data for: ', country_code, '\n')
    }
    
    api_url <- str_glue(
      '{base_url}{endpoint}/{country_code}?latest=true&scope_code=1')
    
    response <- HEAD(api_url, headers)
    
    if (status_code(response) == 200) {
      n_species <- as.numeric(response$headers$`total-count`)
      
      # total-count may exist but be empty
      if (length(n_species) == 0) n_species <- NA
    } else {
      print("Country not found.")
      n_species <- NA
    }
    
    results <- add_row(results,
                       country_code = country_code,
                       n_species = n_species)
    
    if (return_vector) {
      return(results$n_species)
    }
  }
  return(results)
}


########################################################################

#' 
#' 

getNeighboursWithSite <- function(list_of_country_codes, 
                                  verbose=FALSE,
                                  return_vector = FALSE) {
  
  results <- tibble(country_code = character(),
                    latitude = numeric())
  
  for(country_code in list_of_country_codes){
    
    if(verbose==TRUE){
      cat('Fetching data for: ', country_code, '\n')
    }
    
    country_name <- countrycode::countrycode(country_code, 
                                             'iso2c',
                                             'country.name')
    
    latitude <- try(rnaturalearth::ne_countries(country = country_name, 
                                                returnclass = 'sf'),
                    silent = T)
    
    if(inherits(latitude, "try-error")){ 
      latitude_value <- NA
    } else {
      latitude_value <- sf::st_coordinates(sf::st_centroid(latitude$geometry))[1,2]
    }
    
    results <- add_row(results,
                       country_code = country_code,
                       latitude = latitude_value)
  }
  if (return_vector) {
    return(results$latitude)
  }
  return(results)
  
}
########################################################################

#' All variables per country
#' Get all variables for a country
#' @param df A dataframe with two columns `country_name` (the name of the country in English) and `country_code` the country ISO two-letters code.
#' @param api_key A  IUCN Red List API key
#'
#' @return A tibble with the country ISO code (`country_code`) and all the variables for each country (`area`, `population`, `gdp_per_capita`, `gdp_in_research`, `latitude`, `n_species`)
#' @export
#'
#' @examples
#' countries <- tibble(country_name = c('Uruguay', 'Argentina'),
#'                     country_code = c('UY', 'AR'))
#' getCountryVariables(df = countries, IUCN_token=Sys.getenv('IUCN_REDLIST_KEY'))

getCountryVariables <- function(df, IUCN_token) {
  
  stopifnot(all(c("country_code", "country_name") %in% names(df)))
  
  # gbif_vals <- getGBIFrecordsPerCountry(df$country_code, return_vectors = TRUE)
  
  # n_records_gbif  <- gbif_vals$n_records_gbif
  # n_records_gbif_inat <- gbif_vals$n_records_gbif_inat
  n_inat_country <- getiNatRecordsPerCountry(df$country_name)
  p_research_grade <- getiNatResearchPropPerCountry(df$country_name)
  n_users_country<- getiNatUsersPerCountry(df$country_name)
  n_taxa_country <-  getiNatTaxaPerCountry(df$country_name)
  area_country <-  getAreaPerCountry(df$country_code)
  population <-  getPopulationPerCountry(df$country_code)
  gdp_per_capita <-  getGDPperCapitaPerCountry(df$country_code)
  gdp_in_research <-  getGDPinResearchPerCountry(df$country_code)
  latitude <-  getLatitudePerCountry(df$country_code)
  n_species <-  getNspeciesPerCountry(api_key=IUCN_token, df$country_code)

  per_country <- countries %>%
    mutate(
      # n_records_gbif      = gbif_vals$n_records_gbif,
      # n_records_gbif_inat = gbif_vals$n_records_gbif_inat,
      n_inat_country      = n_inat_country,
      p_research_grade    = p_research_grade,
      n_users_country     = n_users_country,
      n_taxa_country      = n_taxa_country,
      area_country        = area_country,
      population          = population,
      gdp_per_capita      = gdp_per_capita,
      gdp_in_research     = gdp_in_research,
      latitude            = latitude,
      n_species           = n_species
    )
  
}

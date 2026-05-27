##################################################################
#' Get the iNaturalist `place_id` for a country
#'
#' @param country_name A country name in English
#' @param verbose Logical. If `TRUE`, prints progress messages. Default is `FALSE`
#'
#' @returns A numeric value corresponding to the iNaturalist `place_id` of the country,
#'   or `NA` if not found
#' @export
#'
#' @examples
#' getCountryiNatPlaceID('Uruguay')
getCountryiNatPlaceID <- function(country_name,
                                  verbose = FALSE) {
  
  api  <- 'https://api.inaturalist.org/v1'
  page <- '&page=1&per_page=10'
  
  if (verbose) {
    cat('Fetching place_id for:', country_name, '\n')
  }
  
  call_url_place <- str_glue('{api}/places/autocomplete?q={country_name}{page}')
  get_json_call_place <- GET(url = URLencode(call_url_place)) %>%
    content(as = "text") %>% fromJSON(flatten = TRUE)
  
  results_place <- as_tibble(get_json_call_place$results)
  
  if (nrow(results_place) != 0) {
    results_place <- results_place %>% filter(admin_level == 0)
    if (nrow(results_place) > 1) {
      results_place <- results_place %>% filter(place_type == 12)
    }
    if (nrow(results_place) > 1 | nrow(results_place) == 0) {
      place_id <- NA
    } else {
      place_id <- results_place$id[results_place$admin_level == 0]
    }
  } else {
    place_id <- NA
  }
  return(place_id)
}

##################################################################

#' Get the number of occurrence records in GBIF for a country
#'
#' Retrieves the total number of georeferenced occurrence records in GBIF, and
#' the subset contributed by iNaturalist, for one or more countries.
#'
#' @param list_of_country_codes A character string or vector of ISO 3166-1 alpha-2 country codes
#' @param verbose Logical. If `TRUE`, prints progress messages. Default is `FALSE`
#' @param return_vectors Logical. If `TRUE`, returns a named list of numeric vectors
#'   (`n_records_gbif` and `n_records_gbif_inat`) instead of a tibble. Default is `FALSE`
#'
#' @returns A tibble with columns: `country_code` (ISO 3166-1 alpha-2 code),
#'   `n_records_gbif` (total georeferenced occurrence records in GBIF without geospatial issues),
#'   and `n_records_gbif_inat` (iNaturalist occurrence records in GBIF without geospatial issues)
#' @export
#'
#' @examples
#' getGBIFrecordsPerCountry('UY')
#' getGBIFrecordsPerCountry(c('AR', 'BR', 'PY'))
getGBIFrecordsPerCountry <- function(list_of_country_codes,
                                     verbose = FALSE,
                                     return_vectors = FALSE) {
  
  iNatKey <- '50c9509d-22c7-4a22-a47d-8c48425ef4a7'
  
  results <- tibble(country_code        = character(),
                    n_records_gbif      = numeric(),
                    n_records_gbif_inat = numeric())
  
  for (code in list_of_country_codes) {
    
    if (verbose) {
      cat('Fetching data for country code:', code, '\n')
    }
    
    n_records_country <- occ_count(country          = code,
                                   hasCoordinate    = TRUE,
                                   hasGeospatialIssue = FALSE)
    
    n_records_gbif_inat <- occ_count(country            = code,
                                     datasetKey         = iNatKey,
                                     hasCoordinate      = TRUE,
                                     hasGeospatialIssue = FALSE)
    
    results <- add_row(results,
                       country_code        = code,
                       n_records_gbif      = n_records_country,
                       n_records_gbif_inat = n_records_gbif_inat)
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

#' Get the number of peer-reviewed publications citing iNaturalist data for a country
#'
#' Retrieves the total number of peer-reviewed publications indexed in the GBIF
#' Literature Registry that use iNaturalist occurrence data with a documented
#' geographic focus on a given country, via the GBIF Literature API
#' (`countriesOfCoverage` parameter, filtered by the iNaturalist dataset key).
#'
#' @param list_of_country_codes A character string or vector of ISO 3166-1 alpha-2 country codes
#' @param verbose Logical. If `TRUE`, prints progress messages. Default is `FALSE`
#' @param return_vector Logical. If `TRUE`, returns a numeric vector instead of a tibble. Default is `FALSE`
#' @param sleep_time Numeric. Seconds to pause every 10 requests to avoid rate-limiting. Default is `2`
#'
#' @returns A tibble with columns: `country_code` (ISO 3166-1 alpha-2 code) and
#'   `n_literature` (number of peer-reviewed publications in the GBIF Literature Registry
#'   that use iNaturalist data with a geographic focus on the country)
#' @export
#'
#' @examples
#' getGBIFcitationsPerCountry('UY')
#' getGBIFcitationsPerCountry(c('AR', 'BR', 'PY'))
getGBIFcitationsPerCountry <- function(list_of_country_codes,
                                       verbose     = FALSE,
                                       return_vector = FALSE,
                                       sleep_time  = 2) {
  
  iNatKey  <- '50c9509d-22c7-4a22-a47d-8c48425ef4a7'
  base_url <- 'https://api.gbif.org/v1/literature/search?'
  
  results <- tibble(country_code = character(),
                    n_literature = numeric())
  
  for (i in seq_along(list_of_country_codes)) {
    
    country_code <- list_of_country_codes[i]
    
    if (verbose) {
      cat('Fetching data for:', country_code, '\n')
    }
    
    call_url <- str_glue('{base_url}countriesOfCoverage={country_code}&gbifDatasetKey={iNatKey}')
    response <- HEAD(call_url)
    
    if (status_code(response) == 200) {
      response <- GET(url = URLencode(call_url)) %>%
        content(as = 'text', encoding = 'UTF-8') %>% fromJSON(flatten = TRUE)
      
      n_lit <- as.numeric(response$count)
    } else {
      print('Country not found.')
      n_lit <- NA
    }
    
    results <- add_row(results,
                       country_code = country_code,
                       n_literature = n_lit)
    
    ## ---- sleep ----
    if (i %% 10 == 0) {
      if (verbose) {
        cat('Sleeping for', sleep_time, 'seconds...\n')
      }
      Sys.sleep(sleep_time)
    }
  }
  if (return_vector) {
    return(results$n_literature)
  }
  return(results)
}

##################################################################

#' Get the number of verifiable observations on iNaturalist for a country
#'
#' @param list_of_country_names A character string or vector of country names in English
#' @param verbose Logical. If `TRUE`, prints progress messages. Default is `FALSE`
#' @param return_vector Logical. If `TRUE`, returns a numeric vector instead of a tibble. Default is `FALSE`
#' @param sleep_time Numeric. Seconds to pause every 10 requests to avoid rate-limiting. Default is `10`
#'
#' @returns A tibble with columns: `country_name` and `n_records_inat`
#'   (total number of verifiable observations on iNaturalist within the country)
#' @export
#'
#' @examples
#' getiNatRecordsPerCountry('Uruguay')
#' getiNatRecordsPerCountry(c('Brazil', 'Argentina'))
getiNatRecordsPerCountry <- function(list_of_country_names,
                                     verbose       = FALSE,
                                     return_vector = FALSE,
                                     sleep_time    = 10) {
  
  results <- tibble(country_name   = character(),
                    n_records_inat = numeric())
  
  api  <- 'https://api.inaturalist.org/v1'
  page <- '&page=1&per_page=1'
  
  for (i in seq_along(list_of_country_names)) {
    
    country_name <- list_of_country_names[i]
    
    if (verbose) {
      cat('Fetching data for:', country_name, '\n')
    }
    
    place_id <- getCountryiNatPlaceID(country_name)
    
    if (!is.na(place_id)) {
      call_url_observations <- str_glue('{api}/observations?verifiable=true&place_id={place_id}')
      
      get_json_call_observations <- GET(url = call_url_observations) %>%
        content(as = "text") %>% fromJSON(flatten = TRUE)
      
      n_obs <- get_json_call_observations$total_results
    } else {
      n_obs <- NA
    }
    
    results <- add_row(results,
                       country_name   = country_name,
                       n_records_inat = n_obs)
    
    if (i %% 10 == 0) {
      if (verbose) {
        cat('Sleeping for', sleep_time, 'seconds...\n')
      }
      Sys.sleep(sleep_time)
    }
  }
  
  if (return_vector) {
    return(results$n_records_inat)
  }
  return(results)
}

getiNatRecordsPerPlaceID <- function(list_of_place_ids,
                                     verbose       = FALSE,
                                     return_vector = FALSE,
                                     sleep_time    = 10) {
  
  results <- tibble(place_id       = numeric(),
                    n_records_inat = numeric())
  
  api  <- 'https://api.inaturalist.org/v1'
  page <- '&page=1&per_page=1'
  
  for (i in seq_along(list_of_place_ids)) {
    
    place_id <- list_of_place_ids[i]
    
    if (verbose) {
      cat('Fetching data for:', place_id, '\n')
    }
    
    if (!is.na(place_id)) {
      call_url_observations <- str_glue('{api}/observations?verifiable=true&place_id={place_id}')
      
      get_json_call_observations <- GET(url = call_url_observations) %>%
        content(as = "text") %>% fromJSON(flatten = TRUE)
      
      n_obs <- get_json_call_observations$total_results
    } else {
      n_obs <- NA
    }
    
    results <- add_row(results,
                       place_id       = place_id,
                       n_records_inat = n_obs)
    
    if (i %% 10 == 0) {
      if (verbose) {
        cat('Sleeping for', sleep_time, 'seconds...\n')
      }
      Sys.sleep(sleep_time)
    }
  }
  
  if (return_vector) {
    return(results$n_records_inat)
  }
  return(results)
}

##################################################################

#' Get the number of verifiable observations uploaded on iNaturalist for a country, by year
#'
#' @param list_of_country_names A character string or vector of country names in English
#' @param years A numeric vector of years to retrieve. Default is `2011:2026`
#' @param verbose Logical. If `TRUE`, prints progress messages. Default is `FALSE`
#' @param sleep_time Numeric. Seconds to pause every 10 requests to avoid rate-limiting. Default is `10`
#'
#' @returns A tibble with columns: `country_name`, `year`, and `n_records_inat`
#'   (total number of verifiable observations on iNaturalist uploaded within the country in that year)
#' @export
#'
#' @examples
#' getiNatRecordsPerCountryYear('Uruguay')
#' getiNatRecordsPerCountryYear(c('Brazil', 'Argentina'))
#' getiNatRecordsPerCountryYear('Uruguay', years = 2015:2020)
#' Get the number of verifiable observations on iNaturalist for a country, by year
#'
#' @param list_of_country_names A character string or vector of country names in English
#' @param years A numeric vector of years to retrieve. Default is \code{2011:2026}
#' @param verbose Logical. If \code{TRUE}, prints progress messages. Default is \code{FALSE}
#' @param sleep_time Numeric. Seconds to pause every 10 requests to avoid rate-limiting. Default is \code{10}
#'
#' @returns A tibble with columns: \code{country_name}, \code{year}, and \code{n_records_inat}
#'   (total number of verifiable observations on iNaturalist within the country in that year)
#' @export
#'
#' @examples
#' getiNatRecordsPerCountryYear('Uruguay')
#' getiNatRecordsPerCountryYear(c('Brazil', 'Argentina'))
#' getiNatRecordsPerCountryYear('Uruguay', years = 2015:2020)
getiNatRecordsPerCountryYear <- function(list_of_country_names,
                                         years      = 2011:2026,
                                         verbose    = FALSE,
                                         sleep_time = 10) {

  results <- tibble(country_name   = character(),
                    year           = numeric(),
                    n_records_inat = numeric())

  api     <- 'https://api.inaturalist.org/v1'
  n_calls <- 0

  for (i in seq_along(list_of_country_names)) {

    country_name <- list_of_country_names[i]

    if (verbose) {
      cat('Fetching data for:', country_name, '\n')
    }

    place_id <- getCountryiNatPlaceID(country_name)

    for (year in years) {

      if (verbose) {
        cat('  Year:', year, '\n')
      }

      if (!is.na(place_id)) {
        call_url_observations <- str_glue('{api}/observations?verifiable=true&place_id={place_id}&created_year={year}')

        get_json_call_observations <- GET(url = call_url_observations) %>%
          content(as = "text") %>% fromJSON(flatten = TRUE)

        n_obs <- get_json_call_observations$total_results
      } else {
        n_obs <- NA
      }

      results <- add_row(results,
                         country_name   = country_name,
                         year           = year,
                         n_records_inat = n_obs)

      ## ---- sleep ----
      n_calls <- n_calls + 1
      if (n_calls %% 10 == 0) {
        if (verbose) {
          cat('Sleeping for', sleep_time, 'seconds...\n')
        }
        Sys.sleep(sleep_time)
      }
    }
  }

  return(results)
}

##################################################################

#' Get the proportion of iNaturalist observations reaching Research Grade for a country
#'
#' @param list_of_country_names A character string or vector of country names in English
#' @param verbose Logical. If `TRUE`, prints progress messages. Default is `FALSE`
#' @param return_vector Logical. If `TRUE`, returns a numeric vector instead of a tibble. Default is `FALSE`
#' @param sleep_time Numeric. Seconds to pause every 10 requests to avoid rate-limiting. Default is `10`
#'
#' @returns A tibble with columns: `country_name` and `p_research_grade`
#'   (proportion of verifiable iNaturalist observations in the country that have reached
#'   Research Grade quality)
#' @export
#'
#' @examples
#' getiNatResearchPropPerCountry('Uruguay')
#' getiNatResearchPropPerCountry(c('Brazil', 'Argentina'))
getiNatResearchPropPerCountry <- function(list_of_country_names,
                                          verbose       = FALSE,
                                          return_vector = FALSE,
                                          sleep_time    = 10) {
  
  results <- tibble(country_name     = character(),
                    p_research_grade = numeric())
  
  api  <- 'https://api.inaturalist.org/v1'
  page <- '&page=1&per_page=1'
  
  for (i in seq_along(list_of_country_names)) {
    
    country_name <- list_of_country_names[i]
    
    if (verbose) {
      cat('Fetching data for:', country_name, '\n')
    }
    
    place_id <- getCountryiNatPlaceID(country_name)
    
    if (!is.na(place_id)) {
      call_url_observations  <- str_glue('{api}/observations?verifiable=true&place_id={place_id}')
      call_url_research_grade <- str_glue('{api}/observations?verifiable=true&quality_grade=research&place_id={place_id}')
      
      get_json_call_observations <- GET(url = call_url_observations) %>%
        content(as = "text") %>% fromJSON(flatten = TRUE)
      
      get_json_call_research_grade <- GET(url = call_url_research_grade) %>%
        content(as = "text") %>% fromJSON(flatten = TRUE)
      
      n_obs          <- get_json_call_observations$total_results
      n_obs_research <- get_json_call_research_grade$total_results
    } else {
      n_obs          <- NA
      n_obs_research <- NA
    }
    
    results <- add_row(results,
                       country_name     = country_name,
                       p_research_grade = n_obs_research / n_obs)
    
    ## ---- sleep ----
    if (i %% 10 == 0) {
      if (verbose) {
        cat('Sleeping for', sleep_time, 'seconds...\n')
      }
      Sys.sleep(sleep_time)
    }
  }
  
  if (return_vector) {
    return(results$p_research_grade)
  }
  return(results)
}

getiNatResearchPropPerPlaceID <- function(list_of_place_ids,
                                          verbose       = FALSE,
                                          return_vector = FALSE,
                                          sleep_time    = 10) {
  
  results <- tibble(place_id         = numeric(),
                    p_research_grade = numeric())
  
  api  <- 'https://api.inaturalist.org/v1'
  page <- '&page=1&per_page=1'
  
  for (i in seq_along(list_of_place_ids)) {
    
    place_id <- list_of_place_ids[i]
    
    if (verbose) {
      cat('Fetching data for:', place_id, '\n')
    }
    
    if (!is.na(place_id)) {
      call_url_observations   <- str_glue('{api}/observations?verifiable=true&place_id={place_id}')
      call_url_research_grade <- str_glue('{api}/observations?verifiable=true&quality_grade=research&place_id={place_id}')
      
      get_json_call_observations <- GET(url = call_url_observations) %>%
        content(as = "text") %>% fromJSON(flatten = TRUE)
      
      get_json_call_research_grade <- GET(url = call_url_research_grade) %>%
        content(as = "text") %>% fromJSON(flatten = TRUE)
      
      n_obs          <- get_json_call_observations$total_results
      n_obs_research <- get_json_call_research_grade$total_results
    } else {
      n_obs          <- NA
      n_obs_research <- NA
    }
    
    results <- add_row(results,
                       place_id         = place_id,
                       p_research_grade = n_obs_research / n_obs)
    
    ## ---- sleep ----
    if (i %% 10 == 0) {
      if (verbose) {
        cat('Sleeping for', sleep_time, 'seconds...\n')
      }
      Sys.sleep(sleep_time)
    }
  }
  
  if (return_vector) {
    return(results$p_research_grade)
  }
  return(results)
}

##################################################################

#' Get the number of observers on iNaturalist for a country
#'
#' @param list_of_country_names A character string or vector of country names in English
#' @param verbose Logical. If `TRUE`, prints progress messages. Default is `FALSE`
#' @param return_vector Logical. If `TRUE`, returns a numeric vector instead of a tibble. Default is `FALSE`
#' @param sleep_time Numeric. Seconds to pause every 10 requests to avoid rate-limiting. Default is `10`
#'
#' @returns A tibble with columns: `country_name` and `n_users`
#'   (number of unique observers who have submitted verifiable observations on iNaturalist in the country)
#' @export
#'
#' @examples
#' getiNatUsersPerCountry('Uruguay')
#' getiNatUsersPerCountry(c('Brazil', 'Argentina'))
getiNatUsersPerCountry <- function(list_of_country_names,
                                   verbose       = FALSE,
                                   return_vector = FALSE,
                                   sleep_time    = 10) {
  
  results <- tibble(country_name = character(),
                    n_users      = numeric())
  
  api  <- 'https://api.inaturalist.org/v1/observations/observers'
  page <- '&page=1&per_page=1'
  
  for (i in seq_along(list_of_country_names)) {
    
    country_name <- list_of_country_names[i]
    
    if (verbose) {
      cat('Fetching data for:', country_name, '\n')
    }
    
    place_id <- getCountryiNatPlaceID(country_name)
    
    if (!is.na(place_id)) {
      call_url_observations <- str_glue('{api}?verifiable=true&place_id={place_id}')
      get_json_call_observations <- GET(url = call_url_observations) %>%
        content(as = "text") %>% fromJSON(flatten = TRUE)
      
      n_users <- get_json_call_observations$total_results
    } else {
      n_users <- NA
    }
    
    results <- add_row(results,
                       country_name = country_name,
                       n_users      = n_users)
    
    ## ---- sleep ----
    if (i %% 10 == 0) {
      if (verbose) {
        cat('Sleeping for', sleep_time, 'seconds...\n')
      }
      Sys.sleep(sleep_time)
    }
  }
  
  if (return_vector) {
    return(results$n_users)
  }
  return(results)
}

getiNatUsersPerPlaceID <- function(list_of_place_ids,
                                   verbose       = FALSE,
                                   return_vector = FALSE,
                                   sleep_time    = 10) {
  
  results <- tibble(place_id = numeric(),
                    n_users  = numeric())
  
  api  <- 'https://api.inaturalist.org/v1/observations/observers'
  page <- '&page=1&per_page=1'
  
  for (i in seq_along(list_of_place_ids)) {
    
    place_id <- list_of_place_ids[i]
    
    if (verbose) {
      cat('Fetching data for:', place_id, '\n')
    }
    
    if (!is.na(place_id)) {
      call_url_observations <- str_glue('{api}?verifiable=true&place_id={place_id}')
      get_json_call_observations <- GET(url = call_url_observations) %>%
        content(as = "text") %>% fromJSON(flatten = TRUE)
      
      n_users <- get_json_call_observations$total_results
    } else {
      n_users <- NA
    }
    
    results <- add_row(results,
                       place_id = place_id,
                       n_users  = n_users)
    
    ## ---- sleep ----
    if (i %% 10 == 0) {
      if (verbose) {
        cat('Sleeping for', sleep_time, 'seconds...\n')
      }
      Sys.sleep(sleep_time)
    }
  }
  
  if (return_vector) {
    return(results$n_users)
  }
  return(results)
}

##################################################################

#' Get the number of species recorded on iNaturalist for a country
#'
#' @param list_of_country_names A character string or vector of country names in English
#' @param verbose Logical. If `TRUE`, prints progress messages. Default is `FALSE`
#' @param return_vector Logical. If `TRUE`, returns a numeric vector instead of a tibble. Default is `FALSE`
#' @param sleep_time Numeric. Seconds to pause every 10 requests to avoid rate-limiting. Default is `10`
#'
#' @returns A tibble with columns: `country_name` and `n_species`
#'   (number of distinct species with at least one verifiable observation on iNaturalist in the country;
#'   excludes higher-rank taxa and ancestors)
#' @export
#'
#' @examples
#' getiNatSpeciesPerCountry('Uruguay')
#' getiNatSpeciesPerCountry(c('Brazil', 'Argentina'))
getiNatSpeciesPerCountry <- function(list_of_country_names,
                                     verbose       = FALSE,
                                     return_vector = FALSE,
                                     sleep_time    = 10) {
  
  results <- tibble(country_name = character(),
                    n_species    = numeric())
  
  api  <- 'https://api.inaturalist.org/v1/observations/species_counts'
  page <- '&page=1&per_page=1'
  
  for (i in seq_along(list_of_country_names)) {
    
    country_name <- list_of_country_names[i]
    
    if (verbose) {
      cat('Fetching data for:', country_name, '\n')
    }
    
    place_id <- getCountryiNatPlaceID(country_name)
    
    if (!is.na(place_id)) {
      call_url_observations <- str_glue('{api}?verifiable=true&place_id={place_id}&rank=species&include_ancestors=false')
      get_json_call_observations <- GET(url = call_url_observations) %>%
        content(as = "text") %>% fromJSON(flatten = TRUE)
      
      n_species <- get_json_call_observations$total_results
    } else {
      n_species <- NA
    }
    
    results <- add_row(results,
                       country_name = country_name,
                       n_species    = n_species)
    
    ## ---- sleep ----
    if (i %% 10 == 0) {
      if (verbose) {
        cat('Sleeping for', sleep_time, 'seconds...\n')
      }
      Sys.sleep(sleep_time)
    }
  }
  
  if (return_vector) {
    return(results$n_species)
  }
  return(results)
}

getiNatSpeciesPerPlaceID <- function(list_of_place_ids,
                                     verbose       = FALSE,
                                     return_vector = FALSE,
                                     sleep_time    = 10) {
  
  results <- tibble(place_id  = numeric(),
                    n_species = numeric())
  
  api  <- 'https://api.inaturalist.org/v1/observations/species_counts'
  page <- '&page=1&per_page=1'
  
  for (i in seq_along(list_of_place_ids)) {
    
    place_id <- list_of_place_ids[i]
    
    if (verbose) {
      cat('Fetching data for:', place_id, '\n')
    }
    
    if (!is.na(place_id)) {
      call_url_observations <- str_glue('{api}?verifiable=true&place_id={place_id}&rank=species&include_ancestors=false')
      get_json_call_observations <- GET(url = call_url_observations) %>%
        content(as = "text") %>% fromJSON(flatten = TRUE)
      
      n_species <- get_json_call_observations$total_results
    } else {
      n_species <- NA
    }
    
    results <- add_row(results,
                       place_id  = place_id,
                       n_species = n_species)
    
    ## ---- sleep ----
    if (i %% 10 == 0) {
      if (verbose) {
        cat('Sleeping for', sleep_time, 'seconds...\n')
      }
      Sys.sleep(sleep_time)
    }
  }
  
  if (return_vector) {
    return(results$n_species)
  }
  return(results)
}

##################################################################

#' Get the number of iNaturalist projects associated with a country
#'
#' @param list_of_country_names A character string or vector of country names in English
#' @param verbose Logical. If `TRUE`, prints progress messages. Default is `FALSE`
#' @param return_vector Logical. If `TRUE`, returns a numeric vector instead of a tibble. Default is `FALSE`
#' @param sleep_time Numeric. Seconds to pause every 10 requests to avoid rate-limiting. Default is `10`
#'
#' @returns A tibble with columns: `country_name` and `n_projects`
#'   (number of iNaturalist projects associated with the country's `place_id`)
#' @export
#'
#' @examples
#' getiNatProjectsPerCountry('Uruguay')
#' getiNatProjectsPerCountry(c('Brazil', 'Argentina'))
getiNatProjectsPerCountry <- function(list_of_country_names,
                                      verbose       = FALSE,
                                      return_vector = FALSE,
                                      sleep_time    = 10) {
  
  results <- tibble(country_name = character(),
                    n_projects   = numeric())
  
  api  <- 'https://api.inaturalist.org/v1'
  page <- '&page=1&per_page=1'
  
  for (i in seq_along(list_of_country_names)) {
    
    country_name <- list_of_country_names[i]
    
    if (verbose) {
      cat('Fetching data for:', country_name, '\n')
    }
    
    place_id <- getCountryiNatPlaceID(country_name)
    
    if (!is.na(place_id)) {
      call_url_projects <- str_glue('{api}/projects?place_id={place_id}')
      get_json_call_projects <- GET(url = call_url_projects) %>%
        content(as = "text") %>% fromJSON(flatten = TRUE)
      
      n_proj <- get_json_call_projects$total_results
    } else {
      n_proj <- NA
    }
    
    results <- add_row(results,
                       country_name = country_name,
                       n_projects   = n_proj)
    
    ## ---- sleep ----
    if (i %% 10 == 0) {
      if (verbose) {
        cat('Sleeping for', sleep_time, 'seconds...\n')
      }
      Sys.sleep(sleep_time)
    }
  }
  
  if (return_vector) {
    return(results$n_projects)
  }
  return(results)
}

getiNatProjectsPerPlaceID <- function(list_of_place_ids,
                                      verbose       = FALSE,
                                      return_vector = FALSE,
                                      sleep_time    = 10) {
  
  results <- tibble(place_id   = numeric(),
                    n_projects = numeric())
  
  api  <- 'https://api.inaturalist.org/v1'
  page <- '&page=1&per_page=1'
  
  for (i in seq_along(list_of_place_ids)) {
    
    place_id <- list_of_place_ids[i]
    
    if (verbose) {
      cat('Fetching data for:', place_id, '\n')
    }
    
    if (!is.na(place_id)) {
      call_url_projects <- str_glue('{api}/projects?place_id={place_id}')
      
      get_json_call_projects <- GET(url = call_url_projects) %>%
        content(as = "text") %>% fromJSON(flatten = TRUE)
      
      n_proj <- get_json_call_projects$total_results
    } else {
      n_proj <- NA
    }
    
    results <- add_row(results,
                       place_id   = place_id,
                       n_projects = n_proj)
    
    if (i %% 10 == 0) {
      if (verbose) {
        cat('Sleeping for', sleep_time, 'seconds...\n')
      }
      Sys.sleep(sleep_time)
    }
  }
  
  if (return_vector) {
    return(results$n_projects)
  }
  return(results)
}

##################################################################
##################################################################
##################################################################

#' Get the total surface area of a country
#'
#' Retrieves the most recent value of the total surface area (km2) for one or more countries
#' from the World Bank API (indicator `AG.SRF.TOTL.K2`).
#'
#' @param list_of_country_codes A character string or vector of ISO 3166-1 alpha-2 country codes
#' @param verbose Logical. If `TRUE`, prints progress messages. Default is `FALSE`
#' @param return_vector Logical. If `TRUE`, returns a numeric vector instead of a tibble. Default is `FALSE`
#' @param sleep_time Numeric. Seconds to pause every 10 requests to avoid rate-limiting. Default is `2`
#'
#' @returns A tibble with columns: `country_code` (ISO 3166-1 alpha-2 code) and
#'   `area` (total surface area in km2,
#'   World Bank indicator `AG.SRF.TOTL.K2`)
#' @export
#'
#' @examples
#' getAreaPerCountry('UY')
#' getAreaPerCountry(c('AR', 'BR', 'PY'))
getAreaPerCountry <- function(list_of_country_codes,
                              verbose       = FALSE,
                              return_vector = FALSE,
                              sleep_time    = 2) {
  
  results <- tibble(country_code = character(),
                    area         = numeric())
  
  api       <- 'https://api.worldbank.org/v2/en/country/'
  indicator <- 'AG.SRF.TOTL.K2'
  format    <- '?mrv=1&format=json'
  
  for (i in seq_along(list_of_country_codes)) {
    
    country_code <- list_of_country_codes[i]
    
    if (verbose) {
      cat('Fetching data for:', country_code, '\n')
    }
    
    call_url <- str_glue('{api}{country_code}/indicator/{indicator}{format}')
    
    get_json_call <- GET(url = call_url) %>%
      content(as = "text") %>% fromJSON(flatten = TRUE)
    
    if (length(get_json_call) == 2) {
      indicator_value <- as.numeric(get_json_call[[2]]$value)
    } else {
      indicator_value <- NA
    }
    
    results <- add_row(results,
                       country_code = country_code,
                       area         = indicator_value)
    
    ## ---- sleep ----
    if (i %% 10 == 0) {
      if (verbose) {
        cat('Sleeping for', sleep_time, 'seconds...\n')
      }
      Sys.sleep(sleep_time)
    }
  }
  
  if (return_vector) {
    return(results$area)
  }
  return(results)
}

######################################################################

#' Get the total population of a country
#'
#' Retrieves the most recent total population estimate for one or more countries
#' from the World Bank API (indicator `SP.POP.TOTL`).
#'
#' @param list_of_country_codes A character string or vector of ISO 3166-1 alpha-2 country codes
#' @param verbose Logical. If `TRUE`, prints progress messages. Default is `FALSE`
#' @param return_vector Logical. If `TRUE`, returns a numeric vector instead of a tibble. Default is `FALSE`
#' @param sleep_time Numeric. Seconds to pause every 10 requests to avoid rate-limiting. Default is `2`
#'
#' @returns A tibble with columns: `country_code` (ISO 3166-1 alpha-2 code) and
#'   `population` (total population, World Bank indicator `SP.POP.TOTL`)
#' @export
#'
#' @examples
#' getPopulationPerCountry('UY')
#' getPopulationPerCountry(c('AR', 'BR', 'PY'))
getPopulationPerCountry <- function(list_of_country_codes,
                                    verbose       = FALSE,
                                    return_vector = FALSE,
                                    sleep_time    = 2) {
  
  results <- tibble(country_code = character(),
                    population   = numeric())
  
  api       <- 'https://api.worldbank.org/v2/en/country/'
  indicator <- 'SP.POP.TOTL'
  format    <- '?mrv=1&format=json'
  
  for (i in seq_along(list_of_country_codes)) {
    
    country_code <- list_of_country_codes[i]
    
    if (verbose) {
      cat('Fetching data for:', country_code, '\n')
    }
    
    call_url <- str_glue('{api}{country_code}/indicator/{indicator}{format}')
    
    get_json_call <- GET(url = call_url) %>%
      content(as = "text") %>% fromJSON(flatten = TRUE)
    
    if (length(get_json_call) == 2) {
      indicator_value <- as.numeric(get_json_call[[2]]$value)
    } else {
      indicator_value <- NA
    }
    
    results <- add_row(results,
                       country_code = country_code,
                       population   = indicator_value)
    
    ## ---- sleep ----
    if (i %% 10 == 0) {
      if (verbose) {
        cat('Sleeping for', sleep_time, 'seconds...\n')
      }
      Sys.sleep(sleep_time)
    }
  }
  
  if (return_vector) {
    return(results$population)
  }
  return(results)
}

######################################################################

#' Get the GDP per capita of a country
#'
#' Retrieves the most recent GDP per capita (current USD) for one or more countries
#' from the World Bank API (indicator `NY.GDP.PCAP.CD`).
#'
#' @param list_of_country_codes A character string or vector of ISO 3166-1 alpha-2 country codes
#' @param verbose Logical. If `TRUE`, prints progress messages. Default is `FALSE`
#' @param return_vector Logical. If `TRUE`, returns a numeric vector instead of a tibble. Default is `FALSE`
#' @param sleep_time Numeric. Seconds to pause every 10 requests to avoid rate-limiting. Default is `2`
#'
#' @returns A tibble with columns: `country_code` (ISO 3166-1 alpha-2 code) and
#'   `gdp_per_capita` (GDP per capita in current USD, World Bank indicator `NY.GDP.PCAP.CD`)
#' @export
#'
#' @examples
#' getGDPperCapitaPerCountry('UY')
#' getGDPperCapitaPerCountry(c('AR', 'BR', 'PY'))
getGDPperCapitaPerCountry <- function(list_of_country_codes,
                                      verbose       = FALSE,
                                      return_vector = FALSE,
                                      sleep_time    = 2) {
  
  results <- tibble(country_code    = character(),
                    gdp_per_capita  = numeric())
  
  api       <- 'https://api.worldbank.org/v2/en/country/'
  indicator <- 'NY.GDP.PCAP.CD'
  format    <- '?mrv=1&format=json'
  
  for (i in seq_along(list_of_country_codes)) {
    
    country_code <- list_of_country_codes[i]
    
    if (verbose) {
      cat('Fetching data for:', country_code, '\n')
    }
    
    call_url <- str_glue('{api}{country_code}/indicator/{indicator}{format}')
    
    get_json_call <- GET(url = call_url) %>%
      content(as = "text") %>% fromJSON(flatten = TRUE)
    
    if (length(get_json_call) == 2) {
      indicator_value <- as.numeric(get_json_call[[2]]$value)
    } else {
      indicator_value <- NA
    }
    
    results <- add_row(results,
                       country_code   = country_code,
                       gdp_per_capita = indicator_value)
    
    ## ---- sleep ----
    if (i %% 10 == 0) {
      if (verbose) {
        cat('Sleeping for', sleep_time, 'seconds...\n')
      }
      Sys.sleep(sleep_time)
    }
  }
  
  if (return_vector) {
    return(results$gdp_per_capita)
  }
  return(results)
}

######################################################################

#' Get research and development expenditure as a percentage of GDP for a country
#'
#' Retrieves the most recent R&D expenditure (% of GDP) for one or more countries
#' from the World Bank API (indicator `GB.XPD.RSDV.GD.ZS`).
#'
#' @param list_of_country_codes A character string or vector of ISO 3166-1 alpha-2 country codes
#' @param verbose Logical. If `TRUE`, prints progress messages. Default is `FALSE`
#' @param return_vector Logical. If `TRUE`, returns a numeric vector instead of a tibble. Default is `FALSE`
#' @param sleep_time Numeric. Seconds to pause every 10 requests to avoid rate-limiting. Default is `2`
#'
#' @returns A tibble with columns: `country_code` (ISO 3166-1 alpha-2 code) and
#'   gdp_in_research (research and development expenditure as a percentage of GDP,
#'   World Bank indicator `GB.XPD.RSDV.GD.ZS`)
#' @export
#'
#' @examples
#' getGDPinResearchPerCountry('UY')
#' getGDPinResearchPerCountry(c('AR', 'BR', 'PY'))
getGDPinResearchPerCountry <- function(list_of_country_codes,
                                       verbose       = FALSE,
                                       return_vector = FALSE,
                                       sleep_time    = 2) {
  
  results <- tibble(country_code    = character(),
                    gdp_in_research = numeric())
  
  api       <- 'https://api.worldbank.org/v2/en/country/'
  indicator <- 'GB.XPD.RSDV.GD.ZS'
  format    <- '?mrv=1&format=json'
  
  for (i in seq_along(list_of_country_codes)) {
    
    country_code <- list_of_country_codes[i]
    
    if (verbose) {
      cat('Fetching data for:', country_code, '\n')
    }
    
    call_url <- str_glue('{api}{country_code}/indicator/{indicator}{format}')
    
    get_json_call <- GET(url = call_url) %>%
      content(as = "text") %>% fromJSON(flatten = TRUE)
    
    if (length(get_json_call) == 2) {
      indicator_value <- as.numeric(get_json_call[[2]]$value)
    } else {
      indicator_value <- NA
    }
    
    results <- add_row(results,
                       country_code    = country_code,
                       gdp_in_research = indicator_value)
    
    ## ---- sleep ----
    if (i %% 10 == 0) {
      if (verbose) {
        cat('Sleeping for', sleep_time, 'seconds...\n')
      }
      Sys.sleep(sleep_time)
    }
  }
  
  if (return_vector) {
    return(results$gdp_in_research)
  }
  return(results)
}

########################################################################

#' Get the latitude of a country's geographic centroid
#'
#' Computes the latitude of the geographic centroid of a country's polygon as a
#' proxy for distance from the equator (and thus expected biodiversity richness).
#'
#' @param list_of_country_codes A character string or vector of ISO 3166-1 alpha-2 country codes
#' @param verbose Logical. If `TRUE`, prints progress messages. Default is `FALSE`
#' @param return_vector Logical. If `TRUE`, returns a numeric vector instead of a tibble. Default is `FALSE`
#'
#' @returns A tibble with columns: `country_code` (ISO 3166-1 alpha-2 code) and
#'   `latitude` (latitude in decimal degrees of the country's geographic centroid,
#'   derived from `rnaturalearth` polygons)
#' @export
#'
#' @examples
#' getLatitudePerCountry('UY')
#' getLatitudePerCountry(c('AR', 'BR', 'PY'))
getLatitudePerCountry <- function(list_of_country_codes,
                                  verbose       = FALSE,
                                  return_vector = FALSE) {
  
  results <- tibble(country_code = character(),
                    latitude     = numeric())
  
  for (country_code in list_of_country_codes) {
    
    if (verbose) {
      cat('Fetching data for:', country_code, '\n')
    }
    
    country_name <- countrycode::countrycode(country_code,
                                             'iso2c',
                                             'country.name')
    
    latitude <- try(rnaturalearth::ne_countries(country      = country_name,
                                                returnclass  = 'sf') %>%
                      st_make_valid(), silent = TRUE)
    
    if (inherits(latitude, "try-error")) {
      latitude_value <- NA
    } else {
      latitude_value <- suppressWarnings(
        sf::st_coordinates(sf::st_centroid(latitude$geometry))[1, 2]
      )
    }
    
    results <- add_row(results,
                       country_code = country_code,
                       latitude     = latitude_value)
  }
  
  if (return_vector) {
    return(results$latitude)
  }
  return(results)
}

########################################################################

#' Get the number of species assessed by the IUCN Red List in a country
#'
#' Retrieves the number of species with an IUCN Red List threat assessment
#' whose native range includes the given country (global scope, latest assessments only).
#'
#' @param api_key A valid IUCN Red List API token (see <https://api.iucnredlist.org>)
#' @param list_of_country_codes A character string or vector of ISO 3166-1 alpha-2 country codes
#' @param verbose Logical. If `TRUE`, prints progress messages. Default is `FALSE`
#' @param return_vector Logical. If `TRUE`, returns a numeric vector instead of a tibble. Default is `FALSE`
#' @param sleep_time Numeric. Seconds to pause every 10 requests to avoid rate-limiting. Default is `2`
#'
#' @returns A tibble with columns: `country_code` (ISO 3166-1 alpha-2 code) and
#'   `iucn_species` (number of species assessed by the IUCN Red List in the country,
#'   filtered to global scope (`scope_code=1`) and latest assessments only)
#' @export
#'
#' @examples
#' getNspeciesPerCountry(api_key = Sys.getenv('IUCN_REDLIST_KEY'), list_of_country_codes = 'UY')
#' getNspeciesPerCountry(api_key = Sys.getenv('IUCN_REDLIST_KEY'), list_of_country_codes = c('AR', 'BR', 'PY'))
getNspeciesPerCountry <- function(api_key,
                                  list_of_country_codes,
                                  verbose       = FALSE,
                                  return_vector = FALSE,
                                  sleep_time    = 2) {
  
  base_url <- "https://api.iucnredlist.org/api/v4"
  endpoint <- "/countries"
  headers  <- add_headers(Authorization = paste("Bearer", api_key))
  
  results <- tibble(country_code = character(),
                    iucn_species = numeric())
  
  for (i in seq_along(list_of_country_codes)) {
    
    country_code <- list_of_country_codes[i]
    
    if (verbose) {
      cat('Fetching data for:', country_code, '\n')
    }
    
    api_url <- str_glue('{base_url}{endpoint}/{country_code}?latest=true&scope_code=1')
    
    response <- HEAD(api_url, headers)
    
    if (status_code(response) == 200) {
      iucn_species <- as.numeric(response$headers$`total-count`)
      
      if (length(iucn_species) == 0) iucn_species <- NA
    } else {
      print("Country not found.")
      iucn_species <- NA
    }
    
    results <- add_row(results,
                       country_code = country_code,
                       iucn_species = iucn_species)
    
    ## ---- sleep ----
    if (i %% 10 == 0) {
      if (verbose) {
        cat('Sleeping for', sleep_time, 'seconds...\n')
      }
      Sys.sleep(sleep_time)
    }
  }
  
  if (return_vector) {
    return(results$iucn_species)
  }
  return(results)
}

########################################################################

#' Check whether any neighbouring country has an iNaturalist Network node
#'
#' For each country, determines whether at least one of its land-border neighbours
#' is a member of the iNaturalist Network, based on spatial adjacency of country polygons.
#'
#' @param list_of_country_codes A character string or vector of ISO 3166-1 alpha-2 country codes
#' @param inat_nodes_names A character vector of country names (in English) that are
#'   members of the iNaturalist Network
#' @param verbose Logical. If `TRUE`, prints progress messages. Default is `FALSE`
#' @param return_vector Logical. If `TRUE`, returns a numeric vector instead of a tibble. Default is `FALSE`
#'
#' @returns A tibble with columns: `country_code` (ISO 3166-1 alpha-2 code) and
#'   `neighbour_has_node` (binary integer: `1` if at least one land-border neighbour
#'   is a member of the iNaturalist Network, `0` otherwise, `NA` if the spatial
#'   operation failed)
#' @export
#'
#' @examples
#' getIfNeighboursHaveSite(
#'   list_of_country_codes = c('UY', 'BO'),
#'   inat_nodes_names = c('Argentina', 'Brazil', 'Chile')
#' )
getIfNeighboursHaveSite <- function(list_of_country_codes,
                                    inat_nodes_names,
                                    verbose       = FALSE,
                                    return_vector = FALSE) {
  
  results <- tibble(country_code       = character(),
                    neighbour_has_node = numeric())
  
  world <- rnaturalearth::ne_countries(scale       = "medium",
                                       returnclass = "sf") %>%
    dplyr::select(iso_a2, geometry)
  
  inat_nodes_codes <- countrycode::countrycode(inat_nodes_names,
                                               "country.name",
                                               "iso2c")
  
  for (country_code in list_of_country_codes) {
    
    if (verbose) {
      cat("Checking neighbours for:", country_code, "\n")
    }
    
    neighbour_test <- try({
      
      target <- world %>% filter(iso_a2 == country_code)
      refs   <- world %>% filter(iso_a2 %in% inat_nodes_codes)
      
      touches            <- sf::st_touches(target, refs)[[1]]
      neighbour_countries <- refs$iso_a2[touches]
      
      as.integer(any(inat_nodes_codes %in% neighbour_countries))
      
    }, silent = TRUE)
    
    if (inherits(neighbour_test, "try-error")) {
      neighbour_value <- NA
    } else {
      neighbour_value <- neighbour_test
    }
    
    results <- add_row(results,
                       country_code       = country_code,
                       neighbour_has_node = neighbour_value)
  }
  
  if (return_vector) {
    return(results$neighbour_has_node)
  }
  return(results)
}

########################################################################

#' Retrieve all study variables for a set of countries
#'
#' A wrapper that sequentially calls all variable-retrieval functions and returns
#' a single tibble with one row per country and one column per variable.
#' Includes rate-limiting pauses between calls.
#'
#' @param df A tibble or data frame with at least two columns: `country_name`
#'   (country name in English) and `country_code` (ISO 3166-1 alpha-2 code).
#'   Row order must be consistent across both columns
#' @param IUCN_token A valid IUCN Red List API token, passed to `getNspeciesPerCountry`
#' @param inat_nodes_names A character vector of country names (in English) that are
#'   members of the iNaturalist Network, passed to `getIfNeighboursHaveSite`
#'
#' @returns A tibble extending `df` with 13 additional columns:
#'   `n_records`, `p_research_grade`, `n_users`, `n_species`,
#'   `n_projects`, `n_literature`, `area`, `population`,
#'   `gdp_per_capita`, `gdp_in_research`, `latitude`,
#'   `iucn_species`, `neighbour_has_node`
#' @export
#'
#' @examples
#' countries <- tibble(
#'   country_name = c('Uruguay', 'Argentina'),
#'   country_code = c('UY', 'AR')
#' )
#' getCountryVariables(
#'   df              = countries,
#'   IUCN_token      = Sys.getenv('IUCN_REDLIST_KEY'),
#'   inat_nodes_names = c('Brazil', 'Chile')
#' )
getCountryVariables <- function(df, IUCN_token, inat_nodes_names) {
  
  stopifnot(all(c("country_code", "country_name") %in% names(df)))
  
  start_time <- Sys.time()
  
  cat('Downloading variable 1/13\n')
  cat(' *** Number of records on iNat\n')
  n_records <- getiNatRecordsPerCountry(df$country_name,
                                        sleep_time    = 10,
                                        verbose       = TRUE,
                                        return_vector = TRUE)
  Sys.sleep(20)
  cat('\n')
  cat('Downloading variable 2/13\n')
  cat(' *** Proportion of records that reached Research Grade\n')
  p_research_grade <- getiNatResearchPropPerCountry(df$country_name,
                                                    sleep_time    = 20,
                                                    verbose       = TRUE,
                                                    return_vector = TRUE)
  Sys.sleep(20)
  cat('\n')
  cat('Downloading variable 3/13\n')
  cat(' *** Number of users on iNat\n')
  n_users <- getiNatUsersPerCountry(df$country_name,
                                    sleep_time    = 20,
                                    verbose       = TRUE,
                                    return_vector = TRUE)
  Sys.sleep(20)
  cat('\n')
  cat('Downloading variable 4/13\n')
  cat(' *** Number of taxa on iNat\n')
  n_species <- getiNatSpeciesPerCountry(df$country_name,
                                        sleep_time    = 20,
                                        verbose       = TRUE,
                                        return_vector = TRUE)
  Sys.sleep(20)
  cat('\n')
  cat('Downloading variable 5/13\n')
  cat(' *** Number of projects on iNat\n')
  n_projects <- getiNatProjectsPerCountry(df$country_name,
                                          sleep_time    = 20,
                                          verbose       = TRUE,
                                          return_vector = TRUE)
  Sys.sleep(20)
  cat('\n')
  cat('Downloading variable 6/13\n')
  cat(' *** Number of peer-reviewed papers in GBIF using iNat data\n')
  n_literature <- getGBIFcitationsPerCountry(df$country_code,
                                             sleep_time    = 2,
                                             verbose       = TRUE,
                                             return_vector = TRUE)
  Sys.sleep(20)
  cat('\n')
  cat('Downloading variable 7/13\n')
  cat(' *** Area\n')
  area <- getAreaPerCountry(df$country_code,
                            sleep_time    = 20,
                            verbose       = TRUE,
                            return_vector = TRUE)
  Sys.sleep(20)
  cat('\n')
  cat('Downloading variable 8/13\n')
  cat(' *** Population\n')
  population <- getPopulationPerCountry(df$country_code,
                                        sleep_time    = 20,
                                        verbose       = TRUE,
                                        return_vector = TRUE)
  Sys.sleep(20)
  cat('\n')
  cat('Downloading variable 9/13\n')
  cat(' *** GDP per capita\n')
  gdp_per_capita <- getGDPperCapitaPerCountry(df$country_code,
                                              sleep_time    = 20,
                                              verbose       = TRUE,
                                              return_vector = TRUE)
  Sys.sleep(20)
  cat('\n')
  cat('Downloading variable 10/13\n')
  cat(' *** % of GDP in research\n')
  gdp_in_research <- getGDPinResearchPerCountry(df$country_code,
                                                sleep_time    = 20,
                                                verbose       = TRUE,
                                                return_vector = TRUE)
  cat('\n')
  cat('Downloading variable 11/13\n')
  cat(' *** Latitude\n')
  latitude <- getLatitudePerCountry(df$country_code,
                                    verbose       = TRUE,
                                    return_vector = TRUE)
  cat('\n')
  cat('Downloading variable 12/13\n')
  cat(' *** Number of species according to IUCN\n')
  iucn_species <- getNspeciesPerCountry(api_key               = IUCN_token,
                                        list_of_country_codes = df$country_code,
                                        verbose               = TRUE,
                                        return_vector         = TRUE)
  cat('\n')
  cat('Downloading variable 13/13\n')
  cat(' *** Neighbour has a node in the iNat Network\n')
  neighbour_has_node <- getIfNeighboursHaveSite(list_of_country_codes = df$country_code,
                                                inat_nodes_names      = inat_nodes_names,
                                                verbose               = TRUE,
                                                return_vector         = TRUE)
  
  end_time <- Sys.time()
  cat('Total time taken:', end_time - start_time, '\n')
  
  per_country <- df %>%
    mutate(
      n_records          = n_records,
      p_research_grade   = p_research_grade,
      n_users            = n_users,
      n_species          = n_species,
      n_projects         = n_projects,
      n_literature       = n_literature,
      area               = area,
      population         = population,
      gdp_per_capita     = gdp_per_capita,
      gdp_in_research    = gdp_in_research,
      latitude           = latitude,
      iucn_species       = iucn_species,
      neighbour_has_node = neighbour_has_node
    )
}
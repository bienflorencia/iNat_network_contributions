# Contributions from the iNaturalist network

## Growing global, empowered locally: the importance of the iNaturalist network

*Florencia Grattarola <a dir="ltr" href="http://orcid.org/0000-0001-8282-5732" target="_blank"><img class="is-rounded" src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" width="15"></a>, Montserrat Almaraz, Carlos Galindo-Leal, Thomas Mesaglio, Colin D. Meurk, Peggy Newman, Leonel Roget, Carolina Soto-Vargas, Patrícia Tiago*

This repository ([https://github.com/bienflorencia/iNat_network_contributions](https://github.com/bienflorencia/iNat_network_contributions)) contains the code and data to reproduce our study on the contributions of iNaturalist Network countries to the global platform.


### iNaturalist Network

![](https://static.inaturalist.org/wiki_page_attachments/3354-original.png)

---

## **Code**

  - `R/variables_per_country.R`: an R file with all the functions used to download the variables per country.  
  - `R/random_forest_iNat.R`: an R file with the functions used to run the random forest models, generate the partial plots and the variable importance plots for the variables.  
  - `vignettes/iNat_network_contributions.qmd`: a Quarto file with the code used to download the data, generate summaries, run the models and produce figures.  
  - `vignettes/iNat_network_contributions.html`: an HTML file with the code used to download the data, generate summaries, run the models and produce figures.  

## **Data**

  - `UnitedStates_data_variables.rds`: an RDS file containing the variables for the US.  
  - `America_data_variables.rds`: an RDS file containing the variables for all countries in America (except the US).  
  - `Asia_data_variables.rds`: an RDS file containing the variables for all countries in Asia.  
  - `Europe_data_variables.rds`: an RDS file containing the variables for all countries in Europe.  
  - `Oceania_data_variables.rds`: an RDS file containing the variables for all countries in Oceania.  
  - `Global_data_variables.rds`:  an RDS file containing the variables for all countries in the four continents.  

Variables in the files: 
  - `continent`: continent where the country is located
  - `country_name`: country name in English         
  - `country_code`: country ISO two-letter code 
  - `site_name`: the name of the iNaturalist site in the country
  - `site_id`:  the site ID of the iNaturalist site in the country
  - `n_records_gbif`: the number of records in GBIF in the country      
  - `n_records_gbif_inat`: the number of records from iNaturalist in GBIF in the country
  - `n_records_inat`: the number of records in iNaturalist in the country
  - `n_users`: the number of users recording in iNaturalist in the country  
  - `n_taxa`: the number of taxa recorded in iNaturalist in the country     
  - `area`: the area of the country in km^2.  
  - `population`: the population of the country.              
  - `gdp_per_capita`: the country's GDP per capita.                 
  - `gdp_in_research`: the research and development expenditure (% of GDP) for a country.  
  - `latitude`: the latitude of a country's geographic centroid (as a proxy of biodiversity richness).        

## **Figures**

  - `partial_plots_n_records.png`: a PNG figure with the partial plots of the variables explaining the number of records in iNaturalist in a country.  
  - `partial_plots_p_gbif.png`: a PNG figure with the partial plots of the variables explaining the proportion of records from iNaturalist on GBIF in a country.  
  - `partial_plots_n_users.png`: a PNG figure with the partial plots of the variables explaining the number of users recording in iNaturalist in a country.   
  - `partial_plots_n_taxa.png`: a PNG figure with the partial plots of the variables explaining the number of taxa recorded in iNaturalist in a country.  
                     
  - `variable_importance_n_records.png`: a PNG figure with variable importance plots showing the contribution of variables to explain the number of records in iNaturalist in a country. 
  - `variable_importance_p_gbif.png`: a PNG figure with variable importance plots showing the contribution of variables to explain the proportion of records from iNaturalist on GBIF in a country.
  - `variable_importance_n_users.png`: a PNG figure with variable importance plots showing the contribution of variables to explain the number of users recording in iNaturalist in a country. 
  - `variable_importance_n_taxa.png`: a PNG figure with variable importance plots showing the contribution of variables to explain the number of taxa recorded in iNaturalist in a country.

## LICENCE

**Data** are available under the terms of the Creative Commons Attribution 4.0 International licence CC-BY 4.0 (https://creativecommons.org/licenses/by/4.0/legalcode.en).   

**Code** is available under the terms of the GPL-3.0 licence (https://www.gnu.org/licenses/gpl-3.0.html). 

## CITATION

> Grattarola F., Almaraz M., Galindo-Leal C., Mesaglio T., Meurk C.D., Newman P., Roget L., Soto-Vargas C., Tiago P. (2025) Growing global, empowered locally: the importance of the iNaturalist network. [Data/Code]


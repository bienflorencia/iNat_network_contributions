# Contributions from the iNaturalist network

## The impact of nationally organised efforts on global citizen-science platforms

*Florencia Grattarola <a dir="ltr" href="http://orcid.org/0000-0001-8282-5732" target="_blank"><img class="is-rounded" src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" width="15"></a>, Montserrat Almaraz, Corey Callaghan, Petr Keil, Cheng-Tao Lin, Thomas Mesaglio, Michelle Monge Velázquez, Gandhi Emanuel Ponce Juárez, Carolina Soto Vargas, Patricia Tiago, Alpo E. Turunen*

This repository ([https://github.com/bienflorencia/iNat_network_contributions](https://github.com/bienflorencia/iNat_network_contributions)) contains the code and data to reproduce our study on the contributions of iNaturalist Network countries to the global platform.


### iNaturalist Network

![](https://static.inaturalist.org/wiki_page_attachments/3354-original.png)

---

## Code

| File | Description |
|------|-------------|
| `R/funs_data_download.R` | Functions to download per-country variables and time series of number of records |
| `R/funs_random_forest.R` | Functions to run random forest models, and generate partial and variable importance plots |
| `R/data_download_time_series.R` | Code to download per-country time series of number of records |
| `R/data_download_variables.R` | Code to download all per-country variables |
| `vignettes/iNat_network_contributions.qmd` | Quarto source file to produce data summaries, run  models, and produces figures |
| `vignettes/iNat_network_contributions.html` | Rendered HTML output of the Quarto file above |

## Data

| File | Description |
|------|----------|
| `data_global_time_series.csv` | a csv file containing the number of records on iNaturalist for each country from 2011 to 2025 |
| `data_global_variables.csv` | a csv file containing all the response and explanatory variables for each country |
| `inat_nodes.csv` | a csv file containing information on the iNaturalist network members, such as the node id, name, type of organisation behind it (government, NGO, university, or museum), and year of creation |
| `inat_places.csv` | a csv file containing a list of all `place_id`s on iNat (from: http://www.inaturalist.org/places/inaturalist-places.csv.zip)  |


### Variables (per country)

**Identity**
- `country_name`: Country name in English
- `country_code`: ISO two-letter country code
- `region`: Geographic region
- `flag`: Flag emoji
- `place_id`: Place identifier on iNaturalist

**iNaturalist Network node** *(where applicable)*
- `node_name`: Name of the iNaturalist Network node
- `node_id`: ID of the iNaturalist Network node
- `node_type`: Institution type of the node
- `node_year`: Year the node was created
- `has_node`: Whether the country has a node in the iNaturalist Network
- `neighbour_has_node`: Whether a neighbouring country has a node

**iNaturalist activity**
- `n_records`: Number of observations
- `n_users`: Number of users who have submitted observations
- `n_taxa`: Number of taxa recorded
- `n_projects`: Number of projects created
- `n_literature`: Number of peer-reviewed articles that use iNaturalist data according to GBIF
- `p_research_grade`: Proportion of observations that reached Research Grade

**Socioeconomic and geographic context**
- `area`: Area (km²)
- `population`: Population (number of inhabitants)
- `gdp_per_capita`: GDP per capita (USD)
- `gdp_in_research`: Research and development (R&D) expenditure (% of GDP)
- `latitude`: Latitude of the geographic centroid (proxy for biodiversity richness)
- `iucn_species`: Number of species according to the IUCN Red List (assessed or a country)

## Figures

Each response variable has two associated figures: **partial plots** (showing the relationship between each predictor and the response) and **variable importance plots** (showing each predictor's relative contribution to the model).

| Response variable | Partial plots | Variable importance |
|---|---|---|
| Number of records | `partial_plots_n_records.png` | `variable_importance_n_records.png` |
 `variable_importance_p_research_grade.png` |
| Number of users | `partial_plots_n_users.png` | `variable_importance_n_users.png` |
| Number of species | `partial_plots_n_species.png` | `variable_importance_n_species.png` |
| Number of projects | `partial_plots_n_projects.png` | `variable_importance_n_projects.png` |
| Number of literature records | `partial_plots_n_literature.png` | `variable_importance_n_literature.png` |
| Proportion of observations that reached Research Grade | `partial_plots_p_research_grade.png` |

Other figures

| File | Description |
|---|---|---|
| `hist_response_variables.png` | Histograms of the 6 response variables |
| `hist_explanatory_variables.png` | Histograms of the 8 explanatory variables |
| `timeline_nodes.png.png` | Timeline of the creation of each node in the network and the type of organisation behind it (government, NGO, university, or museum) |
| `time_series_nodes.png.png` | Time series showing the number of records per year (from 2011 to 2025) for each country in the network (including the year it was created), compared to all countries |

## Documents

  - `poster_LivingData.pdf`: Poster presented at the Living Data Conference 2025, Bogotá (<https://www.livingdata2025.com/posters.html?poster=7020814>)


## Citation

> Grattarola F., Almaraz M., Callaghan C., Keil P., Lin C.-T., Mesaglio T., Monge Velázquez M., Ponce Juárez G.E., Soto Vargas C., Tiago P., Turunen A. (2026) The impact of nationally organised efforts on global citizen-science platforms. [Data/Code]

## License

**Data** are released under [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/legalcode.en).  
**Code** is released under [GPL-3.0](https://www.gnu.org/licenses/gpl-3.0.html).  

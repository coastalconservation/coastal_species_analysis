<h2 align="center"> 
  
<img src="https://github.com/coastalconservation/.github/blob/main/photos/cc-hexlogo-lowquality.png?raw=true" alt="Coastal Conservation Capstone group logo: hex sticker with rocky coastline and lighthouse illustration" width="300">

<h1 align="center">

Coastal Species Analysis Repository 



## Table of Contents 
[Overview](#overview)

[Folder Descriptions](#folder-descriptions)

[Data Sources](#data-sources)

[Repository Structure](#repository-structure)

[Authors and Contributors](#authors-and-contributors) 


## Overview 

This repository contains all scripts used to identify intertidal species ranges along the California coast, analyze historical range and abundance, develop species distribution models, and create habitat suitability maps. See the folder descriptions below for a detailed breakdown of the repository structure!


## Folder Descriptions 

### scripts: 

This folder contains all of the code scripts used to conduct our analysis. Each subfolder within this folder holds a different part of the analysis. 

--------------------------------------------------------------------------------------------------------------

**functions/** 

This folder contains functions used throughout the project. Here are the descriptions of each function and where each function is utilized:  


| File Name                                                 |  Description                          | Usage
| ----------------------------------------------------------| ------------------------------------- |----------------------------|
| `buffer_classification.R` |  Create range edge descriptions for each species at each buffer | range_classification/ | 
| `clean_biodiv.R` | Takes the MARINe CBS Excel file name and merges key sheets for presence-absence analysis | range_classification/,  range_shift_framework/,  modeling/ |
| `cumulative_density_dataframe.R` | Computes the cumulative sum of density and the normalized cumulative density for each species group and year | range_shift_framework/ | 
| `model_sing_species.R` | Runs an ensemble species distribution model for a single given species | modeling/ | 
| `range_classification.R` | Calculates species ranges within defined coastline segments using species biodiversity data | range_classification/ | 
| `range_extent_prediction.R` | Creates a visualization of a species' range boundaries over time by plotting the latitudinal extent for each year | range_shift_framework/ | 


**range_classification/**

This folder contains the scripts used in identifying the range edges of intertidal species along the California coast. To view more about the workflow of this part of the analysis, see the `CA_range_classification.qmd` document within this folder that outlines the methods used, sources the functions, and executes the analysis. 


**range_shift_framework/** 

This folder contains the scripts used in analyzing abundance trends of a select group of species that were identified as having a range edge near Point Conception. The purpose of this analysis is to view the density of the intertidal species along the coast and to view the range extent over time, to try and identify historical range shifts. To view the process of the analysis, see the `range_shift_analysis.qmd` document within this folder, which outlines the methods used, sources the functions, executes the empirical cumulative distribution analysis, and generates plots.  

**modeling/** 

This folder contains all the scripts used in producing the habitat suitability maps based on ensemble species distribution models for a select group of intertidal species that were identified as having a range edge near Point Conception. The purpose of this analysis is to model where suitable habitat for a particular species has been historically based on the MARINe biodiversity survey data, predict the suitable habitat in the year 2050 based on a middle-of-the-road emission scenario, and create a change detection map that identifies predicted loss of habitat. To view the process of this analysis, see the `INSERT QMD NAME` document in this folder that sources all the functions, runs the species distribution models, and prepares the habitat suitability maps for the dashboard.   


----------------------------------------------------------------------------------------------------------------------------------------------

### outputs: 



----------------------------------------------------------------------------------------------------------------------------------------------

## Data Sources 
----------------------------------------------------------------------------------------------------------------------------------------------
### Coastal Biodiversity Survey Data

**Description**: Community-wide biodiversity data of intertidal species along the Western coast of North America from Alaska to Baja California

**Data Access**: From the Multi-Agency Rocky Intertidal Network (MARINe) via [data request form](https://marine.ucsc.edu/explore-the-data/contact/data-request-form.html). Accessed by The Nature Conservancy and Dr. Erica Nielsen in early 2025. 

**Use in project**: This is the primary dataset that is used throughout all parts of our analysis: range edge categorization, range shift analysis, habitat suitability maps, and web dashboard.

**Citation**: marine.ucsc.edu (2024). Coastal Biodiversity Surveys | MARINe. [online] Available at: [/contact/data-request-form](https://marine.ucsc.edu/explore-the-data/contact/data-request-form.html) [Accessed 8 Jan. 2025].

-----------------------------------------------------------------------------------------------------------------------------------------------
### Bio-ORACLE Environmental variable rasters

**Description**: Environmental inputs for species distribution models

Environmental variables: 

- Air temperature (tas) 
- Mixed layer depth (mlotst)
- Ocean temperature (thetao)
- Salinity (so) 
- Cloud fraction (clt)
- Oxygen (o2)

Files used: 

*VARIABLES UNDER HISTORICAL/PRESENT TIME*

- tas_baseline_2000_2020_depthsurf
- o2_baseline_2000_2018_depthmax
- mlotst_baseline_2000_2019_depthsurf
- thetao_baseline_2000_2019_depthsurf
- so_baseline_2000_2019_depthsurf
- clt_baseline_2000_2020_depthsurf
                    
*PROJECTED VARIABLES UNDER SSP 460 FOR THE YEAR 2050*

- tas_ssp460_2020_2100_depthsurf
- o2_ssp460_2020_2100_depthmax
- mlotst_ssp460_2020_2100_depthsurf
- thetao_ssp460_2020_2100_depthsurf
- so_ssp460_2020_2100_depthsurf
- clt_ssp460_2020_2100_depthsurf

**Data Access**: These raster files were accessed via the Bio-ORACLE database in R using the `{biooracler}` package. Documentation about using this package to download the files can be found on the [biooracler GitHub repository](https://github.com/bio-oracle/biooracler).

**Use in project**: These environmental variable rasters were used as inputs for the ensemble species distribution models to produce habitat suitability maps.

**Citation**: v3.0 Assis, J., Fernández Bejarano, S.J., Salazar, V.W., Schepers, L., Gouvêa, L., Fragkopoulou, E., Leclercq, F., Vanhoorne, B., Tyberghein, L., Serrão, E.A., Verbruggen, H., De Clerck, O. (2024) Bio-ORACLE v3.0. Pushing marine data layers to the CMIP6 Earth system models of climate change research. Global Ecology and Biogeography. DOI: 10.1111/geb.13813

-----------------------------------------------------------------------------------------------------------------------------------------------
### California Boundary

**Description**: Shapefile of California used for mapping

**Data Access**: [US Census Bureau TIGER/Line Shapefiles](https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.2024.html#list-tab-790442341).

**Use in project**: Used in any map that contains the California boundary. 

**Citation**: U.S. Census Bureau. (2024). TIGER/Line Shapefile, 2024, state, California. U.S. Department of Commerce, U.S. Census Bureau. Retrieved [October 15, 2024]

-----------------------------------------------------------------------------------------------------------------------------------------------
### Dangermond Preserve Boundary 

**Description**: Shapefile of TNC's Jack and Laura Dangermond Preserve used for mapping

**Data Access**: Downloaded online from the [Dangermond Preserve Geospatial Hub via ArcGIS Online](https://dangermondpreserve-tnc.hub.arcgis.com/datasets/54ca8cdd3a0b4822b5f728ba34ca01f2_2/explore). 

**Use in project**: Used in mapping to show preserve location. 

**Citation**: (2020). JLDP Boundary (jldp_boundary) [Shapefile]. Retrieved October 15, 2024, from ArcGIS Online.

-----------------------------------------------------------------------------------------------------------------------------------------------
### California Coastline Segments

**Description**: Coastline linestring that spans the California coastline 

**Data Access**: Acquired via The Nature Conservancy and Dr. Erica Nielsen. (Created in QGIS, early 2025)

**Use in project**: Used in range edge classification and mapping 

-----------------------------------------------------------------------------------------------------------------------------------------------


## Repository Structure
```
coastal_species_analysis 
|
├── scripts/   # Analysis and modeling scripts
│	│
│	├── range_classifcation/      # Scripts for species list & map creation
│   	│	├──  CA_range-classification.qmd
│   	│	├──  ca_segment_classification.R
│   	│	├──  dangermond_range_edges.R
│   	│	├──  marine_sites.R
│   	│	└──  range_classificaton.R 
│	│
│	├── range_shift_framework/   # Scripts for range shift framework
│   	│	├── range_shift_analysis.qmd
│   	│	└── range_shift_classification.qmd
│	│
│	├── modeling/             # Scripts for species distribution modeling
│   	│	├──  ssdm_modeling_test_files
│   	│	├──  download-BioORACLE.R
│   	│	├──  single_species_model_server.qmd
│   	│	├──  single_species_model_server.rmarkdown
│   	│	├──  ssdm_modeling_test.html
│   	│	└──  ssdm_modeling_test.qmd
│	│
│	├── functions/     # Functions
│   	│	├── buffer_classification.R   
│   	│	├── clean_biodiv.R
│   	│	├── cumulative_density_dataframe.R
│   	│	├── model_sing_species.R 
│   	│	├── range_classification.R
│   	│	├── range_extent_prediction.R
│   	└──	└── range_extent_prediction.R
│
├── outputs/             # Results, figures, and reports
│       ├── tables/          # Summary tables
│       ├── figures/         # Maps and plots
│       └──  models/          # SDM results
│
├── README.md        # Project overview & setup instructions
│  
└── .gitignore           
```


## Authors and Contributors

#### Authors 

- Amanda Overbye  [GitHub](https://github.com/Aoverbye) | [Website](https://aoverbye.github.io/) | [LinkedIn](https://www.linkedin.com/in/amanda-overbye-3a6364161/) 
- Ian Morris-Sibaja  [GitHub](https://github.com/imsibaja) | [Website](https://imsibaja.github.io/) | [LinkedIn](https://www.linkedin.com/in/imsibaja/) 
- Jordan Sibley  [GitHib](https://github.com/jordancsibley) | [Website](https://jordancsibley.github.io/) | [LinkedIn](https://www.linkedin.com/in/jordancsibley/)  
- Matteo Torres  [GitHub](https://github.com/matteo-torres) | [Website](https://matteo-torres.github.io/) | [LinkedIn](https://www.linkedin.com/in/matteo-torres-876a62234/)

#### Client 

Dr. Erica Nielsen  | Anthony W. LaFetra Point Conception Research Fellow | The Nature Conservancy in California | erica.nielsen@tnc.org

#### Advisor 

Dr. Bruce Kendall | Bren School Professor; Associate Dean | kendall@bren.ucsb.edu |  [Bren page](https://bren.ucsb.edu/people/bruce-kendall)



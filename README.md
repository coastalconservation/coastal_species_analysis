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

This repository contains all scripts used to identify intertidal species ranges along the California coast, analyze contemporary ranges over time, develop species distribution models, and create habitat suitability maps. See the folder descriptions below for a detailed breakdown of the repository structure!


## Folder Descriptions 

### scripts: 

This folder contains all of the code scripts used to conduct our analysis. Each subfolder within this folder holds a different part of the analysis. 


- **range_classification/**

This folder contains the scripts used in identifying the range edges of intertidal species along the California coast. To view more about the workflow of this part of the analysis, see the `CA_range_classification.qmd` document within this folder that outlines the methods used, sources the functions, and executes the analysis. 


- **range_shift_analysis/** 

This folder contains the scripts used in analyzing range trends of a select group of species that were identified as having a range edge near Point Conception. The purpose of this analysis is to view the core range of the intertidal species along the coastline over time, to try and identify historical range shifts. To view the process of the analysis, see the `range_shift_analysis.qmd` document within this folder, which outlines the methods used, sources the functions, executes the empirical cumulative distribution analysis, and generates plots.  

- **modeling/** 

This folder contains all the scripts used in producing the habitat suitability maps based on ensemble species distribution models for a select group of intertidal species that were identified as having a range edge near Point Conception. The purpose of this analysis is to model where suitable habitat for a particular species has been historically based on the MARINe biodiversity survey data, predict the suitable habitat in the year 2050 based on a middle-of-the-road emission scenario, and create a change detection map that identifies predicted loss of habitat. To view the process of this analysis, see the `intertidal_species_esdms.qmd` document in this folder that sources all the functions, runs the species distribution models, and prepares the habitat suitability maps for the dashboard.   

- **priority_monitoring_assessment/**

This folder contains the script used in creating the species priority monitoring analysis. This analysis involves consolidating the results of the range classification, contemporary range shift, and future suitable habitat results as a way to identify intertidal species that are at risk of a range extension into or range contraction out of the Point Conception region. To view the process of this analysis, see the  `monitoring_priority_analysis.qmd` document within this folder.

- **functions/** 

This folder contains functions used throughout the project. Here are the descriptions of each function and where each function is utilized:  


| File Name                                                 |  Description                          | Project Component 
| ----------------------------------------------------------| ------------------------------------- |----------------------------|
| `calculate_suitability_change.R` |  Calculates both raw and percent change in habitat suitability between current and projected rasters for a list of species | modeling/, priority_monitoring_assessment/ | 
| `clean_biodiv.R` | Takes the MARINe CBS Excel file name and merges key sheets for presence-absence analysis | range_classification/,  range_shift_framework/,  modeling/ |
|`clean_isles_biodiv.R` | Same function as `clean_biodiv.R`, but it includes observations at MARINe sites on the Channel Islands |  modeling/ |
| `cumulative_density_dataframe.R` | Computes the cumulative sum of density and the normalized cumulative density for each species group and year | range_shift_framework/ | 
| `historic_range_trends.R` | Creates a visualization of a species' range boundaries over time by plotting the latitudinal extent for each year | range_shift_analysis/ | 
| `single_species_model.R` | Runs an ensemble species distribution model for a single given species | modeling/ | 



----------------------------------------------------------------------------------------------------------------------------------------------

### archive_data: 

This folder contains a subset of data used throughout the project. 


- **CA_coastal_buffer_modeling/** 

This folder contains a shapefile that is a 20 km buffer of the California coastline. It was used in the pre-processing of the Bio-ORACLE environmental rasters to mask out areas that were not along the coastline.

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
│
│	├── functions/     # Functions
│   	│	├── calculate_suitability_change.R
│   	│	├── clean_biodiv.R
│   	│	├── clean_isles_biodiv.R
│   	│	├── cumulative_density_dataframe.R
│   	│	├── historic_range_trends.R
│   	│	└── single_species_model.R
│	│
│	├── modeling/             # Scripts for species distribution modeling
│   	│	├──  download-BioORACLE.R
│   	│	├──  intertidal_species_esdms.qmd
│   	│	└──  raster_analysis.qmd
│	│
│	├── priority_monitoring_assessment/      # Scripts for species priority analysis 
│   	│	└── monitoring_priority_analysis.qmd
│	│
│	├── range_classifcation/      # Scripts for species list & map creation
│   	│	├──  CA_range-classification.qmd
│   	│	├── MARINe_sites_snapping.R
│   	│	├──  ca_segment_classification.R
│   	│	├──  marine_sites.R
│   	│	└──  species_range_classificaton.R 
│	│
│	├── range_shift_analysis/   # Scripts for range shift framework
│   	└──	└── range_shift_analysis.qmd
│	
│
├── archive_data/             
│       └──  CA_coastal_buffer_modeling/   # 20 km coastline buffer shapefile 
│
├── .gitignore 
│
├── LICENSE  # Mozilla Public License Version 2.0
│
├── README.md        # Project overview & setup instructions
│
├── coastal_species_analysis.Rproj
│
└──  session_info.txt  # Repository project dependencies        
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



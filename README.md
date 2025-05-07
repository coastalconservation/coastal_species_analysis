<h2 align="center"> 
  
<img src="https://github.com/coastalconservation/.github/blob/main/photos/cc-hexlogo-lowquality.png?raw=true" alt="Coastal Conservation Capstone group logo: hex sticker with rocky coastline and lighthouse illustration" width="300">

<h1 align="center">

Coastal Species Analysis Repository 





## Table of Contents 
[Overview](#overview)

[Data](#data)

[Folder Descriptions](#folder-descriptions)

[Repository Structure](#repository-structure)

[Authors and Contributors](#authors-and-contributors) 


## Overview 

This repository contains all scripts used to identify intertidal species ranges along the California coast, analyze historical range and abundance, develop species distribution models, and create habitat suitability maps. See the folder descriptions below for a detailed breakdown of the repository structure!

## Data 


## Folder Descriptions 

### scripts: 

This folder contains all of the code scripts used to conduct our analysis. Each subfolder within this folder holds a different part of the analysis. 

**functions/** 

This folder contains functions used throughout the project: 

- `buffer_classification.R`: Create range edge descriptions for each species at each buffer
- `clean_biodiv.R`: Takes the MARINe CBS Excel file name and merges key sheets for presence-absence analysis
- `cumulative_density_dataframe.R`: Computes the cumulative sum of density and the normalized cumulative density for each species group and year
- `cumulative_density_graph.R`: Creates a cumulative density graph for a given species to visualize abundance changes over space and time
- `model_sing_species.R`: Runs an ensemble species distribution model for a single given species
- `range_classification.R`: Calculates species ranges within defined coastline segments using species biodiversity data
- `range_extent_prediction.R`: Creates a visualization of a species' range boundaries over time by plotting the latitudinal extent for each year


## Repository Structure
```
coastal_species_analysis 
|
├── scripts/   # Analysis and modeling scripts
│	
│	├── R/
│   	│	├──  dangermond_range_edges.R
│   	│	├──  marine_sites.R
│	│
│	├── range_classifcation/      # Scripts for species list & map creation
│   	│	├──  CA_range-classification.qmd
│   	│	├──  
│	│
│	├── range_shift_framework/   # Scripts for range shift framework
│   	│	├── range_shift_analysis.qmd
│   	│	├── range_shift_classification.qmd
│	│
│	├── modeling/             # Scripts for species distribution modeling
│   	│	├──  ssdm_modeling_test_files
│   	│	├──  download-BioORACLE.R
│   	│	├──  single_species_model_server.qmd
│   	│	├──  single_species_model_server.rmarkdown
│   	│	├──  ssdm_modeling_test.html
│   	│	├──  ssdm_modeling_test.qmd
│	│
│	├── functions/     # Functions
│   	│	├── clean_biodiv.R  # Reads in and cleans the biodiv data      
│   	│	├── species_ranges.R  # create range edge descriptions 
│   	│	├── range_edge_list # produces list of range edge classifications 
│
├── outputs/             # Results, figures, and reports
│       ├── tables/          # Summary tables
│       ├── figures/         # Maps and plots
│       ├── models/          # SDM results
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

Dr. Bruce Kendall | Bren School Professor; Associate Dean | [Bren page](https://bren.ucsb.edu/people/bruce-kendall)



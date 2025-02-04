
<!-- README.md is generated from README.Rmd. Please edit that file -->

# human\_populations\_by\_coral\_reefs

Repository for the study titled “An assessment of people living by coral
reefs over space and time”

[![DOI](https://zenodo.org/badge/510611252.svg)](https://zenodo.org/badge/latestdoi/510611252)

## 1. Contents of repository

### [`data/`](https://github.com/amysw13/human_populations_by_coral_reefs/tree/main/data)

Contains

-   Coral reef buffers from 5 - 100 km, including US state buffers.
-   Coral reef countries details
-   Coral reef countries and US states spatial polygons

**Note:** LandScan and GADM data is not included here but can be
accessed from their websites

-   [LandScan](https://landscan.ornl.gov)
-   [GADM](https://gadm.org/data.html)

### [`scripts/`](https://github.com/amysw13/human_populations_by_coral_reefs/tree/main/scripts)

Contains scripts for buffer creation, population extractions, analysis
and plotting

### [`Output_pop/`](https://github.com/amysw13/human_populations_by_coral_reefs/tree/main/Output_pop)

Contains populations outputs from 2019 and 2020 only, as examples

### [`Plot/`](https://github.com/amysw13/human_populations_by_coral_reefs/tree/main/Plot)

Contains plots from the study

## 2. Study workflow

Use the following scripts in order to repeat population extractions and
analysis of the study

1.**buffers.R**

-   Coral reef data cleaning
-   Custom function to project coral reef buffers by country
-   Buffer creation for distances (5km, 10km, 30km, 50km, 100km)
-   Buffer data cleaning
-   Coral reef country spatial dataframe
-   US buffer creation

2.**population\_extraction\_functions.R**

-   Population extraction functions for use with LandScan raster data

3.**population\_extraction\_functions\_US.R**

-   Population extraction functions for US states associated with coral
    reefs

4.**Population\_merging\_wrangling.R**

-   Merging population data for coral reef countries
    -   Only 2020 and 2019 examples included in repository
    -   Obtaining population growth values from The World Bank
-   Summarising for “world” statistics (all coral reef countries)
-   Creating dataframes for plotting:
    -   Region
    -   SID
    -   Income\_group

5.**population\_plotting.R**

-   Plotting figures from study
    -   Excluding code for bump graphs

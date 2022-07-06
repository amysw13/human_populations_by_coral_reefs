
<!-- README.md is generated from README.Rmd. Please edit that file -->

# human\_populations\_by\_coral\_reefs

Repository for the study titled “An assessment of people living by coral
reefs over space and time”

## 1. Contents of repository

### `data-raw/`

Contains raw data

### `data/`

Contains coral reef buffers

### `scripts/`

Contains scripts for buffer creation, population extraction, analysis
and plotting

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

-   Merging population data by years

5.**Grouping\_population\_df.R**

-   Creating dataframes for:
    -   Region
    -   SID
    -   Income\_group

6.**Plotting\_main\_graphs.R**

-   Plotting the main figures found in the study

7.**Tables\_df.R**

-   Wrangling df’s to create tables for study

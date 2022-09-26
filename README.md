# Health of neonates born in the maternity hospital in Bern, Switzerland, 1880-1900 and 1914-1922

## Paper

submitted

## Data

The data is public available via Zenodo:
<br >



## Content of this repository

### Structure

```
.
+-- R
+-- data
+-- data_raw
+-- output


```

### `R` folder 

This folder contains all R scripts.
  
  - `data.R` : prepares the data for the analysis
  - `Flu_Exposure.R` : prepares the data for the flu  exposure
  - `Figure1.R` : code to create Figure 1
  - `Figure2.R` : code to create Figure 2
  - `Figure3.R` : code to create Figure 3
  - `Table1.R`  : code to create Table 1
  - `Supplement_Figure2.R`  : code to create Supplement Figure 2
  - `Supplement_Figure3to6.R`  : code to create Supplement Figure 3 to 6
  - `Supplement_Figure7and9.R`  : code to create Supplement Figure 7 and 9
  - `Supplement_Figure8.R`  : code to create Supplement Figure 8
  - `Supplement_Table3.R`  : code to create Supplement Table 3
  - `Supplement_Table4.R`  : code to create Supplement Table 4
  - `Supplement_Table5to7.R`  : code to create Supplement Table 5 to 7
  - `Supplement_Table8.R`  : code to create Supplement Table 8
  
### `data` folder

This folder contains the created data from `data.R` and `Flu_Exposure.R`
  - `data_flu.RData` : flu exposure for each week, Number_weeks indicates the number of weeks a women is exposed to the flu, weeks.range=the period in calender weeks when the woman was exposed
  - `databern.RData` : data including all information including exposure to flu

### `data_raw` folder
This folder contains the raw data for the analysis:
  - `Bern_birth.csv` : data from the maternity hospital in Bern for the years 1880- 1900 and 1914-1922
  
### `output` folder

This folder contains all outputs.

### `master.R` 

This skript contains information of the used R packages, R scripts, plotting parameters etc, please run first ` load("data/databern.RData")`.

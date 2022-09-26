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
  - `Bootstrapping.R` : function of the variable selection
  - `function_data.R` : function to define data for analysis
  - `lineareRegression.R` : code to calculate the adjusted R-square values of the respective models
  - `Table1_wilcox_chi2tests.R` : code to create table 1
  - `plots_body_composition.R` : code to create figure 1
  - `Appendix_Table1.R` : code to create figure 1
  
### `data` folder

This folder contains the data.
  - `data_study.csv` : contains socio-demographic, health- and lifestyle factors, and body scan variables of each participant
  - `healthscore.csv` : contains "healthy score" from the food frequency questions calculated from from five food categories: fruits, vegetables, wholegrain
products, meat, and sweet/salty snacks. For each category the officially recommended minimum or maximum amount of weekly intake was used as the cut-off value and a point was assigned if the recommendation was met. A score from 0 to 5 was built to reflect the overall healthiness of the diet.

### `output` folder

This folder contains the bootstrapping results of each outcome.

### `master.R` 

This skript contains information of the used R packages, number of bootstrapping, set seed etc.. To run the function `bootsptrapping.R`, please run first `data.R`.

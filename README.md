# Evaluating the feasibility of achieving building performance standards targets
This repository contains the data and code for the paper ["Evaluating the feasibility of achieving building performance standards targets"](https://doi.org/10.1016/j.enbuild.2023.112989), published in *Energy and Buildings*. 

## Contents  
- [Citation](#citation)  
- [Repository Structure](#repository-structure)  
- [Objective](#objective)  
- [Data](#data)  
- [Data Cleaning and Aggregation](#data-cleaning-and-aggregation)  
- [Analysis](#analysis)  
	- [Site EUI reduction](#site-eui-reduction)  
	- [Total energy savings](#total-energy-savings)  
	- [Economic investment](#economic-investment)	

## Citation
Webb, Amanda L., and Colby McConnell. 2023. “Evaluating the Feasibility of Achieving Building Performance Standards Targets.” Energy and Buildings 288 (June): 112989. https://doi.org/10.1016/j.enbuild.2023.112989.

## Repository Structure
The repository is divided into three directories:
- `/data/`: Cleaned building energy benchmarking data and supporting datasets used in this study. This directory has three sub-directories: 
	- `/benchmarking_data/`: Original downloaded benchmarking data (left empty; download relevant files from the benchmarking data links below)
	- `/cleaned_data/`: Cleaned and filtered benchmarking data for each city
	- `/aggregated_data/`: Single file aggregating the cleaned and filtered benchmarking data for each city
- `/analysis/`: R scripts for cleaning, aggregating, and analyzing the benchmarking data
- `/results/`: Output produced by R script

## Objective
The goal of this study was to evaluate the feasibility of achieving buidling performance standards (BPS) targets. BPS are a promising policy tool for reducing energy use and greenhouse gas emissions in existing buildings, and assessing the scope and extent of the investment required by BPS is critical for shaping their design and preparing jurisdictions for the effort ahead. Municipal benchmarking data for three property types—offices, multifamily housing, and K-12 schools—in 10 U.S. cities was analyzed and compared to the site energy use intensity (EUI) targets in ASHRAE Standard 100–2018. 

## Data
There are two types of data associated with this project: (1) building energy benchmarking data and (2) supporting data. 

### Benchmarking data
Benchmarking data from 10 U.S. cities was used as the basis for this analysis. This data is collected by each city through a mandatory benchmarking ordinance, and provides annual energy consumption and limited additional information (e.g., property type, floor area) for each building subject to the ordinance. For most cities, this data is made publicly available, and benchmarking data were downloaded from the relevant public website for each city. The 10 cities, year of benchmarking data (in parentheses), and data source (via hyperlink) used in this analysis are: [Boston (2018)](https://data.boston.gov/dataset/building-energy-reporting-and-disclosure-ordinance), [Denver (2019)](https://www.denvergov.org/opendata/dataset/city-and-county-of-denver-energize-denver-anonymized-benchmarking-data), [Los Angeles (2019)](https://data.lacity.org/City-Infrastructure-Service-Requests/Existing-Buildings-Energy-Water-Efficiency-EBEWE-P/9yda-i4ya), [New York City (2017)](https://www.nyc.gov/html/gbee/html/plan/ll84_scores.shtml), [Orlando (2019)](https://data.cityoforlando.net/dataset/BEWES-Building-Data/f63n-kp6t), [Philadelphia (2019)](https://opendataphilly.org/datasets/large-building-energy-benchmarking-data/), [San Francisco (2019)](https://data.sfgov.org/Energy-and-Environment/Existing-Buildings-Energy-Performance-Ordinance-Re/j2j3-acqj), [Seattle (2019)](https://data.seattle.gov/dataset/2019-Building-Energy-Benchmarking/3th6-ticf), St. Louis (2019), and [Washington DC (2017)](https://doee.dc.gov/publication/2017-building-benchmarking-dataset). Benchmarking data for St. Louis were not publicly available and were instead obtained by request from the City of St. Louis Office of Building Performance.

### Supporting data
The file [eui_targets.csv](data/eui_targets.csv) contains site EUI targets for each of the three property types and 10 cities analyzed in this study.  These targets were derived from the site EUI targets in ASHRAE Standard 100-2018, *Energy Efficiency in Existing Buildings*, Table 7-2a. These targets were developed by ASHRAE using data from the [2003 Commercial Building Energy Consumption Survey (CBECS)](https://www.eia.gov/consumption/commercial/data/2003/) and [2005 Residential Energy Consumption Survey (RECS)](https://www.eia.gov/consumption/residential/data/2005/) and then modified using building energy simulation, and represent the 25th percentile site EUI for each property type in that climate zone. The office and K-12 school targets used in this study represent an average of the ASHRAE Standard 100-2018 targets for those building types, and the multifamily housing target used the "Apartment building (5+ units)" property type. 

The file [retrofit_costs.csv](data/retrofit_costs.csv) contains retrofit cost estimates expressed in $/ft<sup>2</sup> for five different depths of energy savings: operational (less than 5% saved), light (5–15% saved), medium (15–25% saved), heavy (25–35% saved), and deep (>35% saved). The values were derived from the [Urban Green Council's Retrofit Market Analysis](https://www.urbangreencouncil.org/wp-content/uploads/2022/11/2019.06.18-Urban-Green-Retrofit-Market-Analysis.pdf), and represent the average of the low and high estimates for each category.   

The file [utility_costs.csv](data/utility_costs.csv) contains utility costs expressed in $/kBtu for each of the 10 cities analyzed in this study. Unit costs for electricity and natural gas were obtained for each city for the year of benchmarking data from the [U.S. Bureau of Labor Statistics](https://www.bls.gov/regions/midwest/data/averageenergyprices_selectedareas_table.htm), and were then averaged for each city to obtain a unit cost of energy. This averaging was based on the assumption that half of a building’s energy savings due to BPS could be attributable to natural gas and half to electricity.

## Data Cleaning and Aggregation

### Data cleaning
The R scripts with the naming convention `bps_data_cleaning_[city code].R` clean and filter the benchmarking data for each city. There are 10 separate R scripts, one for each city. The process used in each script is the same, but the specifics differ slightly from city to city due to differences in the benchmarking data.  The process has four major steps: (1) initial data cleaning, (2) data filtering, (3) duplicate removal, and (4) property type subsetting. An additional data check was also performed to compare reported vs. calculated site EUIs. 

#### Initial data cleaning
First, initial data cleaning removes any completely blank rows from the dataset and gives consistent names to the variables of interest:  

```
# Rename variables of interest 
minus_blank_bench <- minus_blank_bench %>% 
  rename(facility_type = `Property Type`,
		 floor_area = `Gross Area (sq ft)`,
		 site_eui = `Site EUI (kBTU/sf)`,
		 property_name = `Property Name`,
		 property_address = Address) 
```

The script then proceeds to remove commas (if present) from number formatting, as well as duplicates due to multiple years of reporting, leaving only data for the year of interest. 

#### Data filtering
Second, the data are filtered to remove missing and extreme observations. Three main criteria were used: (1) property type missing or listed as not available; (2) floor area missing, listed as not available, or listed with a value of 0; (3) site EUI missing, listed as not available, or out of range (less than or equal to 0 or greater than 1,000 kBtu/ft<sup>2</sup>).  

For each variable, the script first identifies and shows the observations meeting the filter criteria, to allow the user to see which properties will be filtered out and to determine whether it is reasonable to filter these out.  The observations meeting the filter criteria are then filtered out: 

```
# Remove observations  
filtered_bench <- minus_blank_bench %>% 
  # Remove observations with facility_type missing or NA
  filter(!is.na(facility_type), facility_type != "", facility_type != "Not Available") %>% 
  # Remove observations with floor area missing, NA, or 0 
  filter(!is.na(floor_area), floor_area != "", floor_area != 0) %>%  
  # Remove observations with site_eui missing or NA  
  filter(!is.na(site_eui) | site_eui != "") %>%
  # Remove observations with site EUIs outside the range 0-1000   
  filter(site_eui > 0 & site_eui <= 1000)     
```

#### Duplicate removal
Third, duplicate observations are removed by finding oberservations that have the same property name, address, and floor area. The script first identifies and shows these observations to allow the user to determine whether these properties are, in fact, duplicates. The script then proceeds to filter these out: 

```
# Remove dupliates based on multi-criteria matching
dups_removed_bench <- filtered_bench %>%
  filter(!duplicated(cbind(property_name, property_address, floor_area)))
```

#### Subset property types of interest
Finally, the dataset is then filtered down to only the three property types of interest in this analysis: office, multifamily housing, and K-12 schools:

```
# Subset observations for building types of interest
filtered_subset_bench <- dups_removed_bench %>% 
  filter(facility_type %in% c('Office', 'Multifamily Housing', 'K-12 School')) %>%
  droplevels()
```

#### Reported vs. calculated site EUI
An additional data check is then performed on the site EUI variable. The benchmarking data for six of the cities (Boston, Denver, New York City, Philadelphia, Seattle, and Washington DC) contains additional variables that can be used to calculate site EUI and compare it to the site EUI reported in the data. 

First, these variables are given consistent names, converted to numeric variables, and any blank values are replaced with 0. Then, they are used to calculate site EUI.  This calculated site EUI is then compared with the reported site EUI in the dataset, and observations that differ by more than +/-10% are flagged and summed.         

```
# Calculate site EUI from total energy use and find difference with reported site EUI 
filtered_subset_bench <- filtered_subset_bench %>% 
  mutate(
	total_site_energy = (electricity * 3.412) + (natural_gas * 100) + district_water + fuels,
	site_eui_calc = total_site_energy / floor_area,
	site_eui_diff = (site_eui- site_eui_calc) / site_eui, 
	# Flag observations with difference greater than +/-10%
	site_eui_diff_10 = as.factor(case_when(
	  site_eui_diff > 0.10 ~ "Yes",
	  site_eui_diff < -0.10 ~ "Yes",
	  TRUE ~ "No")))

# Count oberservations with difference greater than +/-10%
summary(filtered_subset_bench$site_eui_diff_10)
```

For all cities except Seattle, less than 2% of properties in the cleaned, filtered dataset had a reported site EUI that differed from the calculated site EUI by more than +/−10%. 

#### Export
The dataset is then reduced down to only the variables of interest commmon to all cities' datasets and exported to the the directory `../cleaned_data/`. The cleaned, filtered file for each city contains five variables for each observation in the dataset:

- `city`: the city in which the building is located
- `prop_id` : a unique ID assigned to each property in the dataset (specific to this analysis, differs from property IDs given by each individual city)
- `facility_type`: the property type
- `floor_area`: the total floor area, ft<sup>2</sup>
- `site_eui`: the annual site EUI, kBtu/ft<sup>2</sup>

### Data aggregation
The R script `bps_data_aggregation.R` aggregates the cleaned, filtered benchmarking data from each city into a single file for analysis.  The script reads all of the .csv files in the directory `../cleaned_data/` and binds them into a single file. 

```
# Read in csv files and bind together
cleaned_all_cities <- list.files(pattern='csv') %>% 
  read_csv() %>% 
  bind_rows()
```
The combined dataset is then exported to the the directory `../aggregated_data/` as the file [cleaned_all_cities.csv](data/aggregated_data/cleaned_all_cities.csv).  Data for St. Louis has been omitted from this file, since the benchmarking data for St. Louis are not publicly available.  

## Analysis
The R script `bps_data_analysis.R` replicates the analysis from the paper.

### Setup
It is recommended that you update to the latest versions of both R and RStudio (if using RStudio) prior to running this script. 

#### Load packages
First, load (or install if you do not already have them installed) the packages required for data analysis and plotting. 

```
# Load required packages for data analysis
library(tidyverse)

# Load required packages for plotting: viridis, cowplot
library(viridis)
library(cowplot)
```

Second, install the packages required for the marginal abatement cost curves (MACCs). The MACCs require the installation of the [ggmacc package](https://github.com/aj-sykes92/ggmacc) via GitHub.  In order to install this package, you first need to install [RTools](https://cran.r-project.org/bin/windows/Rtools/). Then, run the following code in R to load the `devtools` package, and then install and load the `ggmacc` package.   

```
# Load required packages for plotting: ggmacc
library(devtools)
devtools::install_github("aj-sykes92/ggmacc")
library(ggmacc)
```

#### Import cleaned benchmarking data
Import the cleaned and aggregated benchmarking data from the [cleaned_all_cities.csv](data/aggregated_data/cleaned_all_cities.csv) file.  The relative filepaths in this script follow the same directory structure as this Github repository, and it is recommended that you use this same structure.  You might have to use `setwd()` to set the working directory to the location of the R script.  

```
# Import cleaned benchmarking data
bench <- read_csv("../data/aggregated_data/cleaned_all_cities.csv",
  col_types = cols(city = "f", prop_id = "i", facility_type = "f",  floor_area = "i", site_eui = "d"))
```

### Data pre-processing
First, two new variables are created from the existing variables in the benchmarking data:

- `city_code`: a three-letter abbreviation for each city
- `floor_area_cat`: the variable `floor_area` categorized into five categories, ft<sup>2</sup>

Then, the site EUI targets and cost data are used to create several additional new variables. 

#### Add site EUI targets 
Import the site EUI targets from the [eui_targets.csv](data/eui_targets.csv) file and assign the relevant target to each building: 

```
# Import site EUI targets
eui_targets <- read_csv("../data/eui_targets.csv",
  col_types = cols(city = "f", facility_type = "f",  target = "i"))

# Assign targets to buildings
bench <- bench %>%
	left_join(eui_targets, by=c("city"="city", "facility_type"="facility_type"))
```

These targets are then used to create eight new variables: 

- `distance_target`: the reduction in site EUI needed to meet the target, kBtu/ft<sup>2</sup>
- `percentage_target`: the percentage reduction in site EUI needed to meet the target, %
- `percentage_target_cat10`: the variable `percentage_target` categorized into 10 categories, %
- `percentage_target_cat5`: the variable `percentage_target` categorized into 5 categories, %
- `percentage_target_cat6`: the variable `percentage_target` categorized into 6 categories, %
- `energy_use`: the total annual enegy use, kBtu 
- `energy_savings`: the annual energy savings that would result from the builing meeting its target, kBtu

#### Add cost data 
Import the retrofit costs from the [retrofit_costs.csv](data/retrofit_costs.csv) file and the utility costs from the [utility_costs.csv](data/utility_costs.csv) file.  The retrofit costs are then assigned to each building based on the property type and the percentage reduction in site EUI needed to meet the target. The utility costs are assigned to each building based on the city in which it is located.  

```
# Import retrofit costs
retrofit_costs <- read_csv("../data/retrofit_costs.csv",
  col_types = cols(percentage_target_cat5 = "f", facility_type = "f",  cost_per_ft2 = "d"))

# Import utility costs
utility_costs <- read_csv("../data/utility_costs.csv",
  col_types = cols(city = "f",  cost_per_kBtu = "d"))

# Assign costs to buildings
bench <- bench %>%
	left_join(retrofit_costs, by=c("percentage_target_cat5"="percentage_target_cat5", 
			  "facility_type"="facility_type")) %>%
	left_join(utility_costs, by=c("city"="city"))
```

These datasets are then used to create three new variables: 

- `capital_cost`: the capital cost that would be required to implement the retrofits needed to meet the BPS target, $
- `cost_savings`: the annual cost savings that would result from the builing meeting its target, $
- `payback`: the simple payback period for the retrofits needed to meet the BPS target, years

### Analysis and Results
The analysis is divided into three parts: (1) the reduction in site EUI needed to meet the targets (2) the total energy savings that would result from meeting the targets, (3) the economic investment required to meet the targets.

#### Site EUI reduction 
To determine the retrofit effort required to meet the target, the site EUI for each property is compared to its target.  The results are provided as boxplots of site EUI compared to the target, as summary statistics, and as a heatmap.  

**Boxplots**: First, site EUI percentiles for each city and building type are computed and compared to the target. The 10th, 25th, 50th, 75th, and 90th percentiles are computed. 

```
# Percentiles of site EUI by city and facility type
pct_eui <- bench %>% 
  group_by(facility_type, city_code) %>%
  summarize(pct_10 = quantile(site_eui, prob = 0.10),
			pct_25 = quantile(site_eui, prob = 0.25),
			pct_50 = quantile(site_eui, prob = 0.50),
			pct_75 = quantile(site_eui, prob = 0.75),
			pct_90 = quantile(site_eui, prob = 0.90),
			target = median(target)) %>%
  arrange(facility_type, desc(pct_50))
```

The results are provided as a table (shown here) and then visualized as a boxplot:

|facility_type       |city_code | pct_10| pct_25| pct_50|  pct_75| pct_90| target|
|:-------------------|:---------|------:|------:|------:|-------:|------:|------:|
|Office              |STL       |  41.20| 60.225|  81.10| 120.875| 153.50|     51|
|Office              |NYC       |  41.04| 57.700|  75.70| 101.925| 145.25|     51|
|Office              |PHL       |  43.76| 56.950|  71.70| 100.000| 144.94|     51|
|Office              |DEN       |  46.70| 55.600|  71.10|  89.400| 118.02|     46|
|Office              |BOS       |  42.18| 54.000|  69.70|  87.850| 115.00|     53|
|Office              |WDC       |  43.60| 51.525|  60.40|  73.100|  91.77|     51|
|Office              |ORL       |  29.00| 48.675|  55.00|  72.950| 116.75|     44|
|Office              |SEA       |  31.34| 39.450|  50.70|  66.300|  92.58|     45|
|Office              |LAX       |  15.92| 32.100|  46.80|  65.875|  89.08|     36|
|Office              |SFO       |  20.80| 30.600|  43.40|  58.600|  80.00|     37|

**Summary statistics**: Second, summary statistics are computed for the number of buildings not meeting the target, the total floor area not meeting the target, and the median reduction in site EUI needed to meet the target. Each statistic is represented as a number and as a percentage. 

```
# Summary statistics for buildings not meeting target: Office buildings 
summary_stats_over_office <- bench %>% 
  filter(facility_type == "Office", meets_target == "No") %>% 
  group_by(facility_type, city) %>% 
  summarise(count=n(),
  	    ft2 = sum(floor_area),
            reduce_eui = median(distance_target),
            reduce_pct = median(percentage_target)) %>%
  mutate(count_pct = count/count_bldgs$Office,
	 ft2_pct = ft2/total_area$Office) %>%
  relocate(count_pct, .after = count) %>%
  relocate(ft2_pct, .after = ft2) 
  ```
  
The results for office buildings:
  
|facility_type |city          | count| count_pct|       ft2|   ft2_pct| reduce_eui| reduce_pct|
|:-------------|:-------------|-----:|---------:|---------:|---------:|----------:|----------:|
|Office        |Boston        |   203| 0.7602996|  52508739| 0.8179539|      24.00|  0.3116883|
|Office        |Denver        |   288| 0.9085174|  47278223| 0.8928274|      28.30|  0.3808883|
|Office        |Los Angeles   |   467| 0.6847507|  76422359| 0.7300137|      21.50|  0.3739130|
|Office        |New York City |  1559| 0.8170860| 447684592| 0.9012041|      32.00|  0.3855422|
|Office        |Orlando       |    45| 0.8035714|   7889970| 0.8669194|      17.50|  0.2845528|
|Office        |Philadelphia  |   150| 0.8021390|  53879323| 0.9016794|      27.60|  0.3511450|
|Office        |San Francisco |   340| 0.6355140|  65923039| 0.8049981|      17.25|  0.3179718|
|Office        |Seattle       |   292| 0.6306695|  43692183| 0.6443538|      16.50|  0.2682907|
|Office        |St. Louis     |    54| 0.8181818|  12304342| 0.8463953|      37.45|  0.4234029|
|Office        |Washington DC |   381| 0.7559524|  89431081| 0.7405502|      15.20|  0.2296073|
  
**Heatmaps**:  Third, the distribution of reduction in site EUI needed to meet the target is quantified. Compared to the median reduction required, the distribution provides a better sense for how many buildings will require light, moderate, or deep energy retrofits. For each city and building type, buildings are "binned" by the percentage reduction in site EUI required to meet the target, and the number and percentage of buildings falling into each bin is computed. The results are summarized as as a table (shown here) and then visualized as a heatmap. 

|facility_type       |city_code |percentage_target_cat10 |    n|     n_pct|n_pct_cat |
|:-------------------|:---------|:-----------------------|----:|---------:|:---------|
|Office              |BOS       |0-10%                   |   27| 0.1330049|10-15%    |
|Office              |BOS       |10-20%                  |   32| 0.1576355|15-20%    |
|Office              |BOS       |20-30%                  |   38| 0.1871921|15-20%    |
|Office              |BOS       |30-40%                  |   40| 0.1970443|15-20%    |
|Office              |BOS       |40-50%                  |   27| 0.1330049|10-15%    |
|Office              |BOS       |50-60%                  |   25| 0.1231527|10-15%    |
|Office              |BOS       |60-70%                  |   11| 0.0541872|5-10%     |
|Office              |BOS       |70-80%                  |    1| 0.0049261|0-5%      |
|Office              |BOS       |80-90%                  |    1| 0.0049261|0-5%      |
|Office              |BOS       |90-100%                 |    1| 0.0049261|0-5%      |

#### Total energy savings 
To understand the total energy savings that would result from meeting the target, the total site energy use is computed for each property. The results are provided as a table of total energy savings, as a Pareto plot, and then as a breakdown of total savings by percentage reduction needed. 

**Total savings**: First, the total energy use for all of the benchmarked buildings is computed, as well as the total energy savings that would result from all buildings meeting their target.  The savings is also represented as a percentage of the total benchmarked energy use.  

```
# Total energy savings by city and facility type: Savings as % of all benchmarked kBtu
total_energy_savings <- bench %>% 
  group_by(facility_type, city) %>% 
  summarise(energy_use = sum(energy_use),
            energy_savings = sum(energy_savings)) %>%
  mutate(pct = energy_savings/energy_use) %>%
  pivot_wider(names_from = facility_type, 
	values_from = c("energy_use", "energy_savings", "pct"), 
	names_sep="_",
	names_vary = 'slowest')
```

**Pareto plot**: Second, a Pareto plot is used to understand the contribution of each building to the total energy savings.  Office buildings in Denver are used as an example.  To develop this plot, one dataset is created for all of the office buildings in Denver, ordered from least to greatest site EUI:

```
# Subset all office buildings for Denver
den_office_all <- bench %>% 
  filter(facility_type == "Office", city == "Denver") %>% 
  arrange(site_eui) %>%
  mutate(percentage_target_cat6 = recode_factor(percentage_target_cat6, 
		"<5%" = "<5", 
		"5-15%" = "<15", 	
		"15-25%" = "<25", 	
		"25-35%" = "<35", 
		"35-50%" = "<50", 
		">50%" = ">50"))
```

A second dataset is then created for just the buildings not meeting the target, ordered from least to greatest total site energy use. Two new variables are created, one for the percentage each building would contribute to the total energy savings, and another to represent the cumulative sum of the total energy savings.  

```
# Subset office buildings not meeting target for Denver: Ordered  
den_office_over_ord <- den_office_over %>%   
  arrange(desc(energy_savings)) %>%
  mutate(pct = energy_savings/sum(energy_savings),
		 pct_cumsum = cumsum(pct))
```

Each of these datasets is color-coded by percentage reduction in site EUI and plotted as side-by-side bar plots:

![Figure 5](/results/figure_5.png)

**Total savings by percentage reduction**: Third, the total energy savings is categorized by the percentage reduction in site EUI needed to meet the target. This shows how much of the total anticipated energy savings in a given city and for a given property type will be due to light, moderate, or deep energy retrofits. 

```
# Total energy savings by city and facility type: Grouped by % reduction 
total_energy_savings_by_pct_savings <- bench %>% 
  filter(meets_target == "No") %>% 
  group_by(facility_type, city_code,  percentage_target_cat6, .drop=FALSE) %>%
  summarise(energy_savings = sum(energy_savings)) %>%
  mutate(energy_savings_pct = energy_savings/sum(energy_savings))
 ```

The results are summarized as as a table (shown here) and then visualized as a stacked barplot. 

|facility_type       |city_code |percentage_target_cat6 | energy_savings| energy_savings_pct|
|:-------------------|:---------|:----------------------|--------------:|------------------:|
|Office              |BOS       |<5%                    |   2.882846e+06|          0.0019134|
|Office              |BOS       |5-15%                  |   4.955352e+07|          0.0328903|
|Office              |BOS       |15-25%                 |   1.356366e+08|          0.0900264|
|Office              |BOS       |25-35%                 |   3.036377e+08|          0.2015342|
|Office              |BOS       |35-50%                 |   5.523338e+08|          0.3666019|
|Office              |BOS       |>50%                   |   4.625866e+08|          0.3070337|

#### Economic investment
To evaluate the economic investment required to implement the retrofits needed to meet BPS targets, the capital costs, cost savings, and simple payback period are computed. The results are provided as a summary table of economic metrics and as marginal abatement cost curves (MACCs).    

**Economic metrics**: First, the median capital cost, sum of total capital costs, and median simple payback are computed for each city and property type.  These are summarized in a table. 

```
# Summary of investment costs by city and building type
summary_stats_economics <- bench %>% 
  group_by(facility_type, city) %>%
  filter(meets_target == "No") %>% 
  summarise(median_cost = median(capital_cost),
            total_cost = sum(capital_cost),
			payback = median(payback),) %>%  
  pivot_wider(names_from = facility_type, 
	values_from = c("median_cost","total_cost","payback"), 
	names_sep="_",
	names_vary = 'slowest')
```

**MACC curves**: Second, the [ggmacc package](https://github.com/aj-sykes92/ggmacc) is used to create MACCs that help evaluate the economic and energy impact of retrofitting each building. Two versions of the plot are created, one color-coded by precentage reduction in site EUI, and the other color-coded by floor area category. These are plotted as side-by-side plots:

![Figure 7](/results/figure_7.png)


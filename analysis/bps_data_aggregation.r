## -----------------------------------------------------------------------------------------------
## Title: bps_data_aggregation.R
## Purpose: Aggregates cleaned benchmarking data from each city into a single file
## Paper: "Evaluating the feasibility of achieving building performance standards targets"
## Author: Amanda Webb and Colby McConnell
## Date: March 1, 2023
## -----------------------------------------------------------------------------------------------

## Setup

# Load required packages for data analysis
library(dplyr)
library(readr)

## Read in files and bind

# Set working directory to cleaned data
setwd("../data/cleaned_data")

# Read in csv files and bind together
cleaned_all_cities <- list.files(pattern='csv') %>% 
  read_csv() %>% 
  bind_rows()

# Check dimensions of data
dim(cleaned_all_cities)

## Export

# Export as a CSV file
write_csv(cleaned_all_cities, "../aggregated_data/cleaned_all_cities.csv")


## -----------------------------------------------------------------------------------------------
## Title: bps_data_cleaning_sfo.R
## Purpose: Cleans and filters benchmarking data from San Francisco
## Paper: "Evaluating the feasibility of achieving building performance standards targets"
## Author: Amanda Webb and Colby McConnell
## Date: March 1, 2023
## -----------------------------------------------------------------------------------------------

## SETUP

# Load packages
library(dplyr)
library(tidyr)
library(readr)

# Set working directory to benchmarking data
setwd("../data/benchmarking_data")

# Import downloaded benchmarking data
all_bench <- read_csv("Existing_Buildings_Energy_Performance_Ordinance_Report.csv",
  locale=locale(encoding="latin1"), col_types = cols(.default = "c"))

# Check dimensions of data
dim(all_bench)
names(all_bench)
head(all_bench)
tail(all_bench)

## INITIAL DATA CLEANING

# Remove any rows that consist only of blank and/or NA
minus_blank_bench <- all_bench[!apply(is.na(all_bench) | all_bench == "", 1, all),]
tail(minus_blank_bench)

# Total observations in dataset 
dim(minus_blank_bench)

# Show blank and/or NA rows that got removed
anti_join(all_bench, minus_blank_bench)
dim(anti_join(all_bench, minus_blank_bench))

# Rename variables of interst 
minus_blank_bench <- minus_blank_bench %>% 
  rename(facility_type = `Property Type - Self Selected`,
		 floor_area = `Floor Area`,
		 site_eui = `2019 Site EUI (kBtu/ft2)`,
		 property_name = `Building Name`,
		 property_address = `Building Address`) 
names(minus_blank_bench)

# Eliminate commas in floor_area and site_eui
minus_blank_bench$floor_area <- gsub(',', '', minus_blank_bench$floor_area)
minus_blank_bench$site_eui <- gsub(',', '', minus_blank_bench$site_eui)

# Specify data types
str(minus_blank_bench)
minus_blank_bench <- minus_blank_bench %>% 
  mutate(facility_type = as.factor(facility_type),
		 floor_area = as.integer(floor_area),  
		 site_eui = as.numeric(site_eui)) 
str(minus_blank_bench)

# Remove duplicates due to multiple reporting years
# (San Francisco data has each year as columns, so only need to select column of interest) 

# Total observations in dataset for year of interest
dim(minus_blank_bench)

## DATA FILTERING

# Examine facility_type variable
levels(minus_blank_bench$facility_type)
summary(minus_blank_bench$facility_type)

# Recode facility_type variable
# (Not needed for San Francisco)

# Show observations with missing or NA facility_type
minus_blank_bench %>% 
  filter(is.na(facility_type) | facility_type == "" | facility_type == "Not Available" | 
	facility_type == "Pending final review" | facility_type == "MISSING DATA" | facility_type == "Unknown")
  
# Examine floor_area variable
summary(minus_blank_bench$floor_area)

minus_blank_bench %>%
  select(floor_area) %>%
  arrange(floor_area)

# Show observations with floor_area missing, NA, or 0
minus_blank_bench %>% 
  filter(is.na(floor_area) | floor_area == "" | floor_area == 0)

# Examine site_eui variable
summary(minus_blank_bench$site_eui)

minus_blank_bench %>%
  select(site_eui) %>%
  arrange(site_eui)
  
# Show observations with site_eui missing or NA
minus_blank_bench %>% 
  filter(is.na(site_eui) | site_eui == "")
  
# Show observations with site EUIs outside the range 0-1000 
minus_blank_bench %>% 
  filter(site_eui <= 0 | site_eui > 1000)  

# Remove observations  
filtered_bench <- minus_blank_bench %>% 
  # Remove observations with facility_type missing or NA
  filter(!is.na(facility_type), facility_type != "", facility_type != "Not Available" | 
    facility_type == "Pending final review" | facility_type == "MISSING DATA" | facility_type == "Unknown") %>% 
  # Remove observations with floor area missing, NA, or 0 
  filter(!is.na(floor_area), floor_area != "", floor_area != 0) %>%  
  # Show observations with site_eui missing or NA  
  filter(!is.na(site_eui) | site_eui != "") %>%
  # Remove observations with site EUIs outside the range 0-1000   
  filter(site_eui > 0 & site_eui <= 1000)   

# Total observations after filtering, before removing duplicates
dim(filtered_bench)

## DUPLICATE REMOVAL

# Examine duplicate observations using property_name matching only

# Show all instances (i.e, duplicates plus original)
filtered_bench %>%
  filter(duplicated(property_name) == TRUE | duplicated(property_name, fromLast=TRUE))

# Show duplicate instances only
filtered_bench %>%
  filter(duplicated(property_name))

# Examine duplicate observations using property_name, property_address, floor_area matching

# Show all instances (i.e, duplicates plus original)
filtered_bench %>%
  filter(duplicated(cbind(property_name, property_address, floor_area)) == TRUE | duplicated(cbind(property_name, property_address, floor_area), fromLast=TRUE))

# Show duplicate instances only
filtered_bench %>%
  filter(duplicated(cbind(property_name, property_address, floor_area)))

# Remove dupliates based on multi-criteria matching
dups_removed_bench <- filtered_bench %>%
  filter(!duplicated(cbind(property_name, property_address, floor_area)))

# Total observations after filtering, after removing duplicates
dim(dups_removed_bench)

## SUBSET PROPERTY TYPES OF INTEREST

# Subset observations for building types of interest
filtered_subset_bench <- dups_removed_bench %>% 
  filter(facility_type %in% c('Office', 'Multifamily Housing', 'K-12 School')) %>%
  droplevels()

# Total observations for property types of interest
summary(filtered_subset_bench$facility_type)
dim(filtered_subset_bench)

## REPORTED VS. CALCULATED SITE EUI
# (Not relevant for San Francisco)    
  
## SELECT VARIABLES OF INTEREST AND EXPORT DATA
export_bench <- filtered_subset_bench %>%
  select(facility_type, floor_area, site_eui) %>%
  mutate(city = "San Francisco",
		 prop_id = row_number()) %>%
  relocate(city, prop_id)   
  
# Export as a CSV file
write_csv(export_bench, "../cleaned_data/cleaned_san_francisco.csv")

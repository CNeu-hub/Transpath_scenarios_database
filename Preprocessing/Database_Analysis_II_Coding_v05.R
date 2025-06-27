###---------------------------------------------------------------------------###
###                     INTERVENTION SCENARIOS DATABASE ANALYSIS 2            ###
###                Recoding of interventions into groups, types, sectors      ###
###                Author: Christian Neumann                                  ###
###                Date: 10th June 2025                                       ###
###---------------------------------------------------------------------------###

###
#1.) Create environment####
###

#load libraries
library(tidyverse) 
library(tm) #needed for text mining analysis
library(readxl) 
library(xlsx)

#load needed functions (developed to work with database)
source("Functions/Coding_functions_v03.R")

#define input/output path 
datapath <- paste(getwd(), "/Input/", sep = "")

#load data output of I_Database_Analysis_Preparation.R
WP3_Subset <- readRDS(paste(datapath, "10_06_25_Preprocessed_Database_Perc_Ch.rds", sep = ""))

#load lookup table for recoding interventions in database
lookup <- read_excel(paste(datapath, "Interventions_lookup_v05.xlsx", sep = ""))
lookup <- lookup[2:6]
colnames(lookup) <- c("Interventions", "Intervention_agg_lvl1", "Intervention_type", "Intervention_category", "Intervention_sector")

###
#2.) Group similar interventions in intervention_aggregation_level1, intervention type, intervention category, and intervention sector based on lookup table####  
###

#convert all intervention columns to character for applying function
WP3_Subset[, 59:74] <- sapply(WP3_Subset[, 59:74], as.character)

#apply function to eliminate double entries of intervention categories
WP3_Subset[, 59:74] <- sapply(WP3_Subset[, 59:74], Unique_values)

#Now create new main intervention column, summarizing all information  
WP3_Subset$Interventions <- apply(WP3_Subset[59:74], 1, function(row) paste0(row, collapse = ","))

#Eliminate nas and double entries
WP3_Subset$Interventions <- Unique_values(WP3_Subset$Interventions)
WP3_Subset$Interventions <- Eliminate_nas(WP3_Subset$Interventions)

#Print table of all interventions, with frequencies of all interventions
Frequency(WP3_Subset$Interventions)  
print(WP3_Subset$Interventions)

#create columns to match colnames of lookup table 
cols = c("Interventions", "Intervention_agg_lvl1", "Intervention_type", "Intervention_category", "Intervention_sector")

WP3_Subset <- WP3_Subset %>%
  separate_rows(Interventions, sep = ",") %>%  # Split interventions into separate rows
  left_join(lookup, by = c("Interventions" = "Interventions")) %>%  # Join with lookup table
  group_by(Index) %>%  # Group by Index
  summarise(
    across(-all_of(cols), ~ first(.), .names = "{.col}"),  
    across(all_of(cols), ~ paste(., collapse = ","), .names = "{.col}")
    )

#Eliminate nas and double entries
WP3_Subset <- WP3_Subset %>%
  mutate(across(all_of(cols), ~ Unique_values(.), .names = "{.col}")) %>%
  mutate(across(all_of(cols), ~ Eliminate_nas(.), .names = "{.col}"))

#Last check if categorization was successfull
Frequency(WP3_Subset$Interventions)
Frequency(WP3_Subset$Intervention_agg_lvl1)
Frequency(WP3_Subset$Intervention_type)
Frequency(WP3_Subset$Intervention_sector)
Frequency(WP3_Subset$Intervention_category)

###
#3.) Create output####
###

#now select information that we are interested in 
WP3_Subset <- WP3_Subset %>%
  select(-c(Policies_EN, Economy_EN, Technology_EN, Lifestyle_change_EN, Climate_industry_policies_CLIMIND, Climate_industry_technology_CLIMIND, Other_climate_industry_CLIMIND,
            Policies_AGRIFOOD, Economy_AGRIFOOD, Technology_AGRIFOOD, Lifestyle_change_AGRIFOOD, Nature_LUBIO, Transport, Infrastructure, Building, Other_OTHER)) %>%
  relocate(c(Interventions, Intervention_agg_lvl1, Intervention_type, Intervention_category, Intervention_sector), .after = Baseline_policy_assumptions) %>%
  relocate(c(SDG13_Time_steps, `SDG13_Temperature_change_(Since pre-industrial age)_raw_data`, `SDG13_Carbon_price_(US$/t CO2)_raw_data`), .after = SDG13_Time_horizon_decades)

#safe resulting df as rds for later input/analysis
saveRDS(WP3_Subset,file = paste(datapath,"10_06_25_Final_Data_Perc_Ch.Rds"))

#Final xlsx output
#now delete , in each variable where entry starts with a , because of preprocessing with clean_entries_function:
WP3_Subset$Interventions <- clean_entries(WP3_Subset$Interventions)
WP3_Subset$Intervention_agg_lvl1 <- clean_entries(WP3_Subset$Intervention_agg_lvl1)
WP3_Subset$Intervention_type <- clean_entries(WP3_Subset$Intervention_type)
WP3_Subset$Intervention_sector <- clean_entries(WP3_Subset$Intervention_sector)
WP3_Subset$Intervention_category <- clean_entries(WP3_Subset$Intervention_category)

#load text description for describing cols output
descriptions <- read_excel(paste(datapath, "final_output_col_text_description.xlsx", sep = ""), col_names = FALSE)
descriptions <- as.vector(unlist(descriptions[1, ]))

#now rbind with WP3_Subset to create description of each column for final database output
WP3_Subset <- rbind(descriptions, WP3_Subset)

#safe resulting df as xlsx (final database output)
write.xlsx(WP3_Subset, file = paste(getwd(), "/Output/Database_v01_10_06_2025_cn.xlsx", sep = ""))

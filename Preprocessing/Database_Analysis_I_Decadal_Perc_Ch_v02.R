###---------------------------------------------------------------------------###
###                   INTERVENTION SCENARIOS DATABASE ANALYSIS 1              ###
###                Prepares scenarios database for further analysis.          ###
###                Author: Christian Neumann                                  ###
###                Date: 10th June 2025                                       ###
###---------------------------------------------------------------------------###

###
#1.) Create environment####
###

#load libraries
library(readxl) 
library(tidyverse) 

#define input/output path 
datapath <- paste0(getwd(), "/Input/")

#load TRANSPATH database
#WP3_Database <- read_excel(datapath %+% "/WP3_Database_R_DRAFT.xlsx")
WP3_Database <- read_excel(paste0(datapath, "Database_v02_06_06_25_cn.xlsx"), sheet = "Scenarios_database", skip = 3)

#subset database to essential information 
#Delete description from subset
WP3_Subset <- WP3_Database %>%
  select(-c("Energy_demand", "Land_use_change/deforestation_regulations", "Crop_yields", "Crop_input_intensity", "Food_demand", "Losses_wastes"), 
    -contains("Description"))

###
#2.) Calculate percentage change for each metric####
#(if not already given/extracted from main source)
###

#create vector to loop over metrics 
Indicators <- c("GDP_PPP", "Population_(mio. people)", "Total_primary_energy_use", "Cropland_area", "Forest_area", "Pasture_area", colnames(WP3_Subset %>% select(contains("SDG"), -ends_with("Impact"), -ends_with("horizon"), -ends_with("measurement"))))

for (col in Indicators) {
  
  sub <- WP3_Subset
  
  #split values into 4 parts (we collected data as follows: c(start, 2030, 2050, end))
  sub1 <- data.frame(str_split(sub[[col]], ",", n = 4, simplify = TRUE), stringsAsFactors = FALSE)
  
  #convert all columns to numeric
  sub1[1:4] <- sapply(sub1[1:4], as.numeric)
  
  #set denominator based on indicator (harmonizing temperature and co2 emissions by average temperature and emissions values for the reference period 1991-2020)
  if (col == "SDG13_Temperature_change_(Since pre-industrial age)") {
    denom <- 14.37 #Average ERA5 temperature for 1991-2020
  } else if (col == "SDG13_Total_CO2_emissions_(Mt CO2/yr)") {
    denom <- 30175.5 #Average EDGAR CO2 emissions for 1991-2020
  } else {
    denom <- NA
  }
  
  #function to calculate percentage change 
  calculate_percentage_change <- function(x) {
    if (!is.na(x[4])) {
      if (!is.na(denom)) {
        return(((x[4] - x[1]) / denom) * 100)
      } else {
        return(((x[4] - x[1]) / abs(x[1])) * 100)
      }
    } else {
      return(x[1]) #This is the case when we already had percentage changes in database (single values)
    }
  }
  
  #apply function rowwise
  sub[[col]] <- apply(sub1, 1, calculate_percentage_change)
  
  #assign result back to data
  WP3_Subset[[col]] <- sub[[col]]
  
}

#3.) Calculate percentage change/decade for each metric####
#separate time frame of indicator cols
time_frame_cols <- colnames(WP3_Subset %>% select(contains("Time_horizon")))

for(col in time_frame_cols) {
  
  sub <- WP3_Subset
  
  #split values into 4 parts (we collected data as follows: c(start, 2030, 2050, end))
  sub1 <- data.frame(str_split(sub[[col]], ",|-", n = 4, simplify = TRUE), stringsAsFactors = FALSE)
  
  #convert all columns to numeric 
  sub1[1:4] <- sapply(sub1[1:4],as.numeric)
  
  #Function to calculate time frame per row 
  calculate_time_frame <- function(x) {
    if (is.na(x[3]) || !is.numeric(x[1:4])) {
      return(x[2]-x[1]) #if percentage changes (then time horizon is given as a period like e.g., 2005-2100)
    } else {
      return(x[4] - x[1]) #if measured in units (then time horizon is given as time steps like e.g., 2005, 2030, 2050, 2100)
    }
  }
  
  #apply function to calculate covered time frame 
  sub[[col]] <- apply(sub1, 1, calculate_time_frame)

  WP3_Subset[[col]] <- sub[[col]]
  
  #create new column after time horizon column containing the final decades
  new_col_name <- paste0(col, "_decades")
  new_col_values <- WP3_Subset[[col]] / 10
  col_index <- which(names(WP3_Subset) == col)
  
  WP3_Subset <- WP3_Subset %>%
    add_column(!!new_col_name := new_col_values, .after = col_index)
}


#Calculate decadal percentage change for each indicator/assumption by dividing the percentage changes by the decades 
#Create indicators and assumptions vector to loop over 
assumptions <- grep("SDG", Indicators, value = TRUE, invert = TRUE)
indicators <- grep("SDG", Indicators, value = TRUE)

#Use lapply to apply function across indicator and assumptions columns 
WP3_Subset[indicators] <- lapply(indicators, function(x) {
  
  SDG <- gsub("^SDG(\\d+)_.*", "\\1", x) 
  
  WP3_Subset[[x]] <- WP3_Subset[[x]]/WP3_Subset[[paste0("SDG", SDG, "_Time_horizon_decades")]] 
  
})

WP3_Subset[assumptions] <- lapply(assumptions, function(x) {
  
  WP3_Subset[[x]] <- WP3_Subset[[x]]/WP3_Subset[[paste0("Quantitative_assumptions", "_time_horizon_decades")]] 
  
})

#4.) Extract relevant information and create final output rds####
#extract temperature and carbon prices, since we use those also for analysis without percentage changes/decade
sub_indicators <- WP3_Database %>% 
  select(Index, SDG13_Time_steps = SDG13_Time_horizon, `SDG13_Temperature_change_(Since pre-industrial age)_raw_data` = `SDG13_Temperature_change_(Since pre-industrial age)`, `SDG13_Carbon_price_(US$/t CO2)_raw_data` = `SDG13_Carbon_price_(US$/t CO2)`)

#Create output subset and select needed information
output <- WP3_Subset %>%
  left_join(., sub_indicators, by = c("Index")) %>%
  select(c(-contains("Type_of_measurement")))

#remove columns that are including only NA values from previous indicator separation
output[output == "NA"] <- NA
output <- output[, colSums(is.na(output)) != nrow(output)]

#now set all scenarios we dont have permission to share derived quantitative data to 0 for upload of data on Zenodo/Github
#create index vector containing the index values of all scenarios that use data from IIASA databases
#index <- c(1:5, 17:50, 71:95, 118:121, 285:293, 315:348, 352:510)

#output <- output %>%
#  mutate(across(all_of(Indicators), ~ ifelse(Index %in% index, NA, .)))

saveRDS(output, file = paste(datapath, "10_06_25_Preprocessed_Database_Perc_Ch.rds", sep = ""))

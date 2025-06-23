###---------------------------------------------------------------------------###
###                Plots wordcloud of interventions applied in scenarios      ###
###                Author: Christian Neumann                                  ###
###                Date: 28.04.2025                                           ###
###---------------------------------------------------------------------------###

###
#1.) Create environment####
###

library(tidyverse)
library(wordcloud)

#load functions 
source("Functions/Coding_functions_v03.R")

#define output paths
figpath <- paste(getwd(), "/Output/Figures/", sep = "")
tablepath <- paste(getwd(),"/Output/Tables/", sep = "")
datapath <- paste(getwd(), "/Input/", sep = "")

#load data 
WP3_Subset <- readRDS(file = paste(datapath, "10_06_25_Final_Data_Perc_Ch.Rds"))

###
#2.) Calculate interventions frequency using Frequency function for policy-screening and target-seeking scenarios####
###

#select policy-screening/target-seeking intervention scenarios 
WP3_Subset <- subset(WP3_Subset, WP3_Subset$`Interventions_Yes/No` == "Yes" & WP3_Subset$Scenario_class == "Intervention")

#apply function 
Interventions <- Frequency(WP3_Subset$Interventions)

cols <- colorRampPalette(c("#9ECAE1", "#08306B"))(50)

pdf(paste(figpath, sep = "", "Wordcloud_interventions.pdf"), width = 8, height = 8)
wordcloud(Interventions$Overview,Interventions$Freq,min.freq = 1, random.order = FALSE, colors = cols, fixed.asp = TRUE)
dev.off()

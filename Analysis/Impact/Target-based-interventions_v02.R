###---------------------------------------------------------------------------###
###                Plots interventions and their frequency that are applied in###
###                target-achieving (1.5°C, bending the curve) scenarios      ###
###                Author: Christian Neumann                                  ###
###                Date: 13.11.2024                                           ###
###---------------------------------------------------------------------------###

###
#1.) Create environment####
###

#load libraries
library(tidyverse)
library(patchwork)

#load functions
source("Functions/Coding_functions_v03.R")

#define output/input paths
figpath <- paste(getwd(), "/Output/Figures/", sep = "")
tablepath <- paste(getwd(),"/Output/Tables/", sep = "")
datapath <- paste(getwd(), "/Input/", sep = "")

#load data 
WP3_Subset <- readRDS(file = paste(datapath, "10_06_25_Final_Data_Perc_Ch.Rds"))

#select policy-screening/target-seeking intervention scenarios 
WP3_Subset <- subset(WP3_Subset, WP3_Subset$`Interventions_Yes/No` == "Yes" & WP3_Subset$Scenario_class == "Intervention" & WP3_Subset$Scenario_type == "policy-screening" | WP3_Subset$Scenario_type == "target-seeking")

###
#2.) Prepare 1.5°C scenarios data####
###

Temp_1.5 <- WP3_Subset %>%
  separate(`SDG13_Temperature_change_(Since pre-industrial age)_raw_data`, into = c("Start","2030","2050","End"), sep = ",") %>%
  filter(str_ends(SDG13_Time_steps, "2100")) %>%
  select(Index, Study_nr., Scenario, `SDG13_Temperature_change_(Since pre-industrial age)_raw_data` = End, Intervention_agg_lvl1) %>%
  mutate(`SDG13_Temperature_change_(Since pre-industrial age)_raw_data` = as.numeric(`SDG13_Temperature_change_(Since pre-industrial age)_raw_data`)) %>%
  filter(., `SDG13_Temperature_change_(Since pre-industrial age)_raw_data` < 1.5)

data <- Frequency(Temp_1.5$Intervention_agg_lvl1)
interventions <- as.vector(data$Overview)

#add up all interventions as separate column
for (i in interventions) {
  Temp_1.5[[i]] <- NA  
}

#fill columns if intervention was applied = 1 in column, if it was not applied in scenario = 0
for (name in interventions) {
  Temp_1.5[[name]] <- ifelse(grepl(name, Temp_1.5$Intervention_agg_lvl1), 1, 0)
}

Temp_1.5 <- pivot_longer(Temp_1.5, all_of(interventions), names_to = "Intervention", values_to = "Value")

#calculate the sum for each group
Scenarios_count <- Temp_1.5 %>%
  group_by(Intervention) %>%
  filter(., Value == 1) %>%
  reframe(Target = "Scenarios meeting the Paris Agreement's 1.5°C target (n = 150)", Scenarios = sum(Value))

###
#3.) Prepare bending the curve scenarios data####
###

#subset biodiversity indicators/metrics 
Bending <- WP3_Subset %>%
  select(Index, Study_nr., Scenario, starts_with("SDG15"), Intervention_agg_lvl1, -SDG15_Impact, -SDG15_Time_horizon, -SDG15_Time_horizon_decades, -SDG15_Deforestation, -`SDG15_Nitrogen_fixation_(Mt N/yr)`) #%>%
  
#invert indicators where an increase is not an benefit for consistency with other indicators!
Bending$`SDG15_Species_affected_by_50%_range_loss_(%)` <- Bending$`SDG15_Species_affected_by_50%_range_loss_(%)`*-1
Bending$`SDG15_Loss_suitable_habitat_(%)` <- Bending$`SDG15_Loss_suitable_habitat_(%)`*-1
Bending$`SDG15_Biodiversity_hotspot_loss_(%)` <- Bending$`SDG15_Biodiversity_hotspot_loss_(%)`*-1
Bending$`SDG15_Reduction_vascular_plant_species_(%)` <- Bending$`SDG15_Reduction_vascular_plant_species_(%)`*-1
Bending$SDG15_Extinction_per_million_species_years <- Bending$SDG15_Extinction_per_million_species_years*-1
Bending$`SDG15_Potentially_disappeared_fraction_of_species_(PDF)` <- Bending$`SDG15_Potentially_disappeared_fraction_of_species_(PDF)`*-1

Bending <- subset(Bending, Bending$`SDG15_Area_of_habitat_(ESH/AOH)` > 0 |  Bending$`SDG15_Biodiversity_intactness_index_(BII)` > 0 | Bending$SDG15_Extinction_per_million_species_years |
                    Bending$`SDG15_Fraction_globally_remaining_species_(cSAR_CB17BDM)` > 0 | Bending$`SDG15_Fraction_globally_remaining_species_(cSAR_US16BDM)` > 0 | Bending$`SDG15_Fraction_regionally_remaining_species_(FRRS)` > 0 | Bending$`SDG15_Geometric_mean_abundance_(GMA)` > 0 | Bending$`SDG15_Habitat_range_size_(%)` > 0 |
                    Bending$SDG15_INSIGHTS_index > 0 | Bending$`SDG15_Living_planet_index_(LPI)` > 0 | Bending$`SDG15_Loss_suitable_habitat_(%)` > 0 | Bending$`SDG15_Mean_species_abundance_terrestrial_(MSA)` > 0 | Bending$`SDG15_Mean_species_richness_(Species p. grid cell)` > 0 |
                    Bending$`SDG15_Potentially_disappeared_fraction_of_species_(PDF)` > 0 | Bending$`SDG15_Pressure-based natural capital index (NCI-pb)` > 0 | Bending$`SDG15_Red_list_index_(RLI)` > 0 | Bending$`SDG15_Reduction_vascular_plant_species_(%)` > 0 | Bending$`SDG15_Species_affected_by_50%_range_loss_(%)` > 0 | Bending$`SDG15_Species_range_protection_level_(%)` > 0)

data <- Frequency(Bending$Intervention_agg_lvl1)
interventions <- as.vector(data$Overview)

#add up all interventions as separate column
for (i in interventions) {
  Bending[[i]] <- NA  
}

#fill columns if intervention was applied = 1 in column, if it was not applied in scenario = 0
for (name in interventions) {
  Bending[[name]] <- ifelse(grepl(name, Bending$Intervention_agg_lvl1), 1, 0)
}

Bending <- pivot_longer(Bending, all_of(interventions), names_to = "Intervention", values_to = "Value")

Scenarios_count2 <- Bending %>%
  group_by(Intervention) %>%
  filter(., Value == 1) %>%
  reframe(Target = "Scenarios reversing declining biodiversity trends (n = 14)", Scenarios = sum(Value))

###
#4.) Plot everything####
###

Scenarios_plot_data <- rbind(Scenarios_count, Scenarios_count2)

Scenarios_1.5 <- subset(Scenarios_plot_data, Scenarios_plot_data$Target == "Scenarios meeting the Paris Agreement's 1.5°C target (n = 150)")

a <- ggplot(data = Scenarios_1.5, aes(x = Scenarios, y = reorder(Intervention, Scenarios, desc), fill = Target)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c("#DDDDDD")) +
  labs(x = "No. of scenarios", title = "Scenarios meeting the Paris Agreement's 1.5°C target (n = 150)", y = "", fill = "") +
  geom_vline(xintercept = 0) +
  theme_classic() +
  theme(legend.position = "none", 
        legend.title = element_text(size = 14, face = "bold"), 
        legend.text = element_text(size = 14),
        legend.justification = "left") +
  guides(color = guide_legend(byrow = TRUE)) +
  theme(axis.text.y = element_text(size=14,hjust=1),
        axis.text.x = element_text(size=14,hjust=0),
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0),size=14,face="bold"),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0),size=14,face="bold"),
        plot.title = element_text(face = "bold", hjust =0, size=14))
a

Scenarios_bending <- subset(Scenarios_plot_data, Scenarios_plot_data$Target == "Scenarios reversing declining biodiversity trends (n = 14)")

b <- ggplot(data = Scenarios_bending, aes(x = Scenarios, y = reorder(Intervention, Scenarios, desc), fill = Target)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c("darkgrey")) +
  labs(x = "No. of scenarios", y = "", title = "Scenarios reversing declining biodiversity trends (n = 14)", fill = "") +
  geom_vline(xintercept = 0) +
  theme_classic() +
  #scale_y_discrete(position = "right") +
  #scale_x_continuous(labels = abs) +
  theme(legend.position = "none", 
        legend.title = element_text(size = 14, face = "bold"), 
        legend.text = element_text(size = 14),
        legend.justification = "left") +
  guides(color = guide_legend(byrow = TRUE)) +
  theme(axis.text.y = element_text(size=14,hjust=1),
        axis.text.x = element_text(size=14,hjust=0),
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0),size=14,face="bold"),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0),size=14,face="bold"),
        plot.title = element_text(face = "bold", hjust =0, size=14))
b

pdf(paste(figpath, "Interventions_Targets_v02.pdf", sep = "/"), height = 12, width = 12)
a/b 
dev.off()


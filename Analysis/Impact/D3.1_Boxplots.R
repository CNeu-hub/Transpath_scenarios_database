###---------------------------------------------------------------------------###
###           Plots Boxplots for scenarios impacts on climate and biodiv.     ###
###           Impacts per no. intervention/sector, archetype, SSP baseline    ###
###           Author: Christian Neumann                                       ###
###           Date: 22.02.2025                                                ###
###---------------------------------------------------------------------------###

###
#1.) Create environment####
###

#load libraries
library(tidyverse)
library(ggpubr)
library(colorspace)
library(RColorBrewer)

#define output/input paths
figpath <- paste(getwd(), "/Output/Figures/", sep = "")
tablepath <- paste(getwd(),"/Output/Tables/", sep = "")
datapath <- paste(getwd(), "/Input/", sep = "")

#load data 
WP3_Data <- readRDS(file = paste(datapath, "10_06_25_Final_Data_Perc_Ch.Rds"))

#data preparation for plotting
data_plot <- WP3_Data %>%
  select(Index, Study_nr., Scenario, Scenario_type, Scenario_class, Archetype, Socioeconomic_scenario, Emissions_scenario, Intervention_agg_lvl1, Intervention_category, Intervention_sector,
         `SDG13_Temperature_change_(Since pre-industrial age)`,
         `SDG13_Total_CO2_emissions_(Mt CO2/yr)`,
         `SDG15_Biodiversity_intactness_index_(BII)`,
         `SDG15_Mean_species_abundance_terrestrial_(MSA)`) %>%
  pivot_longer(., c("SDG13_Temperature_change_(Since pre-industrial age)", "SDG13_Total_CO2_emissions_(Mt CO2/yr)", 
                    "SDG15_Biodiversity_intactness_index_(BII)", "SDG15_Mean_species_abundance_terrestrial_(MSA)"), names_to = "Indicator", values_to = "Indicator_value") %>%
  filter(., !is.na(Indicator_value)) %>%
  mutate(Indicator = case_when(Indicator == "SDG13_Temperature_change_(Since pre-industrial age)" ~ "Temperature change",
                               Indicator == "SDG13_Total_CO2_emissions_(Mt CO2/yr)" ~ "Total CO2 emissions",
                               Indicator == "SDG15_Biodiversity_intactness_index_(BII)" ~ "Biodiversity intactness index",
                               Indicator == "SDG15_Mean_species_abundance_terrestrial_(MSA)" ~ "Mean species abundance"))

###
#2.) Scenario type/class boxplot####
###

#remove exploratory scenarios because some of those are also aiming for sustainable development, this will conflict with sectors and number of interventions plots (distorting results)
#we could not extract the interventions applied within those scenarios so they appear in those plots with 0 interventions in 0 sectors, but this will distort the control group for those plots, so we create another dataset for the scenarios type, archetypes, ssp and rcp plots here
data_plot2 <- data_plot
data_plot <- subset(data_plot, data_plot$Scenario_type != "exploratory")

#exclude all combinations with n >=3
data_counts_scenario_class <- data_plot2 %>%
  count(Indicator, Scenario_class, Scenario_type) %>%
  filter(., n >= 3) %>%
  mutate(n = case_when(Indicator == "Biodiversity intactness index" & Scenario_class == "Modest-intervention" ~ NA_real_,
    TRUE ~ as.numeric(n)))

col_pal <- grey.colors(7)
col_pal
swatchplot(col_pal)

scales <- list(
  scale_x_continuous(limits = c(-1, 1)),
  scale_x_continuous(limits = c(-3, 1)),
  scale_x_reverse(limits = c(3, -1)),
  scale_x_reverse(limits = c(30, -30))
)
data_plot2 <- data_plot2 %>%
  mutate(
    Scenario_type = factor(Scenario_type, 
                           levels = c("policy-screening", "exploratory", "reference", "target-seeking"), 
                           ordered = TRUE)  # Ensure it's ordered
  )

#Boxplot for scenario class depending on scenario type
#filter dataset, to exclude combination with n <3
scenario_class_plot <- ggplot(data = data_plot2 %>% group_by(Indicator, Scenario_class, Scenario_type) %>%
                                filter(n() >= 3) %>% ungroup(), 
                              aes(x = Indicator_value, y = Scenario_class, fill = Scenario_type)) +
  #ggbeeswarm::geom_beeswarm(position = position_dodge(), cex = 0.5, alpha = 0.5) +
  geom_boxplot(outlier.shape = NA) +
  #geom_point() +
  scale_fill_manual(values = c("reference" = "#4D4D4D",
                               "target-seeking" = "#AEAEAE",
                               "policy-screening" = "#969696",
                               "exploratory" = "#DDDDDD")) + 
  geom_text(data = data_counts_scenario_class, aes(x = case_when(Indicator == "Biodiversity intactness index" ~ 0.7, 
                                                                 Indicator == "Mean species abundance" ~ 0.7, 
                                                                 Indicator == "Temperature change" ~ -0.6,
                                                                 TRUE ~ -25), 
                                                   y = as.numeric(as.factor(Scenario_class)) + 
                                                     case_when(
                                                       Scenario_type == "policy-screening" ~ -0.28,
                                                       Scenario_type == "exploratory"     ~ -0.08,
                                                       Scenario_type == "reference"       ~ 0.10,
                                                       TRUE                               ~ 0.28),
                                                   label = case_when(Indicator == "Biodiversity intactness index" & Scenario_class == "Modest-intervention" ~ paste(""),
                                                                     TRUE ~ paste("n =", n))), hjust = 0, size = 14/.pt) +
  facet_wrap(~Indicator, scales = "free_x") +
  labs(fill = "Scenario type", x = "Impact [% change/decade relative to reference]", y = "") +
  theme_classic() +
  ggh4x::facetted_pos_scales(x = scales) +
  theme(#aspect.ratio = 0.75,
    legend.box = "vertical",
    legend.justification = "left",
    legend.position = "bottom", legend.title = element_text(size = 14, face = "bold"), legend.text = element_text(size = 14),
    axis.text.y = element_text(size=14,hjust=1),
    axis.text.x = element_text(size=14),
    axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0),size=14,face="bold"),
    axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0),size=14,face="bold"),
    plot.title = element_text(face = "bold", hjust = 0,size=14),
    strip.text = element_text(size = 14)) 
scenario_class_plot

pdf(paste(figpath, "Scenario_class_impact.pdf"), width = 13, height = 7)
scenario_class_plot
dev.off()

###
#3.) Impact boxplot per number of interventions/sectors impact####
###

#prepare data for boxplot for intervention number and sector number based on scenario type
data_plot <- data_plot %>%
  mutate(Intervention_agg_lvl1 = str_remove(Intervention_agg_lvl1, "^,")) %>%
  mutate(Number_interventions =  str_count(Intervention_agg_lvl1, ",") + 1) %>%
  mutate(Intervention_sector = str_remove(Intervention_sector, "^,")) %>%
  mutate(Number_sectors = str_count(Intervention_sector, ",") + 1) %>%
  mutate(Number_interventions = case_when(is.na(Number_interventions) ~ 0, TRUE ~ Number_interventions)) %>%
  mutate(Number_sectors = case_when(is.na(Number_sectors) ~ 0, TRUE ~ Number_sectors)) %>%
  mutate(Number_interventions_cat = case_when(Number_interventions == 0 ~ "0",  
                                              TRUE ~ as.character(cut(Number_interventions, 
                                                                      breaks = c(0, 5, 10, 20), 
                                                                      labels = c("1-5", "6-10", "> 10"),
                                                                      include.lowest = FALSE)))) %>%
  mutate(Number_sectors_cat = case_when(Number_sectors == 0 ~ "0",  
                                              TRUE ~ as.character(cut(Number_sectors, 
                                                                      breaks = c(0, 3, 6, 10), 
                                                                      labels = c("1-3", "3-6", "> 6"),
                                                                      include.lowest = FALSE))))

#now plot the interventions number
data_plot$Number_interventions_cat <- factor(data_plot$Number_interventions_cat, levels = rev(c("0", "1-5", "6-10", "> 10")))
data_counts_interventions_number <- data_plot %>%
  count(Indicator, Number_interventions_cat) 

intervention_number_plot <- ggplot(data = data_plot, 
                              aes(x = Indicator_value, y = Number_interventions_cat, fill = Number_interventions_cat)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_brewer(palette = "Blues", direction = -1) +
  geom_text(data = data_counts_interventions_number, aes(x = case_when(Indicator == "Biodiversity intactness index" ~ 0.7, 
                                                                       Indicator == "Mean species abundance" ~ 0.7, 
                                                                       Indicator == "Temperature change" ~ -0.6,
                                                                       TRUE ~ -25), 
                                                   y = as.numeric(as.factor(Number_interventions_cat)) ,
                                                   label = paste("n =", n)), hjust = 0, size = 14/.pt) +
  facet_wrap(~Indicator, scales = "free_x") +
  labs(fill = "Number of interventions", x = "Impact [% change/decade relative to reference]", y = "") +
  theme_classic() +
  ggh4x::facetted_pos_scales(x = scales) +
  theme(#aspect.ratio = 0.75,
    legend.box = "vertical",
    legend.justification = "left",
    legend.position = "bottom", legend.title = element_text(size = 14, face = "bold"), legend.text = element_text(size = 14),
    axis.text.y = element_text(size=14,hjust=1),
    axis.text.x = element_text(size=14),
    axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0),size=14,face="bold"),
    axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0),size=14,face="bold"),
    plot.title = element_text(face = "bold", hjust = 0,size=14),
    strip.text = element_text(size = 14)) 
intervention_number_plot

pdf(paste(figpath, "No_interventions_impact.pdf"), width = 13, height = 7)
intervention_number_plot
dev.off()

#now plot the sectors number#
data_plot$Number_sectors_cat <- factor(data_plot$Number_sectors_cat, levels = rev(c("0", "1-3", "3-6", "> 6")))
data_counts_sectors_number <- data_plot %>%
  count(Indicator, Number_sectors_cat) 

sector_number_plot <- ggplot(data = data_plot, 
                              aes(x = Indicator_value, y = Number_sectors_cat, fill = Number_sectors_cat)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_brewer(palette = "Blues", direction = -1) +
  geom_text(data = data_counts_sectors_number, aes(x = case_when(Indicator == "Biodiversity intactness index" ~ 0.7, 
                                                                 Indicator == "Mean species abundance" ~ 0.7, 
                                                                 Indicator == "Temperature change" ~ -0.6,
                                                                 TRUE ~ -25), 
                                                         y = as.numeric(as.factor(Number_sectors_cat)) ,
                                                         label = paste("n =", n)), hjust = 0, size = 14/.pt) +
  facet_wrap(~Indicator, scales = "free_x") +
  labs(fill = "Number of sectors", x = "Impact [% change/decade relative to reference]", y = "") +
  theme_classic() +
  ggh4x::facetted_pos_scales(x = scales) +
  theme(#aspect.ratio = 0.75,
    legend.box = "vertical",
    legend.justification = "left",
    legend.position = "bottom", legend.title = element_text(size = 14, face = "bold"), legend.text = element_text(size = 14),
    axis.text.y = element_text(size=14,hjust=1),
    axis.text.x = element_text(size=14),
    axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0),size=14,face="bold"),
    axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0),size=14,face="bold"),
    plot.title = element_text(face = "bold", hjust = 0,size=14),
    strip.text = element_text(size = 14)) 
sector_number_plot

pdf(paste(figpath, "No_sectors_impact.pdf"), width = 13, height = 7)
sector_number_plot
dev.off()

###
#4.) Archetypes impact boxplot####
###

#exclude all combinations with n >=3
data_counts_archetypes <- data_plot2 %>%
  count(Indicator, Archetype) %>%
  filter(., n >= 3)

archetypes_plot <- ggplot(data = data_plot2 %>% group_by(Indicator, Archetype) %>%
                            filter(n() >= 3) %>% ungroup(), 
                          aes(x = Indicator_value, y = Archetype, fill = Archetype)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_brewer(palette = "Pastel2", direction = -1) +
  geom_text(data = data_counts_archetypes, aes(x = case_when(Indicator == "Biodiversity intactness index" ~ 0.7, 
                                                             Indicator == "Mean species abundance" ~ 0.7, 
                                                             Indicator == "Temperature change" ~ -0.6,
                                                             TRUE ~ -25), 
                                               y = as.numeric(as.factor(Archetype)) ,
                                               label = paste("n =", n)), hjust = 0, size = 14/.pt) +
  facet_wrap(~Indicator, scales = "free_x") +
  labs(fill = "Archetype", x = "Impact [% change/decade relative to reference]", y = "") +
  theme_classic() +
  ggh4x::facetted_pos_scales(x = scales) +
  theme(#aspect.ratio = 0.75,
    legend.box = "vertical",
    legend.justification = "left",
    legend.position = "bottom", legend.title = element_text(size = 14, face = "bold"), legend.text = element_text(size = 14),
    axis.text.y = element_text(size=14,hjust=1),
    axis.text.x = element_text(size=14),
    axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0),size=14,face="bold"),
    axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0),size=14,face="bold"),
    plot.title = element_text(face = "bold", hjust = 0,size=14),
    strip.text = element_text(size = 14)) 
archetypes_plot

pdf(paste(figpath, "Archetypes_impact.pdf"), width = 14, height = 7)
archetypes_plot
dev.off()

###
#5.) SSPs impact boxplot####
###

#subset data for scenarios that used 1 of 5 SSPs as baseline assumptions
data_plot3 <-  data_plot2 %>%
  filter(., Socioeconomic_scenario == "SSP1" | Socioeconomic_scenario == "SSP2" | Socioeconomic_scenario == "SSP3" | Socioeconomic_scenario == "SSP4" | Socioeconomic_scenario == "SSP5")

#exclude all combinations with n >=3
data_counts_socioeconomic <- data_plot3 %>%
  count(Indicator, Socioeconomic_scenario) %>%
  filter(., n >= 3)

ssp_plot <- ggplot(data = data_plot3 %>% group_by(Indicator, Socioeconomic_scenario) %>%
                     filter(n() >= 3) %>% ungroup(), 
                   aes(x = Indicator_value, y = Socioeconomic_scenario, fill = Socioeconomic_scenario)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_brewer(palette = "Pastel2", direction = -1) +
  geom_text(data = data_counts_socioeconomic, aes(x = case_when(Indicator == "Biodiversity intactness index" ~ 0.7, 
                                                                Indicator == "Mean species abundance" ~ 0.7, 
                                                                Indicator == "Temperature change" ~ -0.6,
                                                                TRUE ~ -25), 
                                                  y = as.numeric(as.factor(Socioeconomic_scenario)) ,
                                                  label = paste("n =", n)), hjust = 0, size = 14/.pt) +
  facet_wrap(~Indicator, scales = "free_x") +
  labs(fill = "Socioeconomic scenario", x = "Impact [% change/decade relative to reference]", y = "") +
  theme_classic() +
  ggh4x::facetted_pos_scales(x = scales) +
  theme(#aspect.ratio = 0.75,
    legend.box = "vertical",
    legend.justification = "left",
    legend.position = "bottom", legend.title = element_text(size = 14, face = "bold"), legend.text = element_text(size = 14),
    axis.text.y = element_text(size=14,hjust=1),
    axis.text.x = element_text(size=14),
    axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0),size=14,face="bold"),
    axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0),size=14,face="bold"),
    plot.title = element_text(face = "bold", hjust = 0,size=14),
    strip.text = element_text(size = 14)) 
ssp_plot

pdf(paste(figpath, "SSPs_impact.pdf"), width = 13, height = 7)
ssp_plot
dev.off()

###
#6.) RCPs impact boxplot####
###

#subset data to all scenarios that used RCPs as emission scenarios trajectorie
data_plot4 <- data_plot2 %>%
  filter(., !is.na(Emissions_scenario))

#exclude all combinations with n >=3
data_counts_emissions <- data_plot4 %>%
  count(Indicator, Emissions_scenario) %>%
  filter(., n >= 3 & !is.na(Emissions_scenario)) 

scales2 <- list(
  scale_x_continuous(limits = c(-3, 1)),
  scale_x_reverse(limits = c(3, -1)),
  scale_x_reverse(limits = c(30, -30))
)

rcp_plot <- ggplot(data = data_plot4 %>% group_by(Indicator, Emissions_scenario) %>%
                     filter(n() >= 3) %>% ungroup(), 
                   aes(x = Indicator_value, y = Emissions_scenario, fill = Emissions_scenario)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_brewer(palette = "Pastel2", direction = -1) +
  geom_text(data = data_counts_emissions, aes(x = case_when(Indicator == "Biodiversity intactness index" ~ 0.9, 
                                                                Indicator == "Mean species abundance" ~ 0.9, 
                                                                Indicator == "Temperature change" ~ -0.9,
                                                                TRUE ~ -28), 
                                                  y = as.numeric(as.factor(Emissions_scenario)) ,
                                                  label = paste("n =", n)), hjust = 0, size = 14/.pt) +
  facet_wrap(~Indicator, scales = "free_x", ncol = 1) +
  labs(fill = "Emission scenario", x = "Impact [% change/decade relative to reference]", y = "") +
  theme_classic() +
  ggh4x::facetted_pos_scales(x = scales2) +
  theme(#aspect.ratio = 0.75,
    legend.box = "vertical",
    legend.justification = "left",
    legend.position = "bottom", legend.title = element_text(size = 14, face = "bold"), legend.text = element_text(size = 14),
    axis.text.y = element_text(size=14,hjust=1),
    axis.text.x = element_text(size=14),
    axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0),size=14,face="bold"),
    axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0),size=14,face="bold"),
    plot.title = element_text(face = "bold", hjust = 0,size=14),
    strip.text = element_text(size = 14)) 
rcp_plot

pdf(paste(figpath, "RCPs_impact.pdf"), width = 8, height = 8)
rcp_plot
dev.off()

###
#7.) Descriptive statistics output####
###

filtered_data <- data_plot2 %>%
  mutate(Grouping = paste(Scenario_class, Scenario_type, Indicator, sep = "_")) #we need this grouping factor here, because not all combinations exist in data (psych::describeBy will fail)
data_scenarios <- psych::describeBy(Indicator_value ~ Grouping, data = filtered_data, mat = TRUE, digits = 2) %>%
  filter(., n >= 3) %>%
  separate_wider_delim(group1, delim = "_", names = c("group1", "group2", "group3")) %>%
  select(2:4, 6:17) %>%
  rename(., c("scenario type" = group1, "scenario type 2" = group2, "indicator" = group3)) %>%
  as.data.frame()
rownames(data_scenarios) <- (seq(1, nrow(data_scenarios), 1))
write.csv(data_scenarios, paste(tablepath, "Descriptive_statistics_scenario_type.csv"))

data_interventions <- psych::describeBy(Indicator_value ~ Number_interventions_cat + Indicator, data = data_plot, mat = TRUE, digits = 2) %>%
  filter(., n >= 3) %>%
  select(2:3, 5:16) %>%
  rename(., c("number of interventions" = group1, "indicator" = group2)) 
rownames(data_interventions) <- (seq(1, nrow(data_interventions), 1))
write.csv(data_interventions, paste(tablepath, "Descriptive_statistics_numberofinterventions.csv"))

data_sectors <- psych::describeBy(Indicator_value ~ Number_sectors_cat + Indicator, data = data_plot, mat = TRUE, digits = 2) %>%
  filter(., n. >= 3) %>%
  select(2:3, 5:16) %>%
  rename(., c("number of sectors" = group1, "indicator" = group2)) 
rownames(data_sectors) <- (seq(1, nrow(data_sectors), 1))
write.csv(data_sectors, paste(tablepath, "Descriptive_statistics_numberofsectors.csv"))

data_archetypes <- psych::describeBy(Indicator_value ~ Archetype + Indicator, data = data_plot2, mat = TRUE, digits = 2) %>%
  filter(., n >= 3) %>%
  select(2:3, 5:16) %>%
  rename(., c("archetype" = group1, "indicator" = group2)) 
rownames(data_archetypes) <- (seq(1, nrow(data_archetypes), 1))
write.csv(data_archetypes, paste(tablepath, "Descriptive_statistics_archetypes.csv"))

data_socio <- psych::describeBy(Indicator_value ~ Socioeconomic_scenario + Indicator, data = data_plot3, mat = TRUE, digits = 2) %>%
  filter(., n >= 3) %>%
  select(2:3, 5:16) %>%
  rename(., c("socioeconomic scenario" = group1, "indicator" = group2)) 
rownames(data_socio) <- (seq(1, nrow(data_socio), 1))
write.csv(data_socio, paste(tablepath, "Descriptive_statistics_socioeconomicscenario.csv"))

data_emm <- psych::describeBy(Indicator_value ~ Emissions_scenario + Indicator, data = data_plot4, mat = TRUE, digits = 2) %>%
  filter(., n. >= 3) %>%
  select(2:3, 5:16) %>%
  rename(., c("emission scenario" = group1, "indicator" = group2)) 
rownames(data_emm) <- (seq(1, nrow(data_emm), 1))
write.csv(data_emm, paste(tablepath, "Descriptive_statistics_emissionscenario.csv"))

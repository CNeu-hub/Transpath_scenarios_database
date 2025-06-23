###---------------------------------------------------------------------------###
###                Plots scenarios carbon prices in relation to temperatures, ###
###                in context with applied interventions                      ###
###                Author: Christian Neumann                                  ###
###                Date: 13.11.2024                                           ###
###---------------------------------------------------------------------------###

###
#1.) Create environment####
###

#load libraries
library(tidyverse) #load the tidyverse to restructure data (dplyr), tidy up data (tidyr), text analysis (stringr)
library(RColorBrewer) #color palettes
library(ggh4x) #individual facet grids customization for ggplot2
library(patchwork) #proper plot aligning
library(scales)

#define output/input paths
figpath <- paste(getwd(), "/Output/Figures/", sep = "")
tablepath <- paste(getwd(),"/Output/Tables/", sep = "")
datapath <- paste(getwd(), "/Input/", sep = "")

#load data 
WP3_Subset <- readRDS(file = paste(datapath, "10_06_25_Final_Data_Perc_Ch.Rds"))

#select policy-screening/target-seeking intervention scenarios 
WP3_Subset <- subset(WP3_Subset, WP3_Subset$`Interventions_Yes/No` == "Yes" & WP3_Subset$Scenario_class == "Intervention" & WP3_Subset$Scenario_type == "policy-screening" | WP3_Subset$Scenario_type == "target-seeking")

###
#2.) Create carbon price/temperature data####
###

#remove all but intervention scenarios & carbon market scenarios
Carbon_price_scenarios <- subset(WP3_Subset, grepl("Greenhouse gas emission markets", WP3_Subset$Intervention_agg_lvl1) & Scenario_class == "Intervention" & !is.na(`SDG13_Carbon_price_(US$/t CO2)_raw_data`) & !is.na(`SDG13_Temperature_change_(Since pre-industrial age)_raw_data`))

#split raw data cols 
Carbon_price_data <- Carbon_price_scenarios %>%
  separate(., SDG13_Time_steps, 
                into = c("Time_start", "Time_2030", "Time_2050", "Time_End"),
                sep = ",", remove = TRUE) %>%
  separate(., `SDG13_Carbon_price_(US$/t CO2)_raw_data`, 
           into = c("Carbon_price_Start", "Carbon_price_2030", "Carbon_price_2050", "Carbon_price_End"),
           sep = ",", remove = TRUE) %>%
  separate(., `SDG13_Temperature_change_(Since pre-industrial age)_raw_data`, 
           into = c("Surface_temperature_change_Start", "Surface_temperature_change_2030", "Surface_temperature_change_2050", "Surface_temperature_change_End"),
           sep = ",", remove = TRUE) 

#convert to numeric type
Carbon_price_data[ , c("Time_start", "Time_2030", "Time_2050", "Time_End",
                       "Carbon_price_Start", "Carbon_price_2030", "Carbon_price_2050", "Carbon_price_End",
                       "Surface_temperature_change_Start", "Surface_temperature_change_2030", "Surface_temperature_change_2050", "Surface_temperature_change_End")] <- lapply(Carbon_price_data[ , c("Time_start", "Time_2030", "Time_2050", "Time_End",
                                                                                                                                                                                                     "Carbon_price_Start", "Carbon_price_2030", "Carbon_price_2050", "Carbon_price_End",
                                                                                                                                                                                                     "Surface_temperature_change_Start", "Surface_temperature_change_2030", "Surface_temperature_change_2050", "Surface_temperature_change_End")], as.numeric)
#exclude all scenarios applying carbon price higher or equal to 10000 US2005dollars/tonne CO2
Carbon_price_sub <- subset(Carbon_price_data,Carbon_price_data$Carbon_price_End < 10000 & Carbon_price_data$Carbon_price_End != 0 & Carbon_price_data$Time_End == "2100")

###
#3.) Calculate number of interventions applied in a scenario and number of sectors within which interventions were applied####
###

#create final plot data + count number of interventions and sectors per scenario and create category
data_plot <- Carbon_price_sub %>%
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

data_plot$Number_interventions_cat <- factor(data_plot$Number_interventions_cat, levels = c("1-5", "6-10", "> 10"))
data_plot$Number_sectors_cat <- factor(data_plot$Number_sectors_cat, levels = c("1-3", "3-6", "> 6"))

###
#4.) Final plot####
###

pdf(paste(figpath, "Carbonprices.pdf"), width = 8, height = 5)
ggplot(data = data_plot, aes(x = (log10(Carbon_price_End)), 
                                    y = Surface_temperature_change_End,
                                    color = Number_interventions_cat, 
                                    size = Number_sectors_cat)) + 
                                    #size = ifelse(Sectors_Number == "> 3", 5, 2))) + 
                                    #shape = Socioeconomic_scenario)) +  # Add shape mapping here
  geom_point(na.rm = TRUE) +  # No need for shape = in geom_point
  #scale_color_viridis_c(option = "inferno", direction = -1) +
  #scale_color_gradient2() 
  scale_x_continuous(breaks = 0:10, labels = math_format(10^.x)) +
  #scale_x_continuous(breaks = c(0, 100, 1000, 6000)) +
  scale_y_continuous(breaks = seq(0, 3, 0.5), limits = c(1, 3)) +
  scale_color_brewer(palette = "Blues") +
  scale_size_manual(values = c("1-3" = 2, "3-6" = 4, "> 6" = 6)) +
  #geom_hline(yintercept = 2, linetype = 2, color = "firebrick") +
  geom_hline(yintercept = 1.5, linetype = 2, color = "firebrick") +
  #geom_vline(xintercept = log(17.6), linetype = 2, color = "firebrick") +
  #ggplot2::annotate("text",
  #                  y = 3,
  #                  x = 3,
  #                  hjust = 0,
  #                  size = 14/.pt,
  #                  label = expression("17.6 US2005$/tCO"[2]*"e in 2023"),
  #                  col = "firebrick") +
  labs(x = expression("Carbon price in 2100 [log10 US$2005/tCO"[2]*"]"), 
       y = "Temperature change in 2100 [°C]", 
       color = "No. of interventions", 
       size = "No. of sectors") +
  theme_classic() +
  theme(#aspect.ratio = 0.75,
    legend.box = "vertical",
    legend.justification = "left",
    legend.box.just = "left",
    legend.position = "bottom", 
    legend.title = element_text(size = 14, face = "bold"), 
    legend.text = element_text(size = 14),
    axis.text.y = element_text(size=14,hjust=1),
    axis.text.x = element_text(size=14),
    axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0),size=14,face="bold"),
    axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0),size=14,face="bold"),
    plot.title = element_text(face = "bold", hjust = 0,size=14),
    strip.text = element_text(size = 14)) 

dev.off()


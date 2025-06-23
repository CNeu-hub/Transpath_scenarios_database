###---------------------------------------------------------------------------###
###                Calculates and plots metrics frequency in circular barplot ###
###                Author: Christian Neumann                                  ###
###                Date: 13.11.2024                                           ###
###---------------------------------------------------------------------------###

###
#1.) Create environment####
###

#load libraries
library(tidyverse)
library(tm) #needed for text mining analysis
library(colorBlindness)

#define output/input paths
figpath <- paste(getwd(), "/Output/Figures/", sep = "")
tablepath <- paste(getwd(),"/Output/Tables/", sep = "")
datapath <- paste(getwd(), "/Input/", sep = "")

#load data 
WP3_Subset <- readRDS(file = paste(datapath, "10_06_25_Final_Data_Perc_Ch.Rds"))

###
#2.) Calculate indicators frequency across scenarios and percentage contribution of unique studies per SDG and metric####
###

#create indicator vector of colnames to loop over
Indicators <- c(colnames(WP3_Subset %>% select(contains("SDG"), -ends_with("Impact"), -ends_with("horizon"), -ends_with("raw_data"), -ends_with("steps"), -ends_with("decades"))))

#create empty data frame with needed variables
sums <- data.frame(indicator = character(length(Indicators)),
                   frequency = numeric(length(Indicators)),
                   studies = numeric(length(Indicators)),
                   SDG = character(length(Indicators)))

#create list to calculate unique studies for each SDG (Important for calculating % of studies that calculated indicators within SDG)
unique_studies_per_SDG <- list()

for (i in seq_along(Indicators)) {
  
  sums[i, "indicator"] <- Indicators[i]
  sums[i, "frequency"] <- sum(!is.na(WP3_Subset[[Indicators[i]]]))
  sums[i, "studies"] <- length(unique(WP3_Subset$Study_nr.[!is.na(WP3_Subset[[Indicators[i]]])]))
  
  #extract first part of the string
  category_parts <- unlist(strsplit(Indicators[i], "_"))
  sums[i, "SDG"] <- category_parts[1]
  
  #calculate unique studies for current indicator
  unique_studies <- unique(WP3_Subset$Study_nr.[!is.na(WP3_Subset[[Indicators[i]]])])
  
  #sum up count of unique studies for each SDG
  SDG <- category_parts[1]
  if (is.null(unique_studies_per_SDG[[SDG]])) {
    unique_studies_per_SDG[[SDG]] <- unique_studies
  } else {
    unique_studies_per_SDG[[SDG]] <- union(unique_studies_per_SDG[[SDG]], unique_studies)
  }
}

#calculate the total number of unique studies
total_unique_studies <- length(unique(WP3_Subset$Study_nr.))

#calculate the number of unique studies per SDG
for (i in seq_along(unique_studies_per_SDG)) {
  unique_studies_per_SDG[[i]] <- length(unique_studies_per_SDG[[i]])
}

sums$indicator <- gsub("_"," ",sums$indicator)
sums$indicator <- gsub("SDG1|SDG2|SDG3|SDG4|SDG5|SDG6|SDG7|SDG8|SDG9|SDG10|SDG11|SDG12|SDG13|SDG14|SDG15|SDG16|SDG17","",sums$indicator)

sums$SDG <- factor(sums$SDG, levels = c("SDG1","SDG2","SDG3","SDG4","SDG5","SDG6","SDG7","SDG8","SDG9","SDG10","SDG11","SDG12","SDG13","SDG14","SDG15","SDG16","SDG17"))

#calculate overall used studies 
studies_count <- sum(n_distinct(WP3_Subset$Study_nr.))

#merge number of unique studies per SDG and sums 
for(sdg in unique(sums$SDG)) {
  sums$unique_studies_SDG_count[sums$SDG == sdg] <- unique_studies_per_SDG[[sdg]]
}

#get the name and the y position of each label
sums$id <- seq(1,nrow(sums))
sums$id2 <- as.numeric(str_remove(sums$SDG, "SDG"))

#calculate percentage coverage of indicators & SDGs per study 
sums <- sums %>% 
  group_by(indicator) %>%
  mutate(Percentage_Indicators = (studies / 64) * 100) %>% #64 = sum of studies, because some assessment reports assessed in one scenario only climate and in another only biodiversity for example, this leads to doublecounting of studies which is why we use 64 instead of the original 60 studies (e.g.: OECD 2050 study)
  ungroup() %>%
  group_by(SDG) %>%
  mutate(Percentage_SDGs = (unique(unique_studies_SDG_count) / 64) *100) %>%
  mutate(xmin = min(as.numeric(id)) - 0.5,
         xmax = max(as.numeric(id)) + 0.5,
         ymin = -0.5,
         ymax = -Percentage_SDGs-15) %>%
  ungroup()

sums$Percentage_Indicators <- round(sums$Percentage_Indicators, digits = 1)
sums$Percentage_SDGs <- round(sums$Percentage_SDGs, digits = 1)

###
#3.) Calculate position indices and coordinates of bars for plotting, create indicator labels####
###

label_data <- sums
label_data$indicator_shortcut <- c("Extreme poverty", "Population < 1 $",
                                   "Hunger risk", "Undernourishment", "Hunger incidence", "Malnourished children", "FPI 2005", "FPI 2010", "FPI 2015", "Change food prices", "Fertilizer use", "Nitrogen use",
                                   "NO2 emissions", "NOx emissions", "SO2 emissions", "SOx emissions", "BC emissions", "PM2.5 emissions",
                                   "MYS", "Adults no education", "Female education", "Primary education", "Gender rat. prim. edu.", "Gender rat. sec. edu.",
                                   "Access improved water", "Access basic sanitation", "Lack improved water", "Water stress river bas.", "Water stress", "Agr. irrigation",
                                   "Share renewables", "UE build. tr. p. cap.", 
                                   "Income convergence", "Unemployment", "Unemployment rate", "Ind. hydrogen electr. share", 
                                   "International inequity", "National equity", "International equity", "Rel. poverty", "Rat. GDP/cap", "10% richest/10% poorest rat.", "GINI coefficient",
                                   "Urban PM2.5",
                                   "Food waste (kcal/cap/day)", "Food waste (Mt/yr)",
                                   "GHG emissions", "CO2 emissions", "AFOLU CO2 emissions", "AFOLU GHG emissions", "Forcing", "CO2 concentration", "Temperature change", "Carbon price",
                                   "River discharge N", "River discharge P", "MTI", "MSA aquatic", "Ocean acidification", 
                                   "Habitat range size", "Species 50% range loss", "GMA", "INSIGHTS Index", "Habitat loss", "Biodiv. hotspot loss", "Red. vasc. plant sp.", "Extinction MSY", "AOH/ESH", "Species range protection lvl.", "Ecoregions protection lvl.", "Deforestation", "NCI-pb", "LPI", "RLI", "MSA terrestrial", "BII", "FRRS", "FGRS CB17BDM", "FGRS US16BDM", "PDF", "Nitrogen fixation", "Mean sp. richness",
                                   "Peace probability", "Equality",
                                   "Int. climate finance") 

#calculate angle for each bar (indicator), used for plotting (i.e. arranging bars)
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar 
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

label_sdgs <- sums 
number_of_bar <- nrow(label_sdgs)
angle <- 90 - 360 * (label_sdgs$id-0.5) /number_of_bar
label_sdgs$hjust <- ifelse(angle < -90, 1, 0)
label_sdgs$angle <- ifelse(angle < -90, angle+180, angle)
label_sdgs$x <- (label_sdgs$xmax+label_sdgs$xmin)/2
label_sdgs$y <- (label_sdgs$ymax+label_sdgs$ymin)/2
label_sdgs <- label_sdgs %>%
  group_by(SDG) %>%
  mutate(angle = mean(angle))

#create labeling and color vectors 
#colors are based on United Nations (2019) HEX codes: https://www.un.org/sustainabledevelopment/wp-content/uploads/2019/01/SDG_Guidelines_AUG_2019_Final.pdf 
colors = c("#e5243b","#DDA63A", "#4C9F38", "#C5192D", "#FF3A21", "#26BDE2", "#FCC30B", "#A21942",
           "#FD6925", "#DD1367", "#FD9D24", "#BF8B2E", "#3F7E44", "#0A97D9", "#56C02B", "#00689D","#19486A" )

labels = c("SDG 1 No Poverty","SDG 2 Zero Hunger","SDG 3 Good Health and Well-Being","SDG 4 Quality Education","SDG 5 Gender Equality","SDG 6 Clean Water and Sanitation","SDG 7 Affordable and Clean Energy","SDG 8 Decent Work and Economic Growth",
           "SDG 9 Industry, Innovation and Infrastructure","SDG 10 Reduced Inequalities","SDG 11 Sustainable Cities and Communities","SDG 12 Responsible Consumption and Production","SDG 13 Climate Action","SDG 14 Life below Water","SDG 15 Life on Land","SDG 16 Peace, Justice and Strong Institutions","SDG 17 Partnerships for the Goals")

labels2 <- c("SDG1", "SDG2", "SDG3", "SDG4", "SDG5", "SDG6", "SDG7", "SDG8", "SDG9", "SDG10", "SDG11", "SDG12", "SDG13", "SDG14", "SDG15", "SDG16", "SDG17")

###
#4.) Plot circular histogram and create exports####
###

#plot circular histogram
pdf(paste(figpath, "Indicators_gap.pdf", sep = ""), height = 15, width = 13, paper = "special")
histogram <- ggplot(sums, aes(x=as.factor(id), y=Percentage_Indicators, fill = SDG)) +     
  geom_bar(stat="identity", alpha = 1) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, 
  ),alpha = 0.25) +
  scale_fill_manual(values = colors, labels = labels2)+
  geom_text(aes(x = id, y = Percentage_Indicators+15, label = paste0(Percentage_Indicators,sep=" ","%")), color="black", fontface="bold",alpha=0.6, size = 16/.pt, angle= label_data$angle, inherit.aes = FALSE) +
  geom_text(data=label_data, aes(x=id, y=Percentage_Indicators+28, label=indicator_shortcut, hjust=hjust), color="black", fontface="bold",alpha=0.6, size = 16/.pt, angle= label_data$angle, inherit.aes = FALSE ) +
  geom_text(data=label_sdgs,aes(x = x, y = y, label = paste0(Percentage_SDGs, sep = " ", "%")),color = "black", fontface = "bold", alpha = 1, size = 16/.pt, angle = label_sdgs$angle, check_overlap = TRUE, inherit.aes = FALSE)+
  labs(fill = "") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = c("bottom"),
    legend.text = element_text(size = 16),
    legend.title = element_text(face = "bold", size = 16),
    plot.margin = unit(rep(-0.1,10), "cm")) +
  guides(fill = guide_legend(nrow = 3)) +
  theme(plot.margin = margin(0, 0, 0, 0)) +
  coord_polar(start = 0)   
histogram
dev.off()

#create csv output 
Table_output <- label_data %>%
  select(indicator_shortcut, indicator, SDG, Percentage_Indicators, Percentage_SDGs) %>%
  rename(., c("Metric abbreviation" = indicator_shortcut, "Metric" = indicator, "SDG" = SDG, "Percentage Indicators" = Percentage_Indicators, "Percentage SDGs" = Percentage_SDGs))

write.csv(Table_output, paste(tablepath, "Indicator_summary.csv", sep = ""))



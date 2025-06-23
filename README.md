# Code repository for descriptive intervention scenarios analysis

### Author: 

Christian Neumann 21-06-2023

### Short description: 

This is the project repository to perform the descriptive analysis of: 

Neumann, C., Alkemade, R., Van Vuuren, D., Seppelt, R. (2025). Global Assessment of Biodiversity-Climate Pathways. Deliverable 3.1. 

The provided code performs a descriptive content and impact analysis using the [intervention scenarios database](www.google.de) of the [Transpath project](www.transpath.eu). The database collects a suite of global model-based policy-screening/target-seeking and policy-oriented exploratory scenarios aiming to mitigate climate change and/or restoring/conserving biodiversity. It includes qualitative and quantitative assumptions underlying the scenarios, accompanied by information about the applied interventions, and impacts of the scenarios.

### Usage: 
The repository includes five folders: 

**1. Analysis:** 

Contains all the code needed to perform the descriptive analysis. Includes scripts to calculate relative frequencies of measured impacts, applied interventions, and scenario types (Gap folder). Furthermore, it contains the code to plot impacts for archetypes, scenario types, number of sectors and interventions applied, etc. 

**2. Functions:** 

Contains some functions used to preprocess as well as work with the database.

**3. Input:** 

Contains the data input needed to perform the preprocessing and analysis of the database. 

**4. Output:** 

Contains the output of the analysis (Figures/Tables) and the preprocessed database output, further provided on [Zenodo](www.google.de).

**5. Preprocessing:** 

Contains the two preprocessing scripts to I) Calculate percentage change per decade for each scenario, and II) Recode the intervention categories using the interventions lookup table in the input folder. 


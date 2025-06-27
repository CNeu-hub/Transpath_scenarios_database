# Code repository for descriptive analysis of global intervention scenarios

### Author:

Christian Neumann 21-06-2023

### Short description:

This is the project repository to perform the descriptive analysis of:

*Neumann, C., Alkemade, R., Van Vuuren, D., Seppelt, R. (2025). Global Assessment of Biodiversity-Climate Pathways. Deliverable 3.1.*

The provided code performs a descriptive content and impact analysis using the [intervention scenarios database](https://zenodo.org/records/15753209) of the [Transpath project](https://www.transpath.eu/). The database collects a suite of global model-based policy-screening/target-seeking and policy-oriented exploratory scenarios aiming to mitigate climate change and/or restoring/conserving biodiversity. It includes qualitative and quantitative assumptions underlying the scenarios, accompanied by information about the applied interventions, and impacts of the scenarios.

### Usage:

All scripts can be executed from scratch after cloning the repository. Only the first preprocessing script ("Database_Analysis_I_Decadal_Perc_Ch_v02.R") can not be executed because we didnt upload the raw data used there to fulfil licencing restrictions (see report). Folder description is provided below.

See the session info below for the R packages needed to execute the scripts.

### Structure:

The repository includes five folders:

**1. Analysis:**

Contains all the code needed to perform the descriptive analysis. Includes scripts to calculate relative frequencies of measured impacts, applied interventions, and scenario types (Gap folder). Furthermore, it contains the code to plot impacts for archetypes, scenario types, number of sectors and interventions applied, etc.

**2. Functions:**

Contains some functions used to preprocess as well as work with the database.

**3. Input:**

Contains the data input needed to perform the preprocessing and analysis of the database.

**4. Output:**

Contains the output of the analysis (Figures/Tables) and the preprocessed database output, further provided on [Zenodo](https://zenodo.org/records/15753209).

**5. Preprocessing:**

Contains the two preprocessing scripts to I) Calculate percentage change per decade for each scenario, and II) Recode the intervention categories using the interventions lookup table in the input folder.

### R session info:

```
utils::sessionInfo()

R version 4.5.0 (2025-04-11)
Platform: aarch64-apple-darwin20
Running under: macOS Sequoia 15.5

Matrix products: default
BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib 
LAPACK: /Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.1

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

time zone: Europe/Berlin
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] scales_1.4.0         colorspace_2.1-1     wordcloud_2.6        patchwork_1.3.0     
 [5] ggpubr_0.6.0         ggh4x_0.3.0          RColorBrewer_1.1-3   colorBlindness_0.1.9
 [9] xlsx_0.6.5           tm_0.7-16            NLP_0.3-2            lubridate_1.9.4     
[13] forcats_1.0.0        stringr_1.5.1        dplyr_1.1.4          purrr_1.0.4         
[17] readr_2.1.5          tidyr_1.3.1          tibble_3.2.1         ggplot2_3.5.2       
[21] tidyverse_2.0.0      readxl_1.4.5        

loaded via a namespace (and not attached):
 [1] gtable_0.3.6       xfun_0.52          htmlwidgets_1.6.4  psych_2.5.3        rstatix_0.7.2     
 [6] lattice_0.22-7     rJava_1.0-11       tzdb_0.5.0         vctrs_0.6.5        tools_4.5.0       
[11] generics_0.1.3     parallel_4.5.0     pkgconfig_2.0.3    checkmate_2.3.2    lifecycle_1.0.4   
[16] compiler_4.5.0     farver_2.1.2       mnormt_2.1.1       carData_3.0-5      htmltools_0.5.8.1 
[21] htmlTable_2.4.3    Formula_1.2-5      crayon_1.5.3       pillar_1.10.2      car_3.1-3         
[26] abind_1.4-8        nlme_3.1-168       tidyselect_1.2.1   digest_0.6.37      stringi_1.8.7     
[31] slam_0.1-55        labeling_0.4.3     cowplot_1.1.3      fastmap_1.2.0      grid_4.5.0        
[36] cli_3.6.5          magrittr_2.0.3     xlsxjars_0.6.1     broom_1.0.8        withr_3.0.2       
[41] backports_1.5.0    timechange_0.3.0   rmarkdown_2.29     ggsignif_0.6.4     cellranger_1.1.0  
[46] hms_1.1.3          evaluate_1.0.3     knitr_1.50         gridGraphics_0.5-1 rlang_1.1.6       
[51] Rcpp_1.0.14        glue_1.8.0         xml2_1.3.8         rstudioapi_0.17.1  R6_2.6.1 

```

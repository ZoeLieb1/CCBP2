###### Additional Databases ########
## ZEL
## 4 July 2025

# read in packages
library(tidyverse)
library(readr)


NTU_data <- read.csv("/Users/zoe/Desktop/CCBP/database_search/zl_new_v_Carbon_Market_Data_20250408.csv", stringsAsFactors = FALSE)


unique(NTU_data$Registry)

additional_data <- read.csv("/Users/zoe/Desktop/CCBP/database_search//////////.csv", stringsAsFactors = FALSE)

unique(additional_data$Registry)


#### bringing in API for 

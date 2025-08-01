### Combining data from keyword searches on Climate Action Data Trust database and removing duplicate projects###
### Zoë Lieb
### 14 Feb 2025
### Carbon Cross Borders Project

# read in packages
library(tidyverse)
library(readr)

#assign each keyword csv

forestry_key <- read.csv('data/forestry-CAD-2025-02-05.csv')
forest_regeneration_key <- read.csv('data/forest-regeneration-CAD-2025-02-05.csv')
forest_key <- read.csv('data/forest-CAD-2025-02-05.csv')
reforest_key <- read.csv('data/reforest-CAD-2025-02-05.csv')
reforestation_key <-  read.csv('data/reforestation-CAD-2025-02-05.csv')
restoration_key <- read.csv('data/restoration-2025-02-05.csv')
tree_key <- read.csv('data/tree-CAD-2025-02-05.csv')

#combine all rows into one dataframe

CADT_data <- rbind(forest_key, forest_regeneration_key, forestry_key, reforest_key, reforestation_key, restoration_key, tree_key)

# only keep distinct rows

CADT_data_unique <- distinct(CADT_data)

write.csv(CADT_data_unique, file = "CADT_data.csv", row.names = FALSE)





##### Finding unique projects in the CADT datasheet that do not already appear in NTU data or Zoë's data find #####


NTU_data <- read.csv("/Users/zoe/Desktop/CCBP/database_search/zl_new_v_Carbon_Market_Data_20250408.csv", stringsAsFactors = FALSE)
CADT_data <- read.csv("/Users/zoe/Desktop/CADT_data.csv", stringsAsFactors = FALSE)


# Get unique IDs from each file
ntu_projects <- NTU_data$Project_Name
cadt_projects <- CADT_data$Project_Name

# Find IDs that are in one file but not the other

unique_to_file2 <- setdiff(cadt_projects, ntu_projects)

unique_rows2 <- CADT_data[CADT_data$Project_Name %in% unique_to_file2, ]


unique_to_file1 <- setdiff(ntu_projects, cadt_projects)

unique_rows1 <- NTU_data[NTU_data$Project_Name %in% unique_to_file1, ]

write.csv(unique_rows1, "NTU_rows_not_in_CADT.csv", row.names = FALSE)

write.csv(unique_rows2, "CADT_rows_not_in_NTU.csv", row.names = FALSE)







#### CAR data search for CCBP #####

# CSV file
car_data <- read.csv("/Users/zoe/Desktop/CCBP/database_search/CAR_data.csv", stringsAsFactors = FALSE)

head(car_data)

# Count unique names in the "Project Developer" column
unique_developers <- unique(car_data$"Project.Developer")

#### checking additional found data #####

# CSV

additional_data <- read.csv("/Users/zoe/Desktop/CCBP/database_search/All_databases_incl.csv", stringsAsFactors = FALSE)



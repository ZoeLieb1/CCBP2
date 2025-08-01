##### Australia ETS data, joining proponent with manual country search ####
##### July 11 2025 #####
#### Zoë Lieb #####


# Load required package
library(dplyr)

# 1. Load your data
proponents <- read.csv("/Users/zoe/Desktop/CCBP/CCBP_database/data/australian_ets_proponents_manual.csv", stringsAsFactors = FALSE)
projects   <- read.csv("/Users/zoe/Desktop/CCBP/database_search/National_databases/Australia_ets_projects.csv", stringsAsFactors = FALSE)

# 2. Join the country info into the projects dataframe
projects_updated <- projects %>%
  left_join(proponents[, c("Project.Proponent", "proponent_country_manual")],
            by = "Project.Proponent")

# 3. Save the updated project data
write.csv(projects_updated, "Australia_ets_projects_with_countries.csv", row.names = FALSE)




######Let’s verify exactly what’s different between the two files before trying another join. A quick check will show us whether any proponent names fail to line-up and why.

library(dplyr)
library(stringr)

# 1. Load both files
proponents <- read.csv("/Users/zoe/Desktop/CCBP/CCBP_database/data/australian_ets_proponents_manual.csv",
                       stringsAsFactors = FALSE,  check.names = FALSE)
projects   <- read.csv("/Users/zoe/Desktop/CCBP/database_search/National_databases/Australia_ets_projects.csv",
                       stringsAsFactors = FALSE,  check.names = FALSE)

# 2. Normalise the key in BOTH tables
clean_key <- function(x) {
  x %>%
    str_trim() %>%               # remove leading / trailing spaces
    str_replace_all("\\s+", " ") %>% # collapse multiple spaces
    str_to_lower()               # case-insensitive match
}

proponents  <- proponents  %>%
  mutate(join_key = clean_key(Project.Proponent))

projects <- projects %>%
  mutate(join_key = clean_key(Project.Proponent))

# 3. What’s unmatched?
unmatched_in_projects <- anti_join(projects,  proponents, by = "join_key")
unmatched_in_props    <- anti_join(proponents, projects,   by = "join_key")

cat("Rows in projects with no match in the proponent key:",
    nrow(unmatched_in_projects), "\n")
cat("Rows in proponent list with no match in projects:",
    nrow(unmatched_in_props), "\n")

# Peek at a few problematic names (if any)
head(unmatched_in_projects$Project.Proponent)
head(unmatched_in_props$Project.Proponent)




# Use the cleaned join_key as before
projects_joined <- projects %>%
  left_join(proponents %>% select(join_key, proponent_country_manual),
            by = "join_key")

# Optional: show how many are still NA
sum(is.na(projects_joined$proponent_country_manual))


# Final export
write.csv(projects_joined,
          "Australia_ets_projects_with_countries.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")

table(is.na(projects_joined$proponent_country_manual))





##### Try again, that's all not working #####


library(dplyr)
library(readr)  # better default than read.csv



# 1. Load data
proponents <- read_csv("/Users/zoe/Desktop/CCBP/CCBP_database/data/australian_ets_proponents_manual.csv")
projects   <- read_csv("/Users/zoe/Desktop/CCBP/database_search/National_databases/Australia_ets_projects.csv")

# 2. Clean and prepare keys for join
proponents <- proponents %>%
  mutate(Project.Proponent = str_trim(Project.Proponent))

projects <- projects %>%
  mutate(Project.Proponent = str_trim(Project.Proponent))

# 3. Perform the join (assumes 1 country per proponent)
projects_filled <- projects %>%
  select(-proponent_country_manual) %>%  # drop the empty column
  left_join(proponents %>% select(Project.Proponent, proponent_country_manual),
            by = "Project.Proponent")

table(is.na(projects_filled$proponent_country_manual))




# 4. Save to CSV
write_csv(projects_filled, "Australia_ets_projects_with_countries.csv")

head(projects_filled)



### TRY AGAIN ###

library(dplyr)
library(stringr)

# Reapply minimal join-cleanup


projects <- read_csv("/Users/zoe/Desktop/CCBP/database_search/National_databases/Australia_ets_projects.csv") %>%
  mutate(Project.Proponent = str_squish(str_to_lower(Project.Proponent)))

proponents <- read_csv("/Users/zoe/Desktop/CCBP/CCBP_database/data/australian_ets_proponents_manual.csv") %>%
  mutate(Project.Proponent = str_squish(str_to_lower(Project.Proponent)))

# Join and find what’s missing
projects_filled <- projects %>%
  left_join(proponents %>% select(Project.Proponent, proponent_country_manual),
            by = "Project.Proponent")

# Check how many are still NA
table(is.na(projects_filled$proponent_country_manual))

# View a few unmatched
unmatched <- projects_filled %>%
  filter(is.na(proponent_country_manual)) %>%
  distinct(Project.Proponent)

print(unmatched, n = 20)





# Reload original files
projects_raw <- read_csv("/Users/zoe/Desktop/CCBP/database_search/National_databases/Australia_ets_projects.csv")
proponents_raw <- read_csv("/Users/zoe/Desktop/CCBP/CCBP_database/data/australian_ets_proponents_manual.csv")

# Clean both join keys the same way
clean_keys <- function(x) {
  x %>%
    str_trim() %>%
    str_squish() %>%
    str_to_lower()
}

projects_clean <- projects_raw %>%
  mutate(join_key = clean_keys(Project.Proponent))

proponents_clean <- proponents_raw %>%
  mutate(join_key = clean_keys(Project.Proponent))

# Check overlap
matched_keys <- intersect(projects_clean$join_key, proponents_clean$join_key)
unmatched_keys <- setdiff(projects_clean$join_key, proponents_clean$join_key)

cat("✅ Matched keys:", length(matched_keys), "\n")
cat("❌ Unmatched keys:", length(unmatched_keys), "\n")

# Show sample unmatched
head(unmatched_keys, 10)



#### Whoops, forgot to remove agriculture method! ####



library(dplyr)
library(readr)

# 1. Load both files
projects <- read_csv("/Users/zoe/Desktop/CCBP/database_search/National_databases/Australia_ets_projects.csv")
proponents <- read_csv("/Users/zoe/Desktop/CCBP/CCBP_database/data/australian_ets_proponents_manual.csv")

# 2. Filter out "Agriculture" method types
projects <- projects %>%
  filter(Method.Type != "Agriculture")  # Make sure this matches your column name exactly

# 3. Trim whitespace from proponent names (just in case)
projects$Project.Proponent <- trimws(projects$Project.Proponent)
proponents$Project.Proponent <- trimws(proponents$Project.Proponent)

# Trim whitespace on both sides (✅ do this!)
projects$Project.Proponent <- trimws(projects$Project.Proponent)
proponents$Project.Proponent <- trimws(proponents$Project.Proponent)


# 4. Remove the empty column before joining
projects <- projects %>%
  select(-proponent_country_manual)

# 5. Do the clean join
projects_joined <- left_join(
  projects,
  proponents %>% select(Project.Proponent, proponent_country_manual),
  by = "Project.Proponent"
)

# 6. Export the result
write_csv(projects_joined, "Australia_ets_projects_with_countries.csv")




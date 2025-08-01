##### Bringing in ICR Program API access for data ####

#### Zoë Lieb

#### 11 July 2025

install.packages("httr")
install.packages("jsonlite")

library(httr)
library(jsonlite)

# Step 1: Call the API
url <- "https://api.carbonregistry.com/v1/projects"
response <- GET(url)

# Step 2: Parse JSON content
data_raw <- content(response, as = "text")
data_list <- fromJSON(data_raw, flatten = TRUE)

# Step 3: Convert to data frame
df <- as.data.frame(data_list)

write.csv(df, "ICR_carbon_projects.csv", row.names = FALSE)


#### Need access token from ICR. I go it by making an account on ICR website, and going to API settings in my dashboard####


library(httr)
library(jsonlite)

# Replace this with your actual token
access_token <- "cru_6d9538fc57c86c95742f49d18b1c5dd891d711c5309bf9ae"

url <- "https://api.carbonregistry.com/v1/projects"

response <- GET(url, add_headers(Authorization = paste("Bearer", access_token)))

data_raw <- content(response, as = "text")
data_list <- fromJSON(data_raw, flatten = TRUE)

df <- as.data.frame(data_list)

#Code to convert list columns to strings
sapply(df, class)

df[] <- lapply(df, function(col) {
  if (is.list(col)) {
    sapply(col, toString)  # or paste(..., collapse = ",")
  } else {
    col
  }
})


write.csv(df, "New_ICR_carbon_projects.csv", row.names = FALSE)



### Trying to flatten the data, because csv didn't work ###


# Ensure the data is deeply flattened
library(jsonlite)
df <- fromJSON(data_raw, flatten = TRUE)  # already likely done

# Check structure
str(df)

# Identify list-columns
sapply(df, class)

# Optional: Convert all list-columns to character
df_clean <- df
df_clean[] <- lapply(df_clean, function(col) {
  if (is.list(col)) {
    sapply(col, function(x) paste(unlist(x), collapse = ", "))
  } else {
    col
  }
})

## force clean to string

df_clean[] <- lapply(df_clean, as.character)

# Now write to CSV
write.csv(df_clean, "New_ICR_carbon_projects.csv", row.names = FALSE)

write.table(df_clean, "Newer_ICR_carbon_projects.csv",
            sep = ";", dec = ",", row.names = FALSE, col.names = TRUE, quote = TRUE)


write.table(df_clean, "EvenMore_Newer_ICR_carbon_projects.csv",
            sep = ";", dec = ",", row.names = FALSE, col.names = TRUE, quote = TRUE,
            fileEncoding = "UTF-8")




#### still CVS not working in excel

# Ensure all columns are character and lists are collapsed
df_clean[] <- lapply(df_clean, function(col) {
  if (is.list(col)) {
    sapply(col, function(x) paste(unlist(x), collapse = ", "))
  } else {
    as.character(col)
  }
})



#### trying again with df clean

# Flatten & clean again just in case
df_clean[] <- lapply(df_clean, function(col) {
  if (is.list(col)) {
    sapply(col, function(x) paste(unlist(x), collapse = ", "))
  } else {
    as.character(col)
  }
})

# Write to CSV with manual UTF-8 BOM for Excel
file_name <- "ICR_carbon_projects_excel_ready.csv"

con <- file(file_name, open = "wb")
writeBin(charToRaw("\xEF\xBB\xBF"), con)  # Write BOM
write.table(df_clean, con,
            sep = ",", dec = ".", row.names = FALSE, col.names = TRUE, quote = TRUE,
            na = "", fileEncoding = "UTF-8")
close(con)


##### try 2 - redoing it from the API download


library(httr)
library(jsonlite)

# Your access token
access_token <- "cru_6d9538fc57c86c95742f49d18b1c5dd891d711c5309bf9ae"
url <- "https://api.carbonregistry.com/v1/projects"

# Get data
response <- GET(url, add_headers(Authorization = paste("Bearer", access_token)))
data_raw <- content(response, as = "text")
data_flat <- fromJSON(data_raw, flatten = TRUE)

# Flatten all columns and clean
df_clean <- as.data.frame(data_flat)
df_clean[] <- lapply(df_clean, function(col) {
  if (is.list(col)) sapply(col, function(x) paste(unlist(x), collapse = ", ")) else as.character(col)
})

# Export to a new, clean UTF-8 CSV that Excel will open correctly
con <- file("ICR_projects_clean.csv", open = "wb")
writeBin(charToRaw("\xEF\xBB\xBF"), con)  # Write UTF-8 BOM
write.table(df_clean, con, sep = ",", dec = ".", quote = TRUE, row.names = FALSE, col.names = TRUE, fileEncoding = "UTF-8")
close(con)


#### try 3, removing broken rows #####

library(httr)
library(jsonlite)

# 1. Get the data
access_token <- "cru_6d9538fc57c86c95742f49d18b1c5dd891d711c5309bf9ae"
url <- "https://api.carbonregistry.com/v1/projects"

response <- GET(url, add_headers(Authorization = paste("Bearer", access_token)))
data_raw <- content(response, as = "text")
data_flat <- fromJSON(data_raw, flatten = TRUE)

# 2. Flatten and clean
df <- as.data.frame(data_flat)
df[] <- lapply(df, function(col) {
  if (is.list(col)) sapply(col, function(x) paste(unlist(x), collapse = ", ")) else as.character(col)
})

# 3. Write to CSV with UTF-8 BOM
csv_path <- "ICR_projects_clean.csv"
con <- file(csv_path, open = "wb")
writeBin(charToRaw("\xEF\xBB\xBF"), con)
write.table(df, con, sep = ",", dec = ".", quote = TRUE, row.names = FALSE, col.names = TRUE, fileEncoding = "UTF-8")
close(con)

# ✅ 4. Reload and remove broken rows (the key part)
df_fixed <- read.csv(csv_path, fileEncoding = "UTF-8-BOM", quote = "\"", fill = TRUE)

# Check how many columns should be there
expected_cols <- 37  # Or use: ncol(df_fixed[df_fixed != ""][1, ]) if unsure

# Remove rows with incorrect number of columns
df_fixed <- df_fixed[nchar(apply(df_fixed, 1, paste, collapse = ",")) > 0, ]  # Drop empty-ish rows
df_fixed <- df_fixed[ncol(df_fixed) == expected_cols, ]  # Keep only good rows

# 5. Export final clean version

# Final export with manual BOM (so Excel opens it cleanly)
final_file <- "ICR_projects_final.csv"

# Open connection and manually write BOM
con <- file(final_file, open = "wb")
writeBin(charToRaw("\xEF\xBB\xBF"), con)

# Append the cleaned CSV content
write.table(df_fixed, con, sep = ",", dec = ".", row.names = FALSE, col.names = TRUE, quote = TRUE, fileEncoding = "UTF-8")
close(con)


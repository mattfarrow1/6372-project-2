
# Setup -------------------------------------------------------------------

library(tidyverse)
library(here)       # relative location references
library(janitor)
library(naniar)     # dealing with missing values


# Load Data ---------------------------------------------------------------

# Read data
bank <- read_delim(here("data - raw", "bank-additional", "bank-additional-full.csv"), 
                   ";", escape_double = FALSE, trim_ws = TRUE)

# Clean column names
bank <- clean_names(bank)

# Examine Data ------------------------------------------------------------

glimpse(bank)

# Deal with Missing Values ------------------------------------------------

# Convert "unknown" to NA
bank_clean <- bank %>% 
  replace_with_na_all(condition = ~ .x == "unknown")

# Count number of mising values in data set
colSums(is.na(bank_clean))

# Other variables that may have missing values that we should explore
unique(bank_clean$pdays)
unique(bank_clean$poutcome)

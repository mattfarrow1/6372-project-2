#######################################
# Bank Marketing Data Set

# https://archive.ics.uci.edu/ml/datasets/bank%20marketing#
#######################################

# Setup -------------------------------------------------------------------

library(tidyverse)
library(here)       # relative location references
library(janitor)    # data cleanup tools
library(naniar)     # dealing with missing values


# Load Data ---------------------------------------------------------------

# Read data
bank <- read_delim(here("data - raw", "bank-additional", "bank-additional-full.csv"), 
                   ";", escape_double = FALSE, trim_ws = TRUE)

# Clean column names
bank <- clean_names(bank)

# Examine Data ------------------------------------------------------------

# What are the dimensions of the data
dim(bank)

# Look at the first and last rows of data
head(bank)
tail(bank)
glimpse(bank)

# Summary statistics
summary(bank)
skimr::skim(bank)

# Other possible functions for examining the data that may be helpful
Hmisc::describe(bank_clean)
psych::describe(bank_clean)
DataExplorer::create_report(bank)

# Deal with Missing Values ------------------------------------------------

# Convert "unknown" to NA
bank_clean <- bank %>% 
  replace_with_na_all(condition = ~ .x == "unknown")

# Convert 999 in pdays to NA
# 999 means client was not previously contacted
bank_clean <- bank_clean %>% 
  replace_with_na(replace = list(pdays = 999))

# Count number of missing values in data set
colSums(is.na(bank_clean))

# Other variables that may have missing values that we should explore
unique(bank_clean$poutcome)

# Examine the Data (Post-Cleanup) -----------------------------------------

# Re-run reports with missing values added
DataExplorer::create_report(bank_clean)

# Reports from inspectdf (categorical variables and Pearson correlation
# coefficients)
inspectdf::inspect_cat(bank_clean)
inspectdf::inspect_cor(bank_clean)


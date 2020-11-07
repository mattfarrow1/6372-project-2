#######################################
# Bank Marketing Data Set

# https://archive.ics.uci.edu/ml/datasets/bank%20marketing#
#######################################

# Setup -------------------------------------------------------------------

library(tidyverse)  # general data processing & plotting
library(here)       # relative location references
library(janitor)    # data cleanup tools
library(naniar)     # dealing with missing values
library(caret)      # misc functions for training and plotting classification and regression models
library(tidymodels)

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
Hmisc::describe(bank)
psych::describe(bank)
# DataExplorer::create_report(bank)

# Data Cleanup ------------------------------------------------------------

# Missing Values

# Several things happening here:
#     - Convert "unknown" to NA
#     - 999 means client was not previously contacted
#     - poutcome has a value "nonexistent" that is essentially missing 
#       since the prospect was not contacted
bank_clean <- bank %>% 
  replace_with_na_all(condition = ~ .x == "unknown") %>% 
  replace_with_na(replace = list(pdays = 999)) %>% 
  replace_with_na(replace = list(poutcome = "nonexistent"))

# Count number of missing values in data set
colSums(is.na(bank_clean))

# Re-level factors
sizes <- ordered(sizes, levels = c("small", "medium", "large"))
bank$day_of_week <- ordered(bank$day_of_week, levels = c("mon", "tue", "wed", "thu", "fri"))
bank$month <- ordered(bank$month, levels = c("mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"))

# Examine the Data (Post-Cleanup) -----------------------------------------

# Re-run reports with missing values added
DataExplorer::create_report(bank_clean)
Hmisc::describe(bank_clean)
psych::describe(bank_clean)
skimr::skim(bank_clean)

# Reports from inspectdf (categorical variables and Pearson correlation
# coefficients)
inspectdf::inspect_cat(bank_clean)
inspectdf::inspect_cor(bank_clean)

# EDA ---------------------------------------------------------------------

# Day of Week
bank_clean %>% 
  ggplot(aes(day_of_week, fill= y)) +
  geom_bar() +
  labs(title = "Is there a more popular day of the week?",
       x = "Day",
       y = "Count",
       fill = "Response") +
  theme_minimal()

# Feature Engineering -----------------------------------------------------

# previous contact
# pdays bucket
# seasonal contact (summer vs. winter)

bank_clean %>% 
  # Are there any housing or personal loans?
  mutate(any_loan = (housing == "yes" | loan == "yes")) %>% 
  # Based on the EDA, it doesn't appear as though day of the week is necessarily
  # notable, but I'll make a factor just in case
  mutate(weekdays_3 = if_else(day_of_week == "mon", "Beginning",
                              if_else(day_of_week == "fri", "End", "Middle"))) %>% 

# Carnival is usually in Feb/Mar

# Preprocess the Data -----------------------------------------------------

# Working with only the non-NA observations, we're left with only 3% of the
# original data...

bank_prep <- bank_clean %>% 
  # Exclude missing data
  na.omit() %>% 
  # For creating models, it is better to have qualitative columns
  # encoded as factors (instead of character strings)
  mutate_if(is.character, as.factor)

# Test/Train Data ---------------------------------------------------------

train_index <- createDataPartition(bank_prep$y,
                                   p = .8,
                                   list = FALSE,
                                   times = 1)

bank_train <- bank_prep[ train_index,]
bank_test  <- bank_prep[-train_index,]

# Down-sample the Data ----------------------------------------------------

# https://topepo.github.io/caret/subsampling-for-class-imbalances.html#subsampling-during-resampling

library(caret)
set.seed(123)

# Convert y to factor
bank_clean$y <- as_factor(bank_clean$y)

# Down-sample
bank_clean_ds <- downSample(x = bank_clean[, -ncol(bank_clean)],
                            y = bank_clean$y)

# Look at the distribution
table(bank_clean_ds$Class)


# FEATURE ENGINEERING -----------------------------------------------------
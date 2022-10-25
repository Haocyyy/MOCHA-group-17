library(readr)
library(dplyr)
library(tidyverse)
library(readxl)

# Load the original data file ---------------------------------------------
data_names <-read_excel("data/POCHA_FULL_DATASET_names.xlsx", sheet = 1)
head(data_names)
data_labels <-read_excel("data/POCHA_FULL_DATASET_labels.xlsx", sheet = 1)
head(data_labels)


# Create Quality table ----------------------------------------------------
Quality <- data_labels %>%
  select(Responseid.,
        c(65, 66, 73, 74, 81, 82, 89, 90, 97, 98, 105, 106, 113, 114, 121, 122, 129, 130, 137, 138) ) %>%
  arrange(name, province, region, segment) %>%
  group_by(name, province, region, segment) %>%
  distinct() %>%
  ungroup() %>%
  mutate(customerid = row_number())

# Create Priority table ---------------------------------------------------







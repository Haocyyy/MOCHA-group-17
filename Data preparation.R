library(readr)
library(dplyr)
library(tidyverse)
library(readx1)

#Load the original data file



data0 <-read_excel("data/POCHA_FULL_DATASET_names.xlsx", sheet = 1)
head(data0)
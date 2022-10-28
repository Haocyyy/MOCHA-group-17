library(readr)
library(dplyr)
library(tidyverse)
#please install this package if not
library(readxl)

# Load the original data file ---------------------------------------------
data_names <-read_excel("data/POCHA_FULL_DATASET_names.xlsx", sheet = 1)
head(data_names)
data_labels <-read_excel("data/POCHA_FULL_DATASET_labels.xlsx", sheet = 1)
head(data_labels)
data_items <- read_delim(file = "data/mocha_items.csv",
                        delim = ";", col_names = TRUE, col_types = NULL,
                        locale = locale(encoding="ISO-8859-1"))
head(data_items)

# Create Respondent table -------------------------------------------------
Respondent <- data_names %>% 
  select(Responseid,Q1,Q2) %>% 
  rename(Respondent_ID = Responseid,Age = Q1,Gender=Q2)

# Create Region table -----------------------------------------------------
Region <-

# Create Health Consumption table -----------------------------------------



# Create Quality of primary care table ----------------------------------------------------

#Create Quality table
Quality <- data_labels %>%
  select(Responseid.,
         c(179:218))

#Rename all column names into their itemnumbers
oldnames = colnames(Quality)
newnames = c("Responseid.", 1:40)

Quality <- Quality %>%
  rename_with(~ newnames[which(oldnames == .x)], .cols = oldnames)

#Create satisfaction score table
Satisfaction_score <- data_labels %>%
  select(Responseid., 219) %>%
  rename("Satisfy" = 2)

#Combine to create Quality of primary care table
Quality_of_primary_care <-cbind(Quality,Satisfaction_score[2]) %>%
  rename('Quality ID' = Responseid.)

# Create Priority table ---------------------------------------------------

#Create most & least table
Most <- data_labels %>%
  select(Responseid.,
         c(65, 73, 81, 89, 97, 105, 113, 121, 129, 137),
         c(226, 234, 242, 250, 258, 266, 274, 282, 290,298))

Least <- data_labels %>%
  select(Responseid.,
         c(66, 74, 82, 90, 98, 106, 114, 122, 130, 138),
         c(227, 235, 243, 251, 259, 267, 275, 283, 291,299))

#Count attributeitems
levelsMost <- unique(do.call(c,Most[2:21]))
outMost <- sapply(levelsMost,function(x)rowSums(Most==x))
colnames(outMost) <- levelsMost

levelsLeast <- unique(do.call(c,Least[2:21]))
outLeast <- sapply(levelsLeast,function(x)rowSums(Least==x))
colnames(outLeast) <- levelsLeast

#Add counted attributeitems to most&least tables + delete the "questions" + put them in order  
Most <- cbind(Most,outMost) %>%
  select(.,-c(2:21)) 

Most <- Most %>% select(order(colnames(Most))) 

Least <- cbind(Least,outLeast) %>%
  select(.,-c(2:21))

Least <- Least %>% select(order(colnames(Least))) 

#Range of all attributes (Most - Least = actual score) to create Priority table
Priority <- Most[c(1:40)] - Least[c(1:40)] 

Priority <-cbind(Priority,Most[41]) %>%
  rename('Priority ID' = Responseid.)


# Seperate itemlabels (attributes) ----------------------------------------






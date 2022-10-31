library(readr)
library(dplyr)
library(tidyverse)
#please install this package if not
library(readxl)
library(rlang)
library(rJava)
options( java.parameters = "-Xmx5000m")
library(xlsx)
library(xlsxjars)

# Load the original data file ---------------------------------------------
#Use library xlsx and read.xlsx method to read xlsx file for UTF-8 encode
#it takes a while to read the xlsx(more than 2 mins)
data_names<-read.xlsx(
  file = "data/POCHA_FULL_DATASET_names.xlsx", sheetIndex=1, 
  encoding = "UTF-8", stringsAsFactors = F)
head(data_names)

data_labels <-read_excel("data/POCHA_FULL_DATASET_labels.xlsx", sheet = 1)
head(data_labels)

data_items <- read_delim(file = "data/mocha_items.csv",
                        delim = ";", col_names = TRUE, col_types = NULL,
                        locale = locale(encoding="ISO-8859-1"))
head(data_items)

# Create Health Consumption table -----------------------------------------
Health_Consumption <- data.frame(health_consumption_id = c(0, 1), 
                                 health_consumption_under_18 = c("No", "Yes")
) 
Health_Consumption$health_consumption_id<- as.integer(
  Health_Consumption$health_consumption_id)

# Create Region table -----------------------------------------------------
Region <-data_names %>% 
  select(Responseid,Q45,Q3UK,Q3NL,Q3DE,Q3ES,Q3PL,Qcountry) %>% 
  rename(Respondent_ID = Responseid,City_size=Q45) %>% 
  #Create "Country" column
  mutate(Country = case_when(
    !is.na(Q3UK)~'United Kingdom',
    !is.na(Q3DE)~'Germany',
    !is.na(Q3NL)~'Netherlands',
    !is.na(Q3ES)~'Spain',
    !is.na(Q3PL)~'Poland'
  )) %>% 
  #Create "Region"column
  mutate(Region = case_when(
    !is.na(Q3UK)~Q3UK,
    !is.na(Q3DE)~Q3DE,
    !is.na(Q3NL)~Q3NL,
    !is.na(Q3ES)~Q3ES,
    !is.na(Q3PL)~Q3PL
  )) %>% 
  #Create "Region ID" column whihc combines responeseid and country
  mutate(Region_ID= paste(Respondent_ID,Qcountry)) %>% 
  select(-Respondent_ID,-Q3UK,-Q3NL,-Q3DE,-Q3ES,-Q3PL,-Qcountry)
#Reorder the columns 
Region <- Region[, c("Region_ID", "Country", "Region","City_size")]

# Create Respondent table -------------------------------------------------
Respondent <- data_names %>% 
  select(Responseid,Q1,Q2,Qcountry,Q4,Q5,Q5_4_other,Q6,
         Q44UK,Q44NL,Q44NL_6_other,Q44DE,Q44ES,Q44PL) %>% 
  rename(Respondent_ID = Responseid,
         Age = Q1,Gender=Q2,Parent_or_not=Q4,
         Nr_of_children=Q5,Children_under_18=Q6,
         Edu_level_UK=Q44UK,Edu_level_DE=Q44DE,
         Edu_level_ES=Q44ES,Edu_level_PL=Q44PL) %>% 
  mutate(Edu_level_NL=case_when(
    !is.na(Q44NL)~Q44NL,
    !is.na(Q44NL_6_other)~Q44NL_6_other,
  )) %>% 
  mutate(Region_ID= paste(Respondent_ID,Qcountry))

Respondent$Nr_of_children<-ifelse(
  Respondent$Nr_of_children=="Other",
  Respondent$Q5_4_other,Respondent$Nr_of_children)
#delete columns
Respondent = subset(Respondent, 
                    select = -c(Qcountry,Q5_4_other,Q44NL,Q44NL_6_other) )

#create "Health_consumption column"
Respondent <-Respondent %>% 
  mutate(Health_consumption=case_when(
    Parent_or_not=='No'~0,
    Children_under_18=='No'~0,
  )) 
Respondent$Health_consumption[is.na(Respondent$Health_consumption)] <-1
# Create Quality of primary care table ----------------------------------------------------

#Create Quality table
Quality_of_primary_care <- data_labels %>%
  select(Responseid.,
         c(179:218)) %>%
  rename('Quality ID' = Responseid.)

#Rename all column names into their itemnumbers
oldnames = colnames(Quality_of_primary_care)
newnames = c("Quality ID", 1:40)

Quality_of_primary_care <- Quality_of_primary_care %>%
  rename_with(~ newnames[which(oldnames == .x)], .cols = oldnames)

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


# Create item table (attributes) (not in star scheme?) ----------------------------------------
Items <- data_items %>%
  rename(Split = "Attribute Item theme",
         ItemID = "Attribute Item nr")

Items[c("Theme","Theme1")] <- str_split_fixed(Items$Split, ';', 2)

Items[c("Keyword","Key1")] <- str_split_fixed(Items$Theme1, '\\[', 2)

Items <- Items[,-2]
Items <- Items[,-4]
Items <- Items[,-5]
  
# Create facttable --------------------------------------------------------

#Create satisfaction score table first (not in star scheme)
Satisfaction_score <- data_labels %>%
  select(Responseid., 219) %>%
  rename("Satisfy" = 2)

#Join tables

Fact <- Respondent %>%
  select(Respondent_ID, Region_ID, Health_consumption)

Fact <- Fact %>%
  full_join(Quality_of_primary_care, by = c("Respondent_ID" = "Quality ID"))

Fact <- Fact %>%
  full_join(Satisfaction_score, by = c("Respondent_ID" = "Responseid."))

Fact <- Fact %>%
  full_join(Priority, by = c ("Respondent_ID" = "Priority ID"))

# Connect to the PostgreSQL database server -------------------------------

library(DBI)
library(RPostgreSQL)

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, port = 5432, host = "bronto.ewi.utwente.nl",
                 dbname = "dab_ds22231a_2", user = "dab_ds22231a_2", 
                 password = "dzhmetxI0WPAepIp",
                 options="-c search_path=project")
Region<-as.data.frame(Region)
Health_Consumption<-as.data.frame(Health_Consumption)
Respondent<-as.data.frame(Respondent)
Quality_of_primary_care<-as.data.frame(Quality_of_primary_care)
Priority<-as.data.frame(Priority)
Items<-as.data.frame(Items)
Fact<-as.data.frame(Fact)

dbWriteTable(con, "Region", value = Region, overwrite = T, row.names = F)
dbWriteTable(con, "Health_Consumption", value = Health_Consumption, 
             overwrite = T, row.names = F)
dbWriteTable(con, "Respondent", value = Respondent, overwrite = T, row.names = F)
dbWriteTable(con, "Quality_of_primary_care", value = Quality_of_primary_care, overwrite = T, row.names = F)
dbWriteTable(con, "Priority", value = Priority, overwrite = T, row.names = F)
dbWriteTable(con, "Items", value = Items, overwrite = T, row.names = F)
dbWriteTable(con, "Fact", value = Fact, overwrite = T, row.names = F)

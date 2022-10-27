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

data_labels <-read_excel("data/POCHA_FULL_DATASET_labels.xlsx", sheet = 1)
head(data_labels)


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



  
  
  
# Connect to the PostgreSQL database server -------------------------------
  
  library(DBI)
  library(RPostgreSQL)

  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, port = 5432, host = "bronto.ewi.utwente.nl",
                   dbname = "dab_ds22231a_2", user = "dab_ds22231a_2", 
                   password = "dzhmetxI0WPAepIp",
                   options="-c search_path=project")
  Region<-as.data.frame(Region)
  Health_Consumption<-as.data.frame
  Respondent<-as.data.frame(Respondent)
  
  dbWriteTable(con, "Region", value = Region, overwrite = T, row.names = F)
  dbWriteTable(con, "Health_Consumption", value = Health_Consumption, 
               overwrite = T, row.names = F)
  dbWriteTable(con, "Respondent", value = Respondent, overwrite = T, row.names = F)


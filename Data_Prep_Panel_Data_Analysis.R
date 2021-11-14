################ Seminar Paper - Econometrics - Winter Term 2021/2022

######## Name: Martin Scheerer
######## Student ID: 5631373

# Libraries  --------------------------------------------------------------
rm(list = ls())

## Now: load all necessary packages used throughout the R file

if (!require("tidyverse")) install.packages("tidyverse")
if (!require("plyr")) install.packages("plyr")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tibble")) install.packages("tibble")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("stringr")) install.packages("stringr")
if (!require("stringi")) install.packages("stringi")
if (!require("zoo")) install.packages("zoo")
if (!require("timetk")) install.packages("timetk")

library(tidyverse)
library(plyr)
library(dplyr)
library(tibble)
library(ggplot2)
library(stringr)
library(stringi)
library(zoo)
library(timetk)


# Data Import and Preparation  --------------------------------------------

#### Policy Data is directly downloaded from the github repository of the 
#### Oxford Covid-19 Government Response Tracker

url_ox <- "https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv"
covid <- read.csv(url_ox)      # non-pharmaceutical interventions

# Filtering only those npi that are imposed on national level (cross-country) and 
# remove columns that are not needed

npi <- covid %>% 
  mutate(Date = as.Date(as.character(Date), "%Y%m%d")) %>% 
  filter(Jurisdiction == "NAT_TOTAL") %>%
  select(-c("ConfirmedDeaths", "ConfirmedCases", "RegionName", "RegionCode", 
            "M1_Wildcard", "Jurisdiction")) 
  
# Remove Economic Measures that do not aim at reducing infections, flags and 
# index variables temporarily to facilitate removal of duplicates of measures

npi <- npi %>% 
  select(-colnames(npi)[which(str_detect(colnames(npi), "^E[[:digit:]]"))]) 

flags <- colnames(npi)[which(str_detect(colnames(npi), "Flag"))]
npi <- npi %>%
  select(-flags)

index_vars <- colnames(covid[which(str_detect(colnames(covid), "Index"))])
vars <- which(colnames(npi) %in% c("Date", index_vars))
npi <- npi[!duplicated(npi[, -vars]), ] 

saveRDS(npi, "Data/npi.rds")

#### Vaccination Data is directly downloaded from the github repository of the 
#### Our World in Data account

url_vac <- "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv"
vaccination <- read.csv(url_vac)

# Replace na with last non-na

vaccination <- vaccination %>%
  mutate(Date = as.Date(date), date = NULL) %>%
  rename_at("location", ~"Country") %>%
  mutate_all(funs(na.locf(., na.rm = FALSE)))

saveRDS(vaccination, "Data/vaccination.rds")

#### New Cases and Deaths downloaded from the github repository of the 
#### Johns Hopkins University

url_jhu_deaths <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
url_jhu_cases <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"

new_deaths <- read.csv(url_jhu_deaths)
new_cases <- read.csv(url_jhu_cases)

# Change format of dataset from wide to long -> column names into Date variable

names(new_deaths)  <- sub('^X', '', names(new_deaths))
names(new_cases)  <- sub('^X', '', names(new_cases))

new_deaths <- new_deaths %>%
  pivot_longer(cols = colnames(new_deaths)[5:ncol(new_deaths)], names_to = "Date", 
               values_to = "Daily_Deaths") %>%
  mutate(Date = as.Date(strptime(Date, "%m.%d.%y"))) %>%
  select(-c(Lat, Long))

new_cases <- new_cases %>%
  pivot_longer(cols = colnames(new_cases)[5:ncol(new_cases)], names_to = "Date", 
               values_to = "Daily_Cases") %>%
  mutate(Date = as.Date(strptime(Date, "%m.%d.%y"))) %>%
  select(-c(Lat, Long))

# Aggregate regions to one country and replace them

reg_death <- new_deaths %>%
  filter(Province.State != "") %>%
  group_by(Country.Region) %>%
  summarise_by_time(.date_var = Date, value = sum(Daily_Deaths)) %>%
  rename_at("value",~"Daily_Deaths") 

new_deaths <- new_deaths %>%
  filter(Province.State == "") %>%
  select(-Province.State) 

new_deaths <- rbind(new_deaths, reg_death) %>%
  rename_at("Country.Region", ~"Country")

# Same for Cases
  
reg_cases <- new_cases %>%
  filter(Province.State != "") %>%
  group_by(Country.Region) %>%
  summarise_by_time(.date_var = Date, value = sum(Daily_Cases)) %>%
  rename_at("value",~"Daily_Cases") 
  
new_cases <- new_cases %>%
  filter(Province.State == "") %>%
  select(-Province.State) 

new_cases <- rbind(new_cases, reg_cases) %>%
  rename_at("Country.Region", ~"Country")

saveRDS(new_cases, "Data/cases.rds")
saveRDS(new_deaths, "Data/deaths.rds")

#### Static Country Indicators World Bank





























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
if (!require("readxl")) install.packages("readxl")
if (!require("lubridate")) install.packages("lubridate")

library(tidyverse)
library(plyr)
library(dplyr)
library(tibble)
library(ggplot2)
library(stringr)
library(stringi)
library(zoo)
library(timetk)
library(readxl)
library(lubridate)

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
  select(-colnames(npi)[which(str_detect(colnames(npi), "^E[[:digit:]]"))]) %>%
  select(-colnames(npi)[which(str_detect(colnames(npi), "Display"))]) %>%
  rename_at("CountryName", ~"Country")

flags <- colnames(npi)[which(str_detect(colnames(npi), "Flag"))]
npi <- npi %>%
  select(-all_of(flags))

npi_kw <- npi %>%
  mutate(Year = format(Date, "%Y")) %>%
  mutate(KW = week(Date)) %>%
  group_by(Country, CountryCode, Year, KW) %>%
  summarise_all(median)
    
saveRDS(npi_kw, "Data/npi_weekly.rds")

## Code for calculating first date of measures
# measure_names <- colnames(npi)[str_detect(colnames(npi), "^[[:Alpha:]][[:digit:]]")]
# 
# countries <- unique(npi$Country)
# 
# # Obtain Dates for individual measures
# 
# new_measures <- data.frame(Date = character(0), Country = character(0), 
#                            Measure = character(0), Intensity = numeric(0))
# 
# for(i in countries){
#   
#   for(j in measure_names){
# 
#     sub <- npi %>% 
#       filter(Country == i) 
#     
#     row_index <- which(sub[, j] != lag(sub[, j]))
#     
#     if(is_empty(row_index)){
#       
#       next
#       
#     }
#       
#     imposed_date <- sub[row_index, "Date"]
#     
#     intensity <-sub[row_index, j]
#     
#     data <- data.frame(Date = imposed_date, Country = i, Intensity = intensity,
#                        Measure = j)
#     
#     new_measures <- rbind(new_measures, data)
#     
#   }
#   
# }
# 
# npi <- npi %>%
#   select(-all_of(measure_names))
# 
# measures <- new_measures %>%
#   arrange(Country, Date) %>%
#   inner_join(., npi, by = c("Date", "Country"))
# 
# saveRDS(measures, "Data/npi.rds")

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
  rename_at("Country.Region", ~"Country") %>%
  rename_at("Daily_Deaths",~"Weekly_Average_Deaths") %>%
  mutate(Year = format(Date, "%Y")) %>%
  mutate(KW = week(Date)) %>%
  group_by(Country, Year, KW) %>%
  summarise_all(mean) %>%
  mutate("One_Week_Lead" = lead(Weekly_Average_Deaths, n = 1), 
         "Two_Week_Lead" = lead(Weekly_Average_Deaths, n = 2), 
         "Three_Week_Lead" = lead(Weekly_Average_Deaths, n = 3),
         "Four_Week_Lead" = lead(Weekly_Average_Deaths, n = 4))

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
  rename_at("Country.Region", ~"Country") %>%
  rename_at("Daily_Cases",~"Weekly_Average_Cases") %>%
  mutate(Year = format(Date, "%Y")) %>%
  mutate(KW = week(Date)) %>%
  group_by(Country, Year, KW) %>%
  summarise_all(mean) %>%
  mutate("One_Week_Lead" = lead(Weekly_Average_Cases, n = 1), 
         "Two_Week_Lead" = lead(Weekly_Average_Cases, n = 2), 
         "Three_Week_Lead" = lead(Weekly_Average_Cases, n = 3),
         "Four_Week_Lead" = lead(Weekly_Average_Cases, n = 4))

saveRDS(new_cases, "Data/cases.rds")
saveRDS(new_deaths, "Data/deaths.rds")

#### Static Country Indicators 
#### https://databank.worldbank.org/indicator/NY.GDP.PCAP.CD/1ff4a498/Popular-Indicators
#### Extract latest value for each variable

gini_hospital <- read_excel("World_Bank_Gini_Hospital.xlsx", sheet = 1, 
                            na = "..")[-435:-439, -2]
colnames(gini_hospital) <- c("Variable", "Country", "CountryCode", as.character(2013:2020))

gini_hosp <- pivot_longer(gini_hospital, colnames(gini_hospital[4:11]), 
                          names_to = "Year", values_to = "Value") %>%
  pivot_wider(names_from = Variable, values_from = Value)
colnames(gini_hosp)[4:5] <- c("Gini_Index", "Hospital_Beds_per_1000_people")

n <- length(unique(gini_hosp$Country))
countries <- unique(gini_hosp$Country)

gini_hospital_data <- data.frame(Country = character(0), Gini = numeric(0), 
                                 Hospital_Beds = numeric(0))

for(i in 1:n){
  
  data <- gini_hosp %>%
    filter(Country == countries[i]) 
  
  gini <- data$Gini_Index
  
  if(sum(!is.na(gini)) == 0){
    
    gini <- NA
    
  } 
  
  else {
    
    gini <- tail(na.omit(data$Gini_Index), n = 1)
    
  }

  hosp <- data$Hospital_Beds_per_1000_people
  
  if(sum(!is.na(hosp)) == 0){
    
    hosp <- NA
    
  } 
  
  else {
    
    hosp <- tail(na.omit(data$Hospital_Beds_per_1000_people), n = 1)
    
  }

  new_data <- data.frame(Country = countries[i], Gini = gini, Hospital_Beds = hosp)
  
  gini_hospital_data <- rbind(gini_hospital_data, new_data)
  
}

#### Extracting further data (different World Bank indicators)

world_bank <- read_excel("World_Bank.xlsx", sheet = 1, na = "..")[-652:-656, -2]
colnames(world_bank) <- c("Variable", "Country", "CountryCode", "Value_2020")

world_bank <- world_bank %>%
  pivot_wider(names_from = Variable, values_from = Value_2020)
colnames(world_bank)[3:5] <- c("GDP_per_Capita_USD", "Population_Over_65", 
                               "Population_Density")

world_bank_complete <- inner_join(world_bank, gini_hospital_data, by = "Country")

#### Country Filter - keeping reliable countries
#### using https://www.transparency.org/en/cpi/2020/index/hkg

wb_full <- world_bank_complete[complete.cases(world_bank_complete), ]

cpi <- read_excel("CPI2020.xlsx", sheet = 1, skip = 2)[, 1:4] %>%
  filter(`CPI score 2020` >= 45)

all <- inner_join(wb_full, cpi, c("CountryCode" = "ISO3")) %>% 
  select(-Country.y) %>%
  rename_at("Country.x", ~"Country")

saveRDS(all, "Data/world_bank_complete.rds")

#### Continent and Latitude Data as controls for seasons
#### Data for map data visualization

geodata <- map_data("world")
saveRDS(geodata, "Data/geodata.rds")




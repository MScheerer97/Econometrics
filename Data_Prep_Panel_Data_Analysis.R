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
  select(-colnames(npi)[which(str_detect(colnames(npi), "^E[[:digit:]]"))]) %>%
  select(-colnames(npi)[which(str_detect(colnames(npi), "Display"))]) %>%
  rename_at("CountryName", ~"Country")

flags <- colnames(npi)[which(str_detect(colnames(npi), "Flag"))]
npi <- npi %>%
  select(-all_of(flags))

measure_names <- colnames(npi)[str_detect(colnames(npi), "^[[:Alpha:]][[:digit:]]")]

countries <- unique(npi$Country)

# Obtain Dates for individual measures

new_measures <- data.frame(Date = character(0), Country = character(0), 
                           Measure = character(0), Intensity = numeric(0))

for(i in countries){
  
  for(j in measure_names){

    sub <- npi %>% 
      filter(Country == i) 
    
    row_index <- which(sub[, j] != lag(sub[, j]))
    
    if(is_empty(row_index)){
      
      next
      
    }
      
    imposed_date <- sub[row_index, "Date"]
    
    intensity <-sub[row_index, j]
    
    data <- data.frame(Date = imposed_date, Country = i, Intensity = intensity,
                       Measure = j)
    
    new_measures <- rbind(new_measures, data)
    
  }
  
}

npi <- npi %>%
  select(-all_of(measure_names))

measures <- new_measures %>%
  arrange(Country, Date) %>%
  inner_join(., npi, by = c("Date", "Country"))

saveRDS(measures, "Data/npi.rds")

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
  mutate("Five_Day_Lead" = lead(Daily_Deaths, n = 5), 
         "Seven_Day_Lead" = lead(Daily_Deaths, n = 7), 
         "Ten_Day_Lead" = lead(Daily_Deaths, n = 10),
         "Thirteen_Day_Lead" = lead(Daily_Deaths, n = 13),
         "Fifteen_Day_Lead" = lead(Daily_Deaths, n = 15),
         "Twenty_Day_Lead" = lead(Daily_Deaths, n = 20))

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
  mutate("Five_Day_Lead" = lead(Daily_Cases, n = 5), 
         "Seven_Day_Lead" = lead(Daily_Cases, n = 7), 
         "Ten_Day_Lead" = lead(Daily_Cases, n = 10),
         "Thirteen_Day_Lead" = lead(Daily_Cases, n = 13),
         "Fifteen_Day_Lead" = lead(Daily_Cases, n = 15),
         "Twenty_Day_Lead" = lead(Daily_Cases, n = 20))

saveRDS(new_cases, "Data/cases.rds")
saveRDS(new_deaths, "Data/deaths.rds")

#### Static Country Indicators World Bank





























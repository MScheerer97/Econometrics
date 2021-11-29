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

# Replace na with last non-na from same group

vaccination <- vaccination %>%
  mutate(Date = as.Date(date), date = NULL) %>%
  rename_at("location", ~"Country") %>%
  select(Country, iso_code, Date, people_vaccinated_per_hundred, 
         people_fully_vaccinated_per_hundred) %>%
  group_by(Country) %>%
  arrange(Country, Date) %>%
  mutate_all(funs(na.locf(., na.rm = FALSE)))

#### WHO Cases and Deaths https://covid19.who.int/table

covid_who <- read.csv("WHO-COVID-19-global-data.csv") %>%
  rename_at("ï..Date_reported", ~ "Date") %>%
  rename_at("Country_code",  ~ "CountryCode") %>%
  select(-WHO_region) %>%
  filter(New_cases >= 0) %>%
  filter(New_deaths >= 0) %>%
  mutate(Date = as.Date(Date))

# Cumulative Cases and Deaths

cum_cases <- covid_who %>%
  select(Country, Cumulative_cases, Date) %>%
  group_by(Country) %>%
  filter(Date == max(Date)) 

cum_deaths <- covid_who %>%
  select(Country, Cumulative_deaths, Date) %>%
  group_by(Country) %>%
  filter(Date == max(Date)) 

# Weekly Cases

new_cases_weekly <- covid_who %>%
  select(Date, Country, New_cases) %>%
  rename_at("New_cases",~"Weekly_Average_Cases") %>%
  mutate(Year = format(Date, "%Y")) %>%
  mutate(KW = week(Date)) %>%
  group_by(Country, Year, KW) %>%
  summarise_all(mean) %>%
  ungroup() %>%
  group_by(Country) %>%
  dplyr::mutate("One_Week_Lead" = lead(Weekly_Average_Cases, n = 1), 
                "Two_Week_Lead" = lead(Weekly_Average_Cases, n = 2), 
                "Three_Week_Lead" = lead(Weekly_Average_Cases, n = 3),
                "Four_Week_Lead" = lead(Weekly_Average_Cases, n = 4)) %>%
  arrange(Country, Year, KW)

# Weekly Deaths

new_deaths_weekly <- covid_who %>%
  select(Date, Country, New_deaths) %>%
  rename_at("New_deaths",~"Weekly_Average_Deaths") %>%
  mutate(Year = format(Date, "%Y")) %>%
  mutate(KW = week(Date)) %>%
  group_by(Country, Year, KW) %>%
  summarise_all(mean) %>%
  ungroup() %>%
  group_by(Country) %>%
  dplyr::mutate("One_Week_Lead" = lead(Weekly_Average_Deaths, n = 1), 
                "Two_Week_Lead" = lead(Weekly_Average_Deaths, n = 2), 
                "Three_Week_Lead" = lead(Weekly_Average_Deaths, n = 3),
                "Four_Week_Lead" = lead(Weekly_Average_Deaths, n = 4)) %>%
  arrange(Country, Year, KW)

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
country_filter <- unique(all$Country)
  
#### Continent and Latitude Data as controls for seasons
#### Data for map data visualization

geodata <- map_data("world") 
geodata$region <- mapvalues(geodata$region, 
                            from = c("South Korea", "Slovakia", "Saint Lucia", 
                                     "UK", "USA"),
                            to = c("Korea, Rep.", "Slovak Republic", "St. Lucia", 
                                   "United Kingdom", "United States"))
saveRDS(geodata, "Data/geodata.rds")

hemi <- geodata %>%
  filter(region %in% country_filter) %>%
  group_by(region) %>%
  dplyr::mutate(hemisphere = ifelse(mean(lat) < 0, "Southern Hemisphere", "Northern Hemisphere")) %>%
  ungroup()
saveRDS(hemi, "Data/hemisphere.rds")

## Adding continent information

continent <- read.csv("https://raw.githubusercontent.com/dbouquin/IS_608/master/NanosatDB_munging/Countries-Continents.csv")
continent$Country <- mapvalues(continent$Country, 
                            from = c("Korea, South", "Slovakia", "Saint Lucia", 
                                     "CZ", "US"),
                            to = c("Korea, Rep.", "Slovak Republic", "St. Lucia", 
                                   "Czech Republic", "United States"))

saveRDS(continent, "Data/continent.rds")

#### Country Population

pop <- read_excel("world_population.xls", sheet = 1, skip = 3) %>%
  select(`Country Name`, `Country Code`, `2020`) %>%
  rename_at("2020", ~ "Total_Population") 
colnames(pop)[1:2] <- c("Country", "CountryCode")
pop <- pop %>%
  filter(Country %in% country_filter)

saveRDS(pop, "Data/total_population.rds")

#### Already filter countries for other data

## npi daily for aggregation
npi$Country <- mapvalues(npi$Country, 
                               from = c("South Korea"),
                               to = c("Korea, Rep."))
npi <- npi %>%
  filter(Country %in% country_filter)
saveRDS(npi, "Data/npi_daily.rds")

## npi_weekly
npi_kw$Country <- mapvalues(npi_kw$Country, 
                            from = c("South Korea"),
                            to = c("Korea, Rep."))

npi_kw <- npi_kw %>%
  filter(Country %in% country_filter)
saveRDS(npi_kw, "Data/npi_weekly.rds")

## vaccination daily
vaccination$Country <- mapvalues(vaccination$Country, from = c("South Korea", "Czechia", 
                                                "Slovakia","Saint Lucia"),
                                       to = c("Korea, Rep.", "Czech Republic", 
                                              "Slovak Republic", "St. Lucia"))
vaccination <- vaccination %>%
  filter(Country %in% country_filter) 

vaccination[is.na(vaccination)] <- 0

saveRDS(vaccination, "Data/vaccination_daily.rds")


## vaccination weekly 
vaccination_week <- vaccination %>%
  rename_at("iso_code", ~ "CountryCode") %>%
  mutate(Year = format(Date, "%Y")) %>%
  mutate(KW = week(Date)) %>%
  group_by(Country, CountryCode, Year, KW) %>%
  summarise_all(max)

saveRDS(vaccination_week, "Data/vaccination_weekly.rds")

## cases and deaths daily
cum_deaths$Country <- mapvalues(cum_deaths$Country, 
                                from = c("Republic of Korea", "Czechia", "Slovakia", "Saint Lucia",
                                         "The United Kingdom", "United States of America"),
                                to = c("Korea, Rep.", "Czech Republic", 
                                       "Slovak Republic", "St. Lucia","United Kingdom",
                                       "United States"))

cum_cases$Country <- mapvalues(cum_cases$Country, 
                               from = c("Republic of Korea", "Czechia", "Slovakia", "Saint Lucia",
                                        "The United Kingdom", "United States of America"),
                               to = c("Korea, Rep.", "Czech Republic", 
                                        "Slovak Republic", "St. Lucia","United Kingdom",
                                      "United States"))

cum_deaths <- cum_deaths %>%
  filter(Country %in% country_filter)

cum_cases <- cum_cases %>%
  filter(Country %in% country_filter)

saveRDS(cum_deaths, "Data/deaths_daily.rds")
saveRDS(cum_cases, "Data/cases_daily.rds")

## cases and deaths weekly
new_deaths_weekly$Country <- mapvalues(new_deaths_weekly$Country, 
                                       from = c("Republic of Korea", "Czechia", "Slovakia", "Saint Lucia",
                                                "The United Kingdom", "United States of America"),
                                       to = c("Korea, Rep.", "Czech Republic", 
                                              "Slovak Republic", "St. Lucia","United Kingdom",
                                              "United States"))

new_cases_weekly$Country <- mapvalues(new_cases_weekly$Country, 
                                      from = c("Republic of Korea", "Czechia", "Slovakia", "Saint Lucia",
                                               "The United Kingdom", "United States of America"),
                                      to = c("Korea, Rep.", "Czech Republic", 
                                             "Slovak Republic", "St. Lucia","United Kingdom",
                                             "United States"))

new_deaths_weekly <- new_deaths_weekly %>%
  filter(Country %in% country_filter) 

new_cases_weekly <- new_cases_weekly %>%
  filter(Country %in% country_filter)

saveRDS(new_cases_weekly, "Data/cases_weekly.rds")
saveRDS(new_deaths_weekly, "Data/deaths_weekly.rds")


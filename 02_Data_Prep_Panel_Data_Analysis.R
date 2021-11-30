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
if (!require("lubridate")) install.packages("lubridate")

library(tidyverse)
library(plyr)
library(dplyr)
library(tibble)
library(ggplot2)
library(stringr)
library(stringi)
library(lubridate)

#### Cross-sectional data

# World Bank Indicators
wb_data <- readRDS("Data/world_bank_complete.rds") %>%
  select(-Region, -CountryCode) 

# Population
pop <- readRDS("Data/total_population.rds") %>%
  mutate(pop_per_10_million = Total_Population/10000000, 
         pop_per_one_hundred_k = Total_Population/100000)

# Continent and Hemisphere
continent <- readRDS("Data/continent.rds")

hemi <- readRDS("Data/hemisphere.rds") %>%
  select(region, hemisphere) %>%
  dplyr::rename(Country = region) %>%
  distinct()

#### Weekly npis: 

npi <- readRDS("Data/npi_weekly.rds") %>%
  ungroup() %>%
  select(-Date) %>%
  select(-CountryCode)
#### Weekly Vaccination Rate

vac <- readRDS("Data/vaccination_weekly.rds") %>%
  ungroup() %>%
  select(-Date) %>%
  select(-CountryCode)

#### Weekly Deaths and Cases

cases_week <- readRDS("Data/cases_weekly.rds") %>%
  select(-Date)

deaths_week <- readRDS("Data/deaths_weekly.rds") %>%
  mutate(Month = month(Date)) %>%
  select(-Date)


#### Merge into final Panel Data

panel_data <- inner_join(cases_week, deaths_week, by = c("Country", "Year", "KW")) %>%
  inner_join(npi, by = c("Country", "Year", "KW")) %>%
  full_join(vac, by = c("Country", "Year", "KW")) %>%
  inner_join(wb_data, by = "Country") %>%
  inner_join(pop, by = "Country") %>%
  inner_join(continent, by = "Country") %>%
  inner_join(hemi, by = "Country") 

panel_data$people_vaccinated_per_hundred[is.na(panel_data$people_vaccinated_per_hundred)] <- 0
panel_data$people_fully_vaccinated_per_hundred[is.na(panel_data$people_fully_vaccinated_per_hundred)] <- 0

panel_data <- panel_data[complete.cases(panel_data), ]

saveRDS(panel_data, "Output/panel_data.rds")


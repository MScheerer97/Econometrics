################ Seminar Paper - Econometrics - Winter Term 2021/2022

######## Name: Martin Scheerer
######## Student ID: 5631373

# Libraries  --------------------------------------------------------------
rm(list = ls())

## Now: load all necessary packages used throughout the R file

if (!require("tidyverse")) install.packages("tidyverse")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tibble")) install.packages("tibble")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("stringr")) install.packages("stringr")
if (!require("stringi")) install.packages("stringi")
if (!require("lubridate")) install.packages("lubridate")

library(tidyverse)
library(dplyr)
library(tibble)
library(ggplot2)
library(stringr)
library(stringi)
library(lubridate)

#### Country Filter and cross-sectional data

wb_data <- readRDS("Data/world_bank_complete.rds") %>%
  select(-Region) 

#### Aggregation of npi variables: 

npi <- readRDS("Data/npi_daily.rds")

measure_cols <- c("Country", colnames(npi)[c(4:14, 17:19)])
investment_cols <- c("Country", colnames(npi)[15:16])

# Days measure was 3
# measures_total <- npi %>%
#   select(all_of(measure_cols)) %>%
#   dplyr::group_by(Country) %>%
#   summarise_each(funs(sum(. == 3, na.rm = TRUE)))

# Days measure was 2 or 3
measures_total <- npi %>%
  select(all_of(measure_cols)) %>%
  dplyr::group_by(Country) %>%
  summarise_each(funs(sum(. %in% c(2, 3), na.rm = TRUE)))

# Sum of investments in healthcare/vaccine
investment_total <- npi %>%
  select(all_of(investment_cols)) %>%
  group_by(Country) %>%
  summarise_each(funs(sum(., na.rm = TRUE)))

#### Total Population

pop <- readRDS("Data/total_population.rds") %>%
  mutate(pop_per_10_million = Total_Population/10000000, 
         pop_per_one_hundred_k = Total_Population/100000)

#### Hemisphere

hemi <- readRDS("Data/hemisphere.rds") %>%
  select(region, hemisphere) %>%
  dplyr::rename(Country = region) %>%
  distinct()

#### New Cases and Deaths

cases <- readRDS("Data/cases_daily.rds") %>%
  select(-Date)

deaths <- readRDS("Data/deaths_daily.rds")%>%
  select(-Date)

#### Vaccination rate in May, August and November

vac <- readRDS("Data/vaccination_daily.rds") %>%
  mutate(month = month(Date)) %>%
  select(-Date, -iso_code) %>%
  filter(month %in% c(3, 5, 7, 9, 11)) %>%
  group_by(Country, month) %>%
  summarise_each(funs(max)) %>%
  pivot_wider(names_from = month, values_from = c(people_vaccinated_per_hundred, 
                                                  people_fully_vaccinated_per_hundred))

colnames(vac) <- colnames(vac) %>%
  str_replace_all(c("3" = "March", "5" = "May", "7" = "July", 
                    "9" = "September", "11" = "November"))

#### Merge ALL data for cross-sectional analysis
## Calculate cases per 10 million and deaths per one hundred thousand
## Divide investment variables by GDP
## Drop unnecessary variables








































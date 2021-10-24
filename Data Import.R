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


library(tidyverse)
library(plyr)
library(dplyr)
library(tibble)
library(ggplot2)



# Data Import and Preparation  --------------------------------------------

#### Policy Data is directly downloaded from the github repository of the 
#### Oxford Covid-19 Government Response Tracker

url_ox <- "https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv"
npi <- read.csv(url_ox)      # non-pharmaceutical interventions

str(npi)

npi <- npi %>% 
  mutate(Date = as.Date(as.character(Date), "%Y%m%d"))

#### Vaccination Data is directly downloaded from the github repository of the 
#### Our World in Data account

url_vac <- "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv"
vaccination <- read.csv(url_vac)

str(vaccination)

vaccination <- vaccination %>%
  mutate(Date = as.Date(date), date = NULL)


#### New Cases and Deaths downloaded from the github repository of the 
#### Johns Hopkins University

url_jhu_deaths <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
url_jhu_cases <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"

new_deaths <- read.csv(url_jhu_deaths)
new_cases <- read.csv(url_jhu_cases)

# change format of dataset from wide to long -> column names into Date variable
names(new_deaths)  <- sub('^X', '', names(new_deaths))
names(new_cases)  <- sub('^X', '', names(new_cases))


new_deaths <- new_deaths %>%
  pivot_longer(cols = colnames(new_deaths)[5:ncol(new_deaths)], names_to = "Date", 
               values_to = "Daily Deaths") %>%
  mutate(Date = as.Date(strptime(Date, "%m.%d.%y")))

new_cases <- new_cases %>%
  pivot_longer(cols = colnames(new_cases)[5:ncol(new_cases)], names_to = "Date", 
               values_to = "Daily Cases") %>%
  mutate(Date = as.Date(strptime(Date, "%m.%d.%y")))







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
policy <- read.csv(url_ox)

str(policy)

policy <- policy %>% 
  mutate(Date = as.Date(as.character(Date), "%Y%m%d"))


#### Vaccination Data is directly downloaded from the github repository of the 
#### Our World in Data account

url_vac <- "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv"
vaccination <- read.csv(url_vac)

str(vaccination)

vaccination <- vaccination %>%
  mutate(Date = as.Date(date), date = NULL)






























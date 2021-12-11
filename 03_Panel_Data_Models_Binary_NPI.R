################ Seminar Paper - Econometrics - Winter Term 2021/2022

######## Name: Martin Scheerer
######## Student ID: 5631373

# Libraries  --------------------------------------------------------------
rm(list = ls())

###### Regression Analysis
## Panel Data with clustered std errors
## FE Model 
## FGLS-FE
## Outcome: percentage of reduction (1-lead/today cases) --> percentage reduction

if (!require("tidyverse")) install.packages("tidyverse")
if (!require("dplyr")) install.packages("dplyr")
if (!require("plyr")) install.packages("plyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("plm")) install.packages("plm")
if (!require("stringr")) install.packages("stringr")
if (!require("sandwich")) install.packages("sandwich")
if (!require("lmtest")) install.packages("lmtest")

library(tidyverse)
library(dplyr)
library(plyr)
library(ggplot2)
library(plm)
library(stringr)
library(sandwich)
library(lmtest)

#### Load Data

cross_sec <- readRDS("Output/cross_section_data.rds")
panel <- readRDS("Output/panel_data.rds")

#### Cross Sectional Analysis ------------------------------------------------

## Static regression to obtain effects mainly for financial interventions




#### Panel Data Models -------------------------------------------------------

## Dynamic models for estimating effects of different measures
## Create factor variable: first create factor for hemisphere and then
## create interaction of month to obtain monthly effect for different hemisphere
## two different data frames for fixed effects vs. pooled ols
## First: create pooled ols data then remove time-invariant variables
## to obtain prepared data for FE-regression

index_vars <- colnames(panel)[str_detect(colnames(panel), "Index")]
rem_vars <- c(index_vars, "CountryCode", "pop_per_one_hundred_k", "pop_per_10_million")

panel <- panel %>%
  select(-all_of(rem_vars))

panel$hemisphere_month <- as.factor(paste(panel$Month, panel$hemisphere))

## Adjust column names

col_change_ind <- which(str_detect(colnames(panel), "^[[:alpha:]]+[[:digit:]]+_"))

colnames(panel)[col_change_ind] <- str_to_title(str_replace_all(str_sub(colnames(panel)[col_change_ind], 4), "\\.", " "))

colnames(panel) <- str_to_title(str_replace_all(colnames(panel), "_", " "))

colnames(panel) <- mapvalues(colnames(panel), from = c("Kw", "Gdp Per Capita Usd"), 
                             to = c("KW", "GDP Per Capita USD"))

## Adjust column data types

factor_vars <- c("Country", "Month", "Hemisphere", "Continent", "KW", "Year")

panel <- panel %>%
  ungroup() %>%
  mutate_at(all_of(factor_vars), as.factor) %>%
  select(-`Vaccination Policy`, -KW, -Year)

## Create FE Data

fe_col_rem <- which(colnames(panel) == "GDP Per Capita USD")
fe_col_end <- which(colnames(panel) == "Hemisphere Month") 
FE_panel <- panel[, -fe_col_rem:-fe_col_end] 

## Create Binary variables: measure required
## Define at which level a variable means required

two_max <- c("C3_Cancel.public.events", "C5_Close.public.transport", "C7_Restrictions.on.internal.movement", 
             "H1_Public.information.campaigns", "H3_Contact.tracing")
two_max <- str_to_title(str_replace_all(str_sub(two_max, 4), "\\.", " "))

three_max <- c("C1_School.closing", "C2_Workplace.closing", "C6_Stay.at.home.requirements", 
               "H2_Testing.policy", "H8_Protection.of.elderly.people")
three_max <- str_to_title(str_replace_all(str_sub(three_max, 4), "\\.", " "))

four_max <- c("C4_Restrictions.on.gatherings", "C8_International.travel.controls", 
              "H6_Facial.Coverings")
four_max <- str_to_title(str_replace_all(str_sub(four_max, 4), "\\.", " "))


FE_panel[two_max] <- as.data.frame(lapply(FE_panel[two_max], 
               function(x) ifelse(x == 2, 1, 0)))

FE_panel[three_max] <- as.data.frame(lapply(FE_panel[three_max], 
                                          function(x) ifelse(x >= 2, 1, 0)))

FE_panel[four_max] <- as.data.frame(lapply(FE_panel[four_max], 
                                          function(x) ifelse(x >= 2, 1, 0)))

npi_vars <- c(two_max, three_max, four_max)

FE_panel <- FE_panel %>%
  mutate_at(all_of(npi_vars), as.factor) 


#### FE regression - four regressions for both, lead cases and lead deaths 
## Resulting in two tables per method: effects on lead cases and lead deaths

## One Lead Cases

fe_regression <- lm(`Four Week Lead Cases` ~ ., data = FE_panel[, c(-3:-5, -7:-11, -24:-25)])

summary(fe_regression)
fe_cl <- vcovCL(fe_regression, cluster = ~Country)

coeftest(fe_regression, vcov = fe_cl)




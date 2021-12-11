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
  mutate(Time = paste(Year, KW)) %>%
  select(-`Vaccination Policy`, -KW, -Year, -`People Vaccinated Per Hundred`)

## Create FE Data

fe_col_rem <- which(colnames(panel) == "GDP Per Capita USD")
fe_col_end <- which(colnames(panel) == "Hemisphere") - 1 
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

npi_vars <- c(two_max, three_max, four_max, "Time")

FE_panel <- FE_panel %>%
  mutate_at(all_of(npi_vars), as.factor) %>%
  mutate(Month = paste(Month, Hemisphere)) %>% 
  select(-Hemisphere, -`Hemisphere Month`)

colnames(FE_panel)[12] <- "Month_Hemisphere"

colnames(FE_panel) <- str_replace_all(colnames(FE_panel), " ", "_")

# FE regression  -----------------------------------------------------------
## Four regressions for both, lead cases and lead deaths
## Resulting in two tables per method: effects on lead cases and lead deaths

### Cases
## One Lead Cases

fe_one_lead_cases <- lm(One_Week_Lead_Cases ~ ., data = FE_panel[, c(-4:-11, -24:-25)])

summary(fe_one_lead_cases)
fe_cl_one_cases <- vcovCL(fe_one_lead_cases, cluster = ~Country)
coeftest(fe_one_lead_cases, vcov = fe_cl_one_cases)

## Two Lead Cases

fe_two_lead_cases <- lm(Two_Week_Lead_Cases ~ ., data = FE_panel[, c(-3, -5:-11, -24:-25)])

summary(fe_two_lead_cases)
fe_cl_two_cases <- vcovCL(fe_two_lead_cases, cluster = ~Country)
coeftest(fe_two_lead_cases, vcov = fe_cl_two_cases)

## Three Lead Cases

fe_three_lead_cases <- lm(Three_Week_Lead_Cases ~ ., data = FE_panel[, c(-3:-4, -6:-11, -24:-25)])

summary(fe_three_lead_cases)
fe_cl_three_cases <- vcovCL(fe_three_lead_cases, cluster = ~Country)
coeftest(fe_three_lead_cases, vcov = fe_cl_three_cases)

## Four Lead Cases

fe_four_lead_cases <- lm(Four_Week_Lead_Cases ~ ., data = FE_panel[, c(-3:-5, -7:-11, -24:-25)])

summary(fe_four_lead_cases)
fe_cl_four_cases <- vcovCL(fe_four_lead_cases, cluster = ~Country)
coeftest(fe_four_lead_cases, vcov = fe_cl_four_cases)


### Deaths
## One Lead Deaths

fe_one_lead_deaths <- lm(One_Week_Lead_Deaths ~ ., data = FE_panel[, c(-2:-6, -9:-11, -24:-25)])

summary(fe_one_lead_deaths)
fe_cl_one_deaths <- vcovCL(fe_one_lead_deaths, cluster = ~Country)
coeftest(fe_one_lead_deaths, vcov = fe_cl_one_deaths)

## Two Lead Deaths

fe_two_lead_deaths <- lm(Two_Week_Lead_Deaths ~ ., data = FE_panel[, c(-2:-6, -8, -10:-11, -24:-25)])

summary(fe_two_lead_deaths)
fe_cl_two_deaths <- vcovCL(fe_two_lead_deaths, cluster = ~Country)
coeftest(fe_two_lead_deaths, vcov = fe_cl_two_deaths)

## Three Lead Deaths

fe_three_lead_deaths <- lm(Three_Week_Lead_Deaths ~ ., data = FE_panel[, c(-2:-6, -8:-9, -11, -24:-25)])

summary(fe_three_lead_deaths)
fe_cl_three_deaths <- vcovCL(fe_three_lead_deaths, cluster = ~Country)
coeftest(fe_three_lead_deaths, vcov = fe_cl_three_deaths)

## Four Lead Deaths

fe_four_lead_deaths <- lm(Four_Week_Lead_Deaths ~ ., data = FE_panel[, c(-2:-6, -8:-10, -24:-25)])

summary(fe_four_lead_deaths)
fe_cl_four_deaths <- vcovCL(fe_four_lead_deaths, cluster = ~Country)
coeftest(fe_four_lead_deaths, vcov = fe_cl_four_deaths)


# FGLS FE Regression ------------------------------------------------------
## Create Subs of data to insert formula into plm 
one_lead_case <- FE_panel[, c(-4:-11, -24:-25)]
two_lead_case <- FE_panel[, c(-3, -5:-11, -24:-25)]
three_lead_case <- FE_panel[, c(-3:-4, -6:-11, -24:-25)]
four_lead_case <- FE_panel[, c(-3:-5, -7:-11, -24:-25)]

one_lead_death <- FE_panel[, c(-2:-6, -9:-11, -24:-25)]
two_lead_death <- FE_panel[, c(-2:-6, -8, -10:-11, -24:-25)]
three_lead_death <- FE_panel[, c(-2:-6, -8:-9, -11, -24:-25)]
four_lead_death <- FE_panel[, c(-2:-6, -8:-10, -24:-25)]

## Create formula
# Cases

varnames <- names(one_lead_case)[c(-3, -4, -19)]
exp <- paste0(varnames,collapse='+')
formula_1c <- paste0(names(one_lead_case[3]), "~", exp)

varnames <- names(two_lead_case)[-3, -4, -19]
exp <- paste0(varnames,collapse='+')
formula_2c <- paste0(names(two_lead_case[3]), "~", exp)

varnames <- names(three_lead_case)[-3]
exp <- paste0(varnames,collapse='+')
formula_3c <- paste0(names(three_lead_case[3]), "~", exp)

varnames <- names(four_lead_case)[-3]
exp <- paste0(varnames,collapse='+')
formula_4c <- paste0(names(four_lead_case[3]), "~", exp)

# Deaths

varnames <- names(one_lead_death)[-3]
exp <- paste0(varnames,collapse='+')
formula_1d <- paste0(names(one_lead_death[3]), "~", exp)

varnames <- names(two_lead_death)[-3]
exp <- paste0(varnames,collapse='+')
formula_2d <- paste0(names(two_lead_death[3]), "~", exp)

varnames <- names(three_lead_death)[-3]
exp <- paste0(varnames,collapse='+')
formula_3d <- paste0(names(three_lead_death[3]), "~", exp)

varnames <- names(four_lead_death)[-3]
exp <- paste0(varnames,collapse='+')
formula_4d <- paste0(names(four_lead_death[3]), "~", exp)

## FGLS Models

fgls_one_lead_case <- pggls(formula_1c, data = one_lead_case, effect = "individual",
      model = "within", index = "Country")

summary(fgls_one_lead_case)

fgls_two_lead_case <- pggls(formula_2c, data = two_lead_case, effect = "individual",
                            model = "within", index = "Country")

summary(fgls_two_lead_case)












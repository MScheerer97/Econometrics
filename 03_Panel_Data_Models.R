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
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("plm")) install.packages("plm")

library(tidyverse)
library(dplyr)
library(ggplot2)
library(plm)

#### Load Data

cross_sec <- readRDS("Output/cross_section_data.rds")
panel <- readRDS("Output/panel_data.rds")



#### Cross Sectional Analysis ------------------------------------------------

## Static regression to obtain effects mainly for financial interventions




#### Panel Data Models -------------------------------------------------------

## Dynamic models for estimating effects of different measures
## Create factor variable: first create factor for hemisphere (1 northern, 0 southern)
## create interaction of month to obtain monthly effect for different hemisphere
## 













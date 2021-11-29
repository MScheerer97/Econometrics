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
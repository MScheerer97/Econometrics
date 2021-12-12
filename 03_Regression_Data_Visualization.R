################ Seminar Paper - Econometrics - Winter Term 2021/2022

######## Name: Martin Scheerer
######## Student ID: 5631373

# Libraries  --------------------------------------------------------------
rm(list = ls())

###### Data Visualization: 

if (!require("tidyverse")) install.packages("tidyverse")
if (!require("plyr")) install.packages("plyr")
if (!require("dplyr")) install.packages("dplyr")
if (!require("knitr")) install.packages("knitr")
if (!require("kableExtra")) install.packages("kableExtra")
if (!require("webshot")) install.packages("webshot")
if (!require("stringr")) install.packages("stringr")
if (!require("stringi")) install.packages("stringi")
if (!require("Hmisc")) install.packages("Hmisc")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("viridis")) install.packages("viridis")
if (!require("ggrepel")) install.packages("ggrepel")
if (!require("pastecs")) install.packages("pastecs")
if (!require("magick")) install.packages("magick")
if (!require("lubridate")) install.packages("lubridate")
if (!require("stargazer")) install.packages("stargazer")

library(tidyverse)
library(plyr)
library(dplyr)
library(knitr)
library(kableExtra)
library(webshot)
library(stringr)
library(stringi)
library(Hmisc)
library(ggplot2)
library(viridis)
library(ggrepel)
library(pastecs)
library(magick)
library(lubridate)
library(stargazer)

data <- readRDS("Output/Regression_Data.rds") %>%
  ungroup()

# Regression Data Descriptives --------------------------------------------

## Histograms for Cases and Deaths: log needed? 

cases_histo <- ggplot(data, aes(x = Weekly_Average_Cases)) +
  geom_histogram(fill = "grey", position  = "dodge") + 
  labs(x = "Weekly average cases", y = "Count") +
  theme_classic()


deaths_histo <- ggplot(data, aes(x = Weekly_Average_Deaths)) +
  geom_histogram(fill = "grey", position = "dodge")+
  labs(x = "Weekly average deaths", y = "Count") +
  theme_classic()

## Descriptive Stats for npis:

data[, 13:23] <- lapply(data[, 13:23], as.numeric) 
data[, 13:23] <- data[, 13:23] -1

npi_reg <- stat.desc(data[, 13:23])
npi_reg <- npi_reg[c("mean", "median", "std.dev"), ]
rownames(npi_reg) <- c("Mean", "Median", "Standard Deviation")

kbl(t(npi_reg)) %>%
  kable_styling(bootstrap_options = "responsive", position = "left", 
                html_font = "times", font_size = 12) %>%
  kable_classic() %>%
  kable_styling(full_width = F) %>%
  row_spec(0, bold = TRUE) %>%
  save_kable(file = "figures/binary_npi_regression.png", zoom = 1.25)

# Regression Tables -------------------------------------------------------
































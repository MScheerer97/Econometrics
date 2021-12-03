################ Seminar Paper - Econometrics - Winter Term 2021/2022

######## Name: Martin Scheerer
######## Student ID: 5631373

# Libraries  --------------------------------------------------------------
rm(list = ls())

###### Data Visualization and Descriptive Statistics
## Table: Country - Total Cases/Deaths
## Tables: npi (e.g. school closed on x days) - Country
## Vis: Scatterplot Cases - Deaths (Points=Country)
## Vis: Total Cases/Deaths in world - time 
## Vis: Total Cases/Deaths - World map
## Vis: Weekly Cases/Deaths - Time (for ALL countries in ONE graph/or for selected ones)
## Corr_Table: npi days - cum cases/deaths

if (!require("tidyverse")) install.packages("tidyverse")
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

library(tidyverse)
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

# Descriptive Statistics - Cross Section --------------------------------------

#### Total Deaths and Cases per Country: Four tables, each 10 observations

cross_sec <- readRDS("Output/cross_section_data.rds") 

cases_deaths_1 <- cross_sec[1:20, ] %>%
  select(Country, Cumulative_cases, Cumulative_deaths, pop_per_10_million, pop_per_one_hundred_k) %>%
  mutate("Cases per 10 Million" = round(Cumulative_cases/pop_per_10_million, 2), 
         "Deaths per 100 Thousand" = round(Cumulative_deaths/pop_per_one_hundred_k, 2)) %>%
  select(Country, "Cases per 10 Million", "Deaths per 100 Thousand") 

colnames(cases_deaths_1) <- str_to_title(colnames(cases_deaths_1))
  
cases_deaths_2 <- cross_sec[21:40, ] %>%
  select(Country, Cumulative_cases, Cumulative_deaths, pop_per_10_million, pop_per_one_hundred_k) %>%
  mutate("Cases per 10 Million" = round(Cumulative_cases/pop_per_10_million, 2), 
         "Deaths per 100 Thousand" = round(Cumulative_deaths/pop_per_one_hundred_k, 2)) %>%
  select(Country, "Cases per 10 Million", "Deaths per 100 Thousand") 

colnames(cases_deaths_2) <- str_to_title(colnames(cases_deaths_2))

tabs <- list(cases_deaths_1, cases_deaths_2)
 
for(i in 1:length(tabs)){
  kbl(tabs[[i]]) %>%
    kable_styling(bootstrap_options = "responsive", position = "left", 
                  html_font = "times", font_size = 12) %>%
    kable_classic() %>%
    kable_styling(full_width = F) %>%
    row_spec(0, bold = TRUE) %>%
    save_kable(file = paste0("figures/cases_death_", i, ".png"), zoom = 1.25)
}


#### World Bank Indicator and CPI

wb_indicator <- cross_sec %>%
  select(Country, GDP_per_Capita_USD, Population_Over_65, Population_Density, Gini, 
         Hospital_Beds, `CPI score 2020`, Total_Population) 
colnames(wb_indicator)[c(2, 3, 4, 5, 6, 8)] <- c("GDP Per Capita USD", "Population Over 65", 
                                                 "Population Density", "Gini Index", 
                                                 "Hospital Beds", "Total Population")

wb_1 <- wb_indicator[1:20, ] 

wb_2 <- wb_indicator[21:40, ] 

tabs <- list(wb_1, wb_2)

for(i in 1:length(tabs)){
  kbl(tabs[[i]]) %>%
    kable_styling(bootstrap_options = "responsive", position = "left", 
                  html_font = "times", font_size = 12) %>%
    kable_classic() %>%
    kable_styling(full_width = F) %>%
    row_spec(0, bold = TRUE) %>%
    save_kable(file = paste0("figures/wb_indicators", i, ".png"), zoom = 1.25)
}

#### Measures Data 

cols <- colnames(cross_sec)[c(1, 4:18)]
measures <- cross_sec %>%
  select(all_of(cols)) %>%
  mutate(H4_Emergency.investment.in.healthcare = round(H4_Emergency.investment.in.healthcare/100000, 2), 
         H5_Investment.in.vaccines = round(H5_Investment.in.vaccines/100000, 2))

colnames(measures)[2:16] <- str_to_title(str_replace_all(str_sub(cols[-1], 4), "\\.", " "))
colnames(measures)[15:16] <- paste(colnames(measures)[15:16], "100 Thousand USD")

measures_1 <- measures[1:20, ] 

measures_2 <- measures[21:40, ] 

tabs <- list(measures_1, measures_2)

for(i in 1:length(tabs)){
  kbl(tabs[[i]]) %>%
    kable_styling(bootstrap_options = "responsive", position = "left", 
                  html_font = "times", font_size = 12) %>%
    kable_classic() %>%
    kable_styling(full_width = T, position = "center") %>%
    row_spec(0, bold = TRUE) %>%
    save_kable(file = paste0("figures/npi_", i, ".png"), zoom = 1.25)
}

#### Vaccination Speed/Development 

vaccination <- cross_sec[, c(1, 19:28)] 
colnames(vaccination)[2:11] <- str_to_title(str_replace_all(colnames(vaccination)[-1], "\\_", " "))

vac_1 <- vaccination[1:20, ] 

vac_2 <- vaccination[21:40, ] 

tabs <- list(vac_1, vac_2)

for(i in 1:length(tabs)){
  kbl(tabs[[i]]) %>%
    kable_styling(bootstrap_options = "responsive", position = "left", 
                  html_font = "times", font_size = 12) %>%
    kable_classic() %>%
    kable_styling(full_width = F) %>%
    row_spec(0, bold = TRUE) %>%
    save_kable(file = paste0("figures/vac", i, ".png"), zoom = 1.25)
}

#### Correlation Overview - Number of days measure required vs. total cases/deaths

cor_data <- cross_sec %>%
  mutate("Cases Per 10 Million" = round(Cumulative_cases/pop_per_10_million, 2), 
         "Deaths Per 100 Thousand" = round(Cumulative_deaths/pop_per_one_hundred_k, 2)) 
  

corr <- rcorr(as.matrix(cor_data[c(40:41, 4:18)]), type = "pearson")
cor_vals <- round(as.data.frame(corr$r)[-1:-2, 1:2], 2)
p_vals <- round(as.data.frame(corr$P)[-1:-2, 1:2], 2)

cor_tab <- cbind(cor_vals, p_vals)

cor_tab_cases <- cor_tab[, c(-2, -4)]
colnames(cor_tab_cases)[2] <- ("p-Value")
rownames(cor_tab_cases) <- str_to_title(str_replace_all(str_sub(rownames(cor_tab_cases), 4), "\\.", " "))
  
cor_tab_deaths <- cor_tab[, c(-1, -3)]
colnames(cor_tab_deaths)[2] <- ("p-Value")
rownames(cor_tab_deaths) <- str_to_title(str_replace_all(str_sub(rownames(cor_tab_deaths), 4), "\\.", " "))
  
  
tabs <- list(cor_tab_cases, cor_tab_deaths)

for(i in 1:length(tabs)){
  kbl(tabs[[i]]) %>%
    kable_styling(bootstrap_options = "responsive", position = "left", 
                  html_font = "times", font_size = 12) %>%
    kable_classic() %>%
    kable_styling(full_width = F) %>%
    row_spec(0, bold = TRUE) %>%
    save_kable(file = paste0("figures/static_correlations", i, ".png"), zoom = 1.25)
}
  
#### Scatterplots
## Take cor_data because cases and deaths already computed

continents <- readRDS("Data/continent.rds")
## merge cor_data and continent

cor_cont <- inner_join(cor_data, continents, by = "Country")

cases_deaths_plot <- ggplot(cor_cont, aes(`Cases Per 10 Million`, `Deaths Per 100 Thousand`)) +
  geom_point(size = 8, aes(colour = Continent)) + 
  scale_color_viridis_d(alpha = 0.65) +
  geom_smooth(method = "lm", se = FALSE) + 
  geom_text_repel(aes(label = Country), angle = 20, nudge_x = ifelse(cor_cont$Country == "Japan", 2, 0)) +
  scale_x_continuous(breaks = seq(0, 3000000, 500000)) + 
  scale_y_continuous(breaks = seq(0, 400, 50)) + 
  labs(x = "Total cases per 10 million population", y = "Total deaths per 100 thousand population") +
  theme_classic() +
  theme(legend.position = c(0.9, 0.5)) +
  guides(colour = guide_legend(title = "Continent\n", title.position = "top"), 
         label.hjust = 0.5) 

ggsave("figures/deaths_cases_scatterplot.png", width = 14)  
  
#### Measure Days Statistics

measure_desc <- round(stat.desc(measures[, -1]), 2)
measure_desc <- measure_desc[c("mean", "median", "std.dev"), ]
rownames(measure_desc) <- c("Mean", "Median", "Standard Deviation")

kbl(measure_desc) %>%
  kable_styling(bootstrap_options = "responsive", position = "left", 
                html_font = "times", font_size = 12) %>%
  kable_classic() %>%
  kable_styling(full_width = F) %>%
  row_spec(0, bold = TRUE) %>%
  save_kable(file = "figures/measures_days_descriptives.png", zoom = 1.25)

#### Barplot - Measure Days Mean Values

measure_bar <- data.frame(t(measure_desc)) 
measure_bar$Measure <- rownames(measure_bar)

rownames(measure_bar) <- NULL
measure_bar <- measure_bar[-c(14:15), ]

measure_barplot <- ggplot(measure_bar, aes(x = reorder(Measure, Mean), y = Mean)) +
  geom_bar(stat = "identity", fill = "grey") + 
  labs(x = "Non-pharmaceutical intervention", y = "Average number of days measure was required") +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, 650, 100)) +
  theme_classic() +
  theme(axis.title.x = element_text(vjust = -1), axis.title.y = element_text(vjust = 2))

ggsave("figures/measure_days_barplot.png")  

# Descriptive Statistics - Panel Data -------------------------------------

panel <- readRDS("Output/panel_data.rds") 

#### 




























################ Seminar Paper - Econometrics - Winter Term 2021/2022

######## Name: Martin Scheerer
######## Student ID: 5631373

# Libraries  --------------------------------------------------------------
rm(list = ls())

###### Regression Analysis: 1st January 2021 to KW 44
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
if (!require("stargazer")) install.packages("stargazer")
if (!require("rqpd")) install.packages("rqpd", repos = "http://R-Forge.R-project.org")
if (!require("knitr")) install.packages("knitr")
if (!require("kableExtra")) install.packages("kableExtra")

library(tidyverse)
library(dplyr)
library(plyr)
library(ggplot2)
library(plm)
library(stringr)
library(sandwich)
library(lmtest)
library(stargazer)
library(rqpd)
library(knitr)
library(kableExtra)


#### Load Data

cross_sec <- readRDS("Output/cross_section_data.rds")
panel <- readRDS("Output/panel_data.rds")

#### Cross Sectional Analysis ------------------------------------------------

## Static regression to obtain effects mainly for financial interventions

cross_sec <- cross_sec %>%
  mutate(Cumulative_cases = Cumulative_cases/pop_per_10_million, 
         Cumulative_deaths = Cumulative_deaths/pop_per_one_hundred_k) %>%
  select(-hemisphere)

cross_sec <- cross_sec[, -c(1, 15:16, 19:27, 34:38)]
col_change_ind <- which(str_detect(colnames(cross_sec), "^[[:alpha:]]+[[:digit:]]+_"))

colnames(cross_sec)[col_change_ind] <- str_to_title(str_replace_all(str_sub(colnames(cross_sec)[col_change_ind], 4), "\\.", " "))

colnames(cross_sec) <- str_to_title(str_replace_all(colnames(cross_sec), "_", " "))
colnames(cross_sec) <- mapvalues(colnames(cross_sec), from = c("Gdp Per Capita Usd"), 
                             to = c("GDP_Per_Capita_USD"))

colnames(cross_sec) <- str_replace_all(colnames(cross_sec), " ", "_")

cross_sec$GDP_Per_Capita_USD <- log(cross_sec$GDP_Per_Capita_USD)
cross_sec$Population_Over_65 <- log(cross_sec$Population_Over_65)

## Linear Regression - cross-sectional analysis

cross_vars <- str_replace_all(colnames(cross_sec[, -3:-13]), "_", " ")[-1:-2]

cross_lm_cases <- lm(Cumulative_Cases~. - Cumulative_Deaths, data = cross_sec[, -3:-13])
coeftest(cross_lm_cases, vcov = vcovHC(cross_lm_cases, type = "HC0"))

cross_lm_deaths <- lm(Cumulative_Deaths~. -Cumulative_Cases, data = cross_sec[, -3:-13])
coeftest(cross_lm_deaths, vcov = vcovHC(cross_lm_deaths, type = "HC0"))

stargazer(cross_lm_cases, cross_lm_deaths, 
          out = "figures/lm_models.html", covariate.labels = cross_vars, align = TRUE,
          column.labels = c("Cases", "Deaths"), 
          star.char = c("*", "**", "***"), star.cutoffs = c(.1, .05, .01), 
          omit.stat = c("rsq", "ser"), 
          add.lines = list(c("Robust standard errors", rep("Yes", 2))),
          type = "html", font.size = "small", dep.var.labels.include = FALSE, 
          column.sep.width = "10pt")


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
  select(-`Vaccination Policy`, -`People Vaccinated Per Hundred`) %>%
  filter(Year == "2021") %>%
  select(-Year)


## Create Binary variables: measure required analog to Chen et. al ADB Economics
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


panel[two_max] <- as.data.frame(lapply(panel[two_max], 
               function(x) ifelse(x == 2, 1, 0)))

panel[three_max] <- as.data.frame(lapply(panel[three_max], 
                                          function(x) ifelse(x >= 2, 1, 0)))

panel[four_max] <- as.data.frame(lapply(panel[four_max], 
                                          function(x) ifelse(x >= 2, 1, 0)))

npi_vars <- c(two_max, three_max, four_max)

panel <- panel %>%
  mutate_at(all_of(npi_vars), as.factor)

colnames(panel) <- str_replace_all(colnames(panel), " ", "_")

## Create FE Data

fe_col_rem <- which(colnames(panel) == "GDP_Per_Capita_USD")
fe_col_end <- which(colnames(panel) == "Hemisphere") - 1 
FE_panel <- panel[, -fe_col_rem:-fe_col_end] 

## Variables according to Carraro et al.
## transform cases to log

FE_panel <- FE_panel %>%
  select(-Protection_Of_Elderly_People, -Hemisphere, -Month) 

logs <- colnames(FE_panel)[str_detect(colnames(FE_panel), "Cases|Deaths")]
FE_panel[, logs] <- FE_panel[, logs] + 0.0000001

FE_panel <- FE_panel %>%
  mutate_at(all_of(logs), log)

saveRDS(FE_panel, "Output/Regression_Data.rds")

# FE Regression  ------------------------------------------------------
## Create Subs of data to insert formula into plm 

one_lead_case <- pdata.frame(FE_panel[, (-5:-12)], index = c("Country", "KW"), 
                               drop.index = F, row.names = T)

two_lead_case <- pdata.frame(FE_panel[, c(-4, -6:-12)], index = c("Country", "KW"), 
                               drop.index = F, row.names = T)

three_lead_case <- pdata.frame(FE_panel[, c(-4:-5, -7:-12)], index = c("Country", "KW"), 
                               drop.index = F, row.names = T)

four_lead_case <- pdata.frame(FE_panel[, c(-4:-6, -8:-12)], index = c("Country", "KW"), 
                               drop.index = F, row.names = T)


one_lead_death <- pdata.frame(FE_panel[, c(-4:-7, -10:-12)], index = c("Country", "KW"), 
                               drop.index = F, row.names = T) 

two_lead_death <- pdata.frame(FE_panel[, c(-4:-7, -9, -11:-12)], index = c("Country", "KW"), 
                               drop.index = F, row.names = T)

three_lead_death <- pdata.frame(FE_panel[, c(-4:-7, -9:-10, -12)], index = c("Country", "KW"), 
                               drop.index = F, row.names = T)

four_lead_death <- pdata.frame(FE_panel[, c(-4:-7, -9:-11)], index = c("Country", "KW"), 
                               drop.index = F, row.names = T)

## Create formula for FE
# Cases

varnames <- names(one_lead_case)[c(-1:-2, -4)]
exp <- paste0(varnames,collapse='+')
formula_1c <- paste0(names(one_lead_case[4]), "~", exp)

varnames <- names(two_lead_case)[c(-1:-2, -4)]
exp <- paste0(varnames,collapse='+')
formula_2c <- paste0(names(two_lead_case[4]), "~", exp)

varnames <- names(three_lead_case)[c(-1:-2, -4)]
exp <- paste0(varnames,collapse='+')
formula_3c <- paste0(names(three_lead_case[4]), "~", exp)

varnames <- names(four_lead_case)[c(-1:-2, -4)]
exp <- paste0(varnames,collapse='+')
formula_4c <- paste0(names(four_lead_case[4]), "~", exp)

# Deaths

# varnames <- names(one_lead_death)[c(-1:-2, -5)]
# exp <- paste0(varnames,collapse='+')
# formula_1d <- paste0(names(one_lead_death[5]), "~", exp)
# 
# varnames <- names(two_lead_death)[c(-1:-2, -5)]
# exp <- paste0(varnames,collapse='+')
# formula_2d <- paste0(names(two_lead_death[5]), "~", exp)

varnames <- names(three_lead_death)[c(-1:-2, -5)]
exp <- paste0(varnames,collapse='+')
formula_3d <- paste0(names(three_lead_death[5]), "~", exp)

varnames <- names(four_lead_death)[c(-1:-2, -5)]
exp <- paste0(varnames,collapse='+')
formula_4d <- paste0(names(four_lead_death[5]), "~", exp)

### Models with clustered std. errors

## Cases

fe_one_case <- plm(formula_1c, data = one_lead_case, model="within")
coef <- coeftest(fe_one_case, vcov = vcovHC(fe_one_case, method = "white2", cluster = "group"))
diag(fe_one_case$vcov) <- coef[, 2]^2

fe_two_case <- plm(formula_2c, data = two_lead_case, model="within")
coef <- coeftest(fe_two_case, vcov = vcovHC(fe_two_case, method = "white2", cluster = "group"))
diag(fe_two_case$vcov) <- coef[, 2]^2

fe_three_case <- plm(formula_3c, data = three_lead_case, model="within")
coef <- coeftest(fe_three_case, vcov = vcovHC(fe_three_case, method = "white2", cluster = "group"))
diag(fe_three_case$vcov) <- coef[, 2]^2

fe_four_case <- plm(formula_4c, data = four_lead_case, model="within")
coef <- coeftest(fe_four_case, vcov = vcovHC(fe_four_case, method = "white2", cluster = "group"))
diag(fe_four_case$vcov) <- coef[, 2]^2


## Print Tables

var_nam <- c("log Weekly Average Cases", str_remove_all(str_replace_all(rownames(coef)[-1], "_", " "), 
                                                        "[[:digit:]]"))

stargazer(fe_one_case, fe_two_case, fe_three_case, fe_four_case, 
          out = "figures/fe_cases.html", covariate.labels = var_nam, align = TRUE,
          column.labels = c("k = 1", "k = 2", "k = 3", "k = 4"), 
          star.char = c("*", "**", "***"), star.cutoffs = c(.1, .05, .01), 
          omit.stat = c("rsq", "f"), add.lines = list(c("Country fixed effects", rep("Yes", 4)), 
                                                      c("Clustered standard errors", rep("Yes", 4))),
          type = "html", omit = 13, font.size = "small", dep.var.labels.include = FALSE, 
          column.sep.width = "10pt")
          
## Deaths

# fe_one_death <- plm(formula_1d, data = one_lead_death, model="within")
# coef <- coeftest(fe_one_death, vcov = vcovHC(fe_one_death, method = "white2", cluster = "group"))
# diag(fe_one_death$vcov) <- coef[, 2]^2
# 
# fe_two_death <- plm(formula_2d, data = two_lead_death, model="within")
# coef <- coeftest(fe_two_death, vcov = vcovHC(fe_two_death, method = "white2", cluster = "group"))
# diag(fe_two_death$vcov) <- coef[, 2]^2

fe_three_death <- plm(formula_3d, data = three_lead_death, model="within")
coef <- coeftest(fe_three_death, vcov = vcovHC(fe_three_death, method = "white2", cluster = "group"))
diag(fe_three_death$vcov) <- coef[, 2]^2

fe_four_death <- plm(formula_4d, data = four_lead_death, model="within")
coef <- coeftest(fe_four_death, vcov = vcovHC(fe_four_death, method = "white2", cluster = "group"))
diag(fe_four_death$vcov) <- coef[, 2]^2

var_nam_death <- var_nam <- c("log Weekly Average Cases",  "log Weekly Average Deaths", 
                              str_remove_all(str_replace_all(rownames(coef)[-1:-2], "_", " "), 
                                                                         "[[:digit:]]"))

stargazer(fe_three_death, fe_four_death, 
          out = "figures/fe_deaths.html", covariate.labels = var_nam_death, align = TRUE,
          column.labels = c("k = 3", "k = 4"), 
          star.char = c("*", "**", "***"), star.cutoffs = c(.1, .05, .01), 
          omit.stat = c("rsq", "f"), add.lines = list(c("Country fixed effects", rep("Yes", 4)), 
                                                      c("Clustered standard errors", rep("Yes", 4))),
          type = "html", omit = 14, font.size = "small", dep.var.labels.include = FALSE, 
          column.sep.width = "1pt")


# # FGLS Fixed Effects  ----------------------------------------------------------
# 
# ## Cases
# 
# fgls_one_case <- pggls(formula_1c, data = one_lead_case, model = "within", effect = "individual", 
#                        index = c("Country", "KW"))
# summary(fgls_one_case)
# 
# fgls_two_case <- pggls(formula_2c, data = two_lead_case, model = "within", effect = "individual")
# summary(fgls_two_case)
# 
# fgls_three_case <- pggls(formula_3c, data = three_lead_case, model = "within", effect = "individual")
# summary(fgls_three_case)
# 
# fgls_four_case <- pggls(formula_4c, data = four_lead_case, model = "within", effect = "individual")
# summary(fgls_four_case)
# 
# ## Deaths
# 
# fgls_one_death <- pggls(formula_1d, data = one_lead_death, model = "within", effect = "individual")
# summary(fgls_one_death)
# 
# fgls_two_death <- pggls(formula_2d, data = two_lead_death, model = "within", effect = "individual")
# summary(fgls_two_death)
# 
# fgls_three_death <- pggls(formula_3d, data = three_lead_death, model = "within", effect = "individual")
# summary(fgls_three_death)
# 
# fgls_four_death <- pggls(formula_4d, data = four_lead_death, model = "within", effect = "individual")
# summary(fgls_four_death)

#### Hausman Test: Fixed or Random?

re_one_case <- plm(formula_1c, data = one_lead_case, model = "random")
pht <- phtest(fe_one_case, re_one_case)

hausman <- round(data.frame(chisq = pht$statistic, df = pht$parameter, pvalue = pht$p.value), 3)
rownames(hausman) <- NULL
colnames(hausman) <- c("Chi-square statistic", "Degree of freedoms", "p-Value")

kbl(hausman) %>%
  kable_styling(bootstrap_options = "responsive", position = "left", 
                html_font = "times", font_size = 12) %>% 
  add_header_above(c("Hausman Test" = 3)) %>%
  footnote(general = "one model is inconsistent", 
                    general_title = "Alternative hypothesis: ",
                    footnote_as_chunk = T, title_format = "italic") %>%
  kable_classic() %>%
  kable_styling(full_width = F) %>%
  row_spec(0, bold = TRUE) %>%
  save_kable(file = "figures/hausman_test_FE.png", zoom = 1.25)

#### --> Hausman Test Result: Fixed effects!

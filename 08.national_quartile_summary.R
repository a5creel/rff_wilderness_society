# National Review / A.M. Creel / July 23rd, 2021
# Goal: Get National summary for quartiles 
# Required Scripts: 01.data_wrangle.R

# --- #
library(vroom)
library(tidyr)
library(stringr)
library(ggplot2)
library(gtools)
library(forcats)
options(scipen=99999)
library(janitor)
library(dplyr)
# --- #

# made in 01.data_wrangle.R
myWorking <- vroom("Datasets/clean_data/lwcf_annualPop_deccenialDemographics.csv") 

#Basic data cleaning
myWorking <- myWorking %>%
  mutate(type = str_sub(type,0,1)) %>% #getting rid of a space 
  mutate(state_fips = as.factor(str_sub(fips, 1, 2))) %>% # getting state fips code
  filter(!is.na(white_pct)) %>% # getting ride of rows where I couldn't get their demographic data
  dplyr::mutate(poc_pct = 100 - white_pct)

# working years: 1975-2018
myWorking %>%
  select(year) %>%
  distinct() %>%
  summary()










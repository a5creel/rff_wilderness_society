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
# library(priceR) # maybe use this for inflation !!!!
# --- #

# made in 01.data_wrangle.R
myWorking <- vroom("Datasets/clean_data/lwcf_annualPop_deccenialDemographics.csv") 

#Basic data cleaning
myWorking <- myWorking %>%
  mutate(type = str_sub(type,0,1)) %>% #getting rid of a space 
  mutate(state_fips = as.factor(str_sub(fips, 1, 2))) %>% # getting state fips code
  filter(!is.na(white_pct)) %>% # getting ride of rows where I couldn't get their demographic data
  dplyr::mutate(poc_pct = 100 - white_pct) 
  # mutate(real_amount = adjust_for_inflation(amount, year, "US", 2018))

# working years: 1975-2018
myWorking %>%
  select(year) %>%
  distinct() %>%
  summary()

# GETTING ALL OF THE QUARTILES -------------------------------------------------

# ATTN: Note that these are national quartiles, not state quartiles. I've still grouped 
# by decade to deal with some of the fluctuation in the amount of grant funding that was given out each year 


# Grouping outcome variables by decade (number of grants received, amount received, average amount received per capita per decade, avg # grants per 100,000 people per decade)
myWorking_decade <- myWorking %>%
  group_by(fips, merge_year) %>%
  mutate(decade_quantity = sum(got_grant)) %>%
  mutate(decade_amount = sum(amount, na.rm = TRUE)) %>%
  mutate(decade_amount_per_cap = sum(amount, na.rm = TRUE)/mean(annual_population, na.rm = TRUE)) %>%
  mutate(decade_quantity_per_cap = sum(got_grant)/mean((annual_population/100000), na.rm = TRUE)) %>%
  select(fips, merge_year, state_fips, ends_with("_pct"), starts_with("decade_"),  med_income_house) %>%
  ungroup() %>%
  distinct()

# ------- Get quartiles of quantity grants received for each decade ------- #
myQuants_quantity <- myWorking_decade %>%
  dplyr::group_by(merge_year) %>%
  dplyr::summarise(quants = list(quantile(decade_quantity, probs = c(.25, .5, .75)))) %>%
  unnest_wider(quants) %>%
  dplyr::rename(decade_quantity_25 = `25%`) %>%
  dplyr::rename(decade_quantity_50 = `50%`) %>%
  dplyr::rename(decade_quantity_75 = `75%`) %>%
  ungroup()

#joining back in
myWorking_decade <- left_join(myWorking_decade, myQuants_quantity, by = "merge_year")
rm(myQuants_quantity)

#making a factor variable for the quartiles (by state and year)
myWorking_decade <- myWorking_decade %>%
  dplyr::mutate(decade_quantity_quants = if_else(decade_quantity > decade_quantity_75, "4", "")) %>% # fourth quartile 
  dplyr::mutate(decade_quantity_quants = if_else(decade_quantity <=decade_quantity_75, "3", decade_quantity_quants)) %>% # third quartile
  dplyr::mutate(decade_quantity_quants = if_else(decade_quantity <= decade_quantity_50, "2", decade_quantity_quants)) %>% # second quartile
  dplyr::mutate(decade_quantity_quants = if_else(decade_quantity <= decade_quantity_25, "1", decade_quantity_quants)) %>% # first quartile 
  dplyr::select(-decade_quantity_75, -decade_quantity_50, - decade_quantity_25) %>%
  dplyr::mutate(decade_quantity_quants = as.factor(decade_quantity_quants))

# ------- Get quartiles of amount of grant funding for each decade ------- #
myQuants_amount <- myWorking_decade %>%
  dplyr::group_by(merge_year) %>%
  dplyr::summarise(quants = list(quantile(decade_amount, probs = c(.25, .5, .75)))) %>%
  unnest_wider(quants) %>%
  dplyr::rename(decade_amount_25 = `25%`) %>%
  dplyr::rename(decade_amount_50 = `50%`) %>%
  dplyr::rename(decade_amount_75 = `75%`)

#joining back in
myWorking_decade <- left_join(myWorking_decade, myQuants_amount, by = "merge_year")
rm(myQuants_amount)

#making a factor variable for the quartiles (by state and year)
myWorking_decade <- myWorking_decade %>%
  dplyr::mutate(decade_amount_quants = if_else(decade_amount > decade_amount_75, "4", "")) %>% # fourth quartile 
  dplyr::mutate(decade_amount_quants = if_else(decade_amount <=decade_amount_75, "3", decade_amount_quants)) %>% # third quartile
  dplyr::mutate(decade_amount_quants = if_else(decade_amount <= decade_amount_50, "2", decade_amount_quants)) %>% # second quartile
  dplyr::mutate(decade_amount_quants = if_else(decade_amount <= decade_amount_25, "1", decade_amount_quants)) %>% # first quartile 
  dplyr::select(-decade_amount_75, -decade_amount_50, - decade_amount_25) %>%
  dplyr::mutate(decade_amount_quants = as.factor(decade_amount_quants))

# ------- Get quartiles of amount of grant funding PER CAPITA for each decade ------- #
myQuants_amount_per_cap <- myWorking_decade %>%
  dplyr::group_by(merge_year) %>%
  dplyr::summarise(quants = list(quantile(decade_amount_per_cap, probs = c(.25, .5, .75), na.rm = TRUE))) %>%
  unnest_wider(quants) %>%
  dplyr::rename(decade_amount_per_cap_25 = `25%`) %>%
  dplyr::rename(decade_amount_per_cap_50 = `50%`) %>%
  dplyr::rename(decade_amount_per_cap_75 = `75%`)

#joining back in
myWorking_decade <- left_join(myWorking_decade, myQuants_amount_per_cap, by = "merge_year")
rm(myQuants_amount_per_cap)

#making a factor variable for the quartiles (by state and year)
myWorking_decade <- myWorking_decade %>%
  dplyr::mutate(decade_amount_per_cap_quants = if_else(decade_amount_per_cap > decade_amount_per_cap_75, "4", "")) %>% # fourth quartile 
  dplyr::mutate(decade_amount_per_cap_quants = if_else(decade_amount_per_cap <=decade_amount_per_cap_75, "3", decade_amount_per_cap_quants)) %>% # third quartile
  dplyr::mutate(decade_amount_per_cap_quants = if_else(decade_amount_per_cap <= decade_amount_per_cap_50, "2", decade_amount_per_cap_quants)) %>% # second quartile
  dplyr::mutate(decade_amount_per_cap_quants = if_else(decade_amount_per_cap <= decade_amount_per_cap_25, "1", decade_amount_per_cap_quants)) %>% # first quartile 
  dplyr::select(-decade_amount_per_cap_75, -decade_amount_per_cap_50, - decade_amount_per_cap_25) %>%
  dplyr::mutate(decade_amount_per_cap_quants = as.factor(decade_amount_per_cap_quants))

# ------- Get quartiles of number of grants  PER CAPITA for each decade ------- #
myQuants_quantity_per_cap <- myWorking_decade %>%
  dplyr::group_by(merge_year) %>%
  dplyr::summarise(quants = list(quantile(decade_quantity_per_cap, probs = c(.25, .5, .75), na.rm = TRUE))) %>%
  unnest_wider(quants) %>%
  dplyr::rename(decade_quantity_per_cap_25 = `25%`) %>%
  dplyr::rename(decade_quantity_per_cap_50 = `50%`) %>%
  dplyr::rename(decade_quantity_per_cap_75 = `75%`)

#joining back in
myWorking_decade <- left_join(myWorking_decade, myQuants_quantity_per_cap, by = "merge_year")
rm(myQuants_quantity_per_cap)

#making a factor variable for the quartiles (by state and year)
myWorking_decade <- myWorking_decade %>%
  dplyr::mutate(decade_quantity_per_cap_quants = if_else(decade_quantity_per_cap > decade_quantity_per_cap_75, "4", "")) %>% # fourth quartile 
  dplyr::mutate(decade_quantity_per_cap_quants = if_else(decade_quantity_per_cap <=decade_quantity_per_cap_75, "3", decade_quantity_per_cap_quants)) %>% # third quartile
  dplyr::mutate(decade_quantity_per_cap_quants = if_else(decade_quantity_per_cap <= decade_quantity_per_cap_50, "2", decade_quantity_per_cap_quants)) %>% # second quartile
  dplyr::mutate(decade_quantity_per_cap_quants = if_else(decade_quantity_per_cap <= decade_quantity_per_cap_25, "1", decade_quantity_per_cap_quants)) %>% # first quartile 
  dplyr::select(-decade_quantity_per_cap_75, -decade_quantity_per_cap_50, - decade_quantity_per_cap_25) %>%
  dplyr::mutate(decade_quantity_per_cap_quants = as.factor(decade_quantity_per_cap_quants))


# CALCULATIONG AVERAGE % POC FOR EACH QUARTILE (funding per cap) NATIONALLY 

myPOC_avgs <- myWorking_decade %>%
  select(poc_pct, decade_amount_per_cap_quants) %>%
  group_by(decade_amount_per_cap_quants) %>%
  mutate(avg_pct_poc = mean(poc_pct)) %>%
  ungroup() %>%
  select(decade_amount_per_cap_quants, avg_pct_poc) %>%
  distinct()

myPov_avgs <- myWorking_decade %>%
  select(inc_below_pov_pct, decade_amount_per_cap_quants) %>%
  group_by(decade_amount_per_cap_quants) %>%
  mutate(avg_pct_pov = mean(inc_below_pov_pct)) %>%
  ungroup() %>%
  select(decade_amount_per_cap_quants, avg_pct_pov) %>%
  distinct()

myRural_avgs <- myWorking_decade %>%
  select(rural_pct, decade_amount_per_cap_quants) %>%
  group_by(decade_amount_per_cap_quants) %>%
  mutate(avg_pct_rural = mean(rural_pct)) %>%
  ungroup() %>%
  select(decade_amount_per_cap_quants, avg_pct_rural) %>%
  distinct()

myMedInc_avgs <- myWorking_decade %>%
  select(med_income_house, decade_amount_per_cap_quants) %>%
  group_by(decade_amount_per_cap_quants) %>%
  mutate(avg_med_income = mean(med_income_house)) %>%
  ungroup() %>%
  select(decade_amount_per_cap_quants, avg_med_income) %>%
  distinct()

myAmount_df <- left_join(myPOC_avgs, myPov_avgs, by = "decade_amount_per_cap_quants")
myAmount_df <- left_join(myAmount_df, myRural_avgs, by = "decade_amount_per_cap_quants")
myAmount_df <- left_join(myAmount_df, myMedInc_avgs, by = "decade_amount_per_cap_quants")


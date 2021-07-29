# Regerssions / A.M. Creel / July 29th, 2021 / 
# History: This script is mostly copy and pasted from 07.quart_regressions.Rmd
# Goal: Include prelinary regressions in PERC presentation

library(vroom)
library(tidyr)
library(stringr)
library(usmap)
library(tidycensus)
library(ggplot2)
library(gtools)
library(forcats)
options(scipen=99999)
library(janitor)
library(dplyr)
library(fixest)
library(kableExtra)
# webshot::install_phantomjs()
# install.packages("magick") #ATTN:: I installed magick and then save_kable stopped working. Not installing it!!
# ---- #



# made in 01.data_wrangle.R (all $ are real 2018 dollars)
myWorking <- vroom("Datasets/clean_data/lwcf_annualPop_deccenialDemographics.csv") 

# Removing counties whose FIPSs code I didn't identify and don't have demo data for
myWorking <- myWorking %>%
  filter(!is.na(fips)) %>%
  distinct() %>%
  mutate(type = as.factor(type)) %>%
  mutate(state_fips = as.factor(str_sub(fips, 1, 2))) %>%
  filter(!is.na(white_pct)) %>%
  dplyr::mutate(poc_pct = 100 - white_pct)

# working years: 1965-2018
myWorking %>%
  select(real_year) %>%
  distinct() %>%
  summary()

# Grouping outcome variables by decade (number of grants received, amount received, average amount received per capita per decade, avg # grants per 100,000 people per decade)
myWorking_decade <- myWorking %>%
  group_by(fips, merge_year) %>%
  mutate(decade_grants = sum(got_grant)) %>%
  mutate(decade_amount = sum(amount, na.rm = TRUE)) %>%
  mutate(decade_per_cap_amount = sum(amount, na.rm = TRUE)/mean(annual_population, na.rm = TRUE)) %>%
  mutate(decade_per_cap_grants = sum(got_grant)/mean((annual_population/100000), na.rm = TRUE)) %>%
  select(state_fips, med_income_house, ends_with("_pct"), starts_with("decade_")) %>%
  mutate(med_income_house = med_income_house/1000) %>%
  ungroup() %>%
  distinct()

#renaming for table
myWorking_decade_pretty <- myWorking_decade %>%
  rename(POC = poc_pct) %>%
  rename(Poverty = inc_below_pov_pct) %>%
  rename(Rural = rural_pct) %>%
  rename(State = state_fips) %>%
  rename(Decade = merge_year) %>%
  rename(Median_Income = med_income_house)


# Regressions: includes poverty.
Total_Quantity <- feols(data = myWorking_decade_pretty, decade_grants ~ POC + Poverty + Rural | State + Decade)
Total_Amount <- feols(data = myWorking_decade_pretty, decade_amount ~ POC + Poverty + Rural | State + Decade)
PerCap_Quantity <- feols(data = myWorking_decade_pretty, decade_per_cap_grants ~ POC + Poverty + Rural | State + Decade)
PerCap_Amount <- feols(data = myWorking_decade_pretty, decade_per_cap_amount ~ POC + Poverty + Rural | State + Decade)

#Output
myTable <- etable(Total_Quantity,
                  Total_Amount,
                  PerCap_Quantity,
                  PerCap_Amount,
                  signifCode = c("***"=0.01, "**"=0.05, "*"=0.10)) %>%
  kbl(booktabs = T, caption = "Change in county's outcome for a 1% increase in demographic characteristic", 
      col.names = c("Quantity per Decade", "Total Amount per Decade", "Quantity per 100K ppl per Decade", "Amount per capita per Decade"))  %>%
  kable_classic_2(full_width = F) 
  footnote(general = paste0("Linear OLS model with fixed effects. Median Income is divided by 1,000. All dollar amounts are real 2018 USD."))
myTable

#saving to pdf
save_kable(myTable, "Exploratory_Output/regressions/PERC_poverty.pdf")


# Regressions: includes median income
Total_Quantity <- feols(data = myWorking_decade_pretty, decade_grants ~ POC  + Rural + Median_Income | State + Decade)
Total_Amount <- feols(data = myWorking_decade_pretty, decade_amount ~ POC  + Rural + Median_Income | State + Decade)
PerCap_Quantity <- feols(data = myWorking_decade_pretty, decade_per_cap_grants ~ POC  + Rural+ Median_Income | State + Decade)
PerCap_Amount <- feols(data = myWorking_decade_pretty, decade_per_cap_amount ~ POC  + Rural + Median_Income | State + Decade)

#Output
myTable <- etable(Total_Quantity,
                  Total_Amount,
                  PerCap_Quantity,
                  PerCap_Amount,
                  signifCode = c("***"=0.01, "**"=0.05, "*"=0.10)) %>%
  kbl(booktabs = T, caption = "Change in county's outcome for a 1% increase in demographic characteristic", 
      col.names = c("Quantity per Decade", "Total Amount per Decade", "Quantity per 100K ppl per Decade", "Amount per capita per Decade"))  %>%
  kable_classic_2(full_width = F) %>%
  footnote(general = paste0("Linear OLS model with fixed effects. Median Income is divided by 1,000. All dollar amounts are real 2018 USD."))
myTable

#saving to pdf
save_kable(myTable, "Exploratory_Output/regressions/PERC_median_income.pdf")



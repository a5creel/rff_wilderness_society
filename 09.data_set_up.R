#Goal: Create the R workspace for suit indexes that all scripts are guaranteed 
# to be using the same data cleaned in the same way 
# Andie Creel / April 2023

rm(list = ls())
library(vroom)
library(tidyr)
library(stringr)
library(ggplot2)
library(gtools)
library(forcats)
options(scipen=99999)
library(janitor)
library(dplyr)
library(zoo)
library(runner)
source('09.functions.R')
library(fixest)
library(kableExtra)
library(ggpubr)

# ----maps ---- #
library(choroplethr)
library(choroplethrMaps)
library(usdata)
library(maps)
library(tidycensus)
data("fips_codes")
data("state.regions")
library(RColorBrewer)

#-------------------------------------------------------------------------------
# NATIONAL DATA CLEANING
# Grant amounts are already adjusted for inflation, adjusting median income (2018 dollars)
#-------------------------------------------------------------------------------
# made in 01.data_wrangle.R
myWorking_og <- vroom("Datasets/clean_data/lwcf_annualPop_deccenialDemographics.csv") 

#cleaning to relevant variables 
myWorking_clean <- myWorking_og %>%
  mutate(state_fips = as.factor(str_sub(fips, 1, 2))) %>% # getting state fips code
  filter(!is.na(white_pct)) %>% # getting rid of rows where I couldn't get their demographic data
  dplyr::mutate(poc_pct = 100 - white_pct) %>% # calculating % people of color as non-hispanic white
  mutate(amount = if_else(is.na(amount), 0, amount)) %>% # adding 0s for amount of grant rewarded
  select(state_fips, fips, real_year, merge_year, annual_population, amount, poc_pct, med_income_house, rural_pct, type) %>%
  filter(amount != 0) %>% # dropping grants where we don't know the grant amount
  # not dropping grants where we don't know med income bc that would drop a decades worth of data (dont have med income in 1970 census)
  filter(!is.na(poc_pct)) %>% #dropping grants where we don't know POC
  filter(!is.na(rural_pct)) #dropping grants where we don't know rural 

#creating new variable where amount is defined as total amount a county got in a single year
myWorking_fips_year <- myWorking_clean %>%
  group_by(fips, real_year) %>%
  mutate(amount = sum(amount)) %>% #total amount a county got in a single year
  ungroup() %>%
  distinct()

#-------------------------------------------------------------------------------
#per cap: per cap through time aka how it's been implemented through time
#-------------------------------------------------------------------------------
myWorking <- myWorking_fips_year %>%
  mutate(amount_per_cap = amount / annual_population) %>% # amount per capita each year
  group_by(fips) %>% # group by county
  mutate(amount_per_cap_avg = sum(amount_per_cap)/54) %>% # average amount per cap
  mutate(avg_med_income_house = mean(med_income_house, na.rm = TRUE)) %>% #average median income
  mutate(avg_poc_pct = mean(poc_pct)) %>%
  mutate(avg_rural_pct = mean(rural_pct)) %>%
  select(state_fips, fips, amount_per_cap_avg, avg_med_income_house, avg_poc_pct, avg_rural_pct) %>% # set up reduce down to one entry per county
  distinct() # reduce

rm(myWorking_og)

#-------------------------------------------------------------------------------
# STATE DATA CLEANING
#-------------------------------------------------------------------------------
fips_codes <- fips_codes %>%
  select(state_code) %>%
  distinct()

#Gets list of dataframes for each state (applys function to whole list of state fips)
state_dfs <- lapply(as.character(fips_codes$state_code), function(st=x){
  df <- myWorking  %>%
    filter(state_fips == st) %>%
    mutate(state_fips = as.character(state_fips))
  return(df)
}) 

# Make a data frame of suits results using getSuits() and list of state dataframes
myResult_state <- data.frame(matrix(ncol=4, nrow = length(state_dfs)))
colnames(myResult_state) <- c("state_fips", "Suits_Amount_Income", "Suits_Amount_POC", "Suits_Amount_Rural" )
for (i in 1:length(state_dfs)) {
  myResult_state$state_fips[i] <- state_dfs[[i]][1,1] #state fips
  myResult_state$Suits_Amount_Income[i] <- getSuits(state_dfs[[i]], x_axis = "Income")$Suits_Amount
  myResult_state$Suits_Amount_POC[i] <- getSuits(state_dfs[[i]], x_axis = "POC")$Suits_Amount
  myResult_state$Suits_Amount_Rural[i] <- getSuits(state_dfs[[i]], x_axis = "Rural")$Suits_Amount
  
}

# clean up state_fips codes
myResult_state <- myResult_state %>%
  mutate(state_fips = str_pad(state_fips, width = 2, pad ="0")) %>%
  mutate(state_fips = as.character(state_fips))

#-------------------------------------------------------------------------------
# Data sets for state maps 
# state_choropleth doesn't work for all fips codes, so we need to only use those in data(state.regions)
#-------------------------------------------------------------------------------
# Income 
myMap_income <- left_join(state.regions, myResult_state, by = c("fips.character" = "state_fips")) %>%
  mutate(value = Suits_Amount_Income)  #for state_choropleth map call

# POC
myMap_poc <- left_join(state.regions, myResult_state, by = c("fips.character" = "state_fips")) %>%
  mutate(value = Suits_Amount_POC)  #for state_choropleth map call

# Rural
myMap_rural <- left_join(state.regions, myResult_state, by = c("fips.character" = "state_fips")) %>%
  mutate(value = Suits_Amount_Rural) #for state_choropleth map call
# mutate(value = if_else(is.na(value), 0, value)) #so that we don't have NAs in map


#-------------------------------------------------------------------------------
# saving the whole r environment 
#-------------------------------------------------------------------------------
rm(fips_codes, state.regions, i)

save.image(file='09.suits_environment.RData')


















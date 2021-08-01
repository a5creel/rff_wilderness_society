# Quartiles of Grant Funding Described / A.M. Creel / July 28th, 2021 / 
# History: This script is mostly copy and pasted from 06.descriptive_reverse_quants.Rmd
# Goal: Export the graphs Maragaret wanted to include in the PERC presentation 
# Brown colors tend to indicated negative numbers and purple tend to indicate positive numbers. 

library(vroom)
library(tidyr)
library(stringr)
library(usmap)
library(tidycensus)
library(ggplot2)
library(gtools)
library(forcats)
library("wesanderson")
options(scipen=99999)
library(janitor)
library(dplyr)
# ---- #
library(choroplethr)
library(choroplethrMaps)
library(usdata)
library(maps)
data(fips_codes)
data(continental_us_states)
# --- #


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
# ATTN: This script only focuses on average amount received per capita per decade! 
myWorking_decade <- myWorking %>%
  group_by(fips, merge_year) %>%
  mutate(decade_grants = sum(got_grant)) %>%
  mutate(decade_amount = sum(amount, na.rm = TRUE)) %>%
  mutate(decade_avg_per_cap_amount = sum(amount, na.rm = TRUE)/mean(annual_population, na.rm = TRUE)) %>%
  mutate(decade_avg_per_cap_grants = sum(got_grant)/mean((annual_population/100000), na.rm = TRUE)) %>%
  select(state_fips, ends_with("_pct"), starts_with("decade_")) %>%
  ungroup() %>%
  distinct()

#getting average POC per county
state_pct_POC <- myWorking_decade %>%
  select(state_fips, poc_pct) %>%
  distinct() %>%
  group_by(state_fips) %>%
  mutate(avg_poc_pct = mean(poc_pct)) %>%
  select(state_fips, avg_poc_pct) %>%
  distinct()

myWorking_decade <- left_join(myWorking_decade, state_pct_POC, by = "state_fips" )

# GETTING QUARTILES ------------------------------------------------------------

# ------- Get quartiles of grants received for each decade ------- #
myQuants <- myWorking_decade %>%
  dplyr::group_by(state_fips, merge_year) %>%
  dplyr::summarise(quants = list(quantile(decade_grants, probs = c(.25, .5, .75)))) %>%
  unnest_wider(quants) %>%
  dplyr::rename(decade_grants_25 = `25%`) %>%
  dplyr::rename(decade_grants_50 = `50%`) %>%
  dplyr::rename(decade_grants_75 = `75%`)
#joining back in
myWorking_decade <- left_join(myWorking_decade, myQuants, by = c("state_fips", "merge_year"))
rm(myQuants)
#making a factor variable for the quartiles (by state and year)
myWorking_decade <- myWorking_decade %>%
  dplyr::mutate(decade_grants_quants = if_else(decade_grants > decade_grants_75, "4", "")) %>% # fourth quartile 
  dplyr::mutate(decade_grants_quants = if_else(decade_grants <=decade_grants_75, "3", decade_grants_quants)) %>% # third quartile
  dplyr::mutate(decade_grants_quants = if_else(decade_grants <= decade_grants_50, "2", decade_grants_quants)) %>% # second quartile
  dplyr::mutate(decade_grants_quants = if_else(decade_grants <= decade_grants_25, "1", decade_grants_quants)) %>% # first quartile 
  dplyr::select(-decade_grants_75, -decade_grants_50, - decade_grants_25) %>%
  dplyr::mutate(decade_grants_quants = as.factor(decade_grants_quants))
# ------- Get quartiles of amount of grant funding for each decade ------- #
myQuants <- myWorking_decade %>%
  dplyr::group_by(state_fips, merge_year) %>%
  dplyr::summarise(quants = list(quantile(decade_amount, probs = c(.25, .5, .75)))) %>%
  unnest_wider(quants) %>%
  dplyr::rename(decade_amount_25 = `25%`) %>%
  dplyr::rename(decade_amount_50 = `50%`) %>%
  dplyr::rename(decade_amount_75 = `75%`)
#joining back in
myWorking_decade <- left_join(myWorking_decade, myQuants, by = c("state_fips", "merge_year"))
rm(myQuants)
#making a factor variable for the quartiles (by state and year)
myWorking_decade <- myWorking_decade %>%
  dplyr::mutate(decade_amount_quants = if_else(decade_amount > decade_amount_75, "4", "")) %>% # fourth quartile 
  dplyr::mutate(decade_amount_quants = if_else(decade_amount <=decade_amount_75, "3", decade_amount_quants)) %>% # third quartile
  dplyr::mutate(decade_amount_quants = if_else(decade_amount <= decade_amount_50, "2", decade_amount_quants)) %>% # second quartile
  dplyr::mutate(decade_amount_quants = if_else(decade_amount <= decade_amount_25, "1", decade_amount_quants)) %>% # first quartile 
  dplyr::select(-decade_amount_75, -decade_amount_50, - decade_amount_25) %>%
  dplyr::mutate(decade_amount_quants = as.factor(decade_amount_quants))
# ------- Get quartiles of amount of grant funding PER CAPITA for each decade ------- #
myQuants <- myWorking_decade %>%
  dplyr::group_by(state_fips, merge_year) %>%
  dplyr::summarise(quants = list(quantile(decade_avg_per_cap_amount, probs = c(.25, .5, .75), na.rm = TRUE))) %>%
  unnest_wider(quants) %>%
  dplyr::rename(decade_avg_per_cap_amount_25 = `25%`) %>%
  dplyr::rename(decade_avg_per_cap_amount_50 = `50%`) %>%
  dplyr::rename(decade_avg_per_cap_amount_75 = `75%`)
#joining back in
myWorking_decade <- left_join(myWorking_decade, myQuants, by = c("state_fips", "merge_year"))
rm(myQuants)
#making a factor variable for the quartiles (by state and year)
myWorking_decade <- myWorking_decade %>%
  dplyr::mutate(decade_avg_per_cap_amount_quants = if_else(decade_avg_per_cap_amount > decade_avg_per_cap_amount_75, "4", "")) %>% # fourth quartile 
  dplyr::mutate(decade_avg_per_cap_amount_quants = if_else(decade_avg_per_cap_amount <=decade_avg_per_cap_amount_75, "3", decade_avg_per_cap_amount_quants)) %>% # third quartile
  dplyr::mutate(decade_avg_per_cap_amount_quants = if_else(decade_avg_per_cap_amount <= decade_avg_per_cap_amount_50, "2", decade_avg_per_cap_amount_quants)) %>% # second quartile
  dplyr::mutate(decade_avg_per_cap_amount_quants = if_else(decade_avg_per_cap_amount <= decade_avg_per_cap_amount_25, "1", decade_avg_per_cap_amount_quants)) %>% # first quartile 
  dplyr::select(-decade_avg_per_cap_amount_75, -decade_avg_per_cap_amount_50, - decade_avg_per_cap_amount_25) %>%
  dplyr::mutate(decade_avg_per_cap_amount_quants = as.factor(decade_avg_per_cap_amount_quants))
# ------- Get quartiles of number of grants  PER CAPITA for each decade ------- #
myQuants <- myWorking_decade %>%
  dplyr::group_by(state_fips, merge_year) %>%
  dplyr::summarise(quants = list(quantile(decade_avg_per_cap_grants, probs = c(.25, .5, .75), na.rm = TRUE))) %>%
  unnest_wider(quants) %>%
  dplyr::rename(decade_avg_per_cap_grants_25 = `25%`) %>%
  dplyr::rename(decade_avg_per_cap_grants_50 = `50%`) %>%
  dplyr::rename(decade_avg_per_cap_grants_75 = `75%`)
#joining back in
myWorking_decade <- left_join(myWorking_decade, myQuants, by = c("state_fips", "merge_year"))
rm(myQuants)
#making a factor variable for the quartiles (by state and year)
myWorking_decade <- myWorking_decade %>%
  dplyr::mutate(decade_avg_per_cap_grants_quants = if_else(decade_avg_per_cap_grants > decade_avg_per_cap_grants_75, "4", "")) %>% # fourth quartile 
  dplyr::mutate(decade_avg_per_cap_grants_quants = if_else(decade_avg_per_cap_grants <=decade_avg_per_cap_grants_75, "3", decade_avg_per_cap_grants_quants)) %>% # third quartile
  dplyr::mutate(decade_avg_per_cap_grants_quants = if_else(decade_avg_per_cap_grants <= decade_avg_per_cap_grants_50, "2", decade_avg_per_cap_grants_quants)) %>% # second quartile
  dplyr::mutate(decade_avg_per_cap_grants_quants = if_else(decade_avg_per_cap_grants <= decade_avg_per_cap_grants_25, "1", decade_avg_per_cap_grants_quants)) %>% # first quartile 
  dplyr::select(-decade_avg_per_cap_grants_75, -decade_avg_per_cap_grants_50, - decade_avg_per_cap_grants_25) %>%
  dplyr::mutate(decade_avg_per_cap_grants_quants = as.factor(decade_avg_per_cap_grants_quants))

##################### MAPS: AMOUNT OF GRANT $$ PER CAPITA #####################


# difference in % POC from 1st quart and 4th quart  ----------------------------

myMap_poc <- myWorking_decade %>%
  select(state_fips, fips, merge_year, poc_pct, decade_avg_per_cap_amount_quants) %>%
  pivot_wider(names_from = decade_avg_per_cap_amount_quants, values_from = c(poc_pct)) %>%
  group_by(state_fips) %>%
  mutate(poc_pct_4 = mean(`4`, na.rm = TRUE)) %>%
  mutate(poc_pct_3 = mean(`3`, na.rm = TRUE)) %>%
  mutate(poc_pct_2 = mean(`2`, na.rm = TRUE)) %>%
  mutate(poc_pct_1 = mean(`1`, na.rm = TRUE)) %>%
  select(state_fips, starts_with("poc_pct_")) %>%
  distinct()

#getting state name for choropleth maps 
fips_codes <- fips_codes %>%
  select(state_code, state_name) %>%
  distinct()

myMap_poc <- left_join(myMap_poc, fips_codes, by = c("state_fips" = "state_code")) %>% 
  rename(region = state_name) %>%
  mutate(region = tolower(region)) %>%
  mutate(value = round(poc_pct_4 - poc_pct_1), 2)

state_choropleth(myMap_poc,
  zoom = continental_us_states) +
  # scale_fill_brewer(palette="YlOrBr", direction = -1)+
  scale_fill_brewer(palette="PuOr")+
  ggtitle(label = "Difference between top and bottom LWCF per capita spending quartiles in % people of color") + 
  labs(fill = "Difference in % ")+
  theme(legend.position="bottom") + 
  theme(plot.title = element_text(size = 15))

#Saving map
ggsave(width = 11, height = 8, filename = "Exploratory_Output/PERC_Presentation_Maps/poc_$perCap.jpeg")



 # difference in % poverty from 1st quart and 4th quart  ----------------------------

myMap_pov <- myWorking_decade %>%
  select(state_fips, fips, merge_year, inc_below_pov_pct, decade_avg_per_cap_amount_quants) %>%
  pivot_wider(names_from = decade_avg_per_cap_amount_quants, values_from = c(inc_below_pov_pct)) %>%
  group_by(state_fips) %>%
  mutate(inc_below_pov_pct_4 = mean(`4`, na.rm = TRUE)) %>%
  mutate(inc_below_pov_pct_3 = mean(`3`, na.rm = TRUE)) %>%
  mutate(inc_below_pov_pct_2 = mean(`2`, na.rm = TRUE)) %>%
  mutate(inc_below_pov_pct_1 = mean(`1`, na.rm = TRUE)) %>%
  select(state_fips, starts_with("inc_below_pov_pct_")) %>%
  distinct()

#getting state name for choropleth maps 
fips_codes <- fips_codes %>%
  select(state_code, state_name) %>%
  distinct()

myMap_pov <- left_join(myMap_pov, fips_codes, by = c("state_fips" = "state_code")) %>% 
  rename(region = state_name) %>%
  mutate(region = tolower(region)) %>%
  mutate(value = round(inc_below_pov_pct_4 - inc_below_pov_pct_1), 2)

state_choropleth(myMap_pov,
  zoom = continental_us_states) +
  scale_fill_brewer(palette="PuOr")+
  # scale_fill_brewer(palette="Purples")+
  ggtitle(label = "Difference between top and bottom LWCF per capita spending quartiles per capita in % of people living in poverty") + 
  labs(fill = "Difference in % ")+
  theme(legend.position="bottom") + 
  theme(plot.title = element_text(size = 15))

#Saving map
ggsave(width = 11, height = 8, filename = "Exploratory_Output/PERC_Presentation_Maps/rural_$perCap.jpeg")


# difference in % urban from 1st quart and 4th quart ----------------------------

myMap_rural <- myWorking_decade %>%
  select(state_fips, fips, merge_year, rural_pct, decade_avg_per_cap_amount_quants) %>%
  pivot_wider(names_from = decade_avg_per_cap_amount_quants, values_from = c(rural_pct)) %>%
  group_by(state_fips) %>%
  mutate(rural_pct_4 = mean(`4`, na.rm = TRUE)) %>%
  mutate(rural_pct_3 = mean(`3`, na.rm = TRUE)) %>%
  mutate(rural_pct_2 = mean(`2`, na.rm = TRUE)) %>%
  mutate(rural_pct_1 = mean(`1`, na.rm = TRUE)) %>%
  select(state_fips, starts_with("rural_pct_")) %>%
  distinct()

#getting state name for choropleth maps 
fips_codes <- fips_codes %>%
  select(state_code, state_name) %>%
  distinct()

myMap_rural <- left_join(myMap_rural, fips_codes, by = c("state_fips" = "state_code")) %>% 
  rename(region = state_name) %>%
  mutate(region = tolower(region)) %>%
  mutate(value = round(rural_pct_4 - rural_pct_1), 2)

state_choropleth(myMap_rural,
  zoom = continental_us_states) +
  # scale_fill_brewer(palette="Purples")+
  scale_fill_brewer(palette="PuOr")+
  ggtitle(label = "Difference between top and bottom LWCF per caita spending quartiles per capita in % rural") + 
  labs(fill = "Difference in % ")+
  theme(legend.position="bottom")+ 
  theme(plot.title = element_text(size = 15))

#Saving map
ggsave(width = 11, height = 8, filename = "Exploratory_Output/PERC_Presentation_Maps/rural_$perCap.jpeg")

##################### MAPS: TOTAL AMOUNT OF GRANT $$ #####################


# difference in % POC from 1st quart and 4th quart  ----------------------------

myMap_poc <- myWorking_decade %>%
  select(state_fips, fips, merge_year, poc_pct, decade_amount_quants) %>%
  pivot_wider(names_from = decade_amount_quants, values_from = c(poc_pct)) %>%
  group_by(state_fips) %>%
  mutate(poc_pct_4 = mean(`4`, na.rm = TRUE)) %>%
  mutate(poc_pct_3 = mean(`3`, na.rm = TRUE)) %>%
  mutate(poc_pct_2 = mean(`2`, na.rm = TRUE)) %>%
  mutate(poc_pct_1 = mean(`1`, na.rm = TRUE)) %>%
  select(state_fips, starts_with("poc_pct_")) %>%
  distinct()

#getting state name for choropleth maps 
fips_codes <- fips_codes %>%
  select(state_code, state_name) %>%
  distinct()

myMap_poc <- left_join(myMap_poc, fips_codes, by = c("state_fips" = "state_code")) %>% 
  rename(region = state_name) %>%
  mutate(region = tolower(region)) %>%
  mutate(value = round(poc_pct_4 - poc_pct_1), 2)

state_choropleth(myMap_poc,
                 zoom = continental_us_states) +
  # scale_fill_brewer(palette="YlOrBr", direction = -1)+
  scale_fill_brewer(palette="PuOr")+
  ggtitle(label = "Difference between top and bottom LWCF total spending quartiles per capita in % people of color") + 
  labs(fill = "Difference in % ")+
  theme(legend.position="bottom") + 
  theme(plot.title = element_text(size = 15))

#Saving map
ggsave(width = 11, height = 8, filename = "Exploratory_Output/PERC_Presentation_Maps/poc_$total.jpeg")



# difference in % poverty from 1st quart and 4th quart  ----------------------------

myMap_pov <- myWorking_decade %>%
  select(state_fips, fips, merge_year, inc_below_pov_pct, decade_amount_quants) %>%
  pivot_wider(names_from = decade_amount_quants, values_from = c(inc_below_pov_pct)) %>%
  group_by(state_fips) %>%
  mutate(inc_below_pov_pct_4 = mean(`4`, na.rm = TRUE)) %>%
  mutate(inc_below_pov_pct_3 = mean(`3`, na.rm = TRUE)) %>%
  mutate(inc_below_pov_pct_2 = mean(`2`, na.rm = TRUE)) %>%
  mutate(inc_below_pov_pct_1 = mean(`1`, na.rm = TRUE)) %>%
  select(state_fips, starts_with("inc_below_pov_pct_")) %>%
  distinct()

#getting state name for choropleth maps 
fips_codes <- fips_codes %>%
  select(state_code, state_name) %>%
  distinct()

myMap_pov <- left_join(myMap_pov, fips_codes, by = c("state_fips" = "state_code")) %>% 
  rename(region = state_name) %>%
  mutate(region = tolower(region)) %>%
  mutate(value = round(inc_below_pov_pct_4 - inc_below_pov_pct_1), 2)

state_choropleth(myMap_pov,
                 zoom = continental_us_states) +
  scale_fill_brewer(palette="YlOrBr", direction = -1)+
  # scale_fill_brewer(palette="PuOr")+
  # scale_fill_brewer(palette="Purples")+
  ggtitle(label = "Difference between top and bottom LWCF total spending quartiles in % living in poverty") + 
  labs(fill = "Difference in % ")+
  theme(legend.position="bottom") + 
  theme(plot.title = element_text(size = 15))

#Saving map
ggsave(width = 11, height = 8, filename = "Exploratory_Output/PERC_Presentation_Maps/poverty_$total.jpeg")


# difference in % urban from 1st quart and 4th quart ----------------------------

myMap_rural <- myWorking_decade %>%
  select(state_fips, fips, merge_year, rural_pct, decade_amount_quants) %>%
  pivot_wider(names_from = decade_amount_quants, values_from = c(rural_pct)) %>%
  group_by(state_fips) %>%
  mutate(rural_pct_4 = mean(`4`, na.rm = TRUE)) %>%
  mutate(rural_pct_3 = mean(`3`, na.rm = TRUE)) %>%
  mutate(rural_pct_2 = mean(`2`, na.rm = TRUE)) %>%
  mutate(rural_pct_1 = mean(`1`, na.rm = TRUE)) %>%
  select(state_fips, starts_with("rural_pct_")) %>%
  distinct()

#getting state name for choropleth maps 
fips_codes <- fips_codes %>%
  select(state_code, state_name) %>%
  distinct()

myMap_rural <- left_join(myMap_rural, fips_codes, by = c("state_fips" = "state_code")) %>% 
  rename(region = state_name) %>%
  mutate(region = tolower(region)) %>%
  mutate(value = round(rural_pct_4 - rural_pct_1), 2)

state_choropleth(myMap_rural,
                 zoom = continental_us_states) +
  scale_fill_brewer(palette="YlOrBr", direction = -1)+
  # scale_fill_brewer(palette="PuOr")+
  ggtitle(label = "Difference between top and bottom LWCF total spending quartiles in % rural") + 
  labs(fill = "Difference in % ")+
  theme(legend.position="bottom")+ 
  theme(plot.title = element_text(size = 15))

#Saving map
ggsave(width = 11, height = 8, filename = "Exploratory_Output/PERC_Presentation_Maps/rural_$total.jpeg")

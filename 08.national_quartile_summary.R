# National Review / A.M. Creel / July 23rd, 2021
# Goal: Get National summary for quartiles 
# Required Scripts: 01.data_wrangle.R
# Warning: for all numbers to be accurate, this script needs to be run sequentially from top to bottom 
# (if you jump around you can have mislabeled cells by accident)

# ATTN: this is the first time I adjust for inflation

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
source('08.functions.R')
library(openxlsx)
# --- #

# BASIC SET UP -----------------------------------------------------------------

# made in 01.data_wrangle.R
myWorking <- vroom("Datasets/clean_data/lwcf_annualPop_deccenialDemographics.csv") 
myInf <- vroom("Datasets/clean_data/inflation_rates.csv") 

# Adjusting for inflation (2018 dollars)
myWorking <- left_join(myWorking, myInf, by = "year") %>%
  mutate(amount = amount/adj_value)
rm(myInf)

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

# ALL GRANTS (except planning): ALL YEARS -----------------------------------------------------------------------

# Grouping outcome variables by decade 
#(number of grants received, amount received, average amount received per capita per decade, avg # grants per 100,000 people per decade)
myWorking_decade <- getDecade(myWorking) 

#getting quartiles added to data frame
# ATTN: Note that these are national quartiles, not state quartiles. I've still grouped 
# by decade to deal with some of the fluctuation in the amount of grant funding that was given out each year 
myWorking_decade <- getQuartiles(myWorking_decade)

# CALCULATIONG AVERAGE % FOR EACH QUARTILE (funding per cap) NATIONALLY
myAmount_all <- getAmountDF(myWorking_decade)

# CALCULATIONG AVERAGE % FOR EACH QUARTILE (quantity per cap) NATIONALLY 
myQuantity_all <- getQuantityDF(myWorking_decade)


# ACQUISITION: ALL YEARS -----------------------------------------------------------------------

#removing anything that isn't a land acquisition grant or combination grant
myWorking_acquisition <- myWorking %>%
  filter(type != "P") %>% # Planning 
  filter(type != "D") %>% # Development
  filter(type != "R") # redevelopment 

# Grouping outcome variables by decade (number of grants received, amount received, average amount received per capita per decade, avg # grants per 100,000 people per decade)
myWorking_decade_A <- getDecade(myWorking_acquisition) 

#getting quartiles added to dataframe
myWorking_decade_A <- getQuartiles(myWorking_decade_A)

# CALCULATIONG AVERAGE % FOR EACH QUARTILE (funding per cap) NATIONALLY
myAmount_acquisition <- getAmountDF(myWorking_decade_A)

# CALCULATIONG AVERAGE % FOR EACH QUARTILE (quantity per cap) NATIONALLY 
myQuantity_acquisition <- getQuantityDF(myWorking_decade_A)

# DEVELOPMENT: ALL YEARS -----------------------------------------------------------------------

#removing anything that isn't a land acquisition grant or combination grant
myWorking_development <- myWorking %>%
  filter(type != "P") %>% # Planning 
  filter(type != "A") %>% # not acquision
  filter(type != "C") # not combination 

# Grouping outcome variables by decade (number of grants received, amount received, average amount received per capita per decade, avg # grants per 100,000 people per decade)
myWorking_decade_D <- getDecade(myWorking_development) 

#getting quartiles added to dataframe
myWorking_decade_D <- getQuartiles(myWorking_decade_D)

# CALCULATIONG AVERAGE % FOR EACH QUARTILE (funding per cap) NATIONALLY
myAmount_development <- getAmountDF(myWorking_decade_D)

# CALCULATIONG AVERAGE % FOR EACH QUARTILE (quantity per cap) NATIONALLY 
myQuantity_development <- getQuantityDF(myWorking_decade_D)


# PUTTING IT ALL TOGETHER: ALL YEARS ------------------------------------------

# --- Amount of funding  --- #

# labeling grant type 
myAmount_all <- myAmount_all %>%
  mutate(grants_included = "All") %>%
  mutate(years = "1975-2018")

myAmount_acquisition <- myAmount_acquisition %>%
  mutate(grants_included = "Acquisition") %>%
  mutate(years = "1975-2018")

myAmount_development <- myAmount_development %>%
  mutate(grants_included = "Development") %>%
  mutate(years = "1975-2018")

# putting it all in one df
myAmount <- bind_rows(myAmount_all, myAmount_acquisition)
myAmount <- bind_rows(myAmount, myAmount_development) %>%
  select(grants_included, years, decade_amount_per_cap_quants, starts_with("avg_"))
rm(myAmount_acquisition, myAmount_all, myAmount_development)

# --- Quantity of funding  --- #

# labeling grant type and years
myQuantity_all <- myQuantity_all %>%
  mutate(grants_included = "All") %>%
  mutate(years = "1975-2018")

myQuantity_acquisition <- myQuantity_acquisition %>%
  mutate(grants_included = "Acquisition") %>%
  mutate(years = "1975-2018")

myQuantity_development <- myQuantity_development %>%
  mutate(grants_included = "Development") %>%
  mutate(years = "1975-2018")

# putting it all in one df
myQuantity <- bind_rows(myQuantity_all, myQuantity_acquisition)
myQuantity <- bind_rows(myQuantity, myQuantity_development) %>%
  select(grants_included, years, decade_quantity_per_cap_quants, starts_with("avg_"))
rm(myQuantity_all, myQuantity_acquisition, myQuantity_development)

rm(myWorking_decade, myWorking_decade_A, myWorking_decade_D)

# YEAR SPLITS: pre Reaegan -- 1975-1980 -------------------------------------------------------------------

# ALL GRANTS

# Step 1: filter years in myWorking to years I want
myWorking_preReagan <- myWorking %>%
  filter(year >= 1975) %>%
  filter(year < 1981)

# Step 2: group by decade
myDecade_preReagan <- getDecade(myWorking_preReagan)

# Step 3: get quantiles 
myDecade_preReagan <- getQuartiles(myDecade_preReagan)

# Step 4: avg % for each quartile -- funding per cap 
myAmount_all_preReagan <- getAmountDF(myDecade_preReagan)

# Step 5: avg % for each quartile -- quantity per cap 
myQuantity_all_preReagan <- getQuantityDF(myDecade_preReagan)

# Step 6: label grant type and years 
myAmount_all_preReagan <- myAmount_all_preReagan %>%
  mutate(grants_included = "All") %>%
  mutate(years = "1975-1980")

myQuantity_all_preReagan <- myQuantity_all_preReagan %>%
  mutate(grants_included = "All") %>%
  mutate(years = "1975-1980")

# Step 7: add to main dataframes
myAmount <- bind_rows(myAmount, myAmount_all_preReagan) 
myQuantity <- bind_rows(myQuantity, myQuantity_all_preReagan)

# Step 8: remove variables 
rm(myWorking_preReagan, myDecade_preReagan, myAmount_all_preReagan, myQuantity_all_preReagan)

# ACQUISITION GRANTS

# Step 1: filter years in myWorking to years I want
myWorking_preReagan_A <- myWorking_acquisition %>%
  filter(year >= 1975) %>%
  filter(year < 1981)

# Step 2: group by decade
myDecade_preReagan_A <- getDecade(myWorking_preReagan_A)

# Step 3: get quantiles 
myDecade_preReagan_A <- getQuartiles(myDecade_preReagan_A)

# Step 4: avg % for each quartile -- funding per cap 
myAmount_all_preReagan_A <- getAmountDF(myDecade_preReagan_A)

# Step 5: avg % for each quartile -- quantity per cap 
myQuantity_all_preReagan_A <- getQuantityDF(myDecade_preReagan_A)

# Step 6: label grant type and years 
myAmount_all_preReagan_A <- myAmount_all_preReagan_A %>%
  mutate(grants_included = "Acquisition") %>%
  mutate(years = "1975-1980")

myQuantity_all_preReagan_A <- myQuantity_all_preReagan_A %>%
  mutate(grants_included = "Acquisition") %>%
  mutate(years = "1975-1980")

# Step 7: add to main dataframes
myAmount <- bind_rows(myAmount, myAmount_all_preReagan_A) 
myQuantity <- bind_rows(myQuantity, myQuantity_all_preReagan_A)

# Step 8: remove variables 
rm(myWorking_preReagan_A, myDecade_preReagan_A, myAmount_all_preReagan_A, myQuantity_all_preReagan_A)

# DEVELOPMENT GRANTS

# Step 1: filter years in myWorking to years I want
myWorking_preReagan_D <- myWorking_development %>%
  filter(year >= 1975) %>%
  filter(year < 1981)

# Step 2: group by decade
myDecade_preReagan_D <- getDecade(myWorking_preReagan_D)

# Step 3: get quantiles 
myDecade_preReagan_D <- getQuartiles(myDecade_preReagan_D)

# Step 4: avg % for each quartile -- funding per cap 
myAmount_All_preReagan_D <- getAmountDF(myDecade_preReagan_D)

# Step 5: avg % for each quartile -- quantity per cap 
myQuantity_All_preReagan_D <- getQuantityDF(myDecade_preReagan_D)

# Step 6: label grant type and years 
myAmount_All_preReagan_D <- myAmount_All_preReagan_D %>%
  mutate(grants_included = "Development") %>%
  mutate(years = "1975-1980")

myQuantity_All_preReagan_D <- myQuantity_All_preReagan_D %>%
  mutate(grants_included = "Development") %>%
  mutate(years = "1975-1980")

# Step 7: add to main dataframes
myAmount <- bind_rows(myAmount, myAmount_All_preReagan_D) 
myQuantity <- bind_rows(myQuantity, myQuantity_All_preReagan_D)

# Step 8: remove variables 
rm(myWorking_preReagan_D, myDecade_preReagan_D, myAmount_All_preReagan_D, myQuantity_All_preReagan_D)

# YEAR SPLITS: Reagan until Obama -- 1981-2008 -------------------------------------------------------------------

# ALL GRANTS

# Step 1: filter years in myWorking to years I want
myWorking_Reagan <- myWorking %>%
  filter(year >= 1981) %>%
  filter(year < 2009)

# Step 2: group by decade
myDecade_Reagan <- getDecade(myWorking_Reagan)

# Step 3: get quantiles 
myDecade_Reagan <- getQuartiles(myDecade_Reagan)

# Step 4: avg % for each quartile -- funding per cap 
myAmount_all_Reagan <- getAmountDF(myDecade_Reagan)

# Step 5: avg % for each quartile -- quantity per cap 
myQuantity_all_Reagan <- getQuantityDF(myDecade_Reagan)

# Step 6: label grant type and years 
myAmount_all_Reagan <- myAmount_all_Reagan %>%
  mutate(grants_included = "All") %>%
  mutate(years = "1981-2008")

myQuantity_all_Reagan <- myQuantity_all_Reagan %>%
  mutate(grants_included = "All") %>%
  mutate(years = "1981-2008")

# Step 7: add to main dataframes
myAmount <- bind_rows(myAmount, myAmount_all_Reagan) 
myQuantity <- bind_rows(myQuantity, myQuantity_all_Reagan)

# Step 8: remove variables 
rm(myWorking_Reagan, myDecade_Reagan, myAmount_all_Reagan, myQuantity_all_Reagan)

# ACQUISITION GRANTS

# Step 1: filter years in myWorking to years I want
myWorking_Reagan_A <- myWorking_acquisition %>%
  filter(year >= 1981) %>%
  filter(year < 2009)

# Step 2: group by decade
myDecade_Reagan_A <- getDecade(myWorking_Reagan_A)

# Step 3: get quantiles 
myDecade_Reagan_A <- getQuartiles(myDecade_Reagan_A)

# Step 4: avg % for each quartile -- funding per cap 
myAmount_all_Reagan_A <- getAmountDF(myDecade_Reagan_A)

# Step 5: avg % for each quartile -- quantity per cap 
myQuantity_all_Reagan_A <- getQuantityDF(myDecade_Reagan_A)

# Step 6: label grant type and years 
myAmount_all_Reagan_A <- myAmount_all_Reagan_A %>%
  mutate(grants_included = "Acquisition") %>%
  mutate(years = "1981-2008")

myQuantity_all_Reagan_A <- myQuantity_all_Reagan_A %>%
  mutate(grants_included = "Acquisition") %>%
  mutate(years = "1981-2008")

# Step 7: add to main dataframes
myAmount <- bind_rows(myAmount, myAmount_all_Reagan_A) 
myQuantity <- bind_rows(myQuantity, myQuantity_all_Reagan_A)

# Step 8: remove variables 
rm(myWorking_Reagan_A, myDecade_Reagan_A, myAmount_all_Reagan_A, myQuantity_all_Reagan_A)

# DEVELOPMENT GRANTS

# Step 1: filter years in myWorking to years I want
myWorking_Reagan_D <- myWorking_development %>%
  filter(year >= 1981) %>%
  filter(year < 2009)

# Step 2: group by decade
myDecade_Reagan_D <- getDecade(myWorking_Reagan_D)

# Step 3: get quantiles 
myDecade_Reagan_D <- getQuartiles(myDecade_Reagan_D)

# Step 4: avg % for each quartile -- funding per cap 
myAmount_All_Reagan_D <- getAmountDF(myDecade_Reagan_D)

# Step 5: avg % for each quartile -- quantity per cap 
myQuantity_All_Reagan_D <- getQuantityDF(myDecade_Reagan_D)

# Step 6: label grant type and years 
myAmount_All_Reagan_D <- myAmount_All_Reagan_D %>%
  mutate(grants_included = "Development") %>%
  mutate(years = "1981-2008")

myQuantity_All_Reagan_D <- myQuantity_All_Reagan_D %>%
  mutate(grants_included = "Development") %>%
  mutate(years = "1981-2008")

# Step 7: add to main dataframes
myAmount <- bind_rows(myAmount, myAmount_All_Reagan_D) 
myQuantity <- bind_rows(myQuantity, myQuantity_All_Reagan_D)

# Step 8: remove variables 
rm(myWorking_Reagan_D, myDecade_Reagan_D, myAmount_All_Reagan_D, myQuantity_All_Reagan_D)








# YEAR SPLITS: Reagan until Obama -- 2009-2018 -------------------------------------------------------------------

# ALL GRANTS

# Step 1: filter years in myWorking to years I want
myWorking_Obama <- myWorking %>%
  filter(year >= 2009) 

# Step 2: group by decade
myDecade_Obama <- getDecade(myWorking_Obama)

# Step 3: get quantiles 
myDecade_Obama <- getQuartiles(myDecade_Obama)

# Step 4: avg % for each quartile -- funding per cap 
myAmount_all_Obama <- getAmountDF(myDecade_Obama)

# Step 5: avg % for each quartile -- quantity per cap 
myQuantity_all_Obama <- getQuantityDF(myDecade_Obama)

# Step 6: label grant type and years 
myAmount_all_Obama <- myAmount_all_Obama %>%
  mutate(grants_included = "All") %>%
  mutate(years = "2009-2018")

myQuantity_all_Obama <- myQuantity_all_Obama %>%
  mutate(grants_included = "All") %>%
  mutate(years = "2009-2018")

# Step 7: add to main dataframes
myAmount <- bind_rows(myAmount, myAmount_all_Obama) 
myQuantity <- bind_rows(myQuantity, myQuantity_all_Obama)

# Step 8: remove variables 
rm(myWorking_Obama, myDecade_Obama, myAmount_all_Obama, myQuantity_all_Obama)

# ACQUISITION GRANTS

# Step 1: filter years in myWorking to years I want
myWorking_Obama_A <- myWorking_acquisition %>%
  filter(year >= 2009) 

# Step 2: group by decade
myDecade_Obama_A <- getDecade(myWorking_Obama_A)

# Step 3: get quantiles 
myDecade_Obama_A <- getQuartiles(myDecade_Obama_A)

# Step 4: avg % for each quartile -- funding per cap 
myAmount_all_Obama_A <- getAmountDF(myDecade_Obama_A)

# Step 5: avg % for each quartile -- quantity per cap 
myQuantity_all_Obama_A <- getQuantityDF(myDecade_Obama_A)

# Step 6: label grant type and years 
myAmount_all_Obama_A <- myAmount_all_Obama_A %>%
  mutate(grants_included = "Acquisition") %>%
  mutate(years = "2009-2018")

myQuantity_all_Obama_A <- myQuantity_all_Obama_A %>%
  mutate(grants_included = "Acquisition") %>%
  mutate(years = "2009-2018")

# Step 7: add to main dataframes
myAmount <- bind_rows(myAmount, myAmount_all_Obama_A) 
myQuantity <- bind_rows(myQuantity, myQuantity_all_Obama_A)

# Step 8: remove variables 
rm(myWorking_Obama_A, myDecade_Obama_A, myAmount_all_Obama_A, myQuantity_all_Obama_A)

# DEVELOPMENT GRANTS

# Step 1: filter years in myWorking to years I want
myWorking_Obama_D <- myWorking_development %>%
  filter(year >= 2009) 

# Step 2: group by decade
myDecade_Obama_D <- getDecade(myWorking_Obama_D)

# Step 3: get quantiles 
myDecade_Obama_D <- getQuartiles(myDecade_Obama_D)

# Step 4: avg % for each quartile -- funding per cap 
myAmount_All_Obama_D <- getAmountDF(myDecade_Obama_D)

# Step 5: avg % for each quartile -- quantity per cap 
myQuantity_All_Obama_D <- getQuantityDF(myDecade_Obama_D)

# Step 6: label grant type and years 
myAmount_All_Obama_D <- myAmount_All_Obama_D %>%
  mutate(grants_included = "Development") %>%
  mutate(years = "2009-2018")

myQuantity_All_Obama_D <- myQuantity_All_Obama_D %>%
  mutate(grants_included = "Development") %>%
  mutate(years = "2009-2018")

# Step 7: add to main dataframes
myAmount <- bind_rows(myAmount, myAmount_All_Obama_D) 
myQuantity <- bind_rows(myQuantity, myQuantity_All_Obama_D)

# Step 8: remove variables 
rm(myWorking_Obama_D, myDecade_Obama_D, myAmount_All_Obama_D, myQuantity_All_Obama_D)






# FORMATTING TABLES! ----------------------------------------------------------

# AMOUNT OF FUNDING

# rename
myAmount <- myAmount %>%
  rename(quants = decade_amount_per_cap_quants) 

# order
myAmount <- myAmount %>%
  arrange(years, grants_included, quants) 

#pivot demographics longer 
myAmount_long <- myAmount%>%
  pivot_longer(!c(grants_included, years, quants), names_to = "demographic", values_to = "Average")

# pivot quarts wider by grant type
myAmount_wide <- pivot_wider(myAmount_long, names_from = grants_included, values_from = "Average")

# pivot quarts wider by quant
myAmount_wide_again <- pivot_wider(myAmount_wide, names_from = quants, values_from = c("All", "Acquisition", "Development"))



# QUANTITY OF GRANTS 

# rename
myQuantity <- myQuantity %>%
  rename(quants = decade_quantity_per_cap_quants) 

# order
myQuantity <- myQuantity %>%
  arrange(years, grants_included, quants) 

#pivot demographics longer 
myQuantity_long <- myQuantity%>%
  pivot_longer(!c(grants_included, years, quants), names_to = "demographic", values_to = "Average")

# pivot quarts wider by grant type
myQuantity_wide <- pivot_wider(myQuantity_long, names_from = grants_included, values_from = "Average")

# pivot quarts wider by quant
myQuantity_wide_again <- pivot_wider(myQuantity_wide, names_from = quants, values_from = c("All", "Acquisition", "Development"))

# WRITING TO EXCELL
write.xlsx(myAmount_wide_again, 'Exploratory_Output/excell_work/amount.xlsx', overwrite = TRUE)
write.xlsx(myQuantity_wide_again, 'Exploratory_Output/excell_work/quantity.xlsx', overwrite = TRUE)





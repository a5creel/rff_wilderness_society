# Andie Creel / June 9th, 2021 / LWCF project for RFF
# Purpose of Script: Taking raw data sets and making them workable 

# ----------------

library(vroom)
library(plyr)
library(dplyr)
library(tidyr)
library(stringr)
library(usmap)
library(tidycensus)

# -------------- AGGREGATED LWCF (not long by year) ---------------

#reading in the files I downloaded from TWS website
one.1 <- vroom("Datasets/raw_data/State LWCF by County (1) copy.csv")
one.2 <- vroom("Datasets/raw_data/State LWCF by County (1) copy 2.csv")
one.3 <- vroom("Datasets/raw_data/State LWCF by County (1).csv")
two <- vroom("Datasets/raw_data/State LWCF by County (2).csv")
three <- vroom("Datasets/raw_data/State LWCF by County (3).csv")
four <- vroom("Datasets/raw_data/State LWCF by County (4).csv")
five <- vroom("Datasets/raw_data/State LWCF by County (5).csv")
six <- vroom("Datasets/raw_data/State LWCF by County (6).csv")

myCounty <- bind_rows(one.1, one.2, one.3, two, three, four, five, six) %>%
  distinct() 

rm(one.1, one.2, one.3, two, three, four, five, six)

vroom_write(myCounty, "Datasets/clean_data/creel_lwcf_tws.csv")

# -------------- IPUMS HISTORIC CENSUS DATA ---------------

# IPUMS Dataset
ipums <- vroom("Datasets/raw_data/nhgis0003_csv/nhgis0003_ts_nominal_county.csv")
ipums_labs <-ipums[1,] #Getting descriptions

ipums <- ipums[-1,] # deleting descriptions from data

# FIPS, Population
# AV0AA -- Persons: Total
# A57AA -- Persons: Urban
# A57AD -- Persons: Rural
# B18AA -- Persons: White (single race)
# B18AB -- Persons: Black or African American (single race)
# B18AC -- American Indian and Alaska Native (single race)
# B18AD -- Asian and Pacific Islander and Other Race (single race)
# B18AD -- Persons: Two or More Races
# A35AA -- Persons: Hispanic or Latino
# B79AA -- Median income in previous year: Households
# AX6AA -- Persons: Poverty status is determined
# CL6AA -- Poverty status is determined ~ Income below poverty level

ipums_thin <- ipums %>%
  dplyr::mutate(FIPS = paste0(STATEFP, COUNTYFP)) %>%   #getting county fips
  #getting variables I want
  select(FIPS, starts_with("AV0AA"), starts_with("A57AA"), starts_with("A57AD"),
         starts_with("B18AA"), starts_with("B18AB"), starts_with("B18AC"), starts_with("B18AD"), 
         starts_with("B18AE"), starts_with("A35AA"), starts_with("B79AA"), starts_with("AX6AA"),
         starts_with("CL6AA")) %>%
  select(-ends_with("FP"), -ends_with("M"), -ends_with("5")) %>% #dropping excess variables 
  pivot_longer(!FIPS, names_to = "variable", values_to = "value") %>% # Pivoting longer so that we can get the name as a variable 
  dplyr::mutate(year = str_sub(variable, -4)) %>% #extracting year from variable name and making new variable
  dplyr::mutate(variable = str_sub(variable, 1,-5)) %>% #trimming year off variable name
  pivot_wider(id_cols = c(FIPS, year), names_from = variable, values_from = value) %>% # pivoting wider now that we have a year column
  dplyr::rename(population = AV0AA) %>% #renaming variables
  dplyr::rename(urban = A57AA) %>%
  dplyr::rename(rural = A57AD) %>%
  dplyr::rename(white = B18AA) %>%
  dplyr::rename(black = B18AB) %>%
  dplyr::rename(native = B18AC) %>%
  dplyr::rename(asian = B18AD) %>%
  dplyr::rename(two_race = B18AE) %>%
  dplyr::rename(hispanic = A35AA) %>%
  dplyr::rename(med_income_house = B79AA) %>%
  dplyr::rename(people_pov = AX6AA) %>%
  dplyr::rename(inc_below_pov = CL6AA) %>%
  dplyr::select(-people_pov) %>% #dropping bc it's almost the same as total pop
  dplyr::mutate(population = as.numeric(population)) %>% #getting variable as numbers 
  dplyr::mutate(urban = as.numeric(urban)) %>%
  dplyr::mutate(rural = as.numeric(rural)) %>%
  dplyr::mutate(white = as.numeric(white)) %>%
  dplyr::mutate(black = as.numeric(black)) %>%
  dplyr::mutate(native = as.numeric(native)) %>%
  dplyr::mutate(asian = as.numeric(asian)) %>%
  dplyr::mutate(two_race = as.numeric(two_race)) %>%
  dplyr::mutate(hispanic = as.numeric(hispanic)) %>%
  dplyr::mutate(med_income_house = as.numeric(med_income_house)) %>%
  dplyr::mutate(inc_below_pov = as.numeric(inc_below_pov)) %>%
  dplyr::mutate(urban_pct = urban / population) %>% #calculating percent of county of certain demographics
  dplyr::mutate(rural_pct = rural / population) %>%
  dplyr::mutate(white_pct = white / population) %>%
  dplyr::mutate(black_pct = black / population) %>%
  dplyr::mutate(native_pct = native / population) %>%
  dplyr::mutate(asian_pct = asian / population) %>%
  dplyr::mutate(two_race_pct = two_race / population) %>%
  dplyr::mutate(hispanic_pct = hispanic / population) %>%
  dplyr::mutate(inc_below_pov_pct = inc_below_pov / population)
  
vroom_write(ipums_thin, "Datasets/clean_data/ipums_decennial.csv")

# -------------- DISAGGREGATED LWCF (long by project) ---------------
# provided by margaret 
myLWCF <- vroom("Datasets/clean_data/LWCF_features_from TWS.csv")


# ----- MERGING LWCF DATASET THAT'S LONG BY PARK WITH DECENNIAL CENSUS ----- #

# Pulling in dataset county.map so that I can merge a FIPS code to the LWCF data margaret provided 
# (data set that was scraped from TWS website which is long by project)

data(county.map)

#Getting relevant variables
county.map <- county.map %>%
  dplyr::select(STATE, NAME, region) %>%
  distinct() %>%
  dplyr::mutate(region = as.character(region)) %>%
  dplyr::mutate(region = if_else(nchar(region)==4, paste0('0', region), region))

#prep for merge
myLWCF <- myLWCF %>%
  dplyr::mutate(county = str_to_title(county)) %>%
  dplyr::mutate(state_fips = fips(state = statefull))

  
#merge to get county fips (ATTN: not able to identify 4,490 projects with a FIPS code ... )
myLWCF <- dplyr::left_join(myLWCF, county.map, by = c("county" = "NAME", "state_fips" = "STATE")) %>%
  dplyr::rename(county_fips = region)

#Getting nearest decade
myLWCF <- myLWCF %>%
  dplyr::mutate(fiscal_year = str_sub(fiscal_year, 1, 4)) %>%
  dplyr::mutate(fiscal_year = as.numeric(fiscal_year)) %>%
  dplyr::mutate(fiscal_year = if_else(fiscal_year == 1965, fiscal_year + 1, fiscal_year)) %>%
  dplyr::mutate(merge_year = round_any(fiscal_year, 10)) %>%
  dplyr::mutate(merge_year = as.character(merge_year))
  

#ATTN: MISSING 5,421 DEMOGRAPHICS FOR PROJECTS
myWorking <- left_join(myLWCF, ipums_thin, by = c("county_fips" = "FIPS", "merge_year" = "year"))


vroom_write(myWorking, "Datasets/clean_data/lwcf_tws_projects_decennial.csv")


test2 <- left_join(myLWCF, ipums_thin, by = c("county_fips" = "FIPS", "merge_year" = "year")) %>%
  filter(merge_year == 1990 | merge_year == 2000 | merge_year == 2010| merge_year == 2020)


hist(as.numeric(myWorking$fiscal_year))


# # -------------- 2010 census (may not need) ---------------
# # Census stuff 
# census_api_key(key = 'b5fdd956635e498b0cc3288ebf9dfc802abbc93a', overwrite = TRUE, install = TRUE)
# # readRenviron("~/.Renviron")
# # Sys.getenv("CENSUS_API_KEY")
# 
# # Decennial variables 
# v10.dec <- load_variables(2010, dataset = "sf1", cache = TRUE)
# 
# # Variables from decennial census 
# # P001001: population 
# # H002002: Urban
# # H002005: rural 
# # P003001: total race
# # P003002: white alone
# # P003003: black
# # P003004: American Indian and Alaska Native alone
# # P003005: Asian alone
# # P003006: Native Hawaiian and Other Pacific Islander alone
# # P003007: Some Other Race alone
# # P003008: Two or More Races
# # P004001: Total Hispanic or Latino 
# # P004003: Hispanic or Latino
# 
# v5.acs <- load_variables(2010, dataset = "acs5", cache = TRUE)
# 
# # Variables from american community survey
# # B06011_001: Median income in the past 12 months
# # HAVEN'T FOUND POVERTY STATUS VARIABLE
# 
# 
# dec10 <- get_decennial(geography = "county", 
#                        variables = "P013001", 
#                        year = 2010)
# 




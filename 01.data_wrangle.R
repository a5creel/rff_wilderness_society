library(vroom)
library(dplyr)
library(tidyr)
library(stringr)
library(usmap)
library(tidycensus)
# ----
library(choroplethr)
library(choroplethrMaps)
library(usdata)
data(continental_us_states)
data(county.map)
# -----

# -------------- AGGREGATED LWCF (not long by year) ---------------

#reading in the files I downloaded 
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

# Getting LWCF data ready for maps ------------
myCounty.map <- myCounty %>%
  mutate(Name = str_to_title(Name)) %>%
  mutate(state.fips = fips(state = State))

# Getting map counties ready to merge in
county.map <- county.map %>%
  dplyr::select(STATE, NAME, region) %>%
  distinct()

# Merging so LWCF is mapable
myCounty.map <- left_join(myCounty.map, county.map, by = c("state.fips" = "STATE", "Name" = "NAME")) %>%
  filter(!is.na(region))


# Total projects ---------------
# Choosing which variable to map
myCounty.map <- myCounty.map %>%
  mutate(value = `Total Projects`)
county_choropleth(myCounty.map,
                 title = "LWCF: Total Projects per County") 

# Per Capital Spending ---------------
# Choosing which variable to map
myCounty.map <- myCounty.map %>%
  mutate(value = `Per Capita LWCF Spending`)
county_choropleth(myCounty.map,
                  title = "LWCF: Per Capita LWCF Spending") 

# Total Spending ---------------
# Choosing which variable to map
myCounty.map <- myCounty.map %>%
  mutate(value = `Total LWCF Dollars`)
county_choropleth(myCounty.map,
                  title = "LWCF: Total LWCF Spending") 


# -------------- DISAGGREGATED LWCF (long by project) ---------------
# provided by margaret 
myLWCF <- vroom("Datasets/clean_data/LWCF_features_from TWS.csv")


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
  mutate(FIPS = paste0(STATEFP, COUNTYFP)) %>%   #getting county fips
  #getting variables I want
  select(FIPS, starts_with("AV0AA"), starts_with("A57AA"), starts_with("A57AD"),
         starts_with("B18AA"), starts_with("B18AB"), starts_with("B18AC"), starts_with("B18AD"), 
         starts_with("B18AE"), starts_with("A35AA"), starts_with("B79AA"), starts_with("AX6AA"),
         starts_with("CL6AA")) %>%
  select(-ends_with("FP"), -ends_with("M"), -ends_with("5")) %>% #dropping excess variables 
  pivot_longer(!FIPS, names_to = "variable", values_to = "value") %>% # Pivoting longer so that we can get the name as a variable 
  mutate(year = str_sub(variable, -4)) %>% #extracting year from variable name and making new variable
  mutate(variable = str_sub(variable, 1,-5)) %>% #trimming year off variable name
  pivot_wider(id_cols = c(FIPS, year), names_from = variable, values_from = value) %>% # pivoting wider now that we have a year column
  rename(population = AV0AA) %>%
  rename(urban = A57AA) %>%
  rename(rural = A57AD) %>%
  rename(white = B18AA) %>%
  rename(black = B18AB) %>%
  rename(native = B18AC) %>%
  rename(asian = B18AD) %>%
  rename(two_race = B18AE) %>%
  rename(hispanic = A35AA) %>%
  rename(med_income_house = B79AA) %>%
  rename(people_pov = AX6AA) %>%
  rename(inc_below_pov = CL6AA)
  
vroom_write(ipums_thin, "Datasets/clean_data/ipums_decennial.csv")


# -------------- 2010 census (may not need) ---------------
# Census stuff 
census_api_key(key = 'b5fdd956635e498b0cc3288ebf9dfc802abbc93a', overwrite = TRUE, install = TRUE)
readRenviron("~/.Renviron")
Sys.getenv("CENSUS_API_KEY")

# Decennial variables 
v10.dec <- load_variables(2010, dataset = "sf1", cache = TRUE)

# Variables from decennial census 
# P001001: population 
# H002002: Urban
# H002005: rural 
# P003001: total race
# P003002: white alone
# P003003: black
# P003004: American Indian and Alaska Native alone
# P003005: Asian alone
# P003006: Native Hawaiian and Other Pacific Islander alone
# P003007: Some Other Race alone
# P003008: Two or More Races
# P004001: Total Hispanic or Latino 
# P004003: Hispanic or Latino

v5.acs <- load_variables(2010, dataset = "acs5", cache = TRUE)

# Variables from american community survey
# B06011_001: Median income in the past 12 months
# HAVEN'T FOUND POVERTY STATUS VARIABLE


dec10 <- get_decennial(geography = "county", 
                       variables = "P013001", 
                       year = 2010)





# Andie Creel / June 16th, 2021 / LWCF project for RFF
# Purpose of Script: Taking raw data sets and making them workable 

# ----------------

library(plyr)
library(dplyr)
library(tidyr)
library(stringr)
library(usmap)
library(tidycensus)
library(vroom)
library(choroplethrMaps) # to pull in the county data (FIPS codes etc)

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




# -------------- IPUMS HISTORIC CENSUS DATA (take two)---------------

# IPUMS Dataset
ipums <- vroom("Datasets/raw_data/nhgis0006_csv/nhgis0006_ts_nominal_county.csv")
ipums_labs <-ipums[1,] #Getting descriptions

ipums <- ipums[-1,] # deleting descriptions from data

# AV0AA: y       Persons: Total
# A57AA: y       Persons: Urban
# A57AB: n       Persons: Urban--Inside urbanized areas
# A57AC: n       Persons: Urban--Outside urbanized areas (in urban clusters)
# A57AD: y       Persons: Rural
# B18AA: y       Persons: White (single race)
# B18AB: y       Persons: Black or African American (single race)
# B18AC: y       Persons: American Indian and Alaska Native (single race)
# B18AD: y       Persons: Asian and Pacific Islander and Other Race (single race)
# B18AE: n       Persons: Two or More Races
# A35AA: y       Persons: Hispanic or Latino
# B69AA: y        Persons: 25 years and over ~ Less than 9th grade
# B69AB: y       Persons: 25 years and over ~ 9th grade to 3 years of college (until 1980) or to some college or associate's degree (since 1990)
# B69AC: n       Persons: 25 years and over ~ 4 or more years of college (until 1980) or bachelor's degree or higher (since 1990)
# B84AA: n       Persons: 16 years and over ~ In labor force
# B84AB: n       Persons: 16 years and over ~ In labor force--In Armed Forces
# B84AC: n       Persons: 16 years and over ~ In labor force--Civilian
# B84AD: n       Persons: 16 years and over ~ In labor force--Civilian--Employed
# B84AE: n       Persons: 16 years and over ~ In labor force--Civilian--Unemployed
# B84AF: y       Persons: 16 years and over ~ Not in labor force
# B79AA: y       Median income in previous year: Households
# BD5AA: y       Per capita income in previous year
# CL6AA: y       Persons: Poverty status is determined ~ Income below poverty level

# Additional variables -------------------

# Margins of Error 
# (Provided for American Community Survey data only) 
# 
# Table 1: (AV0) Total Population
# AV0AAM:      Margin of error: Persons: Total
# 
# Table 2: (B69) Persons 25 Years and Over by Educational Attainment [3]
# B69AAM:      Margin of error: Persons: 25 years and over ~ Less than 9th grade
# B69ABM:      Margin of error: Persons: 25 years and over ~ 9th grade to 3 years of college (until 1980) or to some college or associate's degree (since 1990)
#         B69ACM:      Margin of error: Persons: 25 years and over ~ 4 or more years of college (until 1980) or bachelor's degree or higher (since 1990)
# 
# Table 3: (B84) Persons 16 Years and Over by Labor Force and Employment Status [6]
# B84AAM:      Margin of error: Persons: 16 years and over ~ In labor force
# B84ABM:      Margin of error: Persons: 16 years and over ~ In labor force--In Armed Forces
# B84ACM:      Margin of error: Persons: 16 years and over ~ In labor force--Civilian
# B84ADM:      Margin of error: Persons: 16 years and over ~ In labor force--Civilian--Employed
# B84AEM:      Margin of error: Persons: 16 years and over ~ In labor force--Civilian--Unemployed
# B84AFM:      Margin of error: Persons: 16 years and over ~ Not in labor force
# 
# Table 4: (B79) Median Household Income in Previous Year
# B79AAM:      Margin of error: Median income in previous year: Households
# 
# Table 5: (BD5) Per Capita Income in Previous Year
# BD5AAM:      Margin of error: Per capita income in previous year
# 
# Table 6: (CL6) Persons* below Poverty Level in Previous Year
# CL6AAM:      Margin of error: Persons: Poverty status is determined ~ Income below poverty level

# stop of additional variables -----------------

ipums_thin <- ipums %>%
  dplyr::mutate(FIPS = paste0(STATEFP, COUNTYFP)) %>%
  dplyr::select(FIPS, YEAR, AV0AA, A57AA, A57AD, B18AA, B18AB, B18AC, B18AD, A35AA, B69AA, B69AB, B84AF, B79AA, BD5AA, CL6AA) %>%
  
  dplyr::rename(population = AV0AA) %>% #renaming variables
  dplyr::rename(urban = A57AA) %>%
  dplyr::rename(rural = A57AD) %>%
  dplyr::rename(white = B18AA) %>%
  dplyr::rename(black = B18AB) %>%
  dplyr::rename(native = B18AC) %>%
  dplyr::rename(asian = B18AD) %>%
  dplyr::rename(hispanic = A35AA) %>%
  dplyr::rename(less9thGrade = B69AA) %>%
  dplyr::rename(ninthToSomeCollege = B69AB) %>%
  dplyr::rename(unemployed = B84AF) %>%
  dplyr::rename(med_income_house = B79AA) %>%
  dplyr::rename(per_cap_income = BD5AA) %>%
  dplyr::rename(inc_below_pov = CL6AA) %>%
  
  dplyr::mutate(population = as.numeric(population)) %>% #getting variable as numbers 
  dplyr::mutate(urban = as.numeric(urban)) %>%
  dplyr::mutate(rural = as.numeric(rural)) %>%
  dplyr::mutate(white = as.numeric(white)) %>%
  dplyr::mutate(black = as.numeric(black)) %>%
  dplyr::mutate(native = as.numeric(native)) %>%
  dplyr::mutate(asian = as.numeric(asian)) %>%
  dplyr::mutate(hispanic = as.numeric(hispanic)) %>%
  dplyr::mutate(less9thGrade = as.numeric(less9thGrade)) %>%
  dplyr::mutate(ninthToSomeCollege = as.numeric(ninthToSomeCollege)) %>%
  dplyr::mutate(unemployed = as.numeric(unemployed)) %>%
  dplyr::mutate(med_income_house = as.numeric(med_income_house)) %>%
  dplyr::mutate(per_cap_income = as.numeric(per_cap_income)) %>%
  dplyr::mutate(inc_below_pov = as.numeric(inc_below_pov)) %>%
  
  dplyr::mutate(noCollegeDegree = less9thGrade + ninthToSomeCollege) %>%   #constructing less than college degree 
  
  dplyr::mutate(urban_pct = urban / population) %>% #calculating percent of county of certain demographics
  dplyr::mutate(rural_pct = rural / population) %>%
  dplyr::mutate(white_pct = white / population) %>%
  dplyr::mutate(black_pct = black / population) %>%
  dplyr::mutate(native_pct = native / population) %>%
  dplyr::mutate(asian_pct = asian / population) %>%
  # dplyr::mutate(two_race_pct = two_race / population) %>%
  dplyr::mutate(hispanic_pct = hispanic / population) %>%
  dplyr::mutate(noCollegeDegree_pct = noCollegeDegree / population) %>%
  dplyr::mutate(unemployed_pct = unemployed / population) %>%
  dplyr::mutate(inc_below_pov_pct = inc_below_pov / population)


impus_nas <- ipums_thin %>%
  dplyr::summarise_all(funs(sum(is.na(.))))

# ATTN: median income per household and per capita income are not available in 1970 and 2010 
# ATTN: 2008-2012 YEAR field is pretty much useless

ipums_thin <- ipums_thin %>%
  dplyr::filter(YEAR != "2008-2012") %>%
  dplyr::filter(YEAR != "2010")

impus_nas_2 <- ipums_thin %>%
  dplyr::summarise_all(funs(sum(is.na(.))))

vroom_write(ipums_thin, "Datasets/clean_data/ipums_decennial.csv")


# population by year, provided by year -------------------------------

# myNBER <- vroom("https://data.nber.org/census/popest/county_population.csv") #works w/ internet
myNBER <- vroom("Datasets/raw_data/county_population.csv")


# this script works through 2009, going to have to do something slightly different for >2010 
myNBER_clean_1 <- myNBER %>% 
  filter(nchar(fipsco) != 5) %>%
  filter(fipsco != "000") %>%
  dplyr::select(fips, starts_with("pop"), -pop20104, -pop2010, -pop2011, -pop2012, -pop2013, -pop2014, -pop19904) %>%
  pivot_longer(!fips, names_to = "variable", values_to = "value") %>% # Pivoting longer so that we can get the name as a variable 
  dplyr::mutate(year = str_sub(variable, -4)) %>% #extracting year from variable name and making new variable
  dplyr::mutate(variable = str_sub(variable, 1,-5)) %>% #trimming year off variable name
  pivot_wider(id_cols = c(fips, year), names_from = variable, values_from = value)  # pivoting wider now that we have a year column


# this script works through >2010
myNBER_clean_2 <- myNBER %>% 
  filter(nchar(fipsco) == 5) %>%
  dplyr::select(fips, -pop20104, pop2010, pop2011, pop2012, pop2013, pop2014, -pop19904) %>%
  pivot_longer(!fips, names_to = "variable", values_to = "value") %>% # Pivoting longer so that we can get the name as a variable 
  dplyr::mutate(year = str_sub(variable, -4)) %>% #extracting year from variable name and making new variable
  dplyr::mutate(variable = str_sub(variable, 1,-5)) %>% #trimming year off variable name
  pivot_wider(id_cols = c(fips, year), names_from = variable, values_from = value)  # pivoting wider now that we have a year column


myNBER_clean <- bind_rows(myNBER_clean_1, myNBER_clean_2) %>%
  rename(annual_population = pop)

vroom_write(ipums_thin, "Datasets/clean_data/annual_population.csv")

# Getting 2010 decennial ----------

# Census stuff
census_api_key(key = 'b5fdd956635e498b0cc3288ebf9dfc802abbc93a', overwrite = TRUE, install = TRUE)
# readRenviron("~/.Renviron")
# Sys.getenv("CENSUS_API_KEY")

v10.dec_1 <- load_variables(2010, dataset = "sf1", cache = TRUE)
v10.dec_2 <- load_variables(2010, dataset = "sf2", cache = TRUE)


myDec <- get_decennial(geography = "county", variables = vars, geometry = FALSE, cache_table = TRUE, year = 2010)



us_population <- get_estimates(geography = "county", product = "population", year = 2010)

# Variables from decennial census
# P001001: population
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
# H002002: Urban
# H002005: rural









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


# ERS urban rural code 

myERS <- vroom("Datasets/clean_data/ERS_urban_rural.csv")

myERS <- myERS %>%
  dplyr::select(FIPS, Population_2010, RUCC_2013, Description)

myLWCF <- left_join(myLWCF, myERS, by = c("county_fips" = "FIPS"))


#ATTN: MISSING 5,421 DEMOGRAPHICS FOR PROJECTS, 979 are for merge year 2020
myWorking <- left_join(myLWCF, ipums_thin, by = c("county_fips" = "FIPS", "merge_year" = "year"))



# -------- getting aggregate county data ready ----------#
#prep for merge
myCounty <- myCounty %>%
  dplyr::mutate(Name = str_to_title(Name)) %>%
  dplyr::mutate(state_fips = fips(state = State))


#merge to get county fips (ATTN: not able to identify 4,490 projects with a FIPS code ... )
myCounty <- dplyr::left_join(myCounty, county.map, by = c("Name" = "NAME", "state_fips" = "STATE")) %>%
  dplyr::rename(county_fips = region)


myCounty <- left_join(myCounty, myERS, by = c("county_fips" = "FIPS"))


# -------------- 2015 American community survey ---------------

# Pulling in for merge year 2020 because I don't have the census info in the IPUMS dataset for any year greater than 2014

# Census stuff
census_api_key(key = 'b5fdd956635e498b0cc3288ebf9dfc802abbc93a', overwrite = TRUE, install = TRUE)
# readRenviron("~/.Renviron")
# Sys.getenv("CENSUS_API_KEY")

# Decennial -------

# Decennial variables: NOTE: already got from IPUMS
# v10.dec <- load_variables(2010, dataset = "sf1", cache = TRUE)

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


# ACS -------

v5.acs <- load_variables(2015, dataset = "acs5", cache = TRUE)

# Variables from American community survey

# B01003_001: population
# ATTN: DIDN'T FIND URBAN 
# ATTN: DIDN'T FIND RURAL
# B02001_001: total race
# B02001_002: white alone
# B02001_003: black
# B02001_004: American Indian and Alaska Native alone
# B02001_005: Asian alone
# B02001_006: Native Hawaiian and Other Pacific Islander alone
# B02001_007: Some Other Race alone
# B02001_008: Two or More Races
# B03001_001: Total Hispanic or Latino
# B03001_012: Hispanic or Latino
# B15003_018: GED
# B15003_017: Highschool diploma

# B06011_001: Median income in the past 12 months

# CONSTRUCTION OF POVERTY LEVEL IN 2015
# C17002_001 -- TOTAL: RATIO OF INCOME TO POVERTY LEVEL IN THE PAST 12 MONTHS
# C17002_002 -- Under .50: RATIO OF INCOME TO POVERTY LEVEL IN THE PAST 12 MONTHS
# C17002_003 -- .50 to .99: RATIO OF INCOME TO POVERTY LEVEL IN THE PAST 12 MONTHS

#Getting count of people of different races/ethnicities
population <- "B01003_001"
total_race <- "B02001_001" #population on census block
white <- "B02001_002" #Estimate!!Total!!White alone
black <- 	"B02001_003" #	Estimate!!Total!!Black or African American alone
native <- "B02001_004" #	Estimate!!Total!!American Indian and Alaska Native alone
asian <- "B02001_005" #Estimate!!Total!!Asian alone
islander <- "B02001_006" #Estimate!!Total!!Native Hawaiian and Other Pacific Islander alone
other <- "B02001_007" #Estimate!!Total!!Some other race alone
two_race <- "B02001_008"
total_hispanic <- "B03001_001"
hispanic <- "B03002_012"
med_income <- "B06011_001"
pov_total <- "C17002_001"
pov_under_.5 <- "C17002_002"
pov_.5_.99 <- "C17002_003"
GED <- "B15003_018"
highschool <- "B15003_017"
bachelors <- "B15003_022"
total_school <- "B15003_001"

#creating list
vars <- list(population, total_race, white, black, native, asian, islander, other, two_race, total_hispanic, hispanic, med_income, pov_total, pov_under_.5, pov_.5_.99, GED, highschool, bachelors, total_school)
rm(population, total_race, white, black, native, asian, islander, other, two_race, total_hispanic, hispanic, med_income, pov_total, pov_under_.5, pov_.5_.99, GED, highschool, bachelors, total_school)

#pulling data from census
ACS <- get_acs(geography = "county", variables = vars, geometry = FALSE, cache_table = TRUE, year = 2015)

#pivot data wide
myACS <- ACS %>%
  select(-moe) %>%
  pivot_wider(names_from = 'variable', values_from = c('estimate')) %>%
  distinct() %>%
  dplyr::mutate(year = 2015) %>%
  dplyr::rename(population = B01003_001) %>%
  dplyr::mutate(urban = NA) %>%
  dplyr::mutate(rural = NA) %>%
  dplyr::select(-B02001_001) %>% #dropping total_race bc it is the same as total pop
  dplyr::rename(white = B02001_002) %>%
  dplyr::rename(black = B02001_003) %>%
  dplyr::rename(native = B02001_004) %>%
  dplyr::rename(asian = B02001_005) %>%
  dplyr::select(-B02001_006) %>% # dropping islander bc I don't have it for earlier years
  dplyr::select(-B02001_007) %>%# dropping other bc I don't have it for earlier years
  dplyr::rename(two_race = B02001_008) %>%
  dplyr::select(-B03001_001) %>% #dropping total_hispanic bc it is the same as total pop
  dplyr::rename(hispanic = B03002_012) %>%
  dplyr::rename(med_income_house = B06011_001) %>%
  dplyr::select(-C17002_001) %>% #dropping pov_total bc it's almost the same as total pop
  dplyr::rename(pov_under_.5 = C17002_002) %>%
  dplyr::rename(pov_.5_.99 = C17002_003) %>%
  dplyr::rename(GED = B15003_018) %>%
  dplyr::rename(highschool = B15003_017) %>%
  dplyr::rename(bachelors = B15003_022) %>%
  dplyr::rename(total_school = B15003_001) %>%
  dplyr::mutate(inc_below_pov = pov_under_.5 + pov_.5_.99) %>%
  dplyr::select(-pov_.5_.99, -pov_under_.5, -NAME) %>%
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
  dplyr::mutate(highschool = as.numeric(highschool)) %>%
  dplyr::mutate(GED = as.numeric(GED)) %>%
  dplyr::mutate(bachelors = as.numeric(bachelors)) %>%
  dplyr::mutate(total_school = as.numeric(total_school)) %>%
  dplyr::mutate(urban_pct = urban / population) %>% #calculating percent of county of certain demographics
  dplyr::mutate(rural_pct = rural / population) %>%
  dplyr::mutate(white_pct = white / population) %>%
  dplyr::mutate(black_pct = black / population) %>%
  dplyr::mutate(native_pct = native / population) %>%
  dplyr::mutate(asian_pct = asian / population) %>%
  dplyr::mutate(two_race_pct = two_race / population) %>%
  dplyr::mutate(hispanic_pct = hispanic / population) %>%
  dplyr::mutate(inc_below_pov_pct = inc_below_pov / population) %>%
  dplyr::mutate(GED_pct = GED / population) %>%
  dplyr::mutate(highschool_pct = highschool / population) %>%
  dplyr::mutate(noCollegeDegree_pct = (total_school - bachelors) / total_school)
    
    


# -------------- Merging ACS with LWCF and IPUMS for years 2015 and above ---------------

not_all_na <- function(x) any(!is.na(x)) # function to check a whole column to see if it's NA

myWorking_20 <- myWorking %>%
  filter(merge_year == 2020) %>%
  mutate(merge_year = 2015) %>%
  select(where(not_all_na)) # function to check a whole column to see if it's NA 

myWorking_20 <- left_join(myWorking_20, myACS, by = c("county_fips" = "GEOID", "merge_year" = "year"))


# recombining with all years 

myWorking_sub_20 <- myWorking %>%
  filter(merge_year != 2020) %>%
  mutate(merge_year = as.numeric(merge_year))

myWorking <- bind_rows(myWorking_sub_20, myWorking_20)


# Saving to clean data set
vroom_write(myWorking, "Datasets/clean_data/lwcf_tws_projects_IPUMS&ACS.csv")



# ------- combining LWCF with ACS only ------- #

myWorking_acsOnly <- left_join(myLWCF, myACS, by = c("county_fips" = "GEOID"))
vroom_write(myWorking_acsOnly, "Datasets/clean_data/lwcf_tws_projects_onlyACS.csv")


# ------- combining aggregate LWCF with ACS only ------- #

myCounty <- left_join(myCounty, myACS, by = c("county_fips" = "GEOID"))
vroom_write(myCounty, "Datasets/clean_data/aggregate_lwcf_tws_projects_onlyACS.csv")











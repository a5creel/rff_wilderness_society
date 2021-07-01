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
ipums <- vroom("Datasets/raw_data/nhgis0010_csv/nhgis0010_ts_nominal_county.csv")
ipums <- ipums[-1,] # deleting descriptions from data

# Variables described in nhgis0010_ts_nominal_county_codebook.txt
# 
# Table 5: (CY6) Persons by Single Race/Ethnicity [5]
# CY6AA:       Persons: Not Hispanic or Latino ~ White (single race reported or, since 2000, race combinations likely to report this single race)
# CY6AB:       Persons: Not Hispanic or Latino ~ Black or African American (single race reported or, since 2000, race combinations likely to report this single race)
# CY6AC:       Persons: Not Hispanic or Latino ~ American Indian, Alaska Native, Asian, and Pacific Islander (single race reported or, since 2000, race combinations likely to report one of these single races)
# CY6AD:       Persons: Not Hispanic or Latino ~ Some Other Race (single race reported or, since 2000, race combinations likely to report this single race)
# CY6AE:       Persons: Hispanic or Latino


# data manipulation -----------------

ipums_clean <- ipums %>%
  dplyr::mutate(FIPS = paste0(STATEFP, COUNTYFP)) %>%
  dplyr::select(FIPS, YEAR, AV0AA, A57AA, A57AD, starts_with("CY6"), B69AA, B69AB, B84AF, B79AA, BD5AA, CL6AA,
                B37AA, B37AB, B39AA, B39AB, B39AC, B39AD, B39AE, B39AF, B39AG, B39AH, CZ5AB, CZ5AD) %>%
  
  dplyr::rename(population = AV0AA) %>% #renaming variables
  dplyr::rename(urban = A57AA) %>%
  dplyr::rename(rural = A57AD) %>%
  dplyr::rename(white = CY6AA) %>%
  dplyr::rename(black = CY6AB) %>%
  dplyr::rename(asian_native = CY6AC) %>%
  dplyr::rename(other = CY6AD) %>%
  dplyr::rename(hispanic = CY6AE) %>%
  dplyr::rename(less9thGrade = B69AA) %>%
  dplyr::rename(ninthToSomeCollege = B69AB) %>%
  dplyr::rename(unemployed = B84AF) %>%
  dplyr::rename(med_income_house = B79AA) %>%
  dplyr::rename(per_cap_income = BD5AA) %>%
  dplyr::rename(inc_below_pov = CL6AA) %>%
  
  dplyr::rename(owner = B37AA) %>% #renaming housing variable
  dplyr::rename(renter = B37AB) %>%
  dplyr::rename(white_owner = B39AA) %>%
  dplyr::rename(black_owner = B39AB) %>%
  dplyr::rename(POC_owner = B39AC) %>%
  dplyr::rename(other_owner = B39AD) %>%
  dplyr::rename(hispanic_owner = CZ5AB) %>%
  dplyr::rename(white_renter = B39AE) %>%
  dplyr::rename(black_renter = B39AF) %>%
  dplyr::rename(POC_renter = B39AG) %>%
  dplyr::rename(other_renter = B39AH) %>%
  dplyr::rename(hispanic_renter = CZ5AD) %>%
  
  
  dplyr::mutate(population = as.numeric(population)) %>% #getting variable as numbers 
  dplyr::mutate(urban = as.numeric(urban)) %>%
  dplyr::mutate(rural = as.numeric(rural)) %>%
  dplyr::mutate(white = as.numeric(white)) %>%
  dplyr::mutate(black = as.numeric(black)) %>%
  dplyr::mutate(asian_native = as.numeric(asian_native)) %>%
  dplyr::mutate(hispanic = as.numeric(hispanic)) %>%
  dplyr::mutate(less9thGrade = as.numeric(less9thGrade)) %>%
  dplyr::mutate(ninthToSomeCollege = as.numeric(ninthToSomeCollege)) %>%
  dplyr::mutate(unemployed = as.numeric(unemployed)) %>%
  dplyr::mutate(med_income_house = as.numeric(med_income_house)) %>%
  dplyr::mutate(per_cap_income = as.numeric(per_cap_income)) %>%
  dplyr::mutate(inc_below_pov = as.numeric(inc_below_pov)) %>%
  
  dplyr::mutate(owner = as.numeric(owner)) %>% #getting housing variable as numbers
  dplyr::mutate(renter = as.numeric(renter)) %>%
  dplyr::mutate(white_owner = as.numeric(white_owner)) %>%
  dplyr::mutate(black_owner = as.numeric(black_owner)) %>%
  dplyr::mutate(POC_owner = as.numeric(POC_owner)) %>%
  dplyr::mutate(other_owner = as.numeric(other_owner)) %>%
  dplyr::mutate(hispanic_owner = as.numeric(hispanic_owner)) %>%
  dplyr::mutate(white_renter = as.numeric(white_renter)) %>%
  dplyr::mutate(black_renter = as.numeric(black_renter)) %>%
  dplyr::mutate(POC_renter = as.numeric(POC_renter)) %>%
  dplyr::mutate(other_renter = as.numeric(other_renter)) %>%
  dplyr::mutate(hispanic_renter = as.numeric(hispanic_renter)) %>%
  
  dplyr::mutate(noCollegeDegree = less9thGrade + ninthToSomeCollege) %>%   #constructing less than college degree 
  
  dplyr::mutate(urban_pct = urban* 100 / population) %>% #calculating percent of county of certain demographics
  dplyr::mutate(rural_pct = rural* 100 / population) %>%
  dplyr::mutate(white_pct = white* 100 / population) %>%
  dplyr::mutate(black_pct = black* 100 / population) %>%
  dplyr::mutate(asian_native_pct = asian_native* 100 / population) %>%
  dplyr::mutate(hispanic_pct = hispanic* 100 / population) %>%
  dplyr::mutate(noCollegeDegree_pct = noCollegeDegree* 100 / population) %>%
  dplyr::mutate(unemployed_pct = unemployed* 100 / population) %>%
  dplyr::mutate(inc_below_pov_pct = inc_below_pov* 100 / population) %>%

  dplyr::mutate(total_house = owner + renter) %>% #calculating percent of county of certain housing variable
  dplyr::mutate(owner_pct = owner* 100 / total_house) %>%
  dplyr::mutate(renter_pct = renter* 100 / total_house) %>%
  dplyr::mutate(white_owner_pct = white_owner* 100 / total_house) %>%
  dplyr::mutate(black_owner_pct = black_owner* 100 / total_house) %>%
  dplyr::mutate(hispanic_owner_pct = hispanic_owner* 100 / total_house) 

impus_nas <- ipums_clean %>%
  dplyr::summarise_all(funs(sum(is.na(.))))

# ATTN: the 2010 data is really oddly split between with the YEAR field "2008-2012" and "2010"

#1970-2000
# ATTN: median income per household and per capita income are not available in 1970

#1970-2000 is all good! 
ipums_clean_1 <- ipums_clean %>%
  dplyr::filter(YEAR != "2008-2012") %>%
  dplyr::filter(YEAR != "2010")

#  -- cleaning up the odd 2008-2012" and "2010" problem -- #
#breaking it into three chunks (_1, _2, _3), cleaning then recombining 

#function to see if whole row is na 
not_all_na <- function(x) any(!is.na(x))

ipums_clean_2 <- ipums_clean %>%
  dplyr::filter(YEAR == "2008-2012") %>%
  mutate(YEAR = "2010") %>%
  select(where(not_all_na))

ipums_clean_3 <- ipums_clean %>%
  dplyr::filter(YEAR == "2010") %>%
  select(where(not_all_na)) %>%
  select(-population)

ipums_clean_2_3 <- left_join(ipums_clean_2, ipums_clean_3, by = c("FIPS", "YEAR"))

# recombining
ipums_clean <- bind_rows(ipums_clean_1, ipums_clean_2_3) %>%
  dplyr::mutate(YEAR = as.numeric(YEAR))

rm(ipums_clean_1, ipums_clean_2, ipums_clean_3, ipums_clean_2_3, impus_nas, ipums)

#writing to clean_data file
vroom_write(ipums_clean, "Datasets/clean_data/ipums_decennial.csv")


#  ----------------- population by year, provided by NBER -----------------
# only goes from 1970 - 2014

# myNBER <- vroom("https://data.nber.org/census/popest/county_population.csv") #works w/ internet
myNBER <- vroom("Datasets/raw_data/county_population.csv")


# this chunk works through 2009, going to have to do something slightly different for >2010 
myNBER_clean_1 <- myNBER %>% 
  filter(nchar(fipsco) != 5) %>%
  filter(fipsco != "000") %>%
  dplyr::select(fips, starts_with("pop"), -pop20104, -pop2010, -pop2011, -pop2012, -pop2013, -pop2014, -pop19904) %>%
  pivot_longer(!fips, names_to = "variable", values_to = "value") %>% # Pivoting longer so that we can get the name as a variable 
  dplyr::mutate(year = str_sub(variable, -4)) %>% #extracting year from variable name and making new variable
  dplyr::mutate(variable = str_sub(variable, 1,-5)) %>% #trimming year off variable name
  pivot_wider(id_cols = c(fips, year), names_from = variable, values_from = value)  # pivoting wider now that we have a year column


# this chunk works through >2010
myNBER_clean_2 <- myNBER %>% 
  filter(nchar(fipsco) == 5) %>%
  dplyr::select(fips, -pop20104, pop2010, pop2011, pop2012, pop2013, pop2014, -pop19904) %>%
  pivot_longer(!fips, names_to = "variable", values_to = "value") %>% # Pivoting longer so that we can get the name as a variable 
  dplyr::mutate(year = str_sub(variable, -4)) %>% #extracting year from variable name and making new variable
  dplyr::mutate(variable = str_sub(variable, 1,-5)) %>% #trimming year off variable name
  pivot_wider(id_cols = c(fips, year), names_from = variable, values_from = value)  # pivoting wider now that we have a year column


myNBER_clean <- bind_rows(myNBER_clean_1, myNBER_clean_2) %>%
  dplyr::rename(annual_population = pop)
rm(myNBER_clean_1, myNBER_clean_2, myNBER)

#getting a merge year for other data
myNBER_clean <- myNBER_clean %>%
  dplyr::mutate(year = as.numeric(year)) 

#  ----------------- population by year, ACS  (2015 - 2018-----------------

census_api_key(key = 'b5fdd956635e498b0cc3288ebf9dfc802abbc93a', overwrite = TRUE, install = TRUE)
# readRenviron("~/.Renviron")
# Sys.getenv("CENSUS_API_KEY")

myACS <-  get_acs(geography = "county", variables = "B01003_001", geometry = FALSE, cache_table = TRUE, year = 2015)

myACS <- myACS %>%
  dplyr::rename(fips = GEOID) %>%
  dplyr::rename(annual_population = estimate) %>%
  dplyr::mutate(year = 2015) %>%
  dplyr::select(fips, annual_population, year)


for (i in 2016:2018) {
  temp <-  get_acs(geography = "county", variables = "B01003_001", geometry = FALSE, cache_table = TRUE, year = i)
  
  temp <- temp %>%
    dplyr::rename(fips = GEOID) %>%
    dplyr::rename(annual_population = estimate) %>%
    dplyr::mutate(year = i) %>%
    dplyr::select(fips, annual_population, year)
  
  myACS <- bind_rows(myACS, temp)
  rm(temp)
}

#combing NBER data and ACS data to get annual pop from 1970 - 2018
myAnnualPop <- bind_rows(myNBER_clean, myACS) 
rm(myNBER_clean, myACS)

vroom_write(myAnnualPop, "Datasets/clean_data/annual_population.csv")

# ----- MERGING LWCF DATASET THAT'S LONG BY PROJECT WITH NBER (ANNUAL POP 1970-2014) AND  ACS (ANNUAL POP 2015-2018), ----- #

# provided by Margaret 
myLWCF <- vroom("Datasets/clean_data/LWCF_features_from TWS.csv")

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
  dplyr::rename(county_fips = region) %>%
  dplyr::mutate(fiscal_year = as.character(fiscal_year)) %>%
  dplyr::mutate(fiscal_year = str_sub(fiscal_year, 1, 4)) %>%
  dplyr::mutate(fiscal_year = as.numeric(fiscal_year)) %>%
  dplyr::select(-c(objectid, county, grantidelement, grantelementtitle, grantsponsor, congdistrict, relate, statefull)) # Removing useless variables 
rm(county.map)

myPop_LWCF <- left_join(myAnnualPop, myLWCF, by = c("fips" = "county_fips" , "year" = "fiscal_year")) %>%
  mutate(got_grant = if_else(is.na(amount), 0, 1)) %>%
  dplyr::mutate(merge_year = round_any(year, 10)) %>%
  dplyr::mutate(merge_year = if_else(merge_year < 1970, 1970, merge_year)) %>% # don't have data earlier than 1970 but have a few grants from the 60s
  dplyr::mutate(merge_year = if_else(merge_year == 2020, 2010, merge_year))  # don't have decennial data for 2020, so using 2010 for now (only 870 grants post 2014)
  

# -------------- MERGING LWCF, ANNUAL POP AND IPUMS DATA (DECENNIAL CENSUS) -------------- #
#Contains annual population levels for every county, decennial demographic characteristics, all LWCF for which I could get a county

myFinal <- left_join(myPop_LWCF, ipums_clean, by = c("merge_year" = "YEAR", "fips" = "FIPS")) %>%
  distinct()


vroom_write(myFinal, "Datasets/clean_data/lwcf_annualPop_deccenialDemographics.csv")

#unique grants
myTest_1 <- myFinal %>%
  filter(!is.na(amount)) %>%
  select(year, fips, amount, type) %>%
  distinct() 

#grants post 2014
myTest_2 <- myFinal %>%
  filter(!is.na(amount)) %>%
  select(year, fips, amount) %>%
  distinct() %>%
  filter(year >= 2015)

# ****************************************************************** # 
# **************** THIS IS OUR FINAL DATASET, ABOVE **************** #
# ****************************************************************** # 


# # -------------- 2015 American community survey ---------------
# # Pulling in for merge year 2020 because I don't have the census info in the IPUMS dataset for any year greater than 2014
# 
# # Census stuff
# census_api_key(key = 'b5fdd956635e498b0cc3288ebf9dfc802abbc93a', overwrite = TRUE, install = TRUE)
# # readRenviron("~/.Renviron")
# # Sys.getenv("CENSUS_API_KEY")
# 
# # ACS -------
# 
# v5.acs <- load_variables(2015, dataset = "acs5", cache = TRUE)
# 
# # Variables from American community survey
# 
# # B01003_001: population
# # ATTN: DIDN'T FIND URBAN 
# # ATTN: DIDN'T FIND RURAL
# # B02001_001: total race
# # B02001_002: white alone
# # B02001_003: black
# # B02001_004: American Indian and Alaska Native alone
# # B02001_005: Asian alone
# # B02001_006: Native Hawaiian and Other Pacific Islander alone
# # B02001_007: Some Other Race alone
# # B02001_008: Two or More Races
# # B03001_001: Total Hispanic or Latino
# # B03001_012: Hispanic or Latino
# # B15003_018: GED
# # B15003_017: Highschool diploma
# 
# # B06011_001: Median income in the past 12 months
# 
# # CONSTRUCTION OF POVERTY LEVEL IN 2015
# # C17002_001 -- TOTAL: RATIO OF INCOME TO POVERTY LEVEL IN THE PAST 12 MONTHS
# # C17002_002 -- Under .50: RATIO OF INCOME TO POVERTY LEVEL IN THE PAST 12 MONTHS
# # C17002_003 -- .50 to .99: RATIO OF INCOME TO POVERTY LEVEL IN THE PAST 12 MONTHS
# 
# #Getting count of people of different races/ethnicities
# population <- "B01003_001"
# total_race <- "B02001_001" #population on census block
# white <- "B02001_002" #Estimate!!Total!!White alone
# black <- 	"B02001_003" #	Estimate!!Total!!Black or African American alone
# native <- "B02001_004" #	Estimate!!Total!!American Indian and Alaska Native alone
# asian <- "B02001_005" #Estimate!!Total!!Asian alone
# islander <- "B02001_006" #Estimate!!Total!!Native Hawaiian and Other Pacific Islander alone
# other <- "B02001_007" #Estimate!!Total!!Some other race alone
# two_race <- "B02001_008"
# total_hispanic <- "B03001_001"
# hispanic <- "B03002_012"
# med_income <- "B06011_001"
# pov_total <- "C17002_001"
# pov_under_.5 <- "C17002_002"
# pov_.5_.99 <- "C17002_003"
# GED <- "B15003_018"
# highschool <- "B15003_017"
# bachelors <- "B15003_022"
# total_school <- "B15003_001"
# 
# #creating list
# vars <- list(population, total_race, white, black, native, asian, islander, other, two_race, total_hispanic, hispanic, med_income, pov_total, pov_under_.5, pov_.5_.99, GED, highschool, bachelors, total_school)
# rm(population, total_race, white, black, native, asian, islander, other, two_race, total_hispanic, hispanic, med_income, pov_total, pov_under_.5, pov_.5_.99, GED, highschool, bachelors, total_school)
# 
# #pulling data from census
# ACS <- get_acs(geography = "county", variables = vars, geometry = FALSE, cache_table = TRUE, year = 2015)
# 
# #pivot data wide
# myACS <- ACS %>%
#   select(-moe) %>%
#   pivot_wider(names_from = 'variable', values_from = c('estimate')) %>%
#   distinct() %>%
#   dplyr::mutate(year = 2015) %>%
#   dplyr::rename(population = B01003_001) %>%
#   dplyr::mutate(urban = NA) %>%
#   dplyr::mutate(rural = NA) %>%
#   dplyr::select(-B02001_001) %>% #dropping total_race bc it is the same as total pop
#   dplyr::rename(white = B02001_002) %>%
#   dplyr::rename(black = B02001_003) %>%
#   dplyr::rename(native = B02001_004) %>%
#   dplyr::rename(asian = B02001_005) %>%
#   dplyr::select(-B02001_006) %>% # dropping islander bc I don't have it for earlier years
#   dplyr::select(-B02001_007) %>%# dropping other bc I don't have it for earlier years
#   dplyr::rename(two_race = B02001_008) %>%
#   dplyr::select(-B03001_001) %>% #dropping total_hispanic bc it is the same as total pop
#   dplyr::rename(hispanic = B03002_012) %>%
#   dplyr::rename(med_income_house = B06011_001) %>%
#   dplyr::select(-C17002_001) %>% #dropping pov_total bc it's almost the same as total pop
#   dplyr::rename(pov_under_.5 = C17002_002) %>%
#   dplyr::rename(pov_.5_.99 = C17002_003) %>%
#   dplyr::rename(GED = B15003_018) %>%
#   dplyr::rename(highschool = B15003_017) %>%
#   dplyr::rename(bachelors = B15003_022) %>%
#   dplyr::rename(total_school = B15003_001) %>%
#   dplyr::mutate(inc_below_pov = pov_under_.5 + pov_.5_.99) %>%
#   dplyr::select(-pov_.5_.99, -pov_under_.5, -NAME) %>%
#   dplyr::mutate(population = as.numeric(population)) %>% #getting variable as numbers 
#   dplyr::mutate(urban = as.numeric(urban)) %>%
#   dplyr::mutate(rural = as.numeric(rural)) %>%
#   dplyr::mutate(white = as.numeric(white)) %>%
#   dplyr::mutate(black = as.numeric(black)) %>%
#   dplyr::mutate(native = as.numeric(native)) %>%
#   dplyr::mutate(asian = as.numeric(asian)) %>%
#   dplyr::mutate(two_race = as.numeric(two_race)) %>%
#   dplyr::mutate(hispanic = as.numeric(hispanic)) %>%
#   dplyr::mutate(med_income_house = as.numeric(med_income_house)) %>%
#   dplyr::mutate(inc_below_pov = as.numeric(inc_below_pov)) %>%
#   dplyr::mutate(highschool = as.numeric(highschool)) %>%
#   dplyr::mutate(GED = as.numeric(GED)) %>%
#   dplyr::mutate(bachelors = as.numeric(bachelors)) %>%
#   dplyr::mutate(total_school = as.numeric(total_school)) %>%
#   dplyr::mutate(urban_pct = urban* 100 / population) %>% #calculating percent of county of certain demographics
#   dplyr::mutate(rural_pct = rural* 100 / population) %>%
#   dplyr::mutate(white_pct = white* 100 / population) %>%
#   dplyr::mutate(black_pct = black* 100 / population) %>%
#   dplyr::mutate(native_pct = native* 100 / population) %>%
#   dplyr::mutate(asian_pct = asian* 100 / population) %>%
#   dplyr::mutate(two_race_pct = two_race* 100 / population) %>%
#   dplyr::mutate(hispanic_pct = hispanic* 100 / population) %>%
#   dplyr::mutate(inc_below_pov_pct = inc_below_pov* 100 / population) %>%
#   dplyr::mutate(GED_pct = GED* 100 / population) %>%
#   dplyr::mutate(highschool_pct = highschool* 100 / population) %>%
#   dplyr::mutate(noCollegeDegree_pct = (total_school - bachelors)* 100 / total_school)
#     
#     
# 
# 
# # -------------- Merging ACS with LWCF and IPUMS for years 2015 and above ---------------
# 
# not_all_na <- function(x) any(!is.na(x)) # function to check a whole column to see if it's NA
# 
# myWorking_20 <- myWorking %>%
#   dplyr::filter(merge_year == 2020) %>%
#   dplyr::mutate(merge_year = 2015) %>%
#   dplyr::select(where(not_all_na)) # function to check a whole column to see if it's NA 
# 
# myWorking_20 <- left_join(myWorking_20, myACS, by = c("county_fips" = "GEOID", "merge_year" = "year"))
# 
# 
# # recombining with all years 
# 
# myWorking_sub_20 <- myWorking %>%
#   filter(merge_year != 2020) %>%
#   mutate(merge_year = as.numeric(merge_year))
# 
# myWorking <- bind_rows(myWorking_sub_20, myWorking_20)
# 
# 
# # Saving to clean data set
# vroom_write(myWorking, "Datasets/clean_data/lwcf_tws_projects_IPUMS&ACS.csv")
# 
# 
# # ------- combining LWCF with ACS only ------- #
# 
# myWorking_acsOnly <- left_join(myLWCF, myACS, by = c("county_fips" = "GEOID"))
# vroom_write(myWorking_acsOnly, "Datasets/clean_data/lwcf_tws_projects_onlyACS.csv")
# 




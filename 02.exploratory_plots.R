# Andie Creel / June 9th, 2021 / LWCF project for RFF
# Purpose of Script: Initial exploratory plots

#-------
library(vroom)
library(dplyr)
library(tidyr)
library(stringr)
library(usmap)
library(tidycensus)
library(ggplot2)
library(fixest)
library(kableExtra)
# ----
#map specific 
library(choroplethr)
library(choroplethrMaps)
library(usdata)
data(continental_us_states)
data(county.map)
# -----




# MAPS -----------------------------------

myCounty <- vroom("Datasets/clean_data/creel_lwcf_tws.csv")

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
                  title = "Total Projects per County",
                  legend = "Projects")
ggsave(width = 11, height = 8, filename = "Exploratory_Output/maps/total_lwcf_projects.png")

# Per Capital Spending ---------------
# Choosing which variable to map
myCounty.map <- myCounty.map %>%
  mutate(value = `Per Capita LWCF Spending`)
county_choropleth(myCounty.map,
                  title = "Per Capita LWCF Spending",
                  legend = "Dollars")
ggsave(width = 11, height = 8, filename = "Exploratory_Output/maps/per_cap_lwcf_spending.png")

# Total Spending ---------------
# Choosing which variable to map
myCounty.map <- myCounty.map %>%
  mutate(value = `Total LWCF Dollars`)
county_choropleth(myCounty.map,
                  title = "Total LWCF Spending",
                  legend = "Dollars")
ggsave(width = 11, height = 8, filename = "Exploratory_Output/maps/total_lwcf_spending.png")




# Getting data ready --------------------------
myWorking <- vroom("Datasets/clean_data/lwcf_tws_projects_onlyACS.csv")

# additional data cleaning 

# Removing counties whose FIPSs code I didn't identify 
myWorking <- myWorking %>%
  filter(!is.na(county_fips)) %>%
  distinct()

#removing grants we don't know the size of
myWorking <- myWorking %>%
  filter(!is.na(amount))

# Adding metro, nonmetro and rural variable
myUrbanKey <- myWorking %>%
  dplyr::select(RUCC_2013, Description) %>%
  distinct()

myWorking <- myWorking %>%
  mutate(metro = if_else(RUCC_2013 == 1 | RUCC_2013 == 2 | RUCC_2013 == 3, 1, 0)) %>%
  mutate(nonmetro = if_else(RUCC_2013 == 4 | RUCC_2013 == 5 | RUCC_2013 == 6, 1, 0)) %>%
  mutate(rural = if_else(RUCC_2013 == 7 | RUCC_2013 == 8 | RUCC_2013 == 9, 1, 0))


# getting ready for deciles
myWorking <- myWorking %>%
  mutate(type = as.factor(type)) %>%
  mutate(black_dec = quantcut(black_pct, q = 10, na.rm=TRUE)) %>%
  mutate(white_dec = quantcut(white_pct, q = 10, na.rm=TRUE)) %>%
  mutate(asian_dec = quantcut(asian_pct, q = 10, na.rm=TRUE)) %>%
  mutate(native_dec = quantcut(native_pct, q = 10, na.rm=TRUE)) %>%
  mutate(hispanic_dec = quantcut(hispanic_pct, q = 10, na.rm=TRUE)) %>%
  mutate(poverty_dec = quantcut(inc_below_pov_pct, q = 10, na.rm=TRUE)) %>%
  mutate(black_dec = fct_recode(black_dec, 
                                ten = levels(black_dec)[10])) %>%
  mutate(white_dec = fct_recode(white_dec, 
                                ten = levels(white_dec)[10])) %>%  
  mutate(asian_dec = fct_recode(asian_dec, 
                                ten = levels(asian_dec)[10])) %>%  
  mutate(native_dec = fct_recode(native_dec, 
                                 ten = levels(native_dec)[10])) %>%  
  mutate(hispanic_dec = fct_recode(hispanic_dec, 
                                   ten = levels(hispanic_dec)[10])) %>%
  mutate(poverty_dec = fct_recode(poverty_dec, 
                                  ten = levels(poverty_dec)[10]))

# making 0/1 for counties in the highest decile for different races and ethnicities 
myWorking <- myWorking %>%
  mutate(white_county = if_else(white_dec == "ten", 1, 0)) %>%
  mutate(black_county = if_else(black_dec == "ten", 1, 0)) %>%
  mutate(asian_county = if_else(asian_dec == "ten", 1, 0)) %>%
  mutate(native_county = if_else(native_dec == "ten", 1, 0)) %>%
  mutate(hispanic_county = if_else(hispanic_dec == "ten", 1, 0)) 


# Removing useless variables 
myWorking <- myWorking %>%
  dplyr::select(-c(objectid, county, grantidelement, grantelementtitle, grantsponsor, congdistrict, relate))


myWhole <- myWorking %>%
  dplyr::group_by(fiscal_year) %>%
  dplyr::summarise(year_total_amount = sum(amount))

myWorking <- left_join(myWorking, myWhole, by = "fiscal_year")
rm(myWhole)

myWorking <- myWorking %>%
  dplyr::mutate(amount_percent = amount / year_total_amount)




# scatter plots 

ggplot(myWorking, aes(x=white_pct, y=amount_percent)) + 
  geom_point()+
  geom_smooth(method=lm) + 
  theme_minimal() +
  ylab("% of annual funding") +
  ggtitle("Relationship between size of awards and white") +
  xlab("% of county that's white")
ggsave(width = 11, height = 8, filename = "Exploratory_Output/scatter/white_pct_vs_annual_amount_pct.png")


ggplot(myWorking, aes(x=black_pct, y=amount_percent)) + 
  geom_point()+
  geom_smooth(method=lm) +
  theme_minimal() +
  ylab("% of annual funding") +
  ggtitle("Relationship between size of awards and black") +
  xlab("% of county that's  black")
ggsave(width = 11, height = 8, filename = "Exploratory_Output/scatter/black_pct_vs_annual_amount_pct.png")


ggplot(myWorking, aes(x=hispanic_pct, y=amount_percent)) + 
  geom_point()+
  geom_smooth(method=lm) +
  theme_minimal() +
  ylab("% of annual funding") +
  ggtitle("Relationship between size of awards and hispanic") +
  xlab("% of county that's  hispanic")
ggsave(width = 11, height = 8, filename = "Exploratory_Output/scatter/hispanic_pct_vs_annual_amount_pct.png")


ggplot(myWorking, aes(x=asian_pct, y=amount_percent)) + 
  geom_point()+
  geom_smooth(method=lm) +
  theme_minimal() +
  ylab("% of annual funding") +
  ggtitle("Relationship between size of awards and asian") +
  xlab("% of county that's  asian")
ggsave(width = 11, height = 8, filename = "Exploratory_Output/scatter/asian_pct_vs_annual_amount_pct.png")


ggplot(myWorking, aes(x=native_pct, y=amount_percent)) + 
  geom_point()+
  geom_smooth(method=lm) +
  theme_minimal() +
  ylab("% of annual funding") +
  ggtitle("Relationship between size of awards and native") +
  xlab("% of county that's  native")
ggsave(width = 11, height = 8, filename = "Exploratory_Output/scatter/native_pct_vs_annual_amount_pct.png")


ggplot(myWorking, aes(x=inc_below_pov_pct, y=amount_percent)) + 
  geom_point()+
  geom_smooth(method=lm) +
  theme_minimal() +
  ylab("% of annual funding") +
  ggtitle("Relationship between size of awards and poverty") +
  xlab("% of county that's  below poverty line")
ggsave(width = 11, height = 8, filename = "Exploratory_Output/scatter/poverty_pct_vs_annual_amount_pct.png")


ggplot(myWorking, aes(x=noCollegeDegree_pct, y=amount_percent)) + 
  geom_point()+
  geom_smooth(method=lm) +
  theme_minimal() +
  ylab("% of annual funding") +
  ggtitle("Relationship between size of awards and lack of college degree") +
  xlab("% of county that lacks a college degree")
ggsave(width = 11, height = 8, filename = "Exploratory_Output/scatter/noCollege_pct_vs_annual_amount_pct.png")


ggplot(myWorking, aes(x=med_income_house, y=amount_percent)) + 
  geom_point()+
  geom_smooth(method=lm) +
  theme_minimal() +
  ylab("% of annual funding") +
  ggtitle("Relationship between size of awards and median income") +
  xlab("median income per household")
ggsave(width = 11, height = 8, filename = "Exploratory_Output/scatter/medianIncome_pct_vs_annual_amount_pct.png")



# distribution of grants 
ggplot(myWorking_dev, aes(x=log(amount))) + geom_histogram(bins=100)

ggplot(myWorking, aes(x=amount_percent)) + geom_histogram(bins=100)
  


quantile(myWorking$amount, probs = c(0, .1, .25, .5, .75, .9, 1))




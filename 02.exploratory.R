# Andie Creel / June 9th, 2021 / LWCF project for RFF
# Purpose of Script: Initial exploratory analysis, may end up being a working scrap file

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


# Initial regressions -------------------
myWorking <- vroom("Datasets/clean_data/lwcf_tws_projects_decennial.csv")

myWorking <- myWorking %>%
  mutate(type = as.factor(type))

hist(log(myWorking$amount), 10000)

#Lot fits data much better

one <- feols(data = myWorking, log(amount) ~ inc_below_pov_pct + type + population| state_fips + fiscal_year)
two <- feols(data = myWorking, log(amount) ~ black_pct + type + population| state_fips + fiscal_year)
three <- feols(data = myWorking, log(amount) ~ white_pct + type + population| state_fips + fiscal_year)
four <- feols(data = myWorking, log(amount) ~ med_income_house + type + population| state_fips + fiscal_year)
five <- feols(data = myWorking, log(amount) ~ native_pct + type + population| state_fips + fiscal_year)
six <- feols(data = myWorking, log(amount) ~ hispanic_pct + type + population| state_fips + fiscal_year)
seven <- feols(data = myWorking, log(amount) ~  inc_below_pov_pct + black_pct + white_pct + native_pct + hispanic_pct + type + population| state_fips + fiscal_year)


etable(one, 
       two, 
       three,
       four,
       five,
       six,
       seven,
       signifCode = c("***"=0.01, "**"=0.05, "*"=0.10)) %>%
  kbl(caption = "Table") %>%
  kable_classic_2(full_width = F) %>%
  add_footnote(" ***=0.01, **=0.05, *=0.10")

resids <- residuals(one)
summary(one)

fe_test <- 

par(mfrow = c(2, 2))
plot(resids)


#Type A is ACQUISITION
#Type C
#Type D
#Type R




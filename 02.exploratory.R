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









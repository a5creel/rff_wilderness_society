#Goal: generate figure for research day 
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
# Loading environment 
#-------------------------------------------------------------------------------
load("09.suits_environment.RData")
source('09.functions.R')

#-------------------------------------------------------------------------------
# CREATING GRAPHS: National 
#-------------------------------------------------------------------------------

# National results for income
myResult_income <- getSuits(myWorking, x_axis = "Income")
national_income <- getGraph_reDay(myResult_income, x_axis = "Income", caption = "Investment per capita is distributed proportionally across counties with varying average median income. ") 

ggsave(filename ="Figures/research_day/national_median_income.jpeg", plot = national_income,  width = 11, height = 8)

# National results for POC
myResult_poc <- getSuits(myWorking, x_axis = "POC")
national_poc <- getGraph_reDay(myResult_poc, x_axis = "POC")

ggsave(filename ="Figures/research_day/national_POC.jpeg", plot = national_poc, width = 11, height = 8)

# National results for Rural
myResult_rural <- getSuits(myWorking, x_axis = "Rural")
national_rural <- getGraph_reDay(myResult_rural, x_axis = "Rural")

ggsave(filename ="Figures/research_day/national_rural.jpeg", plot = national_rural, width = 11, height = 8)


# blank graph 
ggplot(as.data.frame(myResult_income$Graph), aes(x=myResult_income$Graph$Income_cs, y=Amount_cs)) + 
  geom_point(alpha = 0) +
  labs(x = "fewest -> most", y = "Cumulative investment per capita", size = 60)+
  theme_bw() + 
  geom_abline(intercept = 0, slope = 1, size = 1.5) +  # neutral line + 
  theme(axis.title = element_text(size=25)) +
  ggtitle(paste0("Suits Index: ", 0)) + 
  theme(plot.title = element_text(hjust = 0.1, vjust = -12, size = 25))

ggsave(filename ="Figures/research_day/national_blank.jpeg", width = 11, height = 8)











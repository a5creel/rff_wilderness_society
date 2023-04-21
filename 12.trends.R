#Goal: Create graphs of how suits indexes have changed over the decades
# Andie Creel / Started April, 2023

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

# Loading environment 
load("09.suits_environment.RData")
rm(myMap_income, myMap_poc, myMap_rural, myResult_state, state_dfs)

#-------------------------------------------------------------------------------
# Create dataframes for each decade
#-------------------------------------------------------------------------------

# using the data that's still long by fips and year (previous analysis was on data NOT long by year)
myData <- myWorking_fips_year%>%
  mutate(decade = round(real_year,digits = -1))

#getting vector of decades
decades <-myData %>%
  select(decade) %>%
  unique() 
decades <- as.numeric(decades$decade)

# creating df for each decade 
decade_dfs <- lapply(decades, function(x){
  df <- myData  %>%
    filter(decade == x) %>%
    mutate(amount_per_cap = amount / annual_population) %>%
    group_by(fips) %>%
    mutate(amount_per_cap_avg = sum(amount_per_cap)/54) %>% # average amount per cap
    mutate(avg_med_income_house = mean(med_income_house, na.rm = TRUE)) %>% #average median income
    mutate(avg_poc_pct = mean(poc_pct)) %>%
    mutate(avg_rural_pct = mean(rural_pct))
  return(df)
}) 

# Make a data frame of suits results using getSuits() and list of decade dfs
myResult_decade <- data.frame(matrix(ncol=4, nrow = length(decade_dfs)))
colnames(myResult_decade) <- c("Decade", "Suits_Amount_Income", "Suits_Amount_POC", "Suits_Amount_Rural" )
for (i in 1:length(decade_dfs)) {
  myResult_decade$Decade[i] <- decade_dfs[[i]]$decade[1] # Decade
  myResult_decade$Suits_Amount_Income[i] <- getSuits(decade_dfs[[i]], x_axis = "Income")$Suits_Amount
  myResult_decade$Suits_Amount_POC[i] <- getSuits(decade_dfs[[i]], x_axis = "POC")$Suits_Amount
  myResult_decade$Suits_Amount_Rural[i] <- getSuits(decade_dfs[[i]], x_axis = "Rural")$Suits_Amount
}

# pivot longer
myDecade <- myResult_decade %>%
  rename(POC = Suits_Amount_POC) %>%
  rename(Poverty = Suits_Amount_Income) %>%
  rename(Rural = Suits_Amount_Rural) %>%
  pivot_longer(cols = c(POC, Poverty, Rural), names_to = "Demographic") 

# ggplot 

ggplot(myDecade, aes(x = Decade, y = value)) + 
  geom_line(aes(color = Demographic), size = 1.5) + 
  geom_point(aes(color = Demographic), size = 2.5) + 
  scale_color_manual(values = c("darkred", "steelblue", "darkgreen")) +
  geom_abline(intercept = 0, slope = 0, size = 1, col = "grey", linetype = "longdash") +  
  theme_bw() + 
  labs(y = "Suits Index")+
  theme(axis.title = element_text(size=25),
        axis.text = element_text(size=15),
        legend.title =  element_text(size=25), 
        legend.text =  element_text(size=15))

ggsave("Figures/trends.jpeg", width = 11, height = 8) 






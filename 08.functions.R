# Required functions for 08.national_quartile_summary.R

# formatting a table to print quartile cut offs for amount per capita
getQuartTable_amount <- function(quant) {
  quant <- quant %>%
    mutate(decade = if_else(merge_year == 2010, paste0(merge_year - 5, " - ", merge_year + 8), paste0(merge_year - 5, " - ", merge_year + 5))) %>%
    mutate(`1` = paste0("0.00 - ", round(decade_amount_per_cap_25,2))) %>%
    mutate(`2` = paste0(round(decade_amount_per_cap_25, 2), " - ", round(decade_amount_per_cap_50, 2))) %>%
    mutate(`3` = paste0(round(decade_amount_per_cap_50, 2), " - ", round(decade_amount_per_cap_75, 2))) %>%
    mutate(`4` = paste0(" > ", round(decade_amount_per_cap_75, 2))) %>%
    select(decade, `1`, `2`, `3`, `4`) %>%
    mutate(outcome = "amount per capita (2018 $)") 
  return(quant)
}

# formatting a table to print quartile cut offs for quantity per 100k
getQuartTable_quantity <- function(quant) {
  quant <- quant %>%
    mutate(decade = if_else(merge_year == 2010, paste0(merge_year - 5, " - ", merge_year + 8), paste0(merge_year - 5, " - ", merge_year + 5))) %>%
    mutate(`1` = paste0("0.00 - ", round(decade_quantity_per_cap_25,2))) %>%
    mutate(`2` = paste0(round(decade_quantity_per_cap_25, 2), " - ", round(decade_quantity_per_cap_50, 2))) %>%
    mutate(`3` = paste0(round(decade_quantity_per_cap_50, 2), " - ", round(decade_quantity_per_cap_75, 2))) %>%
    mutate(`4` = paste0(" > ", round(decade_quantity_per_cap_75, 2))) %>%
    select(decade, `1`, `2`, `3`, `4`) %>%
    mutate(outcome = "quantity per 100k") 
  return(quant)
}

# give myWorking_decade and get myWorking_decade with quartiles 
getQuartiles <- function(myDec) {
  
  myWorking_decade <- myDec
  
  # ------- Get quartiles of quantity grants received for each decade ------- #
  myQuants_quantity <- myWorking_decade %>%
    dplyr::group_by(merge_year) %>%
    dplyr::summarise(quants = list(quantile(decade_quantity, probs = c(.25, .5, .75)))) %>%
    unnest_wider(quants) %>%
    dplyr::rename(decade_quantity_25 = `25%`) %>%
    dplyr::rename(decade_quantity_50 = `50%`) %>%
    dplyr::rename(decade_quantity_75 = `75%`) %>%
    ungroup()
  
  #joining back in
  myWorking_decade <- left_join(myWorking_decade, myQuants_quantity, by = "merge_year")
  rm(myQuants_quantity)
  
  #making a factor variable for the quartiles (by year)
  myWorking_decade <- myWorking_decade %>%
    dplyr::mutate(decade_quantity_quants = if_else(decade_quantity > decade_quantity_75, "4", "")) %>% # fourth quartile 
    dplyr::mutate(decade_quantity_quants = if_else(decade_quantity <=decade_quantity_75, "3", decade_quantity_quants)) %>% # third quartile
    dplyr::mutate(decade_quantity_quants = if_else(decade_quantity <= decade_quantity_50, "2", decade_quantity_quants)) %>% # second quartile
    dplyr::mutate(decade_quantity_quants = if_else(decade_quantity <= decade_quantity_25, "1", decade_quantity_quants)) %>% # first quartile 
    dplyr::select(-decade_quantity_75, -decade_quantity_50, - decade_quantity_25) %>%
    dplyr::mutate(decade_quantity_quants = as.factor(decade_quantity_quants))
  
  # ------- Get quartiles of amount of grant funding for each decade ------- #
  myQuants_amount <- myWorking_decade %>%
    dplyr::group_by(merge_year) %>%
    dplyr::summarise(quants = list(quantile(decade_amount, probs = c(.25, .5, .75)))) %>%
    unnest_wider(quants) %>%
    dplyr::rename(decade_amount_25 = `25%`) %>%
    dplyr::rename(decade_amount_50 = `50%`) %>%
    dplyr::rename(decade_amount_75 = `75%`)
  
  #joining back in
  myWorking_decade <- left_join(myWorking_decade, myQuants_amount, by = "merge_year")
  rm(myQuants_amount)
  
  #making a factor variable for the quartiles (by year)
  myWorking_decade <- myWorking_decade %>%
    dplyr::mutate(decade_amount_quants = if_else(decade_amount > decade_amount_75, "4", "")) %>% # fourth quartile 
    dplyr::mutate(decade_amount_quants = if_else(decade_amount <=decade_amount_75, "3", decade_amount_quants)) %>% # third quartile
    dplyr::mutate(decade_amount_quants = if_else(decade_amount <= decade_amount_50, "2", decade_amount_quants)) %>% # second quartile
    dplyr::mutate(decade_amount_quants = if_else(decade_amount <= decade_amount_25, "1", decade_amount_quants)) %>% # first quartile 
    dplyr::select(-decade_amount_75, -decade_amount_50, - decade_amount_25) %>%
    dplyr::mutate(decade_amount_quants = as.factor(decade_amount_quants))
  
  # ------- Get quartiles of amount of grant funding PER CAPITA for each decade ------- #
  myQuants_amount_per_cap <- myWorking_decade %>%
    dplyr::group_by(merge_year) %>%
    dplyr::summarise(quants = list(quantile(decade_amount_per_cap, probs = c(.25, .5, .75), na.rm = TRUE))) %>%
    unnest_wider(quants) %>%
    dplyr::rename(decade_amount_per_cap_25 = `25%`) %>%
    dplyr::rename(decade_amount_per_cap_50 = `50%`) %>%
    dplyr::rename(decade_amount_per_cap_75 = `75%`)
  
  #joining back in
  myWorking_decade <- left_join(myWorking_decade, myQuants_amount_per_cap, by = "merge_year")

  #making a factor variable for the quartiles (by year)
  myWorking_decade <- myWorking_decade %>%
    dplyr::mutate(decade_amount_per_cap_quants = if_else(decade_amount_per_cap > decade_amount_per_cap_75, "4", "")) %>% # fourth quartile 
    dplyr::mutate(decade_amount_per_cap_quants = if_else(decade_amount_per_cap <=decade_amount_per_cap_75, "3", decade_amount_per_cap_quants)) %>% # third quartile
    dplyr::mutate(decade_amount_per_cap_quants = if_else(decade_amount_per_cap <= decade_amount_per_cap_50, "2", decade_amount_per_cap_quants)) %>% # second quartile
    dplyr::mutate(decade_amount_per_cap_quants = if_else(decade_amount_per_cap <= decade_amount_per_cap_25, "1", decade_amount_per_cap_quants)) %>% # first quartile 
    dplyr::select(-decade_amount_per_cap_75, -decade_amount_per_cap_50, - decade_amount_per_cap_25) %>%
    dplyr::mutate(decade_amount_per_cap_quants = as.factor(decade_amount_per_cap_quants))
  
  # ------- Get quartiles of number of grants  PER CAPITA for each decade ------- #
  myQuants_quantity_per_cap <- myWorking_decade %>%
    dplyr::group_by(merge_year) %>%
    dplyr::summarise(quants = list(quantile(decade_quantity_per_cap, probs = c(.25, .5, .75), na.rm = TRUE))) %>%
    unnest_wider(quants) %>%
    dplyr::rename(decade_quantity_per_cap_25 = `25%`) %>%
    dplyr::rename(decade_quantity_per_cap_50 = `50%`) %>%
    dplyr::rename(decade_quantity_per_cap_75 = `75%`)
  
  #joining back in
  myWorking_decade <- left_join(myWorking_decade, myQuants_quantity_per_cap, by = "merge_year")
  
  #making a factor variable for the quartiles (by year)
  myWorking_decade <- myWorking_decade %>%
    dplyr::mutate(decade_quantity_per_cap_quants = if_else(decade_quantity_per_cap > decade_quantity_per_cap_75, "4", "")) %>% # fourth quartile 
    dplyr::mutate(decade_quantity_per_cap_quants = if_else(decade_quantity_per_cap <=decade_quantity_per_cap_75, "3", decade_quantity_per_cap_quants)) %>% # third quartile
    dplyr::mutate(decade_quantity_per_cap_quants = if_else(decade_quantity_per_cap <= decade_quantity_per_cap_50, "2", decade_quantity_per_cap_quants)) %>% # second quartile
    dplyr::mutate(decade_quantity_per_cap_quants = if_else(decade_quantity_per_cap <= decade_quantity_per_cap_25, "1", decade_quantity_per_cap_quants)) %>% # first quartile 
    dplyr::select(-decade_quantity_per_cap_75, -decade_quantity_per_cap_50, - decade_quantity_per_cap_25) %>%
    dplyr::mutate(decade_quantity_per_cap_quants = as.factor(decade_quantity_per_cap_quants))
  
  #returns a the working dataset, as well as two tables that display the quartile cut offs by decade
  return(list(df = myWorking_decade, quant_amount = getQuartTable_amount(myQuants_amount_per_cap), quant_quantity = getQuartTable_quantity(myQuants_quantity_per_cap)))
}


# give myWorking_decade w/ quartiles and get avg percent for each demographic (POC, pov, rural, median income)
# gets a datafram summarizing averge pct in each quartile for each demographic characteristic 

getAmountDF <- function(myDec) {

  myWorking_decade = myDec
  
  myPOC_avgs <- myWorking_decade %>%
    select(poc_pct, decade_amount_per_cap_quants) %>%
    group_by(decade_amount_per_cap_quants) %>%
    mutate(avg_pct_poc = mean(poc_pct)) %>%
    ungroup() %>%
    select(decade_amount_per_cap_quants, avg_pct_poc) %>%
    distinct()
  
  myPov_avgs <- myWorking_decade %>%
    select(inc_below_pov_pct, decade_amount_per_cap_quants) %>%
    group_by(decade_amount_per_cap_quants) %>%
    mutate(avg_pct_pov = mean(inc_below_pov_pct)) %>%
    ungroup() %>%
    select(decade_amount_per_cap_quants, avg_pct_pov) %>%
    distinct()
  
  myRural_avgs <- myWorking_decade %>%
    select(rural_pct, decade_amount_per_cap_quants) %>%
    group_by(decade_amount_per_cap_quants) %>%
    mutate(avg_pct_rural = mean(rural_pct)) %>%
    ungroup() %>%
    select(decade_amount_per_cap_quants, avg_pct_rural) %>%
    distinct()
  
  myMedInc_avgs <- myWorking_decade %>%
    select(med_income_house, decade_amount_per_cap_quants) %>%
    group_by(decade_amount_per_cap_quants) %>%
    mutate(avg_med_income = mean(med_income_house, na.rm = TRUE)) %>%
    ungroup() %>%
    select(decade_amount_per_cap_quants, avg_med_income) %>%
    distinct()
  
  myAmount_total <- left_join(myPOC_avgs, myPov_avgs, by = "decade_amount_per_cap_quants")
  myAmount_total <- left_join(myAmount_total, myRural_avgs, by = "decade_amount_per_cap_quants")
  myAmount_total <- left_join(myAmount_total, myMedInc_avgs, by = "decade_amount_per_cap_quants")
  rm(myPOC_avgs, myPov_avgs, myRural_avgs, myMedInc_avgs)
  return(myAmount_total)

}

getQuantityDF <- function(myDec) {
  
  myWorking_decade = myDec
  
  
  myPOC_avgs <- myWorking_decade %>%
    select(poc_pct, decade_quantity_per_cap_quants) %>%
    group_by(decade_quantity_per_cap_quants) %>%
    mutate(avg_pct_poc = mean(poc_pct)) %>%
    ungroup() %>%
    select(decade_quantity_per_cap_quants, avg_pct_poc) %>%
    distinct()
  
  myPov_avgs <- myWorking_decade %>%
    select(inc_below_pov_pct, decade_quantity_per_cap_quants) %>%
    group_by(decade_quantity_per_cap_quants) %>%
    mutate(avg_pct_pov = mean(inc_below_pov_pct)) %>%
    ungroup() %>%
    select(decade_quantity_per_cap_quants, avg_pct_pov) %>%
    distinct()
  
  myRural_avgs <- myWorking_decade %>%
    select(rural_pct, decade_quantity_per_cap_quants) %>%
    group_by(decade_quantity_per_cap_quants) %>%
    mutate(avg_pct_rural = mean(rural_pct)) %>%
    ungroup() %>%
    select(decade_quantity_per_cap_quants, avg_pct_rural) %>%
    distinct()
  
  myMedInc_avgs <- myWorking_decade %>%
    select(med_income_house, decade_quantity_per_cap_quants) %>%
    group_by(decade_quantity_per_cap_quants) %>%
    mutate(avg_med_income = mean(med_income_house, na.rm = TRUE)) %>%
    ungroup() %>%
    select(decade_quantity_per_cap_quants, avg_med_income) %>%
    distinct()
  
  myQuantity_total <- left_join(myPOC_avgs, myPov_avgs, by = "decade_quantity_per_cap_quants")
  myQuantity_total <- left_join(myQuantity_total, myRural_avgs, by = "decade_quantity_per_cap_quants")
  myQuantity_total <- left_join(myQuantity_total, myMedInc_avgs, by = "decade_quantity_per_cap_quants")
  rm(myPOC_avgs, myPov_avgs, myRural_avgs, myMedInc_avgs)
  
  return(myQuantity_total)
  
}


# Grouping outcome variables by decade 
#(number of grants received, amount received, average amount received per capita per decade, avg # grants per 100,000 people per decade)
getDecade <- function(myWork) {
  myWorking_decade <- myWork %>%
    filter(type != "P") %>% # not including planning grants bc they're given to states, not counties
    group_by(fips, merge_year) %>%
    mutate(decade_quantity = sum(got_grant)) %>%
    mutate(decade_amount = sum(amount, na.rm = TRUE)) %>%
    mutate(decade_amount_per_cap = sum(amount, na.rm = TRUE)/mean(annual_population, na.rm = TRUE)) %>%
    mutate(decade_quantity_per_cap = sum(got_grant)/mean((annual_population/100000), na.rm = TRUE)) %>%
    select(fips, merge_year, state_fips, ends_with("_pct"), starts_with("decade_"),  med_income_house) %>%
    ungroup() %>%
    distinct()
  return(myWorking_decade)
}







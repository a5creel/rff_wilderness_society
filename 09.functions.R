# Functions for Suits index work 
# Andie Creel
# Started July 2022

#-------------------------------------------------------------------------------
# get suits index for quantity and amount 
#-------------------------------------------------------------------------------

#must be df that has the following variables: avg_med_income_house, amount_per_cap_cum, quantity_per_cap_cum
getSuits <- function(df){
  # step one:  proportion of median income, LWCF funding (amount) & grants (quantity)
  df$income_prop <- df$avg_med_income_house/sum(df$avg_med_income_house, na.rm = TRUE)
  df$amount_prop <- df$amount_per_cap_cum/sum(df$amount_per_cap_cum, na.rm = TRUE)
  df$quantity_prop <- df$quantity_per_cap_cum / sum(df$quantity_per_cap_cum, na.rm = TRUE)
  
  #step two: sort based on prop of median income (ascending)
  df <- arrange(df, income_prop)
  
  #step three: get cumulative distributions 
  income_cumsum <- sum_run(df$income_prop)
  quantity_cumsum <- sum_run(df$quantity_prop)
  amount_cumsum <- sum_run(df$amount_prop)
  
  # Step four: use trapezoid rule to get area under curve 
  # diff is trap hight and rollmean is (b1+b2)/2
  area_L_quantity <- sum(diff(income_cumsum)*rollmean(quantity_cumsum, 2)) 
  area_L_amount <- sum(diff(income_cumsum)*rollmean(amount_cumsum, 2)) 
  area_K <- .5 # triangle [(0,0), (0,1), (1,1)]
  
  # Step five: calculate suits index 
  suits_amount <- (area_K - area_L_amount)/area_K
  suits_quantity <- (area_K - area_L_quantity)/area_K
  
  # print(paste('Suits index for amount of funding and average median income:', suits_amount))
  # print(paste('Suits index for quantity of grants and average median income:', suits_quantity))
  return(list(Suits_Amount = suits_amount, Suits_Quantity = suits_quantity, Graph = list(Amount_cs = amount_cumsum, Quantity_cs = quantity_cumsum, Income_cs = income_cumsum)))
}




#-------------------------------------------------------------------------------
# print suits index results (graph and indexes)
#-------------------------------------------------------------------------------

printSuits <- function(myR){
  #suits index
  print(paste0("Suits index for the amount of funding: ", round(myR$Suits_Amount, 3)))
  print(paste0("Suits index for the quantity of grants: ", round(myR$Suits_Quantity, 3)))
  
  # amount graph
  aGraph <- ggplot(as.data.frame(myR$Graph), aes(x=Income_cs, y=Amount_cs)) + 
    geom_point(size = .5, colour = 'steelblue') +
    ggtitle("Amount of Funding", subtitle = "Each point is  a county") +
    xlab("Cumulative prop. of avg. median income") + 
    ylab("Cumulative prop. of amount of LWCF funds") +
    theme_bw()+ 
    geom_abline(intercept = 0, slope = 1) # neutral line 
  
  # quantity graph
  qGraph <- ggplot(as.data.frame(myR$Graph), aes(x=Income_cs, y=Quantity_cs)) + 
    geom_point(size = .5, colour = 'darkgreen') +
    ggtitle("Quantity of Grants", subtitle = "Each point is  a county") +
    xlab("Cumulative prop. of avg. median income") + 
    ylab("Cumulative prop. of quant. of grants") +
    theme_bw() + 
    geom_abline(intercept = 0, slope = 1) # neutral line
  
  return(list(Amout_Graph = aGraph,  Quantity_Graph = qGraph))

}

myState_choropleth <- function (df, title = "", legend = "", num_colors = 7, zoom = NULL, reference_map = FALSE) {
    c = StateChoropleth$new(df)
    c$title = title
    c$legend = legend
    c$set_num_colors(num_colors)
    c$set_zoom(zoom)
    c$show_labels = FALSE #gets ride of abbreviations on states
    if (reference_map) {
      if (is.null(zoom)) {
        stop("Reference maps do not currently work with maps that have insets, such as maps of the 50 US States.")
      }
      c$render_with_reference_map()
    }
    else {
      c$render()
    }
  }


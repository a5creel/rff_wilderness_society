# Functions for Suits index work 
# Andie Creel
# Started July 2022

#-------------------------------------------------------------------------------
# get suits index for quantity and amount for avg med income 
#-------------------------------------------------------------------------------

#must be df that has the following variables: avg_med_income_house, amount_per_cap_cum, quantity_per_cap_cum
getSuits <- function(df, x_axis = "Income"){
  # step one:  proportion of x variables, LWCF funding (amount) & grants (quantity)
  df$income_prop <- df$avg_med_income_house/sum(df$avg_med_income_house, na.rm = TRUE)
  df$poc_prop <- df$avg_poc_pct/sum(df$avg_poc_pct, na.rm = TRUE)
  df$amount_prop <- df$amount_per_cap_cum/sum(df$amount_per_cap_cum, na.rm = TRUE)
  df$quantity_prop <- df$quantity_per_cap_cum / sum(df$quantity_per_cap_cum, na.rm = TRUE)
  
  #step two: sort based on prop of variable of interest (ascending in privilege)
  if (x_axis == "Income"){
    df <- arrange(df, income_prop)
  } else if (x_axis == "POC"){
    df <- arrange(df, -poc_prop) #notice that this is decending ing % poc, therefore it runs most poc to whitest counties. 
  } else {
    stop("You have not specified a viable variable of interest. Options are: Income, POC.")
  }
  
  
  #step three: get cumulative distributions 
  income_cumsum <- sum_run(df$income_prop)
  poc_cumsum <- sum_run(df$poc_prop)
  quantity_cumsum <- sum_run(df$quantity_prop)
  amount_cumsum <- sum_run(df$amount_prop)
  
  # Step four: use trapezoid rule to get area under curve 
  # diff is trap hight and rollmean is (b1+b2)/2
  if (x_axis == "Income"){
    area_L_quantity <- sum(diff(income_cumsum)*rollmean(quantity_cumsum, 2)) 
    area_L_amount <- sum(diff(income_cumsum)*rollmean(amount_cumsum, 2)) 
    area_K <- .5 # triangle [(0,0), (0,1), (1,1)]
  } else if (x_axis == "POC"){
    area_L_quantity <- sum(diff(poc_cumsum)*rollmean(quantity_cumsum, 2)) 
    area_L_amount <- sum(diff(poc_cumsum)*rollmean(amount_cumsum, 2)) 
    area_K <- .5 # triangle [(0,0), (0,1), (1,1)]
  } 
  
  # Step five: calculate suits index 
  suits_amount <- ( area_L_amount - area_K)/area_K
  suits_quantity <- (area_L_quantity - area_K)/area_K
  
  # print(paste('Suits index for amount of funding and average median income:', suits_amount))
  # print(paste('Suits index for quantity of grants and average median income:', suits_quantity))
  return(list(Suits_Amount = suits_amount, Suits_Quantity = suits_quantity, 
              Graph = list(Amount_cs = amount_cumsum, 
                           Quantity_cs = quantity_cumsum, 
                           Income_cs = income_cumsum,
                           Poc_cs = poc_cumsum),
              Variable = x_axis))
}


#-------------------------------------------------------------------------------
# print suits index results (graph and indexes)
#-------------------------------------------------------------------------------

printSuits <- function(myR, x_axis = "Income"){
  #check 
  if(myR$Variable != x_axis) {stop("The variable of interest used to calulate your result doesn't match the variable of interest given.")}
  
  
  #suits index
  amount_title = paste0("Amount of Funding. Suits Index: ", round(myR$Suits_Amount, 3))
  quantity_title = paste0("Quantity of Grant. Suits Index: ", round(myR$Suits_Quantity, 3))
  
  if(x_axis == "Income"){
    x_var <- myR$Graph$Income_cs
    x_label <- "poorest -> richest counties (avg median income)"
  }else if (x_axis == "POC"){
    x_var <- myR$Graph$Poc_cs
    x_label <- "Highest %POC -> whitest counties (avg % POC)"
  } else{
    stop("You have not specified a viable variable of interest. Options are: Income, POC.")
  }
  
  # amount graph
  aGraph <- ggplot(as.data.frame(myR$Graph), aes(x=x_var, y=Amount_cs)) + 
    geom_point(size = .5, colour = 'steelblue') +
    ggtitle(amount_title, subtitle = "Each point is  a county") +
    xlab(x_label) + 
    ylab("Cumulative proportion of LWCF funds (amount per cap)") +
    theme_bw()+ 
    geom_abline(intercept = 0, slope = 1) # neutral line 
  
  # quantity graph
  qGraph <- ggplot(as.data.frame(myR$Graph), aes(x=x_var, y=Quantity_cs)) + 
    geom_point(size = .5, colour = 'darkgreen') +
    ggtitle(quantity_title, subtitle = "Each point is  a county") +
    xlab(x_label) + 
    ylab("Cumulative proportion of grants (quantity per cap)") +
    theme_bw() + 
    geom_abline(intercept = 0, slope = 1) # neutral line
  
  return(list(Amout_Graph = aGraph,  Quantity_Graph = qGraph))

}

#Rewrote code to get rid of state abbreviation 
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


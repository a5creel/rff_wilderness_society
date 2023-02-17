# Functions for Suits index work 
# Andie Creel
# Started July 2022

#-------------------------------------------------------------------------------
# get suits index for amount for avg med income 
#-------------------------------------------------------------------------------

#must be df that has the following variables: avg_med_income_house, amount_per_cap_avg
getSuits <- function(df, x_axis = "Income"){
  # step one:  proportion of x variables, LWCF funding (amount) 
  df$income_prop <- df$avg_med_income_house/sum(df$avg_med_income_house, na.rm = TRUE)
  df$poc_prop <- df$avg_poc_pct/sum(df$avg_poc_pct, na.rm = TRUE)
  df$rural_prop <- df$avg_rural_pct/sum(df$avg_rural_pct, na.rm = TRUE)
  df$amount_prop <- df$amount_per_cap_avg/sum(df$amount_per_cap_avg, na.rm = TRUE)
  
  #step two: sort based on prop of variable of interest (ascending in privilege)
  if (x_axis == "Income"){
    df <- arrange(df, income_prop)
  } else if (x_axis == "POC"){
    df <- arrange(df, -poc_prop) #notice that this is descending % poc, therefore it runs most poc to whitest counties. 
  } else if (x_axis == "Rural"){
    df <- arrange(df, -rural_prop) #most rural to most urban 
  } else {
    stop("You have not specified a viable variable of interest. Options are: Income, POC.")
  }
  
  
  #step three: get cumulative distributions 
  income_totsum <- sum_run(df$income_prop)
  poc_totsum <- sum_run(df$poc_prop)
  rural_totsum <- sum_run(df$rural_prop)

  # quantity_totsum <- sum_run(df$quantity_prop)
  amount_totsum <- sum_run(df$amount_prop)
  
  # Step four: use trapezoid rule to get area under curve 
  # diff is trap hight and rollmean is (b1+b2)/2
  area_K <- .5 # triangle [(0,0), (0,1), (1,1)]
  
  if (x_axis == "Income"){
    area_L_amount <- sum(diff(income_totsum)*rollmean(amount_totsum, 2)) 
  } else if (x_axis == "POC"){
    area_L_amount <- sum(diff(poc_totsum)*rollmean(amount_totsum, 2)) 
  } else if (x_axis == "Rural"){
    area_L_amount <- sum(diff(rural_totsum)*rollmean(amount_totsum, 2)) 
  } 
  
  # Step five: calculate suits index 
  suits_amount <- ( area_L_amount - area_K)/area_K

 return(list(Suits_Amount = suits_amount, 
              Graph = list(Amount_cs = amount_totsum, 
                           Income_cs = income_totsum,
                           Poc_cs = poc_totsum,
                           Rural_cs = rural_totsum),
              Variable = x_axis))
}


#-------------------------------------------------------------------------------
# print suits index results (graph and indexes)
#-------------------------------------------------------------------------------

getGraph <- function(myR, x_axis = "Income", caption = ""){
  #check 
  if(myR$Variable != x_axis) {stop("The variable of interest used to calulate your result doesn't match the variable of interest given.")}
  
  
  #suits index
  amount_caption = paste0("Figue YY: The suits index ", round(myR$Suits_Amount, 3), ". ", caption)

  if(x_axis == "Income"){
    x_var <- myR$Graph$Income_cs
    x_label <- "poorest to richest counties "
    y_label <- "Accumulated investment per capita"
  }else if (x_axis == "POC"){
    x_var <- myR$Graph$Poc_cs
    x_label <- "highest % POC to whitest counties"
    y_label <- ""
  } else if (x_axis == "Rural"){
    x_var <- myR$Graph$Rural_cs
    x_label <- "most rural to most urban counties"
    y_label <- ""
  } else{
    stop("You have not specified a viable variable of interest. Options are: Income, POC.")
  }
  
  # amount graph
  aGraph <- ggplot(as.data.frame(myR$Graph), aes(x=x_var, y=Amount_cs)) + 
    geom_point(size = .5, colour = 'steelblue') +
    xlab(x_label) + 
    ylab(y_label) +
    theme_bw()+ 
    geom_abline(intercept = 0, slope = 1) + # neutral line + 
    # labs(caption = str_wrap(amount_caption, 120)) +
    theme(plot.caption.position = "plot",
          plot.caption = element_text(hjust = 0, size = 12))
  
  return(aGraph)

}

#Rewrote code to get rid of state abbreviation 
myState_choropleth <- function (df, title = "", legend = "", num_colors = 7, zoom = NULL, reference_map = FALSE) {
    c = StateChoropleth$new(df)
    c$title = title
    c$legend = legend
    c$set_zoom(zoom)
    c$show_labels = FALSE #gets ride of abbreviations on states
    c$ggplot_scale = scale_fill_steps2(low = "darkred", mid = "white", high = "darkblue",
                                       midpoint = 0,
                                       n.breaks = 9,
                                       na.value="black",
                                       limits=c(-1, 1))
    c$set_num_colors(num_colors)
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


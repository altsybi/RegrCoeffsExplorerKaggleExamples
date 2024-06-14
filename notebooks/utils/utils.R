# define additional helpful functions 

# Re-balance factor variables
rebalance_factors = function(data, factor_var) {  # if frequency of one level is > sum of other levels, then rebalance
  # Calculate the frequencies of each factor level
  factor_freq = table(data[[factor_var]])
  
  # Identify the factor level with the highest frequency
  max_freq_level = names(factor_freq)[which.max(factor_freq)]
  
  # Calculate the sum of frequencies of other factor levels
  other_freq_sum = sum(factor_freq[!names(factor_freq) %in% max_freq_level])
  
  # Check if the highest frequency is higher than the sum of other frequencies
  if (factor_freq[max_freq_level] > other_freq_sum) {
    # Identify the factor levels to be combined into "other"
    other_levels = names(factor_freq)[!names(factor_freq) %in% max_freq_level]
    
    # Create a new factor variable with rearranged levels
    data[[factor_var]] = fct_collapse(data[[factor_var]], Other = other_levels)
  }
  
  # Return the modified data
  return(data)
}


# Plot Barplots and Histograms
plot_figures = function(vars, ncol, plot_type="bar") {
  plot_list = list()
  
  # Iterate over each variable and create a figure
  for (col in colnames(vars)) {
    if (plot_type == "bar") {
      # Create a barplot for the current factor variable
      plot_list[[col]] = ggplot(vars, aes(x=.data[[col]])) +
        geom_bar() +
        labs(title=paste("Barplot of", col), x=col, y="Count") +
        theme(axis.text.x=element_text(angle=90))
      
    } else if (plot_type == "hist") {
      # Create a histogram for the current continuous variable
      plot_list[[col]] = ggplot(vars, aes(x=.data[[col]])) +
        geom_histogram(bins=30) +
        labs(title=paste("Histogram of", col), x=col, y="Frequency")
      
    } else {
      stop("Variable 'plot_type' should be either 'bar' or 'hist', got: ", plot_type)
    }
  }
  
  grid.arrange(arrangeGrob(grobs=plot_list, ncol=ncol))
}


# Change size and transparency of points in the boxplot in OR plots
customized_plots = function(or_plots) {
  or_plots$"BoxPlot"$layers[[2]]$aes_params$size = 0.5
  or_plots$"BoxPlot"$layers[[2]]$aes_params$alpha = 0.1
  
  or_plots = ggarrange(or_plots$"BarPlot", or_plots$"BoxPlot", ncol=1, nrow=2, 
                       common.legend=TRUE, legend="bottom")
  return(or_plots)
}
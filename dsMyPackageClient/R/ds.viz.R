
#' @title Create visualizations from DataShield data
#' @description Creates histogram and boxplot from aggregated data
#' @param connections DataShield connections
#' @param variable variable name to analyze
#' @param breaks number of histogram breaks
#' @return summary statistics data frame
#' @author Your Name
#' @export
ds.viz <- function(connections, variable, breaks = 10) {
  # Get aggregated histogram data from server
  call_text <- paste0("histDataDS(", variable, ", ", breaks, ")")
  results <- DSI::datashield.aggregate(connections, call_text)
  
  # Extract data from first server (modify if you have multiple servers)
  server_data <- results[[1]]
  
  # Create plots
  par(mfrow = c(1, 2))
  
  # Histogram
  hist_breaks <- server_data$breaks
  hist_counts <- server_data$counts
  hist_mids <- (hist_breaks[-1] + hist_breaks[-length(hist_breaks)]) / 2
  
  barplot(hist_counts, names.arg = round(hist_mids, 2), 
          col = rainbow(length(hist_counts)),
          main = "Histogram", xlab = "Value", ylab = "Frequency")
  
  # For boxplot, we can only show summary statistics as a simple plot
  summary_stats <- server_data$summary
  
  # Create a simple representation since we don't have raw data
  plot(1, type = "n", xlim = c(0.5, 1.5), ylim = c(summary_stats$min, summary_stats$max),
       xlab = "", ylab = "Value", main = "Summary Statistics", xaxt = "n")
  axis(1, at = 1, labels = "Data")
  
  # Draw simple box representation
  rect(0.8, summary_stats$q25, 1.2, summary_stats$q75, col = "red")
  segments(0.8, summary_stats$median, 1.2, summary_stats$median, lwd = 3)
  segments(1, summary_stats$min, 1, summary_stats$q25)
  segments(1, summary_stats$q75, 1, summary_stats$max)
  segments(0.9, summary_stats$min, 1.1, summary_stats$min)
  segments(0.9, summary_stats$max, 1.1, summary_stats$max)
  
  par(mfrow = c(1, 1))
  
  # Return summary statistics
  return(summary_stats)
}

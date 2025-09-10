
#' @title Generate histogram data for plotting
#' @description Returns aggregated histogram data that can be safely shared
#' @param x a numeric vector
#' @param breaks number of breaks for histogram
#' @return a list containing histogram data and summary stats
#' @author Your Name
#' @export
histDataDS <- function(x, breaks = 10) {
  # Check if input is numeric
  if (!is.numeric(x)) {
    stop("Input must be numeric")
  }
  
  # Remove missing values
  x_clean <- x[!is.na(x)]
  
  # Create histogram data
  hist_data <- hist(x_clean, breaks = breaks, plot = FALSE)
  
  # Return aggregated data (no individual values exposed)
  result <- list(
    breaks = hist_data$breaks,
    counts = hist_data$counts,
    summary = data.frame(
      count = length(x_clean),
      min = min(x_clean),
      q25 = quantile(x_clean, 0.25),
      median = median(x_clean),
      mean = mean(x_clean),
      q75 = quantile(x_clean, 0.75),
      max = max(x_clean),
      sd = sd(x_clean)
    )
  )
  
  return(result)
}

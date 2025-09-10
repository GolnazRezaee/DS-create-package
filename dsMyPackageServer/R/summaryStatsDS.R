#' @title Summary statistics for a numeric vector
#' @description This function calculates summary statistics for a numeric vector
#' @param x a numeric vector
#' @return a data frame with summary statistics
#' @author Your Name
#' @export
summaryStatsDS <- function(x) {
  # Check if input is numeric
  if (!is.numeric(x)) {
    stop("Input must be numeric")
  }

  # Remove missing values
  x_clean <- x[!is.na(x)]

  # Calculate summary statistics
  result <- data.frame(
    count = length(x_clean),
    min = min(x_clean),
    q25 = quantile(x_clean, 0.25),
    median = median(x_clean),
    mean = mean(x_clean),
    q75 = quantile(x_clean, 0.75),
    max = max(x_clean),
    sd = sd(x_clean)
  )

  return(result)
}

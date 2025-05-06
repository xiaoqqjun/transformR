# Content for R/evaluate_transforms.R:
#' Evaluate Transformations
#'
#' Evaluates different transformations based on normality measures (skewness and kurtosis).
#'
#' @param transformed_data A list of transformed datasets
#' @return A data frame with skewness and kurtosis for each transformation
#' @export
#' @importFrom moments skewness kurtosis
evaluate_transformations <- function(transformed_data) {
  # Extract names of transformations
  transform_names <- names(transformed_data)

  # Calculate skewness for each transformation
  skewness_values <- sapply(transformed_data, function(x) {
    if (is.list(x) && "data" %in% names(x)) {
      moments::skewness(x$data, na.rm = TRUE)
    } else {
      moments::skewness(x, na.rm = TRUE)
    }
  })

  # Calculate excess kurtosis for each transformation
  kurtosis_values <- sapply(transformed_data, function(x) {
    if (is.list(x) && "data" %in% names(x)) {
      moments::kurtosis(x$data, na.rm = TRUE) - 3  # Excess kurtosis (normal = 0)
    } else {
      moments::kurtosis(x, na.rm = TRUE) - 3       # Excess kurtosis (normal = 0)
    }
  })

  # Combine results
  evaluation <- data.frame(
    Transformation = transform_names,
    Skewness = skewness_values,
    ExcessKurtosis = kurtosis_values,
    AbsSkewness = abs(skewness_values),
    AbsExcessKurtosis = abs(kurtosis_values)
  )

  # Add normality metric (combination of skewness and kurtosis)
  # Lower is better - we want skewness close to 0 and excess kurtosis close to 0
  evaluation$NormalityScore <- sqrt(evaluation$AbsSkewness^2 + evaluation$AbsExcessKurtosis^2)

  # Sort by the normality score
  evaluation <- evaluation[order(evaluation$NormalityScore), ]

  # Return the evaluation table
  return(evaluation)
}

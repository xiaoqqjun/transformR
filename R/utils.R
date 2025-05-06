# Content for R/utils.R:
#' Check Normality of Data
#'
#' Evaluates the normality of a numeric vector using various tests and metrics.
#'
#' @param x Numeric vector to evaluate
#' @param full_report Logical, whether to return full details or just a summary
#' @return If full_report=TRUE, a list with various normality metrics;
#'         if full_report=FALSE, a data frame with skewness and kurtosis
#' @export
#' @importFrom moments skewness kurtosis
#' @importFrom stats shapiro.test qqnorm qqline
check_normality <- function(x, full_report = FALSE) {
  # Basic checks
  if (!is.numeric(x)) {
    stop("Input must be a numeric vector")
  }

  # Remove NAs
  x <- x[!is.na(x)]

  # Calculate basic statistics
  n <- length(x)
  mean_x <- mean(x)
  sd_x <- sd(x)
  skew <- moments::skewness(x)
  kurt <- moments::kurtosis(x) - 3  # Excess kurtosis (normal = 0)

  # Shapiro-Wilk test (limited to 5000 observations)
  if (n <= 5000) {
    sw_test <- stats::shapiro.test(x)
    sw_p <- sw_test$p.value
  } else {
    # For large samples, use a random subsample
    sw_test <- stats::shapiro.test(sample(x, 5000))
    sw_p <- sw_test$p.value
    warning("Shapiro-Wilk test performed on a random subsample of 5000 observations")
  }

  # Create summary
  summary_df <- data.frame(
    Statistic = c("N", "Mean", "SD", "Skewness", "Excess Kurtosis", "Shapiro-Wilk p-value"),
    Value = c(n, mean_x, sd_x, skew, kurt, sw_p)
  )

  if (full_report) {
    # Create QQ plot data
    qq_data <- stats::qqnorm(x, plot = FALSE)
    qq_line <- stats::qqline(x, plot = FALSE)

    # Return full report
    return(list(
      summary = summary_df,
      shapiro_test = sw_test,
      qq_data = qq_data,
      is_normal = (abs(skew) < 0.5 && abs(kurt) < 0.5 && sw_p > 0.05)
    ))
  } else {
    # Return just the summary
    return(summary_df)
  }
}

#' Generate Example Datasets
#'
#' Creates example datasets with different shapes for testing transformations.
#'
#' @param n Number of observations to generate
#' @param type Type of distribution: "right_skew", "left_skew", "heavy_tail", or "bimodal"
#' @return A numeric vector with the generated data
#' @export
#' @examples
#' # Generate right-skewed data
#' right_skewed <- generate_example_data(100, "right_skew")
#' hist(right_skewed)
#'
#' # Transform and evaluate
#' result <- best_transform(right_skewed, all_transforms = TRUE, plot = TRUE)
#' print(result$evaluation)
generate_example_data <- function(n = 100, type = "right_skew") {
  set.seed(123)  # For reproducibility

  if (type == "right_skew") {
    # Exponential distribution (right-skewed)
    data <- rexp(n, 0.2)
  } else if (type == "left_skew") {
    # Reflected gamma distribution (left-skewed)
    data <- max(rgamma(n, 5, 0.5)) - rgamma(n, 5, 0.5)
  } else if (type == "heavy_tail") {
    # t-distribution with low degrees of freedom (heavy tails)
    data <- rt(n, df = 3)
  } else if (type == "bimodal") {
    # Mixture of two normal distributions
    group <- sample(1:2, n, replace = TRUE, prob = c(0.7, 0.3))
    data <- ifelse(group == 1, rnorm(n, -2, 1), rnorm(n, 2, 0.5))[1:n]
  } else {
    stop("Invalid type. Choose from: 'right_skew', 'left_skew', 'heavy_tail', 'bimodal'")
  }

  return(data)
}

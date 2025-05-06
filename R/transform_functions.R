# Content for R/transform_functions.R:
#' Apply Square Root Transformation
#'
#' Applies a square root transformation to data, with appropriate shifting for
#' non-positive values.
#'
#' @param x Numeric vector to transform
#' @return Transformed numeric vector
#' @export
transform_sqrt <- function(x) {
  min_val <- min(x, na.rm = TRUE)

  if (min_val < 0) {
    shift_value <- abs(min_val) + 0.01  # Add small constant to avoid zeros
    result <- sqrt(x + shift_value)
  } else if (min_val == 0) {
    result <- sqrt(x + 0.01)
  } else {
    result <- sqrt(x)
  }

  return(result)
}

#' Apply Box-Cox Transformation
#'
#' Applies a Box-Cox transformation to data, automatically finding the optimal lambda.
#'
#' @param x Numeric vector to transform
#' @return A list containing the transformed data and the lambda value used
#' @export
#' @importFrom MASS boxcox
transform_boxcox <- function(x) {
  # Shift to positives if needed
  min_val <- min(x, na.rm = TRUE)
  shift_value <- if(min_val <= 0) abs(min_val) + 0.01 else 0
  x_pos <- x + shift_value

  # Find optimal lambda
  bc <- MASS::boxcox(x_pos ~ 1, plotit = FALSE)
  lambda <- bc$x[which.max(bc$y)]

  # Apply transformation
  if (abs(lambda) < 1e-10) {  # Lambda close to zero
    result <- log(x_pos)
  } else {
    result <- (x_pos^lambda - 1) / lambda
  }

  return(list(data = result, lambda = lambda, shift = shift_value))
}

#' Apply Yeo-Johnson Transformation
#'
#' Applies a Yeo-Johnson transformation to data, which works for both positive and negative values.
#'
#' @param x Numeric vector to transform
#' @return A list containing the transformed data and the lambda value used
#' @export
#' @importFrom car powerTransform yjPower
transform_yeojohnson <- function(x) {
  # Find optimal lambda
  tryCatch({
    yj_model <- car::powerTransform(x, family = "yjPower")
    lambda <- yj_model$lambda
    result <- car::yjPower(x, lambda)
    return(list(data = result, lambda = lambda))
  }, error = function(e) {
    # Fallback if car::powerTransform fails
    warning("Yeo-Johnson transformation failed: ", e$message,
            ". Using default lambda = 0 (log transform)")
    # Use log transform (lambda = 0) as fallback
    min_val <- min(x, na.rm = TRUE)
    shift_value <- if(min_val <= 0) abs(min_val) + 0.01 else 0
    result <- log(x + shift_value)
    return(list(data = result, lambda = 0))
  })
}

#' Apply Inverse Transformation
#'
#' Applies an inverse (1/x) transformation to data, with appropriate shifting for
#' non-positive values.
#'
#' @param x Numeric vector to transform
#' @return Transformed numeric vector
#' @export
transform_inverse <- function(x) {
  min_val <- min(x, na.rm = TRUE)

  if (min_val <= 0) {
    shift_value <- abs(min_val) + 0.01
    result <- 1 / (x + shift_value)
  } else {
    result <- 1 / x
  }

  return(result)
}

#' Apply Rank-Based Normalization
#'
#' Transforms data using ranks and the inverse normal CDF.
#'
#' @param x Numeric vector to transform
#' @return Transformed numeric vector
#' @export
#' @importFrom stats qnorm
transform_rank <- function(x) {
  # Handle ties with average
  ranks <- rank(x, na.last = "keep", ties.method = "average")
  # Calculate normalized rank
  n <- sum(!is.na(x))
  probs <- ranks / (n + 1)  # Use n+1 to avoid extremes at 0 and 1
  # Apply inverse normal
  result <- stats::qnorm(probs)

  return(result)
}

#' Apply Hyperbolic Arcsine Transformation
#'
#' Applies the inverse hyperbolic sine transformation, which works well for both
#' positive and negative values.
#'
#' @param x Numeric vector to transform
#' @return Transformed numeric vector
#' @export
transform_asinh <- function(x) {
  return(asinh(x))
}

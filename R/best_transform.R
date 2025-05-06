# Content for R/best_transform.R:
#' Find the Best Transformation
#'
#' Applies multiple transformation methods to a variable and identifies the best one
#' based on normality criteria.
#'
#' @param x Numeric vector to transform
#' @param method Character string specifying the selection method: "auto" (default, based on normality score),
#'        "skewness" (minimal absolute skewness), or "kurtosis" (minimal absolute excess kurtosis)
#' @param all_transforms Logical, whether to return all transformations (TRUE) or only the best one (FALSE)
#' @param plot Logical, whether to create histogram plots of all transformations
#' @return If all_transforms=TRUE, a list with all transformations, evaluation metrics, and the name of the best transformation;
#'         if all_transforms=FALSE, only the best transformed data
#' @export
#' @examples
#' # Generate right-skewed data
#' set.seed(123)
#' x <- rexp(100, 0.2)
#'
#' # Find best transformation
#' result <- best_transform(x)
#'
#' # Get all transformations and evaluation
#' all_results <- best_transform(x, all_transforms = TRUE)
#' print(all_results$evaluation)
#'
#' # Plot all transformations
#' best_transform(x, plot = TRUE)
best_transform <- function(x, method = "auto", all_transforms = FALSE, plot = FALSE) {
  # Check if input is valid
  if (!is.numeric(x)) {
    stop("Input must be a numeric vector")
  }

  # Apply all transformations
  transformed <- list(
    original = x,
    sqrt = transform_sqrt(x),
    boxcox = transform_boxcox(x),
    yeojohnson = transform_yeojohnson(x),
    inverse = transform_inverse(x),
    rank = transform_rank(x),
    asinh = transform_asinh(x)
  )

  # Evaluate transformations
  eval_results <- evaluate_transformations(transformed)

  # Determine best transformation based on method
  if (method == "skewness") {
    best_idx <- which.min(eval_results$AbsSkewness)
  } else if (method == "kurtosis") {
    best_idx <- which.min(eval_results$AbsExcessKurtosis)
  } else {  # "auto" or any other value
    best_idx <- which.min(eval_results$NormalityScore)
  }

  best_name <- as.character(eval_results$Transformation[best_idx])

  # Plot if requested
  if (plot) {
    plot_transforms(transformed, eval_results)
  }

  # Return results based on all_transforms parameter
  if (all_transforms) {
    # Extract actual data from list transformations
    transformed_data <- lapply(transformed, function(t) {
      if (is.list(t) && "data" %in% names(t)) t$data else t
    })

    # Return all transformations, evaluation, and best transformation name
    return(list(
      transformed = transformed,
      transformed_data = transformed_data,
      evaluation = eval_results,
      best = best_name
    ))
  } else {
    # Return only the best transformation
    best_transform <- transformed[[best_name]]
    if (is.list(best_transform) && "data" %in% names(best_transform)) {
      return(best_transform$data)
    } else {
      return(best_transform)
    }
  }
}

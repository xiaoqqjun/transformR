# Content for R/plot_transforms.R:
#' Plot All Transformations
#'
#' Creates histograms of the original data and all transformations.
#'
#' @param transformed A list containing all transformed datasets
#' @param evaluation A data frame with evaluation metrics (optional)
#' @return Invisibly returns NULL, called for side effect of plotting
#' @export
#' @importFrom graphics par hist
plot_transforms <- function(transformed, evaluation = NULL) {
  # Determine number of plots
  n_plots <- length(transformed)

  # Set up the plotting area
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par))

  # Calculate grid dimensions
  n_cols <- min(4, n_plots)
  n_rows <- ceiling(n_plots / n_cols)

  graphics::par(mfrow = c(n_rows, n_cols))

  # Function to extract data from transformation result
  get_data <- function(x) {
    if (is.list(x) && "data" %in% names(x)) x$data else x
  }

  # Plot each transformation
  for (i in seq_along(transformed)) {
    name <- names(transformed)[i]
    data <- get_data(transformed[[i]])

    # Add evaluation metrics to title if available
    if (!is.null(evaluation)) {
      eval_row <- evaluation[evaluation$Transformation == name, ]
      if (nrow(eval_row) > 0) {
        title <- sprintf("%s\nSkew: %.2f, Kurt: %.2f",
                         name, eval_row$Skewness, eval_row$ExcessKurtosis)
      } else {
        title <- name
      }
    } else {
      title <- name
    }

    # Create histogram
    graphics::hist(data, main = title, xlab = "Value",
                   col = "lightblue", border = "white")
  }

  # Reset to single plot
  invisible(NULL)
}

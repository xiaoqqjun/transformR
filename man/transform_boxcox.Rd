% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transform_functions.R
\name{transform_boxcox}
\alias{transform_boxcox}
\title{Apply Box-Cox Transformation}
\usage{
transform_boxcox(x)
}
\arguments{
\item{x}{Numeric vector to transform}
}
\value{
A list containing the transformed data and the lambda value used
}
\description{
Applies a Box-Cox transformation to data, automatically finding the optimal lambda.
}

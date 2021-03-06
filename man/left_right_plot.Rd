% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_plot.R
\name{left_right_plot}
\alias{left_right_plot}
\title{Plot two datasets in left and right direction}
\usage{
left_right_plot(label = "# of segments", left = 500, right = 200,
  unit = "")
}
\arguments{
\item{label}{\code{character} Character vector of the labels for each row of
the histogram}

\item{left}{\code{numeric}  Vector of the left side plot values}

\item{right}{\code{numeric}  Vector of the right side plot values}

\item{unit}{\code{character}  A unit that can be added to all numerical values and
will be displayed in the plot}
}
\description{
Plot two datasets in left and right direction
}
\details{
from: https://stackoverflow.com/questions/18265941/two-horizontal-bar-charts-with-shared-axis-in-ggplot2-similar-to-population-pyr
}
\author{
Sebastian Wolf \email{sebastian@mail-wolf.de}
}

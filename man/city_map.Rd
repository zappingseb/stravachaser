% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/city_map.R
\name{city_map}
\alias{city_map}
\title{Module Server to provide a city map}
\usage{
city_map(input, output, session, city_data = NULL, city_id = NULL)
}
\arguments{
\item{input}{shiny input object}

\item{output}{shiny output object}

\item{session}{shiny session}

\item{city_data}{\code{data.frame} Data frame with segment data from two cities}

\item{city_id}{\code{numeric} City to take from segment_data (1 | 2)}
}
\description{
Module Server to provide a city map
}

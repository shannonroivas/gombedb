% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_dat.R
\name{gombe_food_bout}
\alias{gombe_food_bout}
\title{Import the food_bout table}
\usage{
gombe_food_bout(rename = TRUE)
}
\arguments{
\item{rename}{Whether to rename columns with friendlier names.  Default is TRUE.}
}
\value{
Returns the food_bout table.
}
\description{
Import the food_bout table
}
\examples{
food_bout = gombe_food_bout()
#imports the food bout table as "food_bout"

}

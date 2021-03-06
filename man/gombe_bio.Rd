% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_dat.R
\name{gombe_bio}
\alias{gombe_bio}
\title{Import the biography table}
\usage{
gombe_bio(convert = TRUE, rename = TRUE)
}
\arguments{
\item{convert}{Whether you want to convert date columns to date format.  Default is TRUE.}

\item{rename}{Whether to rename columns with friendlier names.  Default is TRUE, so set to false for
compatibility with old code.}
}
\value{
Returns the biography table.
}
\description{
Import the biography table
}
\examples{
bio = gombe_bio()
#saves the biography table as "bio"

}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_dat.R
\name{gombe_fem_repro_hist_NEW}
\alias{gombe_fem_repro_hist_NEW}
\title{Import NEW female reproductive states table}
\usage{
gombe_fem_repro_hist_NEW(convert = TRUE)
}
\arguments{
\item{convert}{Whether you want to convert date columns to date format.  Default is TRUE.}
}
\value{
Returns the female reproductive states table from a csv file.  For faster loading
  use the gombe_fem_repro_hist command, which loads from an .RData file.  NB: uses the newer
  reproductive STATES table, not the older reproductive history table
}
\description{
Import NEW female reproductive states table
}
\examples{
frh = gombe_fem_repro_hist_NEW()
#saves the female reproductive history table as "frh"

}

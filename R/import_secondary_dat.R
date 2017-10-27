# These functions import secondary gombe database tables into R

#' Import food lookup tables
#'
#' @usage gombe_food_lookup_tabs()
#' @return Returns the food_lookup, food_part_lookup, food_vars_lookup, and new_food_list tables.
#'   The tables are returned as a list, which can be separated by assigning list elements to
#'   new tables
#' @examples
#' food_lookup_tabs = gombe_food_lookup_tabs()
#' #imports the food lookup tables as list called "food_lookup_tabs"
#'
#' #then to get food_part_lookup table as a data frame:
#' food_part_lookup = food_lookup_tabs$food_part_lookup
#'
#' @export
#'


gombe_food_lookup_tabs <- function() {
  oldwd = getwd()
  setwd("C:/Users/Joseph Feldblum/Dropbox (Duke Bio_Ea)/Home Drive_db/Research/DATABASE/excel files/secondary tables")
  load(file = "food_lookup_tabs.RData")
  setwd(oldwd)
  rm(oldwd)
  fdlkp = list(food_lookup, food_part_lookup, food_vars_lookup, new_food_list)
  names(fdlkp) = c("food_lookup", "food_part_lookup", "food_vars_lookup", "new_food_list")
  return(fdlkp)
}



#' Import phenology tables
#'
#' @usage gombe_phenology()
#' @return Returns the ripe_fruit, unripe_fruit, flowers, climate_data, and tree_locations tables.
#'   The tables are returned as a list, which can be separated by assigning list elements to
#'   new data frames
#' @examples
#' phenology = gombe_phenology()
#' #imports the phenology as list called "phenology"
#'
#' #then to get ripe_fruit table as a data frame:
#' ripe_fruit = phenology$ripe_fruit
#'
#' @export
#'


gombe_phenology <- function() {
  oldwd = getwd()
  setwd("C:/Users/Joseph Feldblum/Dropbox (Duke Bio_Ea)/Home Drive_db/Research/DATABASE/excel files/secondary tables")
  load(file = "phenology.RData")
  setwd(oldwd)
  rm(oldwd)
  return(phenology)
}

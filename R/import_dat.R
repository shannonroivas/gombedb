# These functions import data from the table into R
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


#' Import the biography table
#'
#' @usage gombe_bio(convert = TRUE, rename = TRUE)
#' @param convert Whether you want to convert date columns to date format.  Default is TRUE.
#' @param rename Whether to rename columns with friendlier names.  Default is TRUE, so set to false for
#'   compatibility with old code.
#' @return Returns the biography table.
#' @examples
#' bio = gombe_bio()
#' #saves the biography table as "bio"
#'
#' @export
#' @importFrom utils read.csv


gombe_bio <- function(convert = TRUE, rename = TRUE) {
  oldwd = getwd()
  setwd("C:/Users/Joseph Feldblum/Dropbox (Duke Bio_Ea)/Home Drive_db/Research/DATABASE/excel files")
  # if(!"lubridate" %in% (.packages())){library(lubridate)}
  bio = read.csv("biography.csv", header = T, stringsAsFactors = F)
  if(convert == TRUE){
    bio$B_Birthdate = lubridate::mdy(bio$B_Birthdate)
    bio$B_BDMin = lubridate::mdy(bio$B_BDMin)
    bio$B_BDMax = lubridate::mdy(bio$B_BDMax)
    bio$B_Entrydate = lubridate::mdy(bio$B_Entrydate)
    bio$B_Departdate = lubridate::mdy(bio$B_Departdate)
  }
  if(rename == TRUE){
    colnames(bio) = c("id", "id_num", "name", "birth_group", "bg_cert", "sex",
                      "momid", "dadid", "dad_pub_info", "firstborn", "bday", "bday_min",
                      "bday_max", "bday_dist", "entrydate", "entry_type", "departdate",
                      "depart_date_error", "departtype")
  }
  setwd(oldwd)
  rm(oldwd)
  return(bio)
}



#' Import the community_membership table
#'
#' @usage gombe_comm_memb(convert = TRUE, rename = TRUE)
#' @param convert Whether you want to convert date columns to date format.  Default is TRUE.
#' @param rename Renames HK to KH and renames column headers to nicer names
#' @return Returns the community_membership table.
#' @examples
#' comm = gombe_comm_memb()
#' #saves the community_membership table as "comm"
#'
#' @export
#' @importFrom utils read.csv


gombe_comm_memb <- function(convert = TRUE, rename = TRUE) {
  oldwd = getwd()
  setwd("C:/Users/Joseph Feldblum/Dropbox (Duke Bio_Ea)/Home Drive_db/Research/DATABASE/excel files")
  # if(!"lubridate" %in% (.packages())){library(lubridate)}
  comm = read.csv("community_membership.csv", header = T, stringsAsFactors = F)
  if(convert == TRUE){
    comm$CM_start_date = lubridate::dmy(comm$CM_start_date)
    comm$CM_end_date = lubridate::dmy(comm$CM_end_date)
    comm$CM_end_date_prelim = lubridate::mdy(comm$CM_end_date_prelim)
  }

  if(rename == TRUE){
    comm$CM_CL_community_id[comm$CM_CL_community_id == "HK"] = "KH"
    names(comm) = c("chimp_id", "start_date", "end_date", "comm_id", "start_source",
                    "end_source", "end_date_prelim")
  }

  setwd(oldwd)
  rm(oldwd)
  return(comm)
}


#' Import NEW female reproductive states table
#'
#' @usage gombe_fem_repro_hist_NEW(convert = TRUE)
#' @param convert Whether you want to convert date columns to date format.  Default is TRUE.
#' @return Returns the female reproductive states table from a csv file.  For faster loading
#'   use the gombe_fem_repro_hist command, which loads from an .RData file.  NB: uses the newer
#'   reproductive STATES table, not the older reproductive history table
#' @examples
#' frh = gombe_fem_repro_hist_NEW()
#' #saves the female reproductive history table as "frh"
#'
#' @export
#' @importFrom utils read.csv


gombe_fem_repro_hist_NEW <- function(convert = TRUE) {
  oldwd = getwd()
  setwd("C:/Users/Joseph Feldblum/Dropbox (Duke Bio_Ea)/Home Drive_db/Research/DATABASE/excel files")
  # if(!"lubridate" %in% (.packages())){library(lubridate)}
  frh = read.csv("female_repro_states.csv", header = T, stringsAsFactors = F)
  if(convert == TRUE){
    frh$CalendarDates = lubridate::mdy(frh$CalendarDates)
  }
  setwd(oldwd)
  rm(oldwd)
  return(frh)
}



#' Import NEW female reproductive states table
#'
#' @usage gombe_fem_repro_hist(rename = TRUE)
#' @return Returns the female reproductive states table.  NB: uses the newer
#'   reproductive STATES table, not the older reproductive history table
#' @param rename Whether to rename columns with friendlier names.  Default is TRUE, so set to false for
#'   compatibility with old code.
#' @examples
#' frh = gombe_fem_repro_hist()
#' #saves the female reproductive history table as "frh"
#'
#' @export
#' @importFrom utils read.csv


gombe_fem_repro_hist <- function(rename = TRUE) {
  oldwd = getwd()
  setwd("C:/Users/Joseph Feldblum/Dropbox (Duke Bio_Ea)/Home Drive_db/Research/DATABASE/excel files")
  load(file = "frh.RData")
  if(rename == TRUE){
    names(frh) = c("id", "date", "estrus_min", "estrus_max", "day_of_estr", "day_of_est_cert", "cycl_start_est_method",
                   "repro_state", "lactat_end_cert", "lact_end_est_method", "concept_est_method", "cycl_decis_source", "parity",
                   "youngest_kid_age", "young_kid_id")
  }
  setwd(oldwd)
  rm(oldwd)
  return(frh)
}


#' Import aggression_event table
#'
#' @usage gombe_agg_event(rename = TRUE, trim = FALSE)
#' @param rename Whether to rename columns with friendlier names.  Default is TRUE.
#' @param trim Whether to remove full description, source, extracted by, and year columns.  Default is FALSE.
#' @return Returns the aggression event table.
#' @examples
#' agg = gombe_agg_event()
#' #saves the aggression event table as "agg"
#'
#' @export
#'


gombe_agg_event <- function(rename = TRUE, trim = FALSE) {
  oldwd = getwd()
  setwd("C:/Users/Joseph Feldblum/Dropbox (Duke Bio_Ea)/Home Drive_db/Research/DATABASE/excel files")
  load(file = "aggression_event.RData")
  if(rename == TRUE){
    colnames(agg) = c("date", "foc_id", "time", "decided", "aggressor_id", "agg_beh",
                      "mult_aggressor_flag", "recip_id", "recip_cert", "recip_beh", "mult_recip_flag", "bad_obs",
                      "bristle", "display", "chase", "contact", "vocal", "fight_cat", "full_descrip",
                      "comm_id", "source", "extracted_by", "year")
  }
  if(trim == TRUE){
    agg = agg[,-c(19,21,22,23)]
  }
  setwd(oldwd)
  rm(oldwd)
  return(agg)
}


#' Import follow table
#'
#' @usage gombe_follow()
#' @return Returns the follow table.
#' @examples
#' fol = gombe_follow()
#' #saves the aggression event table as "fol"
#'
#' @export
#'


gombe_follow <- function() {
  oldwd = getwd()
  setwd("C:/Users/Joseph Feldblum/Dropbox (Duke Bio_Ea)/Home Drive_db/Research/DATABASE/excel files")
  load(file = "follow.RData")
  setwd(oldwd)
  rm(oldwd)
  return(fol)
}



#' Import follow_arrival table
#'
#' @usage gombe_fol_arr(rename = TRUE)
#' @param rename Whether to rename columns with friendlier names.  Default is TRUE.
#' @return Returns the follow arrival table.
#' @examples
#' fol_arr = gombe_fol_arr()
#' #saves the aggression event table as "fol_arr"
#'
#' @export
#'


gombe_fol_arr <- function(rename = TRUE) {
  oldwd = getwd()
  setwd("C:/Users/Joseph Feldblum/Dropbox (Duke Bio_Ea)/Home Drive_db/Research/DATABASE/excel files")
  load(file = "follow_arrival.RData")

  if(rename == T){
  names(fol_arr) = c("date", "foc_id", "arr_id", "seq_num", "certain", "type_of_nest",
                "type_of_cycle", "time_start", "time_end", "durat_of_obs", "update")
  }
  setwd(oldwd)
  rm(oldwd)
  return(fol_arr)
}


#' Import male post-fission communities table
#'
#' @usage gombe_fiss_comms()
#' @return Returns a table of communities males ended up in post 1973 fission.
#' @examples
#' fiss_com = gombe_fiss_comms()
#' #saves the aggression event table as "fiss_com"
#'
#' @export
#'


gombe_fiss_comms <- function() {
  oldwd = getwd()
  setwd("C:/Users/Joseph Feldblum/Dropbox (Duke Bio_Ea)/Home Drive_db/Research/DATABASE/excel files")
  load(file = "fission_communities.RData")
  setwd(oldwd)
  rm(oldwd)
  return(fiss_com)
}


#' Import the food_bout table
#'
#' @usage gombe_food_bout(rename = TRUE)
#' @param rename Whether to rename columns with friendlier names.  Default is TRUE.
#' @return Returns the food_bout table.
#' @examples
#' food_bout = gombe_food_bout()
#' #imports the food bout table as "food_bout"
#'
#' @export
#'


gombe_food_bout <- function(rename = TRUE) {
  oldwd = getwd()
  setwd("C:/Users/Joseph Feldblum/Dropbox (Duke Bio_Ea)/Home Drive_db/Research/DATABASE/excel files")
  load(file = "food_bout.RData")
  if(rename == TRUE){
    names(food_bout) = c("date", "foc_id", "start_fd_time", "end_fd_time", "duration", "local_fd_part",
                      "local_fd_name", "local_fd_part2", "local_fd_name2", "local_fd_part_writ", "local_fd_name_writ", "update")
  }
  setwd(oldwd)
  rm(oldwd)
  return(food_bout)
}


#' Import the A-Rec attendance table --KK ONLY--
#'
#' @usage gombe_a_rec(rename = TRUE, trim = TRUE)
#' @param rename Whether to rename columns with friendlier names.  Default is TRUE.
#' @param trim Whether to remove community ID, observers, and old cycle info columns.
#'   Default is TRUE.
#' @return Returns the a_rec table.
#' @examples
#' a_rec = gombe_a_rec()
#' #imports the A-Record attendance table as "a_rec"
#'
#' @export
#'

gombe_a_rec = function(rename = TRUE, trim = TRUE) {

  oldwd = getwd()
  setwd("C:/Users/Joseph Feldblum/Dropbox (Duke Bio_Ea)/Home Drive_db/Research/DATABASE/excel files")
  load(file = "A-Rec attendance.RData")
  if(rename == TRUE){
    names(a_rec) = c("date", "comm_id", "chimp_id", "seq_num", "type_of_cycle", "obs1_id", "obs2_id",
                         "bananas_given", "degr_of_arr", "degr_of_depart", "time_start", "time_end",
                     "durat_of_obs", "day", "month", "year", "cycle_old")
  }
  if(trim == TRUE){
    a_rec = a_rec[,-c(2, 6, 7, 17)]

  }
  setwd(oldwd)
  rm(oldwd)
  return(a_rec)

}


#' Import pantgrunt_event table
#'
#' @usage gombe_pg_event(rename = TRUE, trim = FALSE, cleaned = TRUE)
#' @param rename Whether to rename columns with friendlier names.  Default is TRUE.
#' @param trim Whether to remove full notes, source, extracted by, and year columns.  Default is FALSE.
#' @param cleaned Whether to load pg table with multiple actors in individual rows rather than
#'   one row separated by ommas
#' @return Returns the pantgrunt event table.
#' @examples
#' pg = gombe_pg_event()
#' #saves the aggression event table as "pg"
#'
#' @export
#'


gombe_pg_event <- function(rename = TRUE, trim = FALSE, cleaned = TRUE) {
  oldwd = getwd()
  setwd("C:/Users/Joseph Feldblum/Dropbox (Duke Bio_Ea)/Home Drive_db/Research/DATABASE/excel files")

  if(cleaned == T) {
    load(file = "pantgrunt_event_clean.RData")
  } else if(cleaned == F){
    load(file = "pantgrunt_event.RData")
    } else
      stop("cleaned must be TRUE or FALSE")

  if(rename == TRUE){
    colnames(pg) = c("date", "foc_id", "time", "actor_id", "recipient_id", "mult_actor_flag",
                      "mult_recip_flag", "two_sided_flag", "pg_notes", "data_source", "extracted_by", "community",
                      "yr")
  }
  if(trim == TRUE){
    pg = pg[,-c(9,10,11,13)]
  }
  setwd(oldwd)
  return(pg)
}

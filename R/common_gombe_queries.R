
#' Add age to a table
#'
#' @usage add_age(old_df, bio_tab = bio, idcol, datecol, newcolname = "age", deletebday = T)
#' @param old_df Name of database to which you wish to add chimp ages
#' @param bio_tab Name of biography table from which to find birthdays. Defaults to bio.
#' @param idcol Name of column in old_df with chimp IDs. Use quotation marks around colname
#' @param datecol Name of column in old_df with date on which to calculate ages. Use quotes.
#' @param newcolname Name of new age column. Defaults to "age".
#' @param deletebday Whether to delete temporary birthday column. Defaults to TRUE.
#' @return Returns the original table plus an age column.
#' @examples
#' bio = gombe_bio()
#' dais = data.frame(date = lubridate::dmy("1/1/2000"),
#'                    id1 = c("WL", "FN", "FF", "AO"),
#'                    id2 = rep("TB", times = 4),
#'                    dai = rnorm(n = 4, mean = 0, sd = 1))
#' dais = add_age(old_df = dais, idcol = "id1",
#'                 datecol = "date", newcolname = "age1")
#' dais = add_age(old_df = dais, idcol = "id2",
#'                 datecol = "date", newcolname = "age2")
#'
#' #adds new column to table that has an age column for "id1", and another for "id2"
#'
#' @export
#'

add_age <- function(old_df, bio_tab = bio, idcol, datecol, newcolname = "age", deletebday = T) {

  if(names(bio_tab)[1] == "id"){

    nn = (1:ncol(old_df))[colnames(old_df) == idcol]

    old_df$bday = bio_tab$bday[match(old_df[,nn], bio_tab$id)]

    dd = (1:ncol(old_df))[colnames(old_df) == datecol]

    if(class(old_df[,dd]) != "Date") {stop("datecol must be of class Date")} #double check this

    old_df$age = (as.numeric((old_df[,dd]-old_df$bday)/365.25))
    names(old_df)[names(old_df) == "age"] = newcolname

    if(deletebday == T) {old_df = old_df[,!names(old_df) %in% c("bday")]}

    return(old_df)

  } else if(names(bio_tab)[1] == "B_AnimID"){

    nn = (1:ncol(old_df))[colnames(old_df) == idcol]

    old_df$bday = bio_tab$B_Birthdate[match(old_df[,nn], bio_tab$B_AnimID)]

    dd = (1:ncol(old_df))[colnames(old_df) == datecol]

    if(class(old_df[,dd]) != "Date") {stop("datecol must be of class Date")} #double check this

    old_df$age = (as.numeric((old_df[,dd]-old_df$bday)/365.25))
    names(old_df)[names(old_df) == "age"] = newcolname

    if(deletebday == T) {old_df = old_df[,!names(old_df) %in% c("bday")]}

    return(old_df)

  } else {stop("use gombe_bio function to import biography table in correct format")}


}



#' Get demography at given date
#'
#' @usage gombe_demog(ddate = "31/12/2016", comms = "all")
#' @param ddate date in form of DAY/MONTH/YEAR, <= "31/12/2016"
#' @param comms Communities desired.  Defaults to all, but can be in ("KK", "MT", "main", periph", "all")
#' @return Returns demography counts on a given date
#' @examples
#' demog = gombe_demog(ddate = "30/6/1986", comms = "KK")
#'   #returns demography counts for KK community on June 30, 1986
#'
#' @export
#'


gombe_demog = function(ddate = "31/12/2016", comms = "all"){

  ddate = lubridate::dmy(ddate)

  if(!comms %in% c("KK", "KH", "MT","main", "periph", "all")){
    stop("comms should be KK, KH, MT, main, periph, or all")}

  if(comms == "all"){comms = c("KK", "MT", "KK_MT", "KK_P0", "KK_P1", "KH", "KK_KL", "MT_KK")}
  if(comms == "periph"){comms = c("KK_MT", "KK_P0", "KK_P1", "KK_KL", "MT_KK")}
  if(comms == "main"){comms = c("KK", "MT", "KH")}

  comm = gombedatabase::gombe_comm_memb()[,-c(5:8)]
  bio = gombedatabase::gombe_bio()
  # bio = bio[,names(bio) %in% c("id", "sex", "bday", "departdate")]

  if(ddate > max(bio$departdate, na.rm = T)){stop("date is after biography end date")}

  comm = comm[comm$start_date <= ddate & comm$end_date >= ddate,]

  bio = bio[bio$bday <= ddate & bio$departdate >= ddate,]
  bio = bio[!is.na(bio$id),]

  bio$comm_id = comm$comm_id[match(bio$id, comm$chimp_id)]

  bio$aclass = ifelse((ddate - bio$bday)/365.25 >=12, "adult", "subadult")

  bio = bio[bio$comm_id %in% comms,]

  demog = plyr::ddply(bio, plyr::.(comm_id, aclass, sex), plyr::summarize,
                      count = length(id))
  demog$dem_date = ddate

  return(demog)
}


#' Return subset of the biography table
#'
#' @usage gombe_bio_id(id)
#' @param id ID desired (as individual or vector of IDs)
#' @return Returns desired subset of biography table.
#' @examples
#' gombe_bio_id(id = "FN")
#' #returns Fanni's life deets
#'
#' @export


gombe_bio_id <- function(id) {
  oldwd = getwd()
  setwd("C:/Users/Joseph Feldblum/Dropbox (Duke Bio_Ea)/Home Drive_db/Research/DATABASE/excel files")
  load(file = "quickbio.RData")
  setwd(oldwd)
  rm(oldwd)
  return(bio[bio$id %in% id,])
}


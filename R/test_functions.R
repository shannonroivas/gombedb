# Test functions
#

#' Import the biography table
#'
#' @usage gombe_bio_test(rename = TRUE)
#' @param rename Whether to rename columns with friendlier names.  Default is TRUE, so set to false for
#'   compatibility with old code.
#' @return Returns the biography table.
#' @examples
#' bio = gombe_bio_test()
#' #saves the biography table as "bio"
#'
#' @export


gombe_bio_test <- function(rename = TRUE) {

  if(rename == FALSE){
    colnames(bio) = c("B_AnimID", "B_AnimID_num", "B_AnimName", "B_BirthGroup", "B_BGCertainty", "B_Sex",
                      "B_MomID", "B_DadID", "B_DadID_publication_info", "B_FirstBorn", "B_Birthdate", "B_BDMin",
                      "B_BDMax", "B_BDDist", "B_Entrydate", "B_Entrytype", "B_Departdate", "B_DepartdateError",
                      "B_Departtype")
  }
  if(rename == TRUE){
    colnames(bio) = c("id", "id_num", "name", "birth_group", "bg_cert", "sex",
                      "momid", "dadid", "dad_pub_info", "firstborn", "bday", "bday_min",
                      "bday_max", "bday_dist", "entrydate", "entry_type", "departdate",
                      "depart_date_error", "departtype")
  }
  return(bio)
}

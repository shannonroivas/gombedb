#' Biography table
#'
#' The biography table!
#'
#' @format A data frame with 332 rows and 19 variables:
#' \describe{
#'   \item{id}{chimp ID}
#'   \item{id_num}{id number}
#'   \item{name}{chimp name}
#'   \item{birth_group}{birth group}
#'   \item{bg_cert}{birth group certainty}
#'   \item{sex}{chimp sex}
#'   \item{momid}{mom's ID (if known)}
#'   \item{dadid}{dad's ID (if known)}
#'   \item{dad_pub_info}{where dad ID first published}
#'   \item{firstborn}{is individual first born to mother}
#'   \item{bday}{birthday}
#'   \item{bday_min}{earliest possible birthday}
#'   \item{bday_max}{latest possible birthday}
#'   \item{bday_dist}{skewness of possible birthdays within plausible window}
#'   \item{entrydate}{date first seen}
#'   \item{entry_type}{type of entry into community}
#'   \item{departdate}{date of departure}
#'   \item{depart_date_error}{uncertainty in depart date}
#'   \item{departtype}{(D)eath, (E)?, (O)still alive, (P)?}
#' }
#' @source none of your business
"bio"

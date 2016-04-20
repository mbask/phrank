#' Get a Date object from numeric year and doy
#'
#' @param year 
#' @param doy 
#'
#' @return a Date object
#' @export
#'
#' @examples
get_date_from_year_doy <- function(year, doy) {
  as.Date(paste(year, doy, sep = "-"), format='%Y-%j')
}

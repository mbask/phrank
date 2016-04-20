#' Title
#'
#' @note To developer: still needs love! Variables must be referenced to col_names
#'
#' @param col_names 
#' @param logger 
#' @param data 
#' @param ... arguments passed to zoo::na.approx or zoo::na.spline, eg to control for the type of spline
#'
#' @return
#' @export
#'
#' @examples
interpolate_index <- function(data, col_names, method = "linear", logger, ...) {
  
  group_no_year <- setdiff(col_names$group_keys, col_names$year)
  logger$log("Interpolating indexes in each group by %s", paste(group_no_year, collapse = ","))
  
  data %<>%
    group_by_(.dots = group_no_year)
  
  if (method == "linear") {
    data %<>%
     mutate(value = na.approx(value, date_relevant, na.rm = FALSE))
  }
  if (method == "spline") {
    data %<>%
      mutate(value = na.spline(value, date_relevant, na.rm = FALSE))
  }
  data
}



get_res <- function(product) {
  switch(product,
         "MOD13Q1" = 0.25,
         "MOD13A1" = 0.5,
         "MOD13A2" = 1,
         "MOD13A3" = 1
  )

}

get_res_str = function(resolution) {
  switch(as.character(resolution),
         "0.25" = "250m",
         "0.5"  = "500m",
         "1"    = "1km")
}

get_aggr_str <- function(product) {
  switch(product,
         "MOD13Q1" = "16_days",
         "MOD13A1" = "16_days",
         "MOD13A2" = "16_days",
         "MOD13A3" = "Monthly",
         "FluxData" = "Daily"
  )
}

get_cycle_length_days <- function(product) {
  switch(product,
         "MOD13Q1" = 365,
         "MOD13A1" = 365,
         "MOD13A2" = 365,
         "MOD13A3" = 365,
         "FluxData" = 365
  )
}

#' Title
#'
#' @param doy 
#'
#' @return
#' @export
#'
#' @examples
#' get_doy_intervals(c(3,4,33), "MOD13Q1")
get_doy_intervals <- function(doy, product) {
  # 16 is the interval in days between two max value composite images of the product MOD13Q1
  aggr               <- get_aggr(product)
  intervals_in_cycle <- round(get_cycle_length_days(product) / aggr)
  right_limit        <- intervals_in_cycle * aggr + 1
  doy_of_composite   <- seq(1, right_limit, by = aggr)
  cut(doy, breaks = doy_of_composite, labels = FALSE, right = FALSE)
}

get_obs_in_each_year <- function(product) {
  switch(get_aggr_str(product),
         "16_days" = 23,
         "Monthly" = 12,
         "Daily"   = 365
  )
}


#' Title
#'
#' @param product 
#'
#' @return
#' @export
#'
#' @examples
get_aggr <- function(product) {
  switch(get_aggr_str(product),
         "16_days" = 16,
         "Monthly" = 30.5,
         "Daily"   = 1
         )
}

get_res_aggr_str <- function(product) {
  paste(get_res_str(get_res(product)), get_aggr_str(product), sep = "_")
}

#' Title
#'
#' @param x 
#' @param y 
#' @param product 
#'
#' @return
#' @export
#'
#' @examples
get_pixel_size = function(x, y, product) {
  ((x * 2 / get_res(product)) + 1) * ((y * 2 / get_res(product)) + 1)
}

# Extract band name from a character vector
#
# Elements of the character vector should be in the format e.g. "MOD13Q1.A2015241.h18v04.005.2015258043158.250m_16_days_pixel_reliability"
# Note that each element must hold only 5 parts separated by "_" character
#
# @param V6 a character vector
# @return vector of band names
# @author Marco Bascietto \email{marco.bascietto@@entecra.it}
# @examples
# extract_band_from_V6("MOD13Q1.A2015241.h18v04.005.2015258043158.250m_16_days_pixel_reliability")
# #"pixelreliability"
extract_band_from_V6 <- function (V6) {
  substr_match <- c(4, 5)
  
  # 2-column matrix with fourth and fifth matched substrings of V6
  tmp <- t(sapply(strsplit(V6, "_", fixed = TRUE), `[`, substr_match))
  # Should fifth matched substring be NA, replace it with an empty string (eg. when V6 is EVI or NDVI bands)
  tmp[, 2] <- ifelse(is.na(tmp[, 2]), "", tmp[, 2])
  
  paste0(tmp[, 1], tmp[, 2])
}



# Extract full band name from a character vector
#
# Elements of the character vector should be in the format e.g. "MOD13Q1.A2015241.h18v04.005.2015258043158.250m_16_days_pixel_reliability"
# Note that each element must hold only 6 parts separated by "." character
#
# @param V6 a character vector
# @return vector of band full names
# @author Marco Bascietto \email{marco.bascietto@@entecra.it}
# @examples
# extract_full_band_from_V6("MOD13Q1.A2015241.h18v04.005.2015258043158.250m_16_days_pixel_reliability")
# #"250m_16_days_pixel_reliability"
extract_full_band_from_V6 <- function (V6) {
  substr_match <- 6
  sapply(strsplit(V6, ".", fixed = TRUE), `[`, substr_match)
}


# Extract date from a character vector
#
# Elements of the character vector should be in the format e.g. "A2010257"
#
# @param date_code vector or modis dates
# @return vector of \code{Date}s
# @author Marco Bascietto \email{marco.bascietto@@entecra.it}
# @examples
# extract_date_from_V8("A2010257")
# #[1] "2010-09-14"
extract_date_from_V8 <- function(date_code) {
  get_date_from_year_doy(substr(date_code, 2, 5), substr(date_code, 6, 8))
}





#' Create a data frame from All Combinations of pixel, Date and id factors
#
#' @param arg_list a list of the following elements:
#' * product character vector of product ids (eg "MOD13Q1")
#' * site_id character vectors of site ids
#' * start_year numeric vector, usually 2000
#' * end_year numeric vector of ending year for each site
#' * pixel_col_names vector of pixel names
#' * count_of_days count of days in between two dates
#'
#' @author Marco Bascietto \email{marco.bascietto@@entecra.it}
#' @return a data.frame
expand_dates_grid <- function(arg_list) {

  build_product_site_grid <- function(product, indexes, site, start_date, end_date, pixel_col_names, count_of_days) {
    date_increment <- paste0(count_of_days, " days")
    # build a vector of dates every count_of_days, starting from first date in each product, site
    year_seq <- seq(
      year(start_date),
      year(end_date), 
      by = 1)
    date_increment <- paste0(count_of_days, " days")
    # build a vector of dates every count_of_days, starting 1st of January of each year
    date_seq <- do.call(
      c,
      lapply(
        year_seq,
        function(year) {
          start_date     <- as.Date(paste0(year, "-01-01" ))
          end_date       <- as.Date(paste0(year, "-12-31" ))
          seq(start_date, end_date, by = date_increment)
        }
    ))
    # Remove trailing dates (i.e. in year 2000 modis started from 18th February, not 1st January)
    date_seq <- date_seq[date_seq >= start_date & date_seq <= end_date]

    expand.grid(
      product = product,
      index   = indexes,
      site_id = site,
      pixel   = pixel_col_names,
      date    = date_seq,
      stringsAsFactors = FALSE
    )
  } # of build_product_site_grid function

  stopifnot(arg_list$count_of_days > 0)
  with(
    arg_list,
    bind_rows(
      mapply(
        build_product_site_grid, 
        product, indexes, site_id, start_date, end_date, 
        MoreArgs = list(pixel_col_names, count_of_days),
        SIMPLIFY = FALSE))
    )
}


#' Title
#'
#' @param value_vector 
#'
#' @return
#' @export
#'
#' @examples
count_na_values <- function(value_vector) {
  stopifnot(!missing(value_vector))
  sum(is.na(value_vector))
}


is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
  abs(x - round(x)) < tol
}


#' Title
#'
#' @param header 
#' @param new 
#' @param old 
#'
#' @return
#' @export
#'
#' @examples
get_new_header <- function(header, new, old) { 
  index <- grep(paste0("^", header, "$"), old)
  new[index]
}


safe.ifelse <- function(cond, yes, no) structure(ifelse(cond, yes, no), class = class(yes))

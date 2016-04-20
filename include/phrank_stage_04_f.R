#' Title
#'
#' @param data 
#' @param col_names 
#' @param logger 
#'
#' @return
#' @export
#'
#' @examples
set_lower_quality_to_na <- function(data, col_names, logger) {

  stopifnot(col_names$min_usefulness %in% colnames(data))

  logger$log("Setting indexes with lower than a usefulness threshold to NA")
  # mutate verb for creating index columns while setting to NA low-quality values
#   tmp_new_raw <- list(
#     col_names   = list(col_names$index_value, "remarks"),
#     col_values  = list(
#       lazyeval::interp(~ifelse(u <= m, NA_real_, i),      .values = with(col_names, list(u = as.name(usefulness), m = as.name(min_usefulness), i = as.name(index_value)))),
#       lazyeval::interp(~ifelse(u <= m, r, NA_character_), .values = with(col_names, list(u = as.name(usefulness), m = as.name(min_usefulness), r = "Lower than usefulness threshold")))))
  tmp_new_raw <- list(
    col_names   = list(col_names$index_value, "remarks"),
    col_values  = list(
      interp(~ifelse(u >= m, NA_real_, i),      .values = with(col_names, list(u = as.name(reliability), m = as.name(min_reliability), i = as.name(index_value)))),
      interp(~ifelse(u >= m, r, NA_character_), .values = with(col_names, list(u = as.name(reliability), m = as.name(min_reliability), r = "Lower than usefulness threshold")))))
  
  
  data %<>% 
#     cbind(
#       get_vi_quality_assessment(.$quality, vi_usefulness))# %>% ##################### variabilizzare quality
    mutate_(.dots = setNames(tmp_new_raw$col_values, tmp_new_raw$col_names))
  
  tmp_bad_values <- sum(!is.na(data$remarks))
  logger$log("%d (%0.2f%%) unuseful values found", tmp_bad_values, tmp_bad_values / nrow(data) * 100)
  
  data
}


#' Parse VI QA SDS bits to human readable descriptions
#'
#' Vegetation Index Quality Assessment Science Data Sets (QA SDS) are packed in a 16 bit numeric for each pixel
#'
#' @param vi_qa_sds a vector of numeric QA SDS indices, NAs are handled gracefully
#' @param sds_parameter_name name of the sds parameter to be looked for in \code{vi_qa_sds}, one of vi_quality, vi_usefulness, aerosol_quantity, adjacent_cloud_detected, atmospheric_BRDF_correction, mixed_clouds, land_water_mask, possible_snow_ice, possible_shadow
#'
#' @return a data.frame with a description column holding the description for each parsed bits packet and sometimes other columns (as a vi_usefulness_level for vi_usefulness \code{sds_parameter_name})
#'
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @importFrom magrittr "%>%"
#'
#' @export
#' @references Kamel Didan, Armando Barreto Munoz, Ramon Solano, Alfredo Huete. MODIS Vegetation Index User's Guide (MOD13 Series) Version 3.00, June 2015 (Collection 6)
#'
#' @author Marco Bascietto \email{marco.bascietto@@entecra.it}
#'
#' @examples
#' get_vi_quality_assessment(c(51229, 51425), vi_quality)
#'#                    vi_quality_description
#'#1 Pixel produced, but most probably cloudy
#'#2 Pixel produced, but most probably cloudy
#' get_vi_quality_assessment(c(51229, NA, 51425), vi_usefulness)
#'#  vi_usefulness_description vi_usefulness_level
#'#1           L1B data faulty          0.06666667
#'#2                      <NA>                  NA
#'#3             Lower quality          0.93333333
get_vi_quality_assessment <- function(vi_qa_sds, sds_parameter_name) {
  
  parameters <- as.list(match.call()[-1])
  sds_parameter_name <- as.character(parameters$sds_parameter_name)
  
  stopifnot(sds_parameter_name %in% names(MOD13Q1_quality_l))
  stopifnot(is.vector(vi_qa_sds))
  
  parameter <- MOD13Q1_quality_l[[sds_parameter_name]]
  
  bit_seq <- with(
    parameter,
    seq(start_bit[1] + 1, start_bit[1] + nchar(value[1]))
  )
  
  tmp <- data.frame(
    value = sapply(vi_qa_sds, function(x) ifelse(is.na(x), NA_character_, paste0(as.numeric(intToBits(x)[bit_seq]), collapse = ""))),
    stringsAsFactors = FALSE
  )
  
  tmp <- tmp %>% dplyr::left_join(parameter, by = "value")
  
  dplyr::select(tmp, -value, -start_bit)
}

#' Title
#'
#' @param modis_df 
#' @param main_cfg_l 
#' @param logger 
#'
#' @return
#' @export
#'
#' @examples
reshape_data <- function(modis_df, main_cfg_l, col_names, invalid_file_name, logger) {
  stopifnot(!missing(main_cfg_l), !missing(modis_df))
  modis_df %>% 
    reshape_filter_data(main_cfg_l, col_names, invalid_file_name, logger) %>%
    mutate(date = extract_date_from_V8(V8))
}




#' Title
#'
#' @param modis_df 
#' @param main_cfg_l 
#' @param logger 
#'
#' @return
#'
#' @examples
reshape_filter_data <- function(modis_df, main_cfg_l, col_names, invalid_file_name, logger) {
  stopifnot(!missing(main_cfg_l), !missing(modis_df))
  
  # Add band type column. The band is coded in "V6" column
  modis_df %<>%
    mutate(band = extract_full_band_from_V6(V6)) %>%
    rename_(.dots = setNames("V7", col_names$product))
  
  logger$log("Gathering data (rows now %d)...", nrow(modis_df), level = DEBUG)

  modis_df %<>%
    gather_(
      key_col     = "pixel", 
      value_col   = "value", 
      gather_cols = main_cfg_l$pixel_column_names)

  # Columns:
  # V1..V10 [except V7] | band | site_id | product | pixel | value
  
  group_criterium = c(setdiff(col_names$group_keys, c(col_names$year, col_names$index)), "band", "V8") # "product", site_id", "pixel", "band", "V8"

  valid_modis_df <- modis_df %>% 
    ungroup() %>% 
    filter_valid_values(main_cfg_l, invalid_file_name, logger) %>% 
    select_(.dots = c("V6", "value", group_criterium))
    
  logger$log("Finalizing refinement of data")
  
  valid_modis_df %<>%
    group_by_(.dots = group_criterium) %>% 
    mutate(V6 = 1:n()) %>% 
    ungroup() %>%
    spread(
      key   = band,
      value = value) %>% # Columns now: # V6|product|site_id|pixel|V8|one column for each band (eg 250m_16_days_composite_day_of_the_year, 250m_16_days_EVI, and so on)
    dplyr::select(-V6) %>% 
    gather_(
      key_col     = col_names$index, 
      value_col   = col_names$index_value, 
      gather_cols = col_names$modis_indexes) # Columns now: # product|site_id|pixel|V8|index|value|one column for each metadata band (ie except the indexes (eg EVI and NDVI or NEE and GPP))
  }


# @references https://lpdaac.usgs.gov/dataset_discovery/modis/modis_products_table/mod13q1
#' Title
#'
#' @param modis_df 
#' @param main_cfg_l 
#' @param invalid_file_name
#' @param logger 
#'
#' @return
#' @export
#'
#' @examples
filter_valid_values <- function(modis_df, main_cfg_l, invalid_file_name, logger) {
  stopifnot(!missing(main_cfg_l), !missing(modis_df))
  stopifnot(grepl(".csv$", invalid_file_name))

  logger$log(
    "Checking for invalid (out of range) values (previously %d rows)...",
    nrow(modis_df),
    level = DEBUG)
  
  modis_df %<>%
    left_join(modis_layers, by = c("product", "band"))

  modis_invalid_df <- modis_df %>%
    filter(value < ML_valid_range_min | value > ML_valid_range_max)

  invalid_rows_count <- nrow(modis_invalid_df)
  logger$log("Found %d invalid values", invalid_rows_count)
  if (invalid_rows_count > 0) {
    write.csv(modis_invalid_df, file = invalid_file_name)
    logger$log("Removed rows written to %s CSV file", invalid_file_name)
  }
  
  modis_valid_df <- modis_df %>%
    anti_join(modis_invalid_df, by = c("site_id", "product", "band", "pixel", "V8")) %>%
    dplyr::select(-starts_with("ML_"))

  logger$log(
    "%d invalid values removed, table is now %d rows",
    invalid_rows_count, nrow(modis_valid_df),
    level = DEBUG)
  
  modis_valid_df
}



#' Title
#'
#' @param data 
#' @param main_cfg_l 
#' @param gaps_join_keys 
#' @param logger 
#'
#' @return
#' @export
#'
#' @examples
fill_modis_data_gaps <- function(data, main_cfg_l, col_names, logger) {
  stopifnot(!missing(data), !missing(main_cfg_l), !missing(logger))

  prior_nrow <- nrow(data)
  logger$log("Filling gaps in modis data (originally %d rows) with NAs...", prior_nrow, level = DEBUG)
  
  arg_list <- as.list(
    data %>% 
      group_by_(.dots = with(col_names, setdiff(group_keys, c(index, year, pixel)))) %>% # assuming start and end dates do not differ among pixels of same site_id and among indexes
      summarise(
        start_date = min(date),
        end_date   = max(date)))

  arg_list <- within(arg_list, {
    pixel_col_names <- main_cfg_l$pixel_column_names
    count_of_days   <- round(main_cfg_l$length_of_cycle_days / main_cfg_l$obs_in_each_year)
    indexes         <- unique(data[, col_names$index])
  })

  #logger$log("Inserting rows for the following groups of data:", arg_list, capture = TRUE, level = DEBUG)
  
  tmp_date_df <- expand_dates_grid(arg_list = arg_list)
  
  gaps_join_keys <- c(setdiff(col_names$group_keys, col_names$year), "date")
  missing_dates_df <- tmp_date_df %>% 
    anti_join(data, by = gaps_join_keys) %>% 
    arrange_(.dots = gaps_join_keys)
  missing_dates_count <- nrow(missing_dates_df)
  class(missing_dates_df) <- "data.frame"
  logger$log("Found %d missing dates rows", missing_dates_count)
  
  if (missing_dates_count > 0) {
    logger$log("Inserting the following missing dates rows",  missing_dates_df, capture = TRUE)
    data %<>% 
      right_join(tmp_date_df, by = gaps_join_keys)
    gaps_count <- nrow(data) - prior_nrow
    if (missing_dates_count != gaps_count) { 
      logger$log("A join mismatch occurred, filled NA row are %d whereas missing rows are %d...", gaps_count, missing_dates_count, level = ERROR)
    }
    logger$log(
      "Filled NAs to %d total rows (%d) * doy (%.02f) gaps",
      gaps_count, main_cfg_l$count_of_pixels_in_grid, gaps_count / main_cfg_l$count_of_pixels_in_grid)
  }
  data
}


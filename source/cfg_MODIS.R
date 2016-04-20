# Load modis sites list from file -----------------------------------------

# Needed for downloading data from MODIS and per site configurations
source("source/import_site_cfg.R")

# Partial configuration list ---------------------------------------------

cfg_l <- within(cfg_l, {
  save_plot             <- FALSE
  modis_bands           <- c("250m_16_days_NDVI", "250m_16_days_EVI", "250m_16_days_VI_Quality", "250m_16_days_pixel_reliability", "250m_16_days_composite_day_of_the_year")
  products              <- c("MOD13Q1") # Only 1 product supported currently
  length_of_cycle_days  <- 365
  #subset_pixels         <- unlist(mapply(seq, from = c(21, 30, 39, 48, 57), to = c(25, 34, 43, 52, 61), SIMPLIFY = FALSE)) # subset a 1.25kmx1.25km square around central pixel (5x5 pixels)
  #subset_pixels         <- unlist(mapply(seq, from = c(31, 40, 49), to = c(33, 42, 51), SIMPLIFY = FALSE)) # subset a 1kmx1km square around central pixel (3x3 pixels)
  #subset_pixels         <- seq(1, 81) # no subset
  subset_pixels         <- 41

  log_file_name         <- paste0("log/phrank_", Sys.Date(), ".log")
  in_path               <- "data/modis/"
  out_path              <- "data/out/modis/"
  data_elaboration_path <- "data/elaboration_path/"
  plot_elaboration_path <- "plot/elaboration_path/"
  plot_phenology_path   <- "plot/phenology/"
  invalid_file_name       <- "removed_invalid_MODIS_data.csv"
  low_on_values_file_name <- "removed_scarse_MODIS_data.csv"
  count_of_pixels_in_grid <- get_pixel_size(x = site_cfg_l$x_size[1], y = site_cfg_l$y_size[1], products)
  metadata_col_count      <- 10
  pixel_column_names      <- paste0("pixel_", seq(1, count_of_pixels_in_grid))
  pixel_column_index      <- seq(1, count_of_pixels_in_grid) + metadata_col_count
  obs_in_each_year        <- get_obs_in_each_year(products[1])
})


# Definition of column names ----------------------------------------------

col_names <- list(
  product     = "product",
  site        = "site_id",
  year        = "year",
  pixel       = "pixel",
  index       = "index",
  index_value = "value",
  date        = "date",
  group_keys     = c("product", "index", "site_id", "year", "pixel")) # compute metric on index grouped by these columns


# Indexes -----------------------------------------------------------------

col_names <- within(col_names, {
  modis_indexes  <- c(  # The actual name of MODIS bands holding indexes
    paste0(get_res_aggr_str(cfg_l$products[1]), "_NDVI"), # No need to change this one
    paste0(get_res_aggr_str(cfg_l$products[1]), "_EVI"))  # No need to change this one
})


# Metadata columns ----------------------------------------------------------

col_names <- within(col_names, {
  long_metadata_col_to_indexes <- c(
    paste0(get_res_aggr_str(cfg_l$products[1]), "_VI_Quality"), # No need to change this one
    paste0(get_res_aggr_str(cfg_l$products[1]), "_pixel_reliability"), # No need to change this one
    paste0(get_res_aggr_str(cfg_l$products[1]), "_composite_day_of_the_year")) # No need to change this one (MUST BE THE LAST ONE)
  short_metadata_col_to_indexes <-  c(
    "quality", # No need to change this one
    "reliability", # No need to change this one
    "doy_composite") # No need to change this one (MUST BE THE LAST ONE, see function "interpolate_index")
  })


# Other column  names -----------------------------------------------------

col_names <- within(col_names, {
  usefulness  <- "vi_usefulness_level" # No need to change this one
  reliability <- "reliability" # No need to change this one
})



# Column names for site_cfg -----------------------------------------------

col_names <- within(col_names, {
  min_usefulness = "min_usefulness_level"
  min_reliability = "min_reliability_level"
  smooth_window  = "smooth_window"
  seasonality    = "seasonality"
  max_NA_months  = paste0("max_NA_", substr(month.name, 1, 3)) # i.e. chr [1:12] "max_NA_Jan" "max_NA_Feb" "max_NA_Mar" "max_NA_Apr" "max_NA_May" "max_NA_Jun" "max_NA_Jul" ...
})

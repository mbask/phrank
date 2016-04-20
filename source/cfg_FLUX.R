# Load modis sites list from file -----------------------------------------

# Needed for per-site configurations
source("source/import_site_cfg.R")

# Complete configuration list ---------------------------------------------

cfg_l <- within(cfg_l, {
  save_plot             <- FALSE
  modis_bands           <- c("NEEst_fMDS", "GPPst_MDS")
  products              <- c("FluxData") # Only 1 product supported currently

  log_file_name         <- paste0("log/phrank_", Sys.Date(), ".log")
  in_path               <- "data/fluxes/"
  out_path              <- "data/out/fluxes/"
  data_elaboration_path <- "data/elaboration_path/"
  plot_elaboration_path <- "plot/elaboration_path/"
  plot_phenology_path   <- "plot/phenology/"
  invalid_file_name     <- "removed_invalid_FLUXES_data.csv"
  low_on_values_file_name <- "removed_scarse_FLUXES_data.csv"
  count_of_pixels_in_grid <- 1
  metadata_col_count      <- 3
  length_of_cycle_days    <- 365
  subset_pixels           <- 1
  pixel_column_names      <- paste0("pixel_", seq(1, count_of_pixels_in_grid))
  pixel_column_index      <- seq(1, count_of_pixels_in_grid) + metadata_col_count
  obs_in_each_year        <- get_obs_in_each_year(products[1])
})



# Configuration for importing flux daily L4 files -------------------------

cfg_l <- within(cfg_l, {
  flux_file_types <- list(
    level       = 4, # {[1..4]}
    aggregation = "d") # {h|d|w|m} eg hourly, daily, weekly, monthly
  fill_values <- list(
    NEE_st_fMDS = -9999.00,
    NEEst_fMDS  = -99999L, # must match "ML_fill_value" in file modis_layers.csv
    GPP_st_MDS  = -9999.00,
    GPPst_MDS   = -99999L) # must match "ML_fill_value" in file modis_layers.csv
  by_scale_factors <- list(
    NEEst_fMDS = -1E3, # We will scale NEE by -1000 (thus positive NEE is a sink of C). Must match "ML_by_scale_factor" in file modis_layers.csv
    GPPst_MDS  = 1E3) # We will scale GPP by 1000. Must match "ML_by_scale_factor" in file modis_layers.csv
  match_to_modis <- list(
    V6      = "1.2.3.4.5.s1_s2_s3_") # matched to beginning of V6 (eg "MOD13Q1.A2015241.h18v04.005.2015258043158.250m_16_days_pixel_reliability")
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


# Indexes ------------------------------------------------------------------

col_names <- within(col_names, {
  modis_indexes = c(
    "s1_s2_s3_NEEst_fMDS", # No need to change this one
    "s1_s2_s3_GPPst_MDS")  # No need to change this one
})


# Metadata columns ----------------------------------------------------------

col_names <- within(col_names, {
  long_metadata_col_to_indexes = c(
    "s1_s2_s3_DoY") # # No need to change this one (MUST BE THE LAST ONE)
  short_metadata_col_to_indexes = c(
    "doy_composite") # No need to change this one (MUST BE THE LAST ONE, see function "interpolate_index")
})


# Other column  names -----------------------------------------------------

col_names <- within(col_names, {
  usefulness  = "vi_usefulness_level"
})


# Column names for site_cfg -----------------------------------------------

col_names <- within(col_names, {
  min_usefulness = "min_usefulness_level"
  smooth_window  = "smooth_window"
  seasonality    = "seasonality"
  max_NA_months  = paste0("max_NA_", substr(month.name, 1, 3)) # i.e. chr [1:12] "max_NA_Jan" "max_NA_Feb" "max_NA_Mar" "max_NA_Apr" "max_NA_May" "max_NA_Jun" "max_NA_Jul" ...
})

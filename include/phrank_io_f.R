# MODIS data I/O methods --------------------------------------------------

#' Title
#'
#' @param modis_data_set 
#' @param save_file_name 
#' @param cfg_class 
#' @param logger 
#'
#' @return
#'
#' @examples
import_modis_data = function(modis_data_set, save_file_name, cfg_class, logger) {
  stopifnot(cfg_class$modis_product_str == cfg_class$products[1]) #because invalid values are coded only for MOD13Q1 product, get_vi_quality_assessment

  if (missing(modis_data_set)) {
    # download modis data set from the Internet
    modis_data_path <- tempdir()
    
    # list of files for the modis product that will be probably overwritten by downloaded modis data
    tmp_modis_file_names <- get_modis_file_list(modis_data_path, cfg_class$modis_product_str)
    
    # remove existing modis product files prior to download new ones
    if (length(tmp_modis_file_names) > 0) {
      unlink(tmp_modis_file_names)
    }
    
    logger$log("MODIS product: %s", cfg_class$modis_product_str, level = DEBUG)
    logger$log("MODIS bands:", cfg_class$get_modis_bands(), capture = TRUE, level = DEBUG)
    logger$log("MODIS subset data:", cfg_class$get_modis_subset(), capture = TRUE, level = DEBUG)
    logger$log("MODIS SaveDir: %s", modis_data_path, level = DEBUG)
    logger$log("MODIS size:", cfg_class$size, capture = TRUE, level = DEBUG)
    logger$log("Start downloading modis data through MODISTools::MODISSubsets...")
    
    # load and write to disk new modis products
    MODISTools::MODISSubsets(
      LoadDat   = cfg_class$get_modis_subset(),
      Products  = cfg_class$modis_product_str,
      Bands     = cfg_class$get_modis_bands(),
      Size      = cfg_class$size,
      SaveDir   = modis_data_path,
      StartDate = TRUE
    )
    
    logger$log("End downloading modis data")
    
    # list of files for the modis products just downloaded
    tmp_modis_file_names <- get_modis_file_list(modis_data_path, cfg_class$modis_product_str)
    
    if (length(tmp_modis_file_names) == 0) {
      self$log("No modis ascii files written to disk in %s! Exiting...", modis_data_path, level = DEBUG)
      stop("Cannot import any modis data file. Exiting...")
    }
    modis_data_set <- load_modis_files(tmp_modis_file_names, cfg_class, logger)
  } else {
    # Import modis data set given as argument in modis_data_set
    count_of_pixel_in_grid <- ncol(modis_data_set) - cfg_class$get_metadata_col_count() # 10 are the metadata columns V1-V10
    if (count_of_pixel_in_grid != cfg_class$get_count_of_pixel_in_grid()) {
      logger$log(
        "Count of pixels in grid in dataset (%d) different from expected (%d). Exiting...",
        count_of_pixel_in_grid, cfg_class$get_count_of_pixel_in_grid(),
        level = FATAL)
      stop("Error during modis data import. Exiting...")
    }
    
    modis_bands <- unique(extract_full_band_from_V6(modis_data_set$V6))
    if (!identical(modis_bands, cfg_class$get_modis_bands())) {
      logger$log(
        "Bands in dataset (printed below) different from expected. Exiting...",
        modis_bands, capture = TRUE,
        level = FATAL)
      stop("Error during modis data import. Exiting...")
    }
    
    logger$log("Importing modis data, first 5 rows:", head(modis_data_set, 5), capture = TRUE)
  }
  
  if (!missing(save_file_name)) save_pristine_modis_data(save_file_name, modis_data_set, cfg_class, logger)
  modis_data_set
}

get_modis_file_list = function(modis_data_path, modis_product_str) {
  stopifnot(!missing(modis_data_path) | !missing(modis_product_str))
  list.files(
    path       = modis_data_path,
    pattern    = paste0(modis_product_str, ".asc$"),
    full.names = TRUE
  )
}

load_modis_files = function(modis_file_names, cfg_class, logger) {
  logger$log("Loading modis ascii files:", modis_file_names, capture = TRUE)
  
  # Load csv modis files
  modis_data_l <- load_files(modis_file_names)
  logger$log("Loaded %d files", length(modis_data_l))
  
  logger$log("Joining tables...", level = DEBUG)
  # Join all modis tables
  modis_data_df <- Reduce(
    function(x, y) inner_join(x, y, by = paste0("V", c(seq(1, 10))))
    , modis_data_l
    , accumulate = FALSE
  )
  logger$log("Joined all tables", level = DEBUG)
  modis_data_df
}


#' Loads all modis csv files
#'
#' MODIS csv files are created by MODISTools::MODISSubsets. They have no header and are location-specific
#'
#' @note All modis data files must hold the same count of columns in order to be binded together row-wise.
#' As a result all locations must hold the same count of pixel and, therefore, the same grid (eg 2km x 2km = 81 pixel, columns V11..V91), on top of the initial 10 columns (V1..V10)
#'
#' @param file_list the list of modis product ascii files, as written by MODISTools::MODISSubsets
#' @return a list of data frames
#'
#' @author Marco Bascietto \email{marco.bascietto@@entecra.it}
#' @examples
#' modis_files <- list.files(path = "data/MODIS/", pattern ="MOD13Q1.asc$", full.names = TRUE)
#' \dontrun{
#' load_files(modis_files)
#' }
load_files <- function(file_list) {
  lapply(
    file_list
    , read.csv
    , header = FALSE
    , as.is  = TRUE
  )
}



save_pristine_modis_data = function(save_file_name, modis_data_set, cfg_class, logger) {
  if (!missing(save_file_name)) {
    logger$log("Saving pristine modis data to %s...", save_file_name)
    write.csv(x = modis_data_set, file = save_file_name, row.names = FALSE)
  }
}


#' Add a new site MODIS data
#'
#' Either by loading a pre-esisting CSV data file or by downloading it from MODIS ORNL web service.
#'
#'
#' @param site_info 
#' @param product 
#' @param bands 
#' @param data_path 
#' @param logger 
#'
#' @return a data frame of modis data downloaded or loaded from disk binded with site_id, res_aggr, count_of_pixels_in_grid, metadata_col_count, pixel_column_names, and obs_in_each_year columns
#' @export
#' @author Marco Bascietto \email{marco.bascietto@@entecra.it}
#'
#' @examples
add_site_record <- function(site_info, product, bands, data_path, logger) {
  
  site <- VI$new(
    lat        = as.numeric(site_info["lat"]),
    long       = as.numeric(site_info["long"]),
    start_date = as.integer(site_info["start_year"]),
    end_date   = as.integer(site_info["end_year"]),
    product    = product,
    x_size     = as.integer(site_info["x_size"]),
    y_size     = as.integer(site_info["y_size"]),
    id         = site_info["site_id"], 
    bands      = bands
  )
  
  modis_data_file_name <- paste0(data_path, site$csv_file_name)
  logger$log("Testing existence of '%s' MODIS data file", modis_data_file_name)
  modis_df <- try(read.csv(modis_data_file_name, as.is = TRUE), silent = TRUE)
  
  if (class(modis_df) != "data.frame") { # CSV modis data file does not exist yet, import modis data from ORNL site and save the CSV modis data file
    modis_df <- import_modis_data(
      save_file_name = modis_data_file_name,
      cfg_class      = site,
      logger         = logger)
  } else {                                # CSV modis data file exists, import it 
    modis_df <- import_modis_data(
      modis_data_set = modis_df,
      cfg_class      = site,
      logger         = logger)
  }

  modis_df$site_id  <- site$id
  rm(site)
  modis_df
}

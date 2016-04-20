# R6 Vegetation index class -----------------------------------------------

VI <- R6::R6Class(
  "VI",
  #
  # Public members
  #
  public = list(
    size        = NULL,
    resolution  = NULL,
    aggregation = NULL,
    band        = NULL,
    id          = NULL,
    
    initialize = function(lat, long, start_date, end_date, id = NULL, x_size = 1, y_size = 1, resolution = NULL, aggregation = NULL, bands, product = NULL) {
      available_products    <- c("MOD13Q1", "MOD13A1", "MOD13A2", "MOD13A3")
      available_aggregation <- c("16_days", "Monthly") # if more than 16_days and Monthly then also edit the private$metadata$obs_in_each_year <- ifelse(self$aggregation == "16_days", 23, 12) line
      #available_bands       <- c("EVI", "NDVI")
      available_resolutions <- c(0.25, 0.50, 1)
      
      stopifnot(
        is.numeric(x_size) && x_size >= 1,
        is.numeric(y_size) && y_size >= 1,
        is.null(resolution)  || resolution %in% available_resolutions,
        is.null(aggregation) || match.arg(aggregation, available_aggregation) %in% available_aggregation,
        is.null(product)     || match.arg(product, available_products) %in% available_products,
        !is.null(id),
        !missing(bands)
      )
      
      # If product argument is given then it has precedence over resolution and aggregation
      if (!is.null(resolution) && is.null(product))
        self$resolution <- resolution
      if (!is.null(aggregation) && is.null(product)) {
        self$aggregation <- match.arg(aggregation, available_aggregation)
      }
      if (!is.null(product)) self$modis_product_str <- product
      
      stopifnot(!is.null(self$modis_product_str))
      
      self$size <- c(x_size, y_size)
      #self$band <- match.arg(band, available_bands)
      self$id   <- id
      
      private$set_modis_subset(lat, long, start_date, end_date)
      private$set_modis_bands(bands)
      private$set_pixel_size()
      private$set_pixel_column_names()
      private$set_obs_in_each_year()
    },
    
    
    # getters -----------------------------------------------------------------
    get_modis_bands = function() {
      private$modis_bands
    },
    
    get_modis_subset = function() {
      private$modis_subset
    },
    
    get_metadata_col_count = function() {
      private$metadata$count_metadata_columns
    },
    
    get_count_of_pixel_in_grid = function() {
      private$count_of_pixel_in_grid
    },
    
    get_pixel_column_names = function() {
      private$pixel_col_names
    },
    
    get_obs_in_each_year = function() {
      private$metadata$obs_in_each_year
    }
  ),
  
  
  
  # Active bindings ---------------------------------------------------------
  
  active = list(
    csv_file_name = function() {
      paste0(private$modis_subset$start.date, "-", private$modis_subset$end.date, "_", self$id, "_day_composite.csv")
    },
    
    resolution_str = function() {
      switch(as.character(self$resolution), "0.25" = "250m", "0.5" = "500m", "1" = "1km")
    },
    
    resolution_aggregation = function() {
      paste(self$resolution_str, self$aggregation, sep = "_")
    },
    
    modis_product_str = function(product) {
      if (missing(product)) {
        switch(self$resolution_aggregation,
               "250m_16_days" = "MOD13Q1",
               "500m_16_days" = "MOD13A1",
               "1km_16_days"  = "MOD13A2",
               "1km_Monthly" = "MOD13A3"
        )
      } else {
        switch(product,
               "MOD13Q1" = { self$resolution <- 0.25; self$aggregation <- "16_days" },
               "MOD13A1" = { self$resolution <- 0.5;  self$aggregation <- "16_days" },
               "MOD13A2" = { self$resolution <- 1;    self$aggregation <- "16_days" },
               "MOD13A3" = { self$resolution <- 1;    self$aggregation <- "Monthly" }
        )
      }
    }
  ),
  
  # Private members ---------------------------------------------------------
  
  private = list(
    modis_subset = NULL,
    modis_bands  = NULL,
    metadata = list(
      count_metadata_columns = 10,
      length_of_cycle_days   = 365, # 1 year = 365 days
      obs_in_each_year       =  NULL  # Count of observations in each year, for each pixel, for MOD13Q1 product
    ),
    modis_data_path        = NULL,
    count_of_pixel_in_grid = NULL,
    pixel_col_names        = NULL,
    vi_col_name            = NULL,
    smooth_vi_col_name     = "smooth_vi",
    modis_data = list(pristine = NULL, current = NULL, invalid = NULL, unfiltered = NULL, full = NULL),
    likely_green_season    = NULL,
    
    set_obs_in_each_year = function() {
      private$metadata$obs_in_each_year <- ifelse(self$aggregation == "16_days", 23, 12)
    },
    
    set_modis_subset = function(lat, long, start_date, end_date) {
      stopifnot(
        is.numeric(lat) && lat >= -90 && lat <= 90,
        is.numeric(long) && long >= -180 && long <= 180,
        is.numeric(start_date) && start_date >= 2000 && start_date <= as.integer(year(Sys.Date())),
        is.numeric(end_date) && end_date >= 2000 && end_date <= as.integer(year(Sys.Date())) && start_date <= end_date
      )
      private$modis_subset <- data.frame(
        lat = lat, long = long, start.date = start_date, end.date = end_date,
        geo_coordinate = paste0(sprintf("%.5f", lat), sprintf("%.5f", long)) # geographical coordinates pasted (needed for Build lat/long matrix)
      )
      # Number of days between ndvi observations
      #private$metadata$days_between_obs <- round(private$metadata$length_of_cycle_days / private$metadata$obs_in_each_year)
      # day of year sequence of each ndvi yearly sequence (i.e. 1, 17, 33, ..., for 16-day Composite)
      #private$metadata$standard_doy <- (seq(1, private$metadata$obs_in_each_year, 1) - 1) * private$metadata$days_between_obs + 1
      # Number of days between interpolated ndvi observations
      #private$metadata$days_between_interp_obs <- private$metadata$days_between_obs / 2
      
    },
    
    set_modis_bands = function(bands) {
      private$modis_bands <- bands
    },
    
    set_pixel_size = function() {
      private$count_of_pixel_in_grid <- ((self$size[1] * 2 / self$resolution) + 1) * ((self$size[2] * 2 / self$resolution) + 1)
    },
    
    set_pixel_column_names = function() {
      private$pixel_col_names <- paste0("pixel_", seq(1, private$count_of_pixel_in_grid))
    }
    
  )
)


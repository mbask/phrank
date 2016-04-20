
# source quality layers data
if (modis_or_flux_flag %in% c("MODIS", "FLUX")) source("support/phrank_use_data.R")

# source R6 VI class needed to download or load from disk modis data
if (modis_or_flux_flag == "MODIS") source("source/phrank_R6_VI_class.R")

# R6 Log class ------------------------------------------------------------

Log  <- R6::R6Class(
  "Log",
  #
  # Public members
  #
  public = list(
    
    initialize = function(script_name) {
      if (!missing(script_name)) { 
        private$script_name <- script_name 
      }
      
      reg.finalizer(
        self,
        function(e) e$stop_logging(),
        onexit = TRUE
      )
    },
    
    start_logging = function(logger = NULL) {
      if (!is.null(logger)) {
        private$logger_name <- logger
        futile.logger::flog.info("%s starts logging", private$script_name, name = private$logger_name)
      }
    },
    
    stop_logging = function() {
      self$log("%s stops logging", private$script_name)
      private$logger_name <- NULL
    },
    
    log = function(msg, ..., level = INFO, capture = FALSE) {
      if (!is.null(private$logger_name)) {
        msg <- paste0(private$script_name, ": ", msg)
        level <- as.character(level)
        switch(
          level,
          "8" = futile.logger::flog.debug(msg, ..., capture = capture, name = private$logger_name),
          "6" = futile.logger::flog.info(msg, ...,  capture = capture, name = private$logger_name),
          "4" = futile.logger::flog.warn(msg, ...,  capture = capture, name = private$logger_name),
          "2" = futile.logger::flog.error(msg, ...,  capture = capture, name = private$logger_name),
          "1" = futile.logger::flog.fatal(msg, ..., capture = capture, name = private$logger_name)
        )
      }
    }
  ),
  
  private = list(
    script_name  = "phrank",
    logger_name = NULL
  )
)
    
    
select_modis_pixels = function(data, main_cfg_l, logger) {
  stopifnot(!missing(logger), !missing(main_cfg_l))
  logger$log("Filtering pixels in modis data grid...")
  
  if (!is.numeric(main_cfg_l$subset_pixels)) {
    logger$log("Pixel subset is not numeric. Exiting...", level = WARN)
    warning("Filtering of modis pixel data not performed")
    stop()
  }
  
  count_dupl_pixels <- sum(duplicated(main_cfg_l$subset_pixels))
  if (count_dupl_pixels > 0) {
    logger$log("Found %d duplicated pixels, removing duplicated pixel ids", count_dupl_pixels, level = WARN)
    main_cfg_l$subset_pixels <- unique(main_cfg_l$subset_pixels)
  }
  all_non_pixel_colnames <- colnames(data)[!grepl("^pixel_[0-9]*$", colnames(data))]
  subset_pixels_colnames <- paste("pixel", main_cfg_l$subset_pixels, sep = "_")

  logger$log("Selecting columns %s...", paste(c(subset_pixels_colnames, all_non_pixel_colnames), collapse=", "))
  data %<>% select_(.dots = c(subset_pixels_colnames, all_non_pixel_colnames))

  data
}

get_smoothed_df <- function (data, col_names, logger) {
  # mutate verb for smooth continuo per ogni pixel/sito/prodotto dal primo giorno del primo anno all'ultimo giorno dell'ultimo anno
  tmp_new_smooth <- list(
    col_names   = list(col_names$index_value),
    col_values  = list(
      lazyeval::interp(
        ~smooth_index(i, smoothing_fun = "run_median", l, k[1]), # k of first row is passed assuming all other in group are equal (as it should be)
        .values = list(i = as.name(col_names$index_value), l = logger, k = as.name(col_names$smooth_window)))))
  tmp_groups <- setdiff(col_names$group_keys, col_names$year)
  data %>%
    arrange_(.dots = c(tmp_groups, "date_relevant")) %>% # Fixed for dplyr version 0.4.3.9000 ("arrange() once again ignores grouping (#1206)") 
    group_by_(.dots = tmp_groups) %>%
    mutate_(.dots = setNames(tmp_new_smooth$col_values, tmp_new_smooth$col_names))
}

#' Title
#'
#' @param unsmoothed_col 
#' @param smoothing_fun 
#' @param logger 
#' @param ... 
#'
#' @return
#'
#' @examples
smooth_index = function(unsmoothed_col, smoothing_fun = "run_median", logger, ...) {
  logger$log("Starting smoothing elaboration on index of %d elements...", length(unsmoothed_col))
  smooth_col <- NULL
  
  if (exists(smoothing_fun)) {
    logger$log("Smoothing function '%s' with arguments %s", smoothing_fun, paste(list(...), collapse = ","), level = DEBUG)
    smooth_col <- match.fun(smoothing_fun)(unsmoothed_col, logger, ...)
  }
  logger$log("Smoothing elaboration ended")
  as.numeric(smooth_col)
}

#' Running median
#'
#' Wrapper for runmed in order to not return attribute k to vector of smoothed values
#' endrule argument is set to "constant"
#'
#' @param x numeric vector, the 'dependent' variable to be smoothed. Passed to runmed
#' @param logger
#' @param k integer width of median window; must be odd. Passed to runmed
#'
run_median <- function(x, logger, k) {
  if (!is.wholenumber(k) | length(k) > 1) {
    logger$log("Running median window not valid. Smoothing not performed", level = WARN)
  } else {
    y <- x
    y[is.na(y)] <- -9999
    frq <- rle(y)
    contig_nas <- frq$length[frq$values == -9999] >= k/4
    if (sum(contig_nas) > 0) {
      logger$log("Too many contiguos NAs (%d). Smoothing not performed", sum(contig_nas), level = WARN)
      tmp <- x
    } else {
      tmp <- runmed(x = x, k = k, endrule = "constant")
      attributes(tmp) <- NULL
    }
    tmp
  }
}

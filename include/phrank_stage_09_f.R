#' Title
#'
#' @param data 
#' @param logger 
#' @param col_names 
#'
#' @return
#' @export
#' @author Marco Bascietto \email{marco.bascietto@@crea.gov.it}
#' @examples
detect_metrics <- function(data, col_names, logger) {
  
  # mutate verb for creating derivatives 1st and 2nd for the smoothed index column
  new_cols <- list(
    names   = list("cum_prop", "is_metric_valid", "delta_1", "delta_2", "is_metric_peak", "is_metric_esos"),
    values  = list(
      interp(~cume_dist(x),     .values = list(x = as.name("value_smooth"))),
      interp(~x > threshold,    .values = list(x = as.name("value_smooth"), threshold = as.name("index_value_threshold"))),
      interp(~get_1st_deriv(x, anticipate = TRUE),    .values = list(x = as.name("value_smooth"))),
      interp(~get_1st_deriv(x, anticipate = FALSE), .values = list(x = as.name("delta_1"))),
      interp(~find_peaks(x)   & is_valid,               .values = list(x = as.name("value_smooth"), is_valid = as.name("is_metric_valid"))),
      interp(~find_peaks(x) & is_valid & x > 0, .values = list(x = as.name("delta_2"), is_valid = as.name("is_metric_valid")))))
  
  # mutate verb for creating derivatives 1st and 2nd for the index column
#   new_cols <- list(
#     names   = list("cum_prop", "is_metric_valid", "delta_1", "delta_2", "is_metric_peak", "is_metric_esos"),
#     values  = list(
#       interp(~cume_dist(x), .values = list(x = as.name(col_names$index_value))),
#       interp(~x > threshold, .values = list(x = as.name(col_names$index_value), threshold = as.name("index_value_threshold"))),
#       interp(~get_1st_deriv(x, anticipate = TRUE), .values = list(x = as.name(col_names$index_value))),
#       interp(~get_1st_deriv(x, anticipate = FALSE), .values = list(x = as.name("delta_1"))),
#       interp(~find_peaks(x) & is_valid, .values = list(x = as.name(col_names$index_value), is_valid = as.name("is_metric_valid"))),
#       interp(~find_peaks(x) & is_valid & x > 0, .values = list(x = as.name("delta_2"), is_valid = as.name("is_metric_valid")))))
  
  logger$log("Computing trend peaks and derivatives...")
  metrics_df <- data %>%
    arrange(.dots = c(col_names$group_keys, "doy_relevant")) %>%  # Fixed for dplyr version 0.4.3.9000 ("arrange() once again ignores grouping (#1206)")  
    group_by_(.dots = col_names$group_keys) %>%
    mutate(value_smooth = zoo::rollmean(value, smooth_window, fill = "extend")) %>% 
    mutate_(.dots   = setNames(new_cols$values, new_cols$names)) %>%
    mutate(window_highest_doy = ceiling(366/smooth_window)*smooth_window) %>% # it will be needed when placing metric doys into intervals
    ungroup()

  logger$log("Computing {S|P|E}OS metric weights...")
  
  # Filter out doys that are not metrics
  # Determine the type of metric (metric_sos, metric_peak, metric_eos)
  # Assign weight to metrics (-cum_prop to peaks and the magnitude of increase [delta_1 to eos and sos])
  # Metrics will be ranked accorded to their weight is ascending order (i.e. the smallest the highest rank)
  metrics_df %<>%
    filter(is_metric_esos | is_metric_peak) %>% 
    mutate(
      metric = ifelse(
        is_metric_peak, 
        "metric_peak", 
        c("metric_eos", "metric_eos", "metric_sos")[sign(delta_1)+2]),
      metric_weight = safe.ifelse(
        is_metric_peak,
        -cum_prop,
        safe.ifelse(
          metric == "metric_sos",
          -delta_1 * (1-cum_prop), # metric_sos
          delta_1  * (1-cum_prop)))) %>% # metric_eos
#         safe.ifelse(
#           metric == "metric_sos",
#           -delta_1 / doy_relevant * (1-cum_prop), # metric_sos
#           delta_1  * doy_relevant * (1-cum_prop)))) %>% # metric_eos
      filter(!is.na(metric))

  
  logger$log("Computing {S|P|E}OS metric groups...", level = DEBUG)
  
  # Assign doys to groups ---------------------------------------------------

  # Select the doy with the lowest weight (ie the highest importance) in each metric
  metrics_df %<>%
    group_by_(.dots = c(col_names$group_keys, "metric")) %>% 
    mutate(
      is_metric_weight_min    = metric_weight == min(metric_weight),
      doy_relevant_min_weight = ifelse(is_metric_weight_min, doy_relevant, NA_integer_)) %>% 
    summarise(doy_relevant_min_weight = floor(mean(doy_relevant_min_weight, na.rm = TRUE))) %>% 
    left_join(metrics_df, by = c(col_names$group_keys, "metric"))
  
  
  # Assign metric doys to a group as wide as the site smooth_window (in terms of doys)
  # This is to collate successive similar metrics into a unique one
  # The lowest weight doy will be the first doy of the "smooth_window" wide group
  metrics_df %<>% 
    mutate(
      metric_group = cut(
        doy_relevant,
        breaks         = get_doy_group_breaks(doy_relevant_min_weight[1], smooth_window[1], window_highest_doy[1]), 
        include.lowest = TRUE, 
        labels         = FALSE)) %>% 
    select(-window_highest_doy)


  logger$log("Collating {S|P|E}OS metrics...", level = DEBUG)
  
  # Collate the doy metrics in each group in a unique metric doy.
  # The weight of the grouped metric is the lowest of all metrics
  # The doy of the grouped metric is the first doy (metric_sos), the last doy (metric_eos) or the 
  # average doy of the interval (peak, average weighted by the weights of the peaks)
  metrics_df %<>%
    group_by_(.dots = c(col_names$group_keys, "metric", "metric_group"))  %>% 
    summarise(
      metric_index = grep(metric[1], c("metric_sos", "metric_peak", "metric_eos")),
      metric_weight = c(sum(metric_weight),  min(metric_weight),                          sum(metric_weight))[metric_index], 
      doy_relevant  = c(first(doy_relevant), mean(doy_relevant, weight = -metric_weight), last(doy_relevant))[metric_index], 
      is_metric_peak = is_metric_peak[1])
# 
  logger$log("Ranking {S|P|E}OS metrics...", level = DEBUG)

  # Rank metrics according to their weight (the smaller weight the higher rank).
  # Peaks set of ties are replaced by their minimum;
  # eos and sos ties are permuted with increasing values at each index set of ties
  # rank has to be wrapped in as.numeric due to issue in dplyr (still in 0.4.3):
  #  https://github.com/hadley/dplyr/issues/489
  #  So the assumption made internally is that the expression gives the same type for all groups. 
  #  The type of the result is indeed the type of the first evaluation of the expression. 
  #  rank returns an integer class when ties = "first" and a numeric class when ties = "average"
  metrics_df %<>% 
#    group_by_(.dots = c(col_names$group_keys, "metric"))  %>% 
    mutate(
      metric_rank = rank(metric_weight, ties.method = c("first", "min")[is_metric_peak[1]+1], na.last = "keep")) %>% 
    ungroup() %>%
    select_(.dots = c(col_names$group_keys, "doy_relevant", "metric", "metric_rank"))

  # Preserve only one doy per each metric and metric_rank and, particularly important for 
  # metric_peak, where a peak may last several days
  # MAY BE UNUSEFUL SINCE METRICS ARE NOW GROUPED
  metrics_df %<>%
    group_by_(.dots = c(col_names$group_keys, "metric", "metric_rank")) %>% 
    summarise(doy_relevant = median(doy_relevant))
    
  logger$log("End of {S|P|E}OS metric estimation")
  metrics_df
}



#' Return a vector of sorted doys, at specific doy intervals
#'
#' The reference doy will be the starting doy for a new group
#' @param reference_doy 
#' @param group_range 
#' @param highest_doy 
#'
#' @return
#' @export
#'
#' @examples
get_doy_group_breaks <- function (reference_doy, group_range, highest_doy) {
  if ((reference_doy - group_range) > 2) {
    first_leg <- seq(from = reference_doy - group_range, to = -group_range, by = -group_range)
  } else {
    first_leg <- 1
  }
  sort(c(first_leg, seq(from = reference_doy - 1, to = highest_doy, by = group_range)))
}

#' Difference over previous value of a numeric vector
#'
#' @param series numeric vector
#' @param anticipate logic scalar, if TRUE the first element of the returned series is removed, and a NA is added as the last element
#'
#' @return numeric vector the same length of series, with NA as first element
#' @author Marco Bascietto \email{marco.bascietto@@crea.gov.it}
#'
#' @examples
#' x <- c(2340, 2500, 2000)
#' get_1st_deriv(x)
#' # [1]   NA  160 -500
#' get_1st_deriv(x, anticipate = TRUE)
#' # [1]  160 -500   NA

get_1st_deriv <- function(series, anticipate = FALSE) {
  stopifnot(is.numeric(series))
  stopifnot(is.logical(anticipate))

  deriv <- series - dplyr::lag(series)
  if (anticipate) {
    deriv <- c(tail(deriv, -1), NA) 
  }
  float_limit <- .Machine$double.eps^0.5
  ifelse(deriv > float_limit | deriv < -float_limit, deriv, 0)
}


#' Whether the sign of the value is negative and the sign of the successive value is positive 
#'
#' When value is negative and successive value is positive the concavity of 
#' the curve switches from downwards to upwards
#' @param series 
#'
#' @return
#' @author Marco Bascietto \email{marco.bascietto@@crea.gov.it}
#' @note http://stackoverflow.com/a/25091643/1079872
#' @examples
detect_change_concavity <- function(series) {
  stopifnot(is.numeric(series))
  updn <- c(0, diff(sign(series)))
  sign(updn)
}


# Title
#
# @param x
#
# @return
# @references http://stackoverflow.com/questions/12945113/finding-peaks-in-vector
find_peaks <- function(x) {
  r <- rle(x)
  rep(
    x     = diff(sign(diff(c(x[1], r$values, x[length(x)])))) == -2, 
    times = r$lengths
  )
}

#' Title
#'
#' @param peak_series
#'
#' @return A numeric vector of the same length as \code{peak_series}
#'
#' @author Marco Bascietto \email{marco.bascietto@@crea.gov.it}
rank_peaks <-function(peak_series) {
  stopifnot(is.numeric(peak_series))
  rank(peak_series, ties.method = "first")
}


#' Title
#'
#' @param weight 
#' @param doy 
#'
#' @return
#'
#' @examples
get_doy_weight <- function(weight, doy) {
  ifelse(is.na(weight), NA_character_, paste(doy, weight, sep = "_"))
}

#' Title
#'
#' @param data 
#' @param group_keys 
#' @param logger 
#'
#' @return
#' @export
#'
#' @examples
get_reasonable_seasons <- function (data, group_keys, logger) {
  
  logger$log("Detecting correct season metrics on each %s group...", paste(group_keys, collapse = ","))
  
  likely_green_season_df <- data %>% 
    group_by_(.dots = group_keys) %>% 
    do(
      likely_metrics = detect_correct_season_metric(
        doy_start = .$doy_relevant[.$metric == "metric_sos"],
        doy_end   = .$doy_relevant[.$metric == "metric_eos"],
        doy_peak  = .$doy_relevant[.$metric == "metric_peak"],
        seasonality = .$seasonality[1],
        group = paste(.$product, .$site_id, .$index, .$year, .$pixel, sep = ","),
        logger))
  
  tmp_gs_df <- Reduce(
    bind_rows, 
    apply(
      likely_green_season_df, 
      1, 
      function(row) {
        seasons <- data.frame(
          metric_sos = row$likely_metrics$start, 
          metric_eos = row$likely_metrics$end, 
          metric_peak = row$likely_metrics$peak)
        seasons %>% 
          cbind(row$product, row$index, row$site_id, row$year, row$pixel)
      }
    )
  )
  colnames(tmp_gs_df)[seq(4, length(colnames(tmp_gs_df)))] <- c(group_keys)
  logger$log("End detection")
  
  tmp_gs_df
}


#' Title
#'
#' @param data 
#' @param season_num 
#'
#' @return
#'
#' @examples
remove_too_early_season_starts <- function(data, season_num) {
  data %>%
    mutate(
      min_end = min(end[season == season_num]),
      is_too_early_start = ifelse(season > season_num, start <= min_end, FALSE)) %>% 
    filter(!is_too_early_start)
}

#' Title
#'
#' @param data 
#' @param season_num 
#'
#' @return
#'
#' @examples
remove_too_late_season_endings <- function(data, season_num) {
  data %>%
    mutate(
      min_start = min(start[season == season_num]),
      is_too_late_end = ifelse(season < season_num, end > min_start, FALSE)) %>% 
    filter(!is_too_late_end)
}


`: seasons dataframe` <- ensures_that(is.data.frame(.),
                                      ncol(.) == 4,
                                      is.numeric(.$start),
                                      is.numeric(.$end),
                                      is.numeric(.$peak),
                                      is.numeric(.$season))

#' Title
#'
#' @param doy_start numeric vector of start doys
#' @param doy_end numeric vector of end doys
#' @param doy_peak numeric vector of peak doys
#' @param seasonality 
#'
#' @return
#'
#' @examples
get_metric_combinations <- function(doy_start, doy_end, doy_peak, seasonality) `: seasons dataframe` ({
#   
#   tmp_metric <- data.frame(
#     start = rep(doy_start, length.out = length(doy_peak)),
#     peak  = doy_peak, 
#     end   = rep(doy_end,   length.out = length(doy_peak)))
#   
#   tmp_metric_combn_df <- tmp_metric %>% expand(start, peak, end)
  tmp_metric_combn_df <- expand.grid(
    start = rep(doy_start, length.out = length(doy_peak)),
    peak  = doy_peak, 
    end   = rep(doy_end,   length.out = length(doy_peak)))

  bind_rows(
    lapply(
      seq(1, seasonality), 
      function(season, df) { 
        df$season <- season
        df }, 
      df = tmp_metric_combn_df))
})


#' Title
#'
#' @param doy_start numeric vector of start doys
#' @param doy_end numeric vector of end doys
#' @param doy_peak numeric vector of peak doys
#' @param seasonality 
#'
#' @return
#' @export
#'
#' @examples
detect_correct_season_metric <- function(doy_start, doy_end, doy_peak, seasonality = 1, groups, logger) `: seasons dataframe` ({
  
  logger$log("Detecting reasonable green seasons for %s", groups)
  logger$log(
    "SOS: %s; POS: % s; EOS: %s; seasonality: %d", 
    paste(doy_start, collapse = ","), 
    paste(doy_peak, collapse = ","),
    paste(doy_end, collapse = ","),
    seasonality, 
    level = DEBUG)
  
  no_reasonable_green_season <- data.frame(start = NA_integer_, end = NA_integer_, peak = NA_integer_, seasonality = seasonality)
  
  if (!seasonality %>% check_that(is.numeric, . <= length(doy_peak), . > 0, . <= 3)) {
    return (no_reasonable_green_season)
  }
  if (!doy_start %>% check_that(is.numeric, length(.) >= seasonality)) {
    return (no_reasonable_green_season)
  }
  if (!doy_end %>% check_that(is.numeric, length(.) >= seasonality)) {
    return (no_reasonable_green_season)
  }
  
  seasons_df <- get_metric_combinations(doy_start, doy_end, doy_peak, seasonality) %>% 
    mutate(
      is_start_before_end = start < end,
      is_peak_in_season   = start < peak & peak < end) %>% 
    filter(is_start_before_end, is_peak_in_season)
  
  if (!seasons_df %>% check_that(nrow(.) > 0 ~ "No valid green season(s) found (ie no combinations of start and end doys with a peak doy found)")) {
    return (no_reasonable_green_season)
  }
  
  seasons_df <- seasons_df %>% 
    select(-starts_with("is_")) %>% 
    distinct(season, start, end, .keep_all = TRUE)  # Fixed for dplyr version 0.4.3.9000 ("distinct() now only keeps the distinct variables. If you want to return all variables (using the first row for non-distinct values) use .keep_all = TRUE (#1110).") 
  
  if (seasonality > 1) seasons_df <- seasons_df %>% remove_too_early_season_starts(1)
  if (seasonality > 2) seasons_df <- seasons_df %>% remove_too_early_season_starts(2)
  if (seasonality > 3) seasons_df <- seasons_df %>% remove_too_early_season_starts(3)
  
  seasons_df <- seasons_df %>%
    mutate(
      season_1_start  = min(start) == start,
      season_n_end    = max(end)   == end,
      is_season_start = season == min(season),
      is_season_end   = season == max(season)) %>% 
    group_by(season) %>%
    filter(season_1_start & is_season_start | season_n_end & is_season_end | season > 1 & season < seasonality) %>% 
    ungroup()
  
  seasons_found <- length(unique(seasons_df$season))
  
  if (seasons_found == 1) {
    seasons_df <- seasons_df %>% # Should more than 1 seasons occur, return the ones with the highest count of peaks in
      rowwise() %>% 
      mutate(peak_in_season = sum(doy_peak %in% seq(start, end))) %>% 
      group_by(season) %>%
      filter(peak_in_season == max(peak_in_season))
  } else {
    if (seasonality > 1 & seasons_found > 1) seasons_df <- seasons_df %>% remove_too_late_season_endings(2)
    if (seasonality > 2 & seasons_found > 2) seasons_df <- seasons_df %>% remove_too_late_season_endings(3)
  }
  
  return_seasons_df <- seasons_df %>% select(start, peak, end, season)
  logger$log("Final reasonable seasons: ", return_seasons_df, capture = TRUE)
  return_seasons_df
})

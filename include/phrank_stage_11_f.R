#' Title
#'
#' @param data 
#' @param index_values 
#' @param col_names 
#'
#' @return
#' @export
#'
#' @examples
get_metric_gpp <- function (data, index_values, col_names) {
  sum_col_names  <- list("metric_gpp")
  sum_col_values <- list(
    lazyeval::interp(~sum(x), .values = list(x = as.name(col_names$index_value))))
  
  data %>% 
    right_join(
      index_values %>%
        select_(.dots = c(col_names$group_keys, col_names$index_value, "doy_relevant")),
      by = c(col_names$group_keys)) %>% 
    ungroup() %>% 
    filter(doy_relevant >= metric_sos, doy_relevant <= metric_eos) %>% 
    group_by_(.dots = c(col_names$group_keys, "season_id")) %>% 
    summarise_(.dots = setNames(sum_col_values, sum_col_names)) %>% 
    inner_join(data, by = c(col_names$group_keys, "season_id"))
}

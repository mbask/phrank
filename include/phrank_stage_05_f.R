#' Title
#'
#' @param data 
#' @param col_names 
#' @param low_on_values_file_name 
#' @param logger 
#'
#' @return
#' @export
#'
#' @examples
remove_too_low_groups <- function(data, col_names, low_on_values_file_name, logger) {
  
  # Ensure that max NAs for 12 months have been configured
  col_names$max_NA_months %>% ensure_that(sum(. %in% colnames(data)) == 12 ~ "Did not find 12 max NAs months in configurations")

  # a group now is month/year/pixel/site_id/index/product
  group_keys <- c(col_names$group_keys, "index_month")

  # Summarise verb for counting NA values in index columns
  tmp_count_nas <- list(
    col_names   = list(col_names$index_value),
    col_values  = list(
      lazyeval::interp(~count_na_values(index), .values = list(index  = as.name(col_names$index_value)))))
  
  # assign month to each point
  # i.e. chr [1:12] "max_NA_Jan" "max_NA_Feb" "max_NA_Mar" "max_NA_Apr" "max_NA_May" "max_NA_Jun" "max_NA_Jul" ...
  data %<>% mutate(index_month = month(date_relevant, label = TRUE, abbr = TRUE))

  # sum NAs per month/year/pixel/site_id/index/product
  count_index_values_df <- data %>%
    group_by_(.dots  = group_keys) %>%
    summarise_(.dots = setNames(tmp_count_nas$col_values, tmp_count_nas$col_names))
  
  # select "max_NA_"month data, gathers in key, value (index_month, max_na) format
  max_monthly_nas_df <- data %>% 
    select_(.dots = c(col_names$site, lazyeval::interp(~starts_with(x), x = "max_NA_"))) %>% 
    group_by_(col_names$site) %>% 
    slice(1)
  colnames(max_monthly_nas_df) <- gsub(pattern = "^max_NA_", "", colnames(max_monthly_nas_df))
  max_monthly_nas_df %<>%
    gather(index_month, max_na, -site_id)

  # join max_monthly_nas_df to count_index_values_df in order to filter count of nas < monthly max_na
  count_index_values_df %<>% 
    left_join(max_monthly_nas_df, by = c(col_names$site, "index_month"))
  
  # Filter verb: keep those indexes with too high NAs 
  # in each month/year/pixel/site_id/index/product group
  filter_out_high_counts <- list(
    lazyeval::interp(
      ~count <= max, 
      .values = list(
        count = as.name(col_names$index_value), 
        max   = as.name("max_na"))))
  
  # keep only month/year/pixel/site_id/index/product groups with 
  # count of NAs <= max_NAs per month;
  # count mounths with lower than threshold count of NAs per each year/pixel/site_id/index/product
  # (aka invalid_months) i.e. n should be = 12 to be accepted
  green_flag_groups_df <- count_index_values_df %>%
    filter_(.dots    = filter_out_high_counts) %>%
    group_by_(.dots = col_names$group_keys) %>%
    tally()
  
  logger$log(
    "Found %d %s groups with higher than theshold count of monthly NAs (not all will be removed depending on max_month_NA_count)", 
    nrow(green_flag_groups_df %>% filter(n < 12)), 
    paste0(col_names$group_keys, collapse = ","))
  
  # filter raw index values belonging to months with count of invalid_month_count < 12
  too_many_nas_df <- data %>%
    left_join(
      green_flag_groups_df,
      by = col_names$group_keys) %>%
    mutate(month_threshold = 12-max_month_NA_count) %>% 
    filter(n < month_threshold) %>% 
    rename(invalid_month_count = n)
  
  low_on_rows_count <- nrow(too_many_nas_df)
  logger$log("Found %d raw observations corresponding to groups with too many monthly NAs", low_on_rows_count)
  if (low_on_rows_count > 0) {
    write.csv(too_many_nas_df, file = paste0(cfg_l$out_path, cfg_l$low_on_values_file_name))
    logger$log("Removed rows written to %s CSV file", cfg_l$low_on_values_file_name)
    data %<>%
      anti_join(too_many_nas_df, by = group_keys)
  }
  data %>% select(-index_month)
}

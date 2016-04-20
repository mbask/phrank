#' Title
#'
#' @param cfg 
#' @param site_id 
#' @param dataset_origin 
#' @param site_id_col 
#'
#' @return
#' @export
#'
#' @examples
#' get_site_cfg("smooth_window", "IT-Col", "MODIS")
#' get_site_cfg("smooth_window", "IT-Col", "FLUX")
get_site_cfg <- function(cfg, site_id, dataset_origin, site_id_col = col_names$site) {
  stopifnot(dataset_origin %in% c("MODIS", "FLUX"))
  stopifnot(site_id %in% unique(site_cfg_l[, site_id_col]))

  cfg <- paste(cfg, dataset_origin, sep = "_")
  stopifnot(cfg %in% colnames(site_cfg_l))
  
  site_cfg_l[site_cfg_l[, site_id_col] == site_id, cfg]
}


#' Title
#'
#' @param data 
#' @param dataset_origin 
#' @param site_id_col 
#'
#' @return
#' @export
#'
#' @examples
#' modis_df %>% join_cfg_to_data("MODIS")
join_cfg_to_data <- function(data, dataset_origin, site_id_col = col_names$site) {
  stopifnot(dataset_origin %in% c("MODIS", "FLUX"))
  
  stopifnot(site_id_col %in% colnames(data))
  
  pattern  <- paste0("_", dataset_origin, "$")
  cfg_cols <- colnames(site_cfg_l)[grepl(pattern, colnames(site_cfg_l))]
  cfg_cols_no_origin <- gsub(pattern, "", cfg_cols)
  
  data %>% 
    left_join(
      site_cfg_l %>% 
        select_(.dots = c(cfg_cols, site_id_col)),
      by = site_id_col) %>% 
    rename_(.dots = setNames(cfg_cols, cfg_cols_no_origin))
}

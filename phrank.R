# Copyright (C) 2015 Marco Bascietto <marco.bascietto@crea.gov.it>
# 
# phrank is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# phrank is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License along
# with this program.  If not, see <http://www.gnu.org/licenses/>.

rm(list = ls())

library(futile.logger)
library(dplyr)
library(tidyr)
library(ggplot2)
library(magrittr)
library(ensurer) # used in a few functions
library(lubridate)
library(zoo) # used in interpolation (stage 7) and in computing trend peaks and derivatives (stage 9)
library(lazyeval)


# Set whether we are dealing with MODIS or FLUX data ----------------------

# modis_or_flux_flag <- "MODIS"
modis_or_flux_flag <- match.arg("F", c("FLUX", "MODIS"))


# Source all R files in include/ directory ---------------------------------

if (modis_or_flux_flag %in% c("MODIS", "FLUX")) {
  sapply(
    list.files("include/", pattern = "^phrank_.*\\.[Rr]$")
    , function(file) {source(file.path("include", file)); NULL}
  )
}


# Set configuration basics ------------------------------------------------
# More configurations in files "source/modis_site_import.R" and "source/flux_site_import.R"

cfg_l <- list()

source(paste0("source/cfg_", modis_or_flux_flag, ".R"))
source("source/initialize.R")


# logger ------------------------------------------------------------------

flog.appender(appender.file(cfg_l$log_file_name), name = "my_log")
#flog.appender(appender.console(), name = "my_log")
flog.threshold(DEBUG, name = "my_log")

logger <- Log$new()
logger$start_logging("my_log")


# Import site data (modis or flux) ----------------------------------------

source(paste0("source/import_data_", modis_or_flux_flag, ".R"))

# temporary variables
rm(list = ls(pattern = "^tmp_"))



# Remove duplicated rows --------------------------------------------------
# happened in 2015 IT-Col MODIS data for A2015001...

tmp_row_count <- nrow(modis_df)
modis_df %<>% distinct(.keep_all = TRUE) # Fixed for dplyr version 0.4.3.9000 ("distinct() now only keeps the distinct variables. If you want to return all variables (using the first row for non-distinct values) use .keep_all = TRUE (#1110).") 

logger$log("Removed %d duplicated rows from raw data", tmp_row_count-nrow(modis_df))


# STAGE 01: Subset modis data ----------------------------------------------

# Rename column titles of vi data to pixel_nn
colnames(modis_df)[cfg_l$pixel_column_index] <- cfg_l$pixel_column_names

indexes_subset_df <- modis_df %>%
  select_modis_pixels(main_cfg_l = cfg_l, logger = logger)

cfg_l <- within(cfg_l, {
  count_of_pixels_in_grid <- length(cfg_l$subset_pixels)
  pixel_column_names      <- colnames(indexes_subset_df)[grepl("^pixel_[0-9]*$", colnames(indexes_subset_df))]
  pixel_column_index      <- NULL
})



# STAGE 02: Refine modis data --------------------------------------------------

indexes_valid_df <- indexes_subset_df %>% 
  reshape_data(cfg_l, col_names, paste0(cfg_l$out_path, cfg_l$invalid_file_name), logger)


# Rename long colum names to short ones, remove unneeded metadata ----------------

colnames(indexes_valid_df)[match(col_names$long_metadata_col_to_indexes, names(indexes_valid_df))] <- col_names$short_metadata_col_to_indexes

indexes_valid_df %<>%
  mutate(index = extract_band_from_V6(index)) %>% 
  dplyr::select(-matches("^V[0-9]+$"))



# Boxplot of doy composite vs doy -----------------------------------------

# indexes_valid_df %<>%
#   mutate(
#     doy = yday(date),
#     month = month(date, label = TRUE, abbr = FALSE),
#     year = year(date),
#     doy_shift = ifelse(doy_composite<doy, 365+doy_composite-doy, doy_composite-doy))
# 
# ggplot(indexes_valid_df, aes(x = month, y = doy_shift)) +
#   geom_boxplot() +
#   geom_jitter(alpha = 1/100)+
#   facet_wrap(~site_id) +
#   theme_bw()
# 
# 

# Boxplot of vi_usefulness_level ------------------------------------------

# indexes_valid_df <- cbind(indexes_valid_df, get_vi_quality_assessment(indexes_valid_df$quality, vi_usefulness)) # %>% ##################### variabilizzare quality
# 
# ggplot(indexes_valid_df, aes(x = month(date), y = vi_usefulness_level)) +
#   geom_boxplot(aes(group = month(date))) +
#   facet_wrap(~site_id) +
#   theme_bw()

# Plots after subset ----------------------------------------------------------

tmp_plot_template <- list(
  aes_string(x = "date", y = col_names$index_value),
  scale_x_date("Date"),
  scale_y_continuous("Index value", label = to_vegetation_index_range),
  geom_line(aes_string(group = col_names$pixel)),
  theme_light())


indexes_valid_df %>% 
  group_by_(.dots = setdiff(col_names$group_keys, c(col_names$pixel, col_names$year))) %>% 
  do({
    save_pdf_plot(
      ggplot(.) %+% 
        tmp_plot_template +
        ggtitle(paste0(.$site_id[1], " raw data")),
      paste0(cfg_l$plot_elaboration_path, .$site_id[1], "/00_raw_", .$site_id[1], "_", .$index[1]), 
      save_plot = cfg_l$save_plot,
      logger)
    data.frame(x=NULL)
  })


# Fill gaps in MODIS data ---------------------------------------------

if (modis_or_flux_flag == "MODIS") {
  indexes_valid_df %<>% 
    fill_modis_data_gaps(cfg_l, col_names, logger) # adds missing dates
}



# Add configuration on per-site basis -------------------------------------

indexes_valid_df %<>% 
  join_cfg_to_data(modis_or_flux_flag)



# Add dates metadata ------------------------------------------------------
# doy_composite is 250m_16_days_composite_day_of_the_year or,
# when row was added (by fill_modis_data_gaps), it is doy of date added (that is the doy of the 16-day intervals)
# .$doy_actual_or_not holds composite date (the actual day the picture was taken in the 16-days interval, 
# in case is_doy_composite_or_doy_MODIS is set to "doy_composite" or the actual date is missing) 
# or one of the the 16-days interval of the maximum value daily composite in case 
# is_doy_composite_or_doy_MODIS is set to "doy"

if (modis_or_flux_flag == "FLUX") {
  indexes_valid_df$is_doy_composite_or_doy = "doy"
}

indexes_valid_df %<>% 
  mutate(
    year = year(date),
    doy  = yday(date),
    date_relevant = safe.ifelse(
      is.na(doy_composite) | is_doy_composite_or_doy == "doy", 
      date, 
      safe.ifelse(
        is_doy_composite_or_doy == "doy_composite", 
        get_date_from_year_doy(year, doy_composite),
        date)),
    doy_relevant = yday(date_relevant))



# STAGE 04: Add quality assessment, set to NA lower quality value ---------------------

if (modis_or_flux_flag == "MODIS") {
  indexes_valid_df %<>% 
    set_lower_quality_to_na(col_names, logger)
}


# STAGE 05: Remove groups too low on count of index values ---------------------------

indexes_qc_df <- indexes_valid_df %>% 
  remove_too_low_groups(col_names, cfg_l$low_on_values_file_name, logger)


# STAGE 05: Plots ----------------------------------------------------------------------

tmp_plot_template <- list(
  aes_string(x = "doy_relevant", y = col_names$index_value),
  scale_x_continuous("DOY"),
  scale_y_continuous("Index value", label = to_vegetation_index_range),
  geom_line(aes_string(group = col_names$pixel)),
  facet_wrap(~year),
  theme_light())

indexes_qc_df %>% 
  group_by_(.dots = setdiff(col_names$group_keys, c(col_names$pixel, col_names$year))) %>% 
  do({
    save_pdf_plot(
      ggplot(.) %+% 
        tmp_plot_template +
        ggtitle(paste0(.$site_id[1], " after QC check")),
      paste0(cfg_l$plot_elaboration_path, .$site_id[1], "/02_QC_", .$site_id[1], "_", .$index[1]), 
      save_plot = cfg_l$save_plot,
      logger)
    data.frame(x=NULL)
  })


# STAGE 06: Inputation of gaps for each site, product, index for FLUX data --------

# if (modis_or_flux_flag == "FLUX") {
#   indexes_filled_df <- indexes_qc_df %>%
#     input_NA_indexes(col_names, logger)
# }



# STAGE 07: Interpolation and removal of trailing and leading NAs ----------

# Arguments passed to approx" R function:
# method = "linear":  aspecifies the interpolation method to be used. Choices are "linear" or "constant".
# rule   = 2:         the value at the closest data extreme is used when interpolation 
#                     is to take place outside the interval [min(x), max(x)].


# if (modis_or_flux_flag == "FLUX") {
#   tmp_xout <- seq(1, cfg_l$length_of_cycle_days, by = 1)
# }
# if (modis_or_flux_flag == "MODIS") {
#   tmp_xout <- with(cfg_l, seq(from = 1, to = length_of_cycle_days, by = round(length_of_cycle_days / obs_in_each_year)))
# }

# interpolation is carried out on intermediate NAs, not on
# leading or trailing NAs on the series
# The latter NAs have to be removed to perform smoothing
indexes_filled_df <- indexes_qc_df %>%
  interpolate_index(col_names, logger = logger, method = "linear") %>% 
  filter(!is.na(value))


# STAGE 08: Smoothing ------------------------------------------------------

indexes_smoothed_df <- indexes_filled_df %>%
  get_smoothed_df(col_names, logger)



# STAGE 08: Smoothing Plots -----------------------------------------------------------

indexes_smoothed_df %>% 
  group_by_(.dots = setdiff(col_names$group_keys, c(col_names$pixel, col_names$year))) %>% 
  do({
    save_pdf_plot(
      ggplot(.) %+% 
        tmp_plot_template +
        ggtitle(paste0(.$site_id[1], " smoothed")),
      paste0(cfg_l$plot_elaboration_path, .$site_id[1], "/03_smoothed_", .$site_id[1], "_", .$index[1]), 
      save_plot = cfg_l$save_plot,
      logger = logger)
    data.frame(x=NULL)
    })


# STAGE 09: Save or load smoothed series ----------------------------------------------------

saveRDS(indexes_smoothed_df, file = paste0(cfg_l$out_path, "smoothed_", modis_or_flux_flag, ".rds"))
#indexes_smoothed_df <- readRDS(file = paste0(cfg_l$out_path, "smoothed_", modis_or_flux_flag, ".rds"))


# STAGE 09: Detect {S|P|E}OS metrics -----------------------------------

phenology_df <- indexes_smoothed_df %>%
  select_(.dots = c(col_names$group_keys, col_names$index_value, "doy_relevant", "index_value_threshold", "smooth_window")) %>% 
  detect_metrics(col_names = col_names, logger) %>% 
  left_join(
    indexes_smoothed_df %>% 
      ungroup() %>% 
      select_(.dots = c(col_names$site, "seasonality")) %>% 
      distinct_(.dots = c(col_names$site, "seasonality"), .keep_all = TRUE), # Fixed for dplyr version 0.4.3.9000 ("distinct() now only keeps the distinct variables. If you want to return all variables (using the first row for non-distinct values) use .keep_all = TRUE (#1110).")
    by = col_names$site) %>% 
  filter(metric_rank <= seasonality) # remove 3th order and beyond metrics


# STAGE 10: Reasonable {S|P|E}OS metrics ----------------------------------

logger$log("STAGE 10: Removing unreasonable season metrics and ranking green seasons...")

metrics_df <- phenology_df %>% 
  get_reasonable_seasons(col_names$group_keys, logger) %>% 
  group_by_(.dots = col_names$group_keys) %>% 
  mutate(season_id = letters[seq_along(pixel)])

# Rank reasonable seasons according to the sum of each own metric ranks (S|P|E}OS)
metrics_df %<>% 
  gather(metric, doy_relevant, starts_with("metric")) %>% 
  inner_join(phenology_df, by = c(col_names$group_keys, "metric", "doy_relevant")) %>% 
  group_by(season_id, add = TRUE) %>% 
  summarise(season_rank = sum(metric_rank)) %>% 
  mutate(season_rank = rank(season_rank, ties.method = "min")) %>% 
  left_join(metrics_df, by = c(col_names$group_keys, "season_id"))
  
logger$log("STAGE 10: End")



# STAGE 11: LOS and GPP metrics --------------------------------------------------------

logger$log("STAGE 11: Estimating LOS and GPP metrics...")
metrics_df %<>% 
  mutate(metric_los = metric_eos - metric_sos) %>% 
  get_metric_gpp(indexes_smoothed_df, col_names)

logger$log("STAGE 11: End")



# STAGE 12: Metrics data saving --------------------------------------------------------

logger$log("STAGE 12: Saving data...")

metrics_by_site_pixel_df <- metrics_df %>% 
  left_join(
    indexes_smoothed_df %>% 
      ungroup() %>% 
      select_(.dots = c(col_names$site, "seasonality")) %>% 
      distinct(.keep_all = TRUE), # Fixed for dplyr version 0.4.3.9000 ("distinct() now only keeps the distinct variables. If you want to return all variables (using the first row for non-distinct values) use .keep_all = TRUE (#1110).")
    by = col_names$site)

saveRDS(metrics_by_site_pixel_df, file = paste0(cfg_l$out_path, "metrics_by_site_pixel.rds"))

tmp_by_site_grouping <- c(setdiff(col_names$group_keys, col_names$pixel), "metric")

metrics_by_site_df <- metrics_df %>% 
  gather_("metric", "doy_relevant", c("metric_eos", "metric_sos", "metric_peak", "metric_los", "metric_gpp")) %>% 
  group_by_(.dots = tmp_by_site_grouping) %>% 
  summarise_each(funs(mean(., na.rm = TRUE)), -pixel)

metrics_by_site_df %>% write.csv(file = paste0(cfg_l$out_path, "metrics_by_site.csv"))
saveRDS(metrics_by_site_df, file = paste0(cfg_l$out_path, "metrics_by_site.rds"))

logger$log("STAGE 12: End")


# Remove temporary variables and by-product tables-------------------------

# temporary variables
rm(list = ls(pattern = "^tmp_"))

# likely_ and gpp tables
rm(list = ls(pattern = "green_season_|_gpp_[1-9]+$|^phenology_df$"))

# End of elaboration ------------------------------------------------------


#rm(logger)

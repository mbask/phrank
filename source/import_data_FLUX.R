# Copyright (C) 2015 Marco Bascietto <marco.bascietto@entecra.it>
# 
# flux_data_workflow.R is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# flux_data_workflow.R is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License along
# with this program.  If not, see <http://www.gnu.org/licenses/>.


# The script loads all txt files found, adds a "filename" column, a "date" column, a "site" column,
# binds all tables together and saves the data frame in a .Rdata file.
# Then it reshapes data in order to be fed to phrank
# 
# The script assumes the file name pattern is like
# "^CEIP_EC_Ln_a_[a-zA-Z0-9]{5}_20[0-9]{2}_v[0-9]{2}\\.txt$"
# where n in level and a is aggregation period (see below to tweak them)
#
# The script assumes the loaded tables hold a "Month" and a "Day" columns,
# to form a sensitive "date" column. "Month" and "Day" columns are successively removed.


library(dplyr)
library(tidyr)



# Load and bind flux data -------------------------------------------------

file_daily_aggregation_pattern <- paste0(
  "^CEIP_EC_L",
  cfg_l$flux_file_types$level,
  "_",
  cfg_l$flux_file_types$aggregation,
  "_[a-zA-Z0-9]{5}_20[0-9]{2}_v[0-9]{2}\\.txt$")


#' Load a csv file and adds date, site_id, file_name columns
#'
#' @param file_name 
#' @param file_path 
#'
#' @return a data frame
load_flux_data_files <- function(file_name, file_path, logger) {
  logger$log("Loading %s file...", file_name)
  flux_data     <- read.csv(paste0(file_path, file_name))
  file_metadata <- t(sapply(strsplit(file_name, "_", fixed = TRUE), `[`, c(5, 6)))
  
  flux_data <- within(flux_data, {
    date      <- as.Date(paste(file_metadata[, 2], Month, Day, sep = "-"))
    site_id   <- paste(substr(file_metadata[, 1], 1, 2), substr(file_metadata[, 1], 3, 5), sep = "-")
    file_name <- file_name
    Month     <- NULL
    Day       <- NULL
    logger$log("Imported flux data for site %s, year %s, from date %s to date %s", site_id, file_metadata[, 2], format(min(date), "%F"), format(max(date), "%F"))
  })
  
  flux_data
}


daily_aggregation_files <- list.files(cfg_l$in_path, pattern = file_daily_aggregation_pattern)

tmp_flux_data_df <- dplyr::bind_rows(
  lapply(
    daily_aggregation_files, 
    load_flux_data_files, 
    file_path = cfg_l$in_path,
    logger    = logger))



# Save raw fluxes table ---------------------------------------------------

saveRDS(tmp_flux_data_df, file = paste0(cfg_l$in_path, "flux_data_raw.rds"))


# Reshaping flux data -----------------------------------------------------

tmp_reshaped_df <- within(tmp_flux_data_df, {
  V8 <- paste0("A", year(date), formatC(DoY, width = 3, flag = "0"))
  NEE_st_fMDS <- as.integer(ifelse(
      NEE_st_fMDS == cfg_l$fill_values$NEE_st_fMDS, 
      cfg_l$fill_values$NEEst_fMDS, 
      NEE_st_fMDS * cfg_l$by_scale_factors$NEEst_fMDS))
  GPP_st_MDS <- as.integer(ifelse(
      GPP_st_MDS == cfg_l$fill_values$NEE_st_fMDS, 
      cfg_l$fill_values$GPPst_MDS, 
      GPP_st_MDS * cfg_l$by_scale_factors$GPPst_MDS))
})
tmp_reshaped_df %<>%
  select(V8, NEE_st_fMDS, GPP_st_MDS, site_id, date, DoY) %>%
  rename(
    NEEst_fMDS = NEE_st_fMDS, 
    GPPst_MDS = GPP_st_MDS) %>%
  gather(V6, pixel_1, NEEst_fMDS, GPPst_MDS, DoY)
tmp_reshaped_df <- within(tmp_reshaped_df, {
    V6 <- paste0(cfg_l$match_to_modis$V6, V6)
    V7 = cfg_l$products[1]
})

modis_df <- tmp_reshaped_df[, c("V6", "V7", "V8", "pixel_1", "date", "site_id")]


# Save ready-for-phrank flux data -----------------------------------------

saveRDS(modis_df, file = paste0(cfg_l$in_path, "flux_data_to_be_fed_to_phrank.rds"))

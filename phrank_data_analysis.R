# Copyright (C) 2016 Marco Bascietto <marco.bascietto@crea.gov.it>
# 
# phrank_data_analysis is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# phrank_data_analysis is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License along
# with this program.  If not, see <http://www.gnu.org/licenses/>.

rm(list = ls())

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(magrittr)
library(lme4)

# SVG plot exports
#library(rsvg)
#library(svglite)

# Set configuration basics ------------------------------------------------

cfg_l <- list(
  modis_product  = "MOD13Q1", # Basically for setting the nn-days doy intervals (16 for MOD13Q1, function get_doy_intervals)
  log_file_name  = paste0("log/", Sys.Date(), "_phrank_data_analysis.log"),
  out_path       = "data/out/",
  modis_period_in_year_count = 23,
  doys_metrics  = c("metric_eos", "metric_sos", "metric_peak", "metric_los")
)

cfg_l <- within(cfg_l, {
  in_path_modis        <- paste0(out_path, "modis/")
  in_path_flux         <- paste0(out_path, "fluxes/")
  phenology_file_flux  <- paste0(in_path_flux,  "metrics_by_site_pixel.rds")
  phenology_file_modis <- paste0(in_path_modis, "metrics_by_site_pixel.rds")
  series_file_flux     <- paste0(in_path_flux,  "smoothed_FLUX.rds")
  series_file_modis    <- paste0(in_path_modis, "smoothed_MODIS.rds")
  series_phenology_out <- paste0(out_path,      "series_and_phenology.rds")
})

modis_or_flux_flag <- match.arg("A", c("FLUX", "MODIS", "ANALYSIS"))

source("source/initialize.R")
source("include/phrank_plot_f.R")
source("include/phrank_date_f.R")
source("include/phrank_modis_f.R")

# Load modis sites list from file -----------------------------------------

source("source/import_site_cfg.R")

site_cfg_l %<>%
  select(site_id, igbp_id, genus_id, lat, elevation, kgcc) # the _MODIS and _FLUX columns have been integrated in the respective data frame during phrank elaboration



# Load smoothed series ----------------------------------------------------

# Loads fluxes and modis series datasets, 
# binds them,
# assigns relevant_doys to 16-days (for MOD13Q1) doy intervals)
# take maximum index value among belonging to the same doy interval, as in MODIS composite (relevant 
# only for flux dataset, since it is daily-based whereas modis is 16-days
# based [MOD13Q1])
tmp_series_df <- readRDS(cfg_l$series_file_flux) %>% 
  bind_rows(readRDS(cfg_l$series_file_modis)) %>% 
  mutate(doy = get_doy_intervals(doy_relevant, cfg_l$modis_product)) %>%
  group_by(product, index, site_id, year, pixel, doy) %>% 
  summarise(value_mean_by_doy = max(value), value_sd_by_doy = sd(value))



# Load phenology data ------------------------------------------------------

# Loads phenology metrics for fluxes and modis datasets,
# binds them,
# consider only a number of season lower or equal to the seasonality,
# in each product, site_id, year, pixel group for each index
raw_phenology_df <- readRDS(cfg_l$phenology_file_flux) %>%
  bind_rows(readRDS(cfg_l$phenology_file_modis)) %>% 
  group_by(product, site_id, index, year, pixel) %>%
  filter(season_rank <= seasonality) %>%  
  ungroup()

# Gathers all metric based on doys (basically excluding metric_gpp) in long format,
# assigns relevant_dooys to 16-days (for MOD13Q1) doy intervals)
phenology_df <- raw_phenology_df %>% 
  gather_("metric", "doy_relevant", cfg_l$doys_metrics) %>% 
  mutate(doy = get_doy_intervals(doy_relevant, cfg_l$modis_product))
  



# Join series, phenology and site features --------------------------------

# Joins smoothed series to phenology metrics (in long form, escluding gpp),
# removes duplicates,
# joins site metadata
tmp_join_keys <- c("product", "site_id", "index", "pixel", "year", "doy")
series_phenology_df <-
  tmp_series_df %>% 
  left_join(
    phenology_df, by = tmp_join_keys) %>% 
  distinct_(.dots = c(tmp_join_keys, "metric", "doy_relevant"), .keep_all = TRUE) %>%  # Fixed for dplyr version 0.4.3.9000 ("distinct() now only keeps the distinct variables. If you want to return all variables (using the first row for non-distinct values) use .keep_all = TRUE (#1110).") 
  left_join(site_cfg_l, by = "site_id")



# Set missing doys to NA --------------------------------------------------

# to prevent ggplot to plot the line accross missing years
# series_phenology_df <- series_phenology_df %>% 
#   complete(product, site_id, index, year, pixel, doy_relevant = 1:365) %>% 
#   mutate(date_relevant = get_date_from_year_doy(year, doy_relevant))



# Save series and phenology to disk ---------------------------------------

saveRDS(series_phenology_df, file = cfg_l$series_phenology_out)


rm(tmp_series_df)



# Plot template for season metric plot by site ----------------------------

tmp_plot_template <- list(
  aes(x = doy, y = value_mean_by_doy, color = index),
  scale_color_discrete("Index"),
  scale_shape_discrete("Metric"),
  scale_x_continuous(paste0(get_aggr_str(cfg_l$modis_product), " periods")),
  scale_y_continuous("Index value", label = to_vegetation_index_range),
  geom_line(aes(group = interaction(index, pixel))),
  facet_wrap(~year),
  theme_bw())


# Season metrics plots by site -------------------------------------------------

series_phenology_df %>% 
  group_by(site_id) %>% 
  do({
    save_pdf_plot(
      ggplot(.) %+% 
        tmp_plot_template + 
        geom_point(data = .[!is.na(.$metric),], aes(shape = factor(metric, levels = c("metric_sos", "metric_peak", "metric_eos"), labels = c("Start", "Peak", "End")))) + 
        ggtitle(paste0("Season metrics for ", unique(.$site_id))),
      paste0("plot/elaboration_path/", unique(.$site_id), "/04_phenology_", unique(.$site_id)),
      save_plot = TRUE)
    data.frame(x=NULL)
  })


# Season metrics plots by site for site PIs -------------------------------

# tmp_PIs_plot_template <- list(
#   aes(x = doy, y = value_mean_by_doy),
#   scale_fill_discrete(guide = "none"),
#   scale_shape_discrete(guide = "none"),
#   scale_x_continuous(paste0(get_aggr_str(cfg_l$modis_product), " periods")),
#   scale_y_continuous("Smoothed GPPstMDS value", label = to_vegetation_index_range),
#   geom_line(),
#   facet_wrap(~year),
#   theme_bw())
# 
# 
# series_phenology_df %>%
#   ungroup() %>% 
#   filter(index == "GPPstMDS") %>% 
#   mutate(
#     metric_plot = factor(metric, levels = cfg_l$doys_metrics, labels = c("End", "Start", "Peak", NA_character_)),
#     metric_label = paste(as.character(metric_plot), "d.o.y.: ", doy_relevant, sep = " ")) %>% 
#   group_by(site_id) %>% 
#   do({
#     save_pdf_plot(
#       ggplot(.) %+% 
#         tmp_PIs_plot_template +
#         geom_point(data = .[!is.na(.$metric),], aes(shape = metric_plot)) + 
#         geom_label_repel(
#           data = .[!is.na(.$metric) & .$metric != "metric_los",], 
#           aes(label = metric_label, fill = metric), 
#           fontface = 'bold', color = 'white',
#           box.padding = unit(0.25, "lines"), point.padding = unit(0.20, "lines"), size = 2) +
#         ggtitle(paste0("Season metrics for ", unique(.$site_id))),
#       sprintf("plot/season_metrics_for_PIs/season_metrics_from_GPPstMDS_%s", unique(.$site_id)),
#       save_plot = TRUE)
#     data.frame(x=NULL)
#   })
# 
# raw_phenology_df %>% 
#   filter(index == "GPPstMDS") %>% 
#   select(site_id, year, starts_with("metric_")) %>% 
#   write.csv(., file = "plot/season_metrics_for_PIs/GPP_phenology_for_PIs.csv")

# Average pixels in phenology --------------------------------------------------

tmp_phenology_df <- phenology_df %>%
  group_by(product, site_id, year, index, metric) %>% 
  summarize(
    doy_relevant_mean    = mean(doy_relevant, na.rm = TRUE), 
    doy_relevant_mean_sd = sd(doy_relevant,   na.rm = TRUE)) %>% 
  mutate(
    doy_mean = get_doy_intervals(doy_relevant_mean, cfg_l$modis_product),
    doy_mean_sd = get_doy_intervals(doy_relevant_mean_sd, cfg_l$modis_product))

phenology_by_site_df <- tmp_phenology_df %>%  # Add site metadata
  left_join(site_cfg_l, by = "site_id") 

phenology_gpp_by_site_df <- raw_phenology_df %>% 
  group_by(product, site_id, index, year) %>% 
  summarize(
    gpp_value_mean    = mean(metric_gpp, na.rm = TRUE),
    gpp_value_mean_sd = sd(metric_gpp, na.rm = TRUE)) %>% 
  left_join(site_cfg_l, by = "site_id") 



# Exploratory boxplots ----------------------------------------------------

tmp_bp <- phenology_by_site_df %>% 
    ggplot(aes(x = igbp_id, y = doy_mean)) +
    geom_boxplot(aes(fill = index)) +
    scale_x_discrete("IGBP land-use designation") +
    facet_wrap(~ metric) +
    ggtitle("Season metrics boxplots") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
ggsave("plot/boxplot/SPEOS_boxplot.pdf", tmp_bp, width = 29.7, height = 21, units = "cm")

tmp_bp_gpp <- phenology_gpp_by_site_df %>% 
  ggplot(aes(x = igbp_id, y = gpp_value_mean)) +
  geom_boxplot(aes(fill = index)) +
  scale_x_discrete("IGBP land-use designation") +
  scale_y_log10("GPP proxy (x 1E4)", label = to_vegetation_index_range) +
  ggtitle("Season GPP boxplot") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
ggsave("plot/boxplot/GPP_boxplot.pdf", tmp_bp_gpp, width = 29.7, height = 21, units = "cm")


# Exploratory trend plots -------------------------------------------------

tmp_tp_sos <- phenology_by_site_df %>% 
  filter(metric == "metric_sos") %>% 
  ggplot(aes(x = year, y = doy_mean, ymax = doy_mean + doy_mean_sd, ymin = doy_mean - doy_mean_sd, color = index)) +
    geom_errorbar(width = 0.25) +
    scale_y_continuous("16-days period") +
    geom_line(aes(group = index)) +
    facet_wrap(~site_id) +
    ggtitle("Start of Growing Season +/- 1 standard deviation") +
    theme_bw()
ggsave("plot/trendplot/SOS_trendplot.pdf", tmp_tp_sos, width = 29.7, height = 21, units = "cm")

tmp_tp_eos <- phenology_by_site_df %>% 
  filter(metric == "metric_eos") %>% 
    ggplot(aes(x = year, y = doy_mean, ymax = doy_mean + doy_mean_sd, ymin = doy_mean - doy_mean_sd, color = index)) +
    geom_errorbar(width = 0.25) +
    scale_y_continuous("16-days period") +
    geom_line(aes(group = index)) +
    facet_wrap(~site_id) +
    ggtitle("End of Growing Season +/-1 standard deviation") +
    theme_bw()
ggsave("plot/trendplot/EOS_trendplot.pdf", tmp_tp_eos, width = 29.7, height = 21, units = "cm")

tmp_tp_los <- phenology_by_site_df %>% 
  filter(metric == "metric_los") %>% 
  ggplot(aes(x = year, y = doy_mean, ymax = doy_mean + doy_mean_sd, ymin = doy_mean - doy_mean_sd, color = index)) +
  geom_errorbar(width = 0.25) +
  scale_y_continuous("16-days period") +
  geom_line(aes(group = index)) +
  facet_wrap(~site_id) +
  ggtitle("Length of Growing Season +/-1 standard deviation") +
  theme_bw()
ggsave("plot/trendplot/LOS_trendplot.pdf", tmp_tp_los, width = 29.7, height = 21, units = "cm")

tmp_tp_gpp <- phenology_gpp_by_site_df %>% 
  ggplot(aes(x = year, y = gpp_value_mean, ymax = gpp_value_mean + gpp_value_mean_sd, ymin = gpp_value_mean - gpp_value_mean_sd, color = index)) +
  geom_errorbar(width = 0.25) +
  scale_y_log10("GPP proxy (x 1E4)", label = to_vegetation_index_range) +
  geom_line(aes(group = index)) +
  facet_wrap(~site_id) +
  ggtitle("GPP of Growing Season +/-1 standard deviation") +
  theme_bw()
ggsave("plot/trendplot/GPP_trendplot.pdf", tmp_tp_gpp, width = 29.7, height = 21, units = "cm")




# Tidy df to perform stat analysis ----------------------------------------

tmp_analysis_df <- phenology_by_site_df %>%
  select(site_id, igbp_id, genus_id, kgcc, year, doy_mean, index, metric)

tmp_analysis_l <- split(tmp_analysis_df, tmp_analysis_df$index)

rename_fluxes_index <- function (fluxes_df) {
  stopifnot("data.frame" %in% class(fluxes_df))
  
  if (fluxes_df$product[1] == "FluxData") {
    tmp_value_index <- which(colnames(fluxes_df) == "doy_mean")
    colnames(fluxes_df)[tmp_value_index] <- paste(colnames(fluxes_df)[tmp_value_index], unique(fluxes_df$index), sep = "_")
    fluxes_df$index <- NULL
  }
  
  fluxes_df$product <- NULL
  fluxes_df
}


tmp_analysis_l <- lapply(tmp_analysis_l, rename_fluxes_index)


# Crea questo data frame:
# +-------+-------+-------+
# |  EVI  +  NEE  +  GPP  |
# +-------+-------+-------+
# | NDVI  +  NEE  +  GPP  |
# +-------+-------+-------+
analysis_df <- tmp_analysis_l$EVI %>% 
  bind_rows(tmp_analysis_l$NDVI) %>% 
  rename(doy_mean_MODIS = doy_mean) %>% 
  left_join(tmp_analysis_l$GPPstMDS, by = c("site_id", "year", "metric", "genus_id", "igbp_id", "kgcc")) %>%
  left_join(tmp_analysis_l$NEEstfMDS, by = c("site_id", "year", "metric", "genus_id", "igbp_id", "kgcc"))

#   bind_rows(
#     tmp_analysis_l$prop %>% left_join(tmp_analysis_l$GPPstMDS, by = c("site_id", "year", "metric", "genus_id", "igbp_id", "kgcc")))



# Exploratory index plot --------------------------------------------------

# analysis_df %<>% filter(genus_id != "Poplar")
# 
# tmp_ip_sos <- analysis_df %>% 
#   filter(metric == "metric_sos") %>% 
#     ggplot(aes(x = doy_mean_GPPstMDS, y = doy_mean, color = index)) + 
#   geom_point() + 
#   geom_smooth(method = lm, se = FALSE) +
#   facet_wrap(~igbp_id+site_id) +
#   theme_bw() +
#   ggtitle("SOS")
# ggsave("plot/indexplot/SOS_indexplot.pdf", tmp_ip_sos, width = 29.7, height = 21, units = "cm")
# 
# tmp_ip_sos <- analysis_df %>% 
#   filter(metric == "metric_eos") %>% 
#   ggplot(aes(x = doy_mean_GPPstMDS, y = doy_mean, color = index)) + 
#   geom_point() + 
#   geom_smooth(method = lm, se = FALSE) +
#   facet_wrap(~igbp_id+site_id) +
#   theme_bw() +
#   ggtitle("EOS")
# ggsave("plot/indexplot/EOS_indexplot.pdf", tmp_ip_sos, width = 29.7, height = 21, units = "cm")
# 
# tmp_ip_los <- analysis_df %>% 
#   filter(metric == "metric_los") %>% 
#   ggplot(aes(x = doy_mean_GPPstMDS, y = doy_mean, color = index)) + 
#   geom_point() + 
#   geom_smooth(method = lm, se = FALSE) +
#   facet_wrap(~igbp_id+site_id) +
#   theme_bw() +
#   ggtitle("LOS")
# ggsave("plot/indexplot/LOS_indexplot.pdf", tmp_ip_los, width = 29.7, height = 21, units = "cm")
# 
# 

# Differences boxplot --------------------------------------------------------

differ_df <- analysis_df %>% 
  filter(metric != "metric_los") %>% 
  mutate(diff = doy_mean_GPPstMDS - doy_mean_MODIS)

differ_count_df <- differ_df %>% 
  group_by(index, site_id, metric) %>% 
  summarise(count = n(), diff_quantile = quantile(diff, probs = 0.95, na.rm = TRUE)) %>% 
  right_join(differ_df) %>% 
  filter(abs(diff) > abs(diff_quantile))

tmp_diff_plot <- ggplot(differ_df, aes(x = metric, y = diff)) +
  geom_boxplot(aes(fill = index)) +
#   geom_label_repel(data = differ_count_df, aes(label = year, fill = index), fontface = 'bold', color = 'white',
#                    box.padding = unit(0.05, "lines"),
#                    point.padding = unit(0.01, "lines"), size = 1) +
  facet_wrap(~site_id) +
  scale_y_continuous("Difference in the count of 16-days periods") +
  scale_x_discrete("Metric") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  ggtitle("Difference between metric estimates derived from GPP and optical indexes\nNegative differences: optical indexes metric are later than GPP metric; positive differences: optical indexes are earlier")
ggsave("plot/boxplot/differences.pdf", tmp_diff_plot, width = 29.7, height = 21, units = "cm")






# Model fit ---------------------------------------------------------------

# lmm_df <- analysis_df %>%
#   mutate(kgcc = relevel(kgcc, ref = "Csa")) %>% 
#   filter(index != "1:1", metric != "metric_los") %>% 
#   group_by(metric, index) %>% 
#   do(model = lmer(doy_mean_GPPstMDS ~ doy_mean + (1|kgcc) + (1|igbp_id), REML = FALSE, data = .))
# 
# 
# apply(lmm_df, MARGIN = 1, function(row) print(summary(row$model)))
# 
# 
# 
# # lmm_dbf <- analysis_df %>% 
# #   filter(index != "1:1", igbp_id == "DBF", index == "EVI") %>% 
# #   lmer(doy_mean_GPPstMDS ~ doy_mean + (1|genus_id), data = .)
# # 
# # lmm_enf <- analysis_df %>% 
# #   filter(index != "1:1", igbp_id == "ENF") %>% 
# #   lmer(doy_mean_GPPstMDS ~ doy_mean + (1|genus_id), data = .)
# # 
# # lmm_ebf <- analysis_df %>% 
# #   filter(index != "1:1", igbp_id == "EBF") %>% 
# #   lm(doy_mean_GPPstMDS ~ doy_mean, data = .)
# # 
# 
# # Extraction of fixed effects ---------------------------------------------
# 
# lmm_fixed_vars_df <- lmm_df %>%
#   do(
#     data.frame(
#       index       = .$index,
#     #  igbp_id      = .$igbp_id,
#       metric      = .$metric,
#       coefficient = names(fixef(.$model)), 
#       coef(summary(.$model)))) %>% 
#   arrange(coefficient, Estimate)
# 
# 
# lmm_fixed_vars_df %>% 
#   filter(coefficient != "(Intercept)") %>% 
#   ggplot(., aes(x = coefficient, y = Estimate)) + 
#   geom_pointrange(aes(ymin = Estimate - Std..Error, ymax = Estimate + Std..Error)) + 
#   coord_flip() +
#   geom_hline(yintercept = 0) +
#   ggtitle("Model fixed coefficients") +
#   theme_bw() +
#   facet_grid(metric~index)
# 
# # Extraction of variances of random effects ---------------------------------------------
# 
# lmm_var_cor_df <- lmm_df %>% 
#   do(
#     cbind(
#       index       = .$index,
#       metric      = .$metric,
#       as.data.frame(VarCorr(.$model)))) %>% 
#   mutate(std = vcov^0.5)
# 
# 
# 
# # Extraction of random effects --------------------------------------------
# 
# lmm_random_intercept_l <- list(
#   igbp_df = lmm_df %>% 
#     do(
#       cbind(
#         index       = .$index,
#         metric      = .$metric,
#         igbp_id     = rownames(ranef(.$model)$igbp_id),
#         intercept   = ranef(.$model)$igbp_id[1])),
#   kgcc_df = lmm_df %>% 
#     do(
#       cbind(
#         index       = .$index,
#         metric      = .$metric,
#         kgcc     = rownames(ranef(.$model)$kgcc),
#         intercept   = ranef(.$model)$kgcc[1])))
# 
# 
# # Caterpillar plots -----------------------------------------------
# 
# apply(
#   lmm_df, 
#   MARGIN = 1, 
#   function(m) { 
#     lapply(
#       ggCaterpillar(ranef(m[["model"]], condVar = TRUE), QQ = FALSE),
#       function(p) p + theme_bw() + ggtitle(paste0("Model random coefficients, ", m$index, ", ", m$metric)))
#     }
#   )
# 


# Logistic regression ----------------------------------------------------

allowance_in_doy_mean_difference <- seq(from = -1, to = 1, by = 1)

random_metric_df <- analysis_df %>% 
  group_by(site_id, igbp_id, genus_id, kgcc, index, year) %>% 
  summarise(
    metric            = "metric_random", 
    doy_mean_MODIS    = sample(1:cfg_l$modis_period_in_year_count, 1), 
    doy_mean_GPPstMDS = sample(1:cfg_l$modis_period_in_year_count, 1),
    doy_mean_NEEstfMDS = sample(1:cfg_l$modis_period_in_year_count, 1))

logistic_analysis_df <- analysis_df %>% 
  bind_rows(random_metric_df)

random_index_df <- logistic_analysis_df %>% 
  group_by(site_id, igbp_id, genus_id, kgcc, year, metric) %>% 
  summarise(
    index = "RANDOM"
  )

# Probability that a optically-derived metric doy matches the GPP-derived metric doy by an allowance
# 1:1 index is given a random TRUE/FALSE match and it will be used as reference for testing
# EVI and NDVI goodness
logistic_analysis_df %<>%
  bind_rows(random_index_df) %>% 
  arrange(site_id, year, index, year) %>% 
  mutate(
    random_doy_mean_match = sample(1:100, n(), replace = TRUE) > 50, 
    doy_mean_match_GPP = ifelse(
      index == "RANDOM", 
      random_doy_mean_match, 
      ifelse(
        doy_mean_GPPstMDS, 
        (doy_mean_MODIS - doy_mean_GPPstMDS) %in% allowance_in_doy_mean_difference,
        NA)),
    doy_mean_match_NEE = ifelse(
      index == "RANDOM", 
      random_doy_mean_match, 
      ifelse(
        doy_mean_NEEstfMDS, 
        (doy_mean_MODIS - doy_mean_NEEstfMDS) %in% allowance_in_doy_mean_difference,
        NA))) %>% 
  select(-random_doy_mean_match)


logistic_analysis_df %<>%
  mutate(
    kgcc     = relevel(kgcc, ref = "Csa"),
    genus_id = relevel(genus_id, ref = "Spruce"),
    metric   = relevel(as.factor(metric), ref = "metric_random"),
    index    = relevel(as.factor(index), ref = "RANDOM"))

glm_GPP_formula <- list(
  model = as.formula("doy_mean_match_GPP ~ metric + (1|igbp_id)"),
  model_reduce = as.formula("doy_mean_match_GPP ~ 1 + (1|igbp_id)"))
glm_NEE_formula <- list(
  model = as.formula("doy_mean_match_NEE ~ metric + (1|igbp_id)"),
  model_reduce = as.formula("doy_mean_match_NEE ~ 1 + (1|igbp_id)"))

glm_GPP_df <- logistic_analysis_df %>%
  group_by(index) %>% 
  do(model = glmer(glm_GPP_formula$model, family = binomial, data = .),
     model_reduce = glmer(glm_GPP_formula$model_reduce, family = binomial, data = .)) 
glm_NEE_df <- logistic_analysis_df %>%
  group_by(index) %>% 
  do(model = glmer(glm_NEE_formula$model, family = binomial, data = .),
     model_reduce = glmer(glm_NEE_formula$model_reduce, family = binomial, data = .)) 

# Sink output to file -----------------------------------------------------

sink("log/logistic_models.txt", append = TRUE)

print(sprintf("## Date/time of analysis: %s ##", Sys.time()))

# Features of the logistic models -----------------------------------------

print("Features of the GPP models ---------")
apply(glm_GPP_df, MARGIN = 1, function(row) { print(row$index); print(summary(row$model)); NULL})
print("Features of the NEE models ---------")
apply(glm_NEE_df, MARGIN = 1, function(row) { print(row$index); print(summary(row$model)); NULL})


# Likelihood ratio test on model explanatory variables  -------------------

print("Likelihood ratio test on GPP model explanatory variables ---------")
apply(glm_GPP_df, MARGIN = 1, function(row) { print(row$index); print(anova(row$model_reduce, row$model, test = "Chisq")); NULL})
print("Likelihood ratio test on NEE model explanatory variables ---------")
apply(glm_NEE_df, MARGIN = 1, function(row) { print(row$index); print(anova(row$model_reduce, row$model, test = "Chisq")); NULL})


# Probabilities of doy matches from the model for each explanatory variable -------

print("Probabilities of doy matches from the GPP model for each explanatory variable ---------")
apply(glm_GPP_df, MARGIN = 1, function(row) { odd <- exp(coef(row$model)$igbp_id); prob = odd/(odd+1); print(row$index); print(prob); NULL})
print("Probabilities of doy matches from the NEE model for each explanatory variable ---------")
apply(glm_NEE_df, MARGIN = 1, function(row) { odd <- exp(coef(row$model)$igbp_id); prob = odd/(odd+1); print(row$index); print(prob); NULL})


# End of sink -------------------------------------------------------------

sink()


# Plot templates ----------------------------------------------------------

tmp_fixed_eff_template <- list(
  aes(x = coefficient, y = Estimate),
  geom_pointrange(aes(ymin = Estimate - Std..Error, ymax = Estimate + Std..Error)),
  scale_y_continuous("Fixed effect estimate", limits = c(-3.5, 3.5)),
  scale_x_discrete("Fixed effect"),
  coord_flip(),
  geom_hline(yintercept = 0),
  theme_bw(),
  facet_grid(~index),
  ggtitle(sprintf("Fixed component of logistic model %s ~ %s", as.character(glm_GPP_formula$model)[2], as.character(glm_GPP_formula$model)[3]))
)

tmp_random_eff_template <- list(
  aes(x = ID, y = y),
  geom_point(size = 1.5, colour = "blue"),
  geom_errorbar(aes(ymin = y - ci, ymax = y + ci), width = 0, colour = "black"),
  geom_hline(yintercept = 0, linetype = "dotted"),
  coord_flip(),
  scale_x_discrete("Levels of random component"),
  scale_y_continuous("Random effect estimate", limits = c(-2, 2)),
  facet_grid(ind~index),
  theme_bw(),
  ggtitle(sprintf("Random components of logistic model %s ~ %s", as.character(glm_GPP_formula$model)[2], as.character(glm_GPP_formula$model)[3]))
)

tmp_prediction_template <- list(
  aes(x = metric, y = doy_mean_match),
  geom_point(aes(shape = doy_mean_match > 0.5)),
  scale_shape_discrete(guide = "none"),
  scale_y_continuous("Probability"),
  scale_x_discrete("Metric"),
  geom_hline(yintercept = length(allowance_in_doy_mean_difference)/cfg_l$modis_period_in_year_count, linetype = "dotted"),
  geom_hline(yintercept = 0.5, linetype = "dotted"),
  facet_grid(igbp_id~index),
  theme_bw(),
  theme(axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0.5)),
  ggtitle("Probability of match between vegetation indexes derived by MODIS and GPP of fluxes towers, by IGBP code")
)

# Logistic model: Extraction of fixed effects ---------------------------------------------

# GPP:

glm_fixed_vars_df <- glm_GPP_df %>%
  do(
    data.frame(
      index       = .$index,
      coefficient = names(fixef(.$model)), 
      coef(summary(.$model)))) %>% 
  arrange(coefficient, Estimate)

glm_fixed_vars_df %>% 
  ggplot(.) %+% 
  tmp_fixed_eff_template
ggsave("plot/model/logistic_GPP_fixed_effects.pdf", width = 29.7, height = 21, units = "cm")


# NEE:

rm(glm_fixed_vars_df)

glm_fixed_vars_df <- glm_NEE_df %>%
  do(
    data.frame(
      index       = .$index,
      coefficient = names(fixef(.$model)), 
      coef(summary(.$model)))) %>% 
  arrange(coefficient, Estimate)

glm_fixed_vars_df %>% 
  ggplot(.) %+% 
  tmp_fixed_eff_template +
  ggtitle(sprintf("Fixed component of logistic model %s ~ %s", as.character(glm_NEE_formula$model)[2], as.character(glm_NEE_formula$model)[3]))
ggsave("plot/model/logistic_NEE_fixed_effects.pdf", width = 29.7, height = 21, units = "cm")


# Logistic model: Extraction of random effects --------------------------------------------

# GPP:

glm_random_vars_df <- bind_rows(
  apply(
    glm_GPP_df, 
    MARGIN = 1, 
    function(row) { 
      random_effects <- extract_random_coeff(row$model)
      index <- row$index
      cbind(random_effects, index) 
    }
  )) %>% 
  mutate(index = relevel(as.factor(index), ref = "RANDOM"))


glm_random_vars_df %>% 
  ggplot(.) %+%
  tmp_random_eff_template

ggsave("plot/model/logistic_GPP_random_effects.pdf", width = 29.7, height = 21, units = "cm")


# NEE:

rm(glm_random_vars_df)
glm_random_vars_df <- bind_rows(
  apply(
    glm_NEE_df, 
    MARGIN = 1, 
    function(row) { 
      random_effects <- extract_random_coeff(row$model)
      index <- row$index
      cbind(random_effects, index) 
    }
  )) %>% 
  mutate(index = relevel(as.factor(index), ref = "RANDOM"))


glm_random_vars_df %>% 
  ggplot(.) %+%
  tmp_random_eff_template +
  ggtitle(sprintf("Random components of logistic model %s ~ %s", as.character(glm_NEE_formula$model)[2], as.character(glm_NEE_formula$model)[3]))

ggsave("plot/model/logistic_NEE_random_effects.pdf", width = 29.7, height = 21, units = "cm")



# Logistic model prediction plot ---------------------------------------------------

tmp_predict_df <- expand.grid(
  metric = c(cfg_l$doys_metrics, "metric_random"), 
  igbp_id = unique(logistic_analysis_df$igbp_id))


tmp_predict_df <- within(tmp_predict_df, {
  metric   <- relevel(as.factor(metric), ref = "metric_random")
})


# GPP:

tmp_predict_l <- apply(glm_GPP_df, MARGIN = 1, function(row) {
  within(tmp_predict_df, {
     doy_mean_match <- predict(row$model, tmp_predict_df, type = "response")
     index <- row$index
  })
})

logistic_prediction_df <- bind_rows(tmp_predict_l) %>% 
  mutate(
    index  = relevel(as.factor(index), ref = "RANDOM"),  
    metric = factor(metric, levels = c("metric_random", "metric_los", "metric_eos", "metric_sos", "metric_los", "metric_peak")))

logistic_prediction_df %>% 
  ggplot(.) %+%
  tmp_prediction_template

ggsave("plot/model/logistic_GPP_prediction.pdf", width = 29.7, height = 21, units = "cm")


# NEE:

rm(tmp_predict_l)
tmp_predict_l <- apply(glm_NEE_df, MARGIN = 1, function(row) {
  within(tmp_predict_df, {
    doy_mean_match <- predict(row$model, tmp_predict_df, type = "response")
    index <- row$index
  })
})

logistic_prediction_df <- bind_rows(tmp_predict_l) %>% 
  mutate(
    index  = relevel(as.factor(index), ref = "RANDOM"),  
    metric = factor(metric, levels = c("metric_random", "metric_los", "metric_eos", "metric_sos", "metric_los", "metric_peak")))

logistic_prediction_df %>% 
  ggplot(.) %+%
  tmp_prediction_template +
  ggtitle("Probability of match between vegetation indexes derived by MODIS and NEE of fluxes towers, by IGBP code")


ggsave("plot/model/logistic_NEE_prediction.pdf", width = 29.7, height = 21, units = "cm")

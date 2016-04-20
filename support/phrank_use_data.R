modis_layers <- read.csv("support/modis_layers.csv", stringsAsFactors = FALSE)
source("support/MOD13Q1_quality_l.R")
#devtools::use_data(modis_layers, MOD13Q1_quality_l, internal = TRUE, overwrite = TRUE)

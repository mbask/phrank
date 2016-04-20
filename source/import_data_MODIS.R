
# Download or load MODIS data ---------------------------------------------

tmp_site_modis_l <- apply(
  site_cfg_l, 
  MARGIN  = 1, 
  add_site_record, 
  product = cfg_l$products[1], 
  bands   = cfg_l$modis_bands, 
  data_path = cfg_l$in_path, 
  logger  = logger)




# Bind MODIS data ---------------------------------------------------------

modis_df <- bind_rows(tmp_site_modis_l)

# Save ready-for-phrank modis data -----------------------------------------

saveRDS(modis_df, file = paste0(cfg_l$in_path, "modis_data_to_be_fed_to_phrank.rds"))

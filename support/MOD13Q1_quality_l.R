MOD13Q1_quality_l <- list(
  vi_quality = data.frame(
    value         = c("00", "01", "10", "11")
    , start_bit   = 0
    , vi_quality_description = c("VI produced with good quality", "VI produced, but check other QA", "Pixel produced, but most probably cloudy", "Pixel not produced due to other reasons than clouds")
    , stringsAsFactors = FALSE
  )
  , vi_usefulness = data.frame(
    value = c("0000", "0001", "0010", "0011", "0100", "0101", "0110", "0111", "1000", "1001", "1010", "1011", "1100", "1101", "1110", "1111")
    , start_bit   = 2
    , vi_usefulness_description = c("Highest quality", "Lower quality", "Decreasing quality", "Decreasing quality", "Decreasing quality", "Decreasing quality", "Decreasing quality", "Decreasing quality", "Decreasing quality", "Decreasing quality", "Decreasing quality", "Lowest quality", "Quality so low that it is not useful", "Quality so low that it is not useful", "L1B data faulty", "Not useful for any other reason/not processed")
    , vi_usefulness_level = seq(1, 0, length.out = 16)
    , stringsAsFactors = FALSE
  )
  , aerosol_quantity = data.frame(
    value = c("00", "01", "10", "11")
    , start_bit   = 6
    , aerosol_quantity = c("Climatology", "Low", "Intermediate", "High")
    , stringsAsFactors = FALSE
  )
  , adjacent_cloud_detected = data.frame(
    value = c("0", "1")
    , start_bit   = 8
    , adjacent_cloud_detected = c("No", "Yes")
    , stringsAsFactors = FALSE
  )
  , atmospheric_BRDF_correction = data.frame(
    value = c("0", "1")
    , start_bit   = 9
    , atmospheric_BRDF = c("No", "Yes")
    , stringsAsFactors = FALSE
  )
  , mixed_clouds = data.frame(
    value = c("0", "1")
    , start_bit   = 10
    , mixed_clouds = c("No", "Yes")
    , stringsAsFactors = FALSE
  )
  , land_water_mask = data.frame(
    value = c("000", "001", "010", "011", "100", "101", "110", "111")
    , start_bit   = 11
    , land_water = c("Shallow ocean", "Land (Nothing else but land)", "Ocean coastlines and lake shorelines", "Shallow inland water", "Ephemeral water", "Deep inland water", "Moderate or continental ocean", "Deep ocean")
    , stringsAsFactors = FALSE
  )
  , possible_snow_ice = data.frame(
    value = c("0", "1")
    , start_bit   = 14
    , possible_snow_ice = c("No", "Yes")
    , stringsAsFactors = FALSE
  )
  , possible_shadow = data.frame(
    value = c("0", "1")
    , start_bit   = 15
    , possible_shadow = c("No", "Yes")
    , stringsAsFactors = FALSE
  )
)

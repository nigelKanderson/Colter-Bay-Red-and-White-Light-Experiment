library(sf)
library(terra)
library(exactextractr)
library(tidyverse)

nlcd_forest <- terra::rast('Annual_NLCD_LndCov_2021_CU_C1V1.tif')



add_habitat <- function(data, forest_raster, buffer = 500) {
  
  
  pts <- st_as_sf(
    data,
    coords = c("lon", "lat"),
    crs = 4326
  )
  
  pts <- st_transform(pts, crs(forest_raster))
  
  pts$geometry <- st_buffer(pts$geometry, dist = 500)
  
  openness <- exact_extract(forest_raster, pts, function(values, coverage_fraction) {
    
    tibble(
      forest = sum(values == 42, na.rm = TRUE),
      open = sum(values %in% c(81,82), na.rm = TRUE),
      developed = sum(values %in% c(21:24), na.rm = TRUE),
      total = length(values)
    ) %>%
      mutate(
        pct_forest = forest / total,
        pct_open = open /total,
        pct_developed = developed / total
      )
    
  })
  
  #forest_vals <- exact_extract(
    #forest_raster,
    #pts_buf,
    #"mean",
  #)
  
  sites_env <- bind_cols(sites, openness)
  
  data_out <- data %>%
    left_join(sites_env, by = "site")
  
  return(data_out)
}

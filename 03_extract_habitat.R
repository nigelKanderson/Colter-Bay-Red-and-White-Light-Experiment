library(sf)
library(terra)
library(exactextractr)
library(tidyverse)

nlcd_forest <- terra::rast('Annual_NLCD_LndCov_2021_CU_C1V1.tif')



add_habitat <- function(data, forest_raster, buffer = 500) {
  
  sites <- data %>%
    dplyr::group_by(site) %>%
    dplyr::summarise(
      lon = first(lon),
      lat = first(lat),
      .groups = "drop"
    )
  
  
  pts <- st_as_sf(
    sites,
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
  
  sites_env <- bind_cols(pts, openness)
  
  sites_env <-  pts %>%
    sf::st_drop_geometry() %>%
    distinct(site, .keep_all = TRUE) %>%
    mutate(openness = openness) %>%
    select(site, openness)
  
  data_out <- data %>%
    left_join(sites_env, by = "site")
  
  saveRDS(data_out, "data_out.rds")
  
  return(data_out)
}

#readr::write_csv(data_out, "data_out.csv")

#saveRDS(data_out, "data_out.rds")

library(sf)
library(terra)
library(exactextractr)
library(tidyverse)

sites <- tibble(
  site = c("CORA1", "DALO1", "CRNR1", "NORO1", "AMLA1","SHLA1", "GSLA1"),
  lon = c(-110.6354, -110.64717, -110.63882, -110.63902, -110.64412, -110.64197, -110.64065),
  lat = c(43.90172, 43.90791, 43.90151, 43.90642, 43.90422, 43.90194, 43.90496)
  
)

sites_sf <-st_as_sf(sites, coords = c('lon', 'lat'), crs=4326)


lc <- terra::rast('Annual_NLCD_LndCov_2021_CU_C1V1.tif')

sites_sf <- st_transform(sites_sf, crs(lc))

sites_sf$geometry <- st_buffer(sites_sf$geometry, dist = 500)

openness <- exact_extract(lc, sites_sf, function(values, coverage_fraction) {
  
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

sites_env <- bind_cols(sites, openness)

data %>%
  left_join(sites_env, by = "site")
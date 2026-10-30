library(tidyverse)
library(glmmTMB)
library(DHARMa)
library(performance)

#data <- readRDS("data/data_out.rds")

#glimpse(data)

run_models <- function(data) {
  library(tidyverse)
  library(glmmTMB)
  library(DHARMa)
  library(performance)

  # Canonical overall model = total bat activity per SITE-NIGHT (not per species,
  # to avoid pseudoreplication). Covariates are constant within a site-night, so
  # take the first value. m0/m1/m2 share this response so AIC is comparable.
  model_data <- data %>%
    filter(
      !is.na(jd), !is.na(pct_forest), !is.na(color),
      !is.na(intensity), !is.na(mean_brightness_site)
    ) %>%
    group_by(site, year, date) %>%
    summarise(
      total_det            = sum(detections),
      color                = first(color),
      intensity            = first(intensity),
      jd                   = first(jd),
      mean_phase           = first(mean_phase),
      pct_nonforest        = first(pct_nonforest),
      mean_brightness_site = first(mean_brightness_site),
      .groups = "drop"
    )

  m0 <- glmmTMB(
    total_det ~ 1 + (1 | site) + (1 | year),
    data = model_data, family = nbinom2()
  )

  m1 <- glmmTMB(
    total_det ~ jd + I(jd^2) + mean_phase + (1 | site) + (1 | year),
    data = model_data, family = nbinom2()
  )

  # Canonical overall model: colour x intensity + covariates, with a year random
  # effect. The switching-interval design is a function of year, so (1|year)
  # subsumes it (and fits far better than a separate switching term).
  m2 <- glmmTMB(
    total_det ~
      intensity * color +
      jd + I(jd^2) + mean_phase + pct_nonforest + mean_brightness_site +
      (1 | year) + (1 | site),
    data = model_data, family = nbinom2()
  )

  list(m0 = m0, m1 = m1, m2 = m2)
}





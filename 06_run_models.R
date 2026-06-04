library(tidyverse)
library(glmmTMB)
library(DHARMa)
library(performance)

#data <- readRDS("data_out.rds")

#glimpse(data)

run_models <- function(data) {
  library(tidyverse)
  library(glmmTMB)
  library(DHARMa)
  library(performance)
  
  model_data <- data %>%
    filter(
      !is.na(weighted_detections),
      !is.na(jd),
      !is.na(pct_forest),
      !is.na(color),
      !is.na(intensity),
      !is.na(mean_brightness_site)
    )
  
  m0 <- glmmTMB(
    weighted_detections ~ 1 + 
      (1|site) +
      (1|year),
    data = model_data,
    family = nbinom2()
  )
  
  m1 <- glmmTMB(
    weighted_detections ~
      jd +
      jd^2 +
      mean_phase +
      (1|site) +
      (1|year),
    data = model_data,
    family = nbinom2()
  )
  
  m2 <- glmmTMB(
    weighted_detections ~
      #jd * color+
      jd +
      jd^2 +
      mean_phase +
      pct_nonforest +
      color * intensity +
      mean_brightness_site +
      #block +
      (1|site),
    data = model_data,
    family = nbinom2()
  )
  
  list(
    m0 = m0,
    m1 = m1,
    m2 = m2
  )
  
}





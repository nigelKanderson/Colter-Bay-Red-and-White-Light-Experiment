library(readr)
library(readxl)
library(dplyr)
library(lubridate)

lighting <- read_excel("/Users/nanderson/Library/CloudStorage/GoogleDrive-nigel_anderson@brown.edu/.shortcut-targets-by-id/1sSdpOAdUOgAVbJGpTKgB3-CJAfnjsvKJ/grandteton_distanceproject/grte_colterbay_lightingschedule_all.xlsx")

names(lighting)

lighting <- lighting %>%
  mutate(date = as.Date(date))

saveRDS(lighting, "data_full.rds")
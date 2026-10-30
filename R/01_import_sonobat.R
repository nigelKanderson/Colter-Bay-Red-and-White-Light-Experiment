#| warning: false
#| message: false

library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(purrr)
library(stringr)

files_all <- list.files(
  path = "/Users/nanderson/Desktop/grte_colterbay_sonobatch_v30.2",
  pattern = "\\.xlsx$",
  full.names = TRUE,
  recursive = TRUE
)

excluded <- files_all[grepl('v430', basename(files_all), ignore.case = TRUE)]

files <- files_all[!grepl('v430', basename(files_all), ignore.case = TRUE)]

cat(
  "Found ", length(files_all), " Excel files.\n",
  "Excluded ", length(excluded), " v430 files. \n",
  "Importing ", length(files), " files. \n"
)
  


process_file <- function(file) {
  df <- tryCatch(
    readxl::read_excel(file),
    error = function(e) return(NULL)
  )
  
  if (is.null(df)) return(NULL)
  
  required_cols <- c("Prob","SppAccp","Filename")
  
  if(!all(required_cols %in% names(df))) return(NULL)
  
  # v30.2 re-exports report no per-call discriminant probability (Prob = "-"),
  # so a detection = any SonoBat-accepted classification (non-NA SppAccp).
  df <- df %>%
    filter(!is.na(SppAccp), !SppAccp %in% c("", "NoID", "noID"))

  df <- df %>%
    mutate(

      Prob = suppressWarnings(as.numeric(Prob)),
      
      file_name_clean = 
        tools::file_path_sans_ext(Filename),
      
      site = 
        stringr::word(file_name_clean, 1, sep = "_"),
      
      date_string = 
        stringr::str_extract(file_name_clean, "\\d{8}"),
      
      time_string = 
        stringr::str_extract(file_name_clean, "\\d{6}$"),
      
      time = lubridate::hms(
        paste0(
          substr(time_string, 1, 2), ":",
          substr(time_string, 3, 4), ":",
          substr(time_string, 5, 6)
        )
      ),
      
      time_clean = str_replace_all(time, "H|M|S", " "),
      time_clean = str_squish(time_clean),
      hours = as.numeric(str_extract(time, "\\d+(?=H)")),
      minutes = as.numeric(str_extract(time, "\\d+(?=M)")),
      seconds = as.numeric(str_extract(time, "\\d+(?=S)")),
      
      hours = ifelse(is.na(hours), 0, hours),
      minutes = ifelse(is.na(minutes), 0, minutes),
      seconds = ifelse(is.na(seconds), 0, seconds),
      
      time_sec = hours * 3600 + minutes * 60 + seconds,
      
      date = 
        lubridate::ymd(date_string),
      
      datetime =
        as.POSIXct(
          paste(date, time),
          tz = "UTC"
        ),
      
      year = 
        lubridate::year(date),
      
      month = 
        lubridate::month(date),
      
      day = 
        lubridate::day(date),
      
      jd = lubridate::yday(date),
      jd2 = jd^2
      
    ) %>%
    
    group_by(site, year, date, jd, jd2, SppAccp, time_clean, datetime, time_sec) %>%
    
    summarise(
      
      detections = n(),

      # no per-call probability in v30.2 exports -> weight = count
      weighted_detections = n(),

      mean_confidence = NA_real_,
      
      .groups = 'drop'
      
    ) %>%
    
    rename(species = SppAccp) %>%
    
    mutate(
      source_file = basename(file)
    )
  
  
}


data_raw <- map_dfr(files, process_file)

# Colter Bay sites were recorded under GRTE distance-project codes in 2022 (same
# physical site, different naming). The internal recording code is the true
# location, so remap it to the Colter Bay site, then keep the 7 Colter Bay sites:
#   GRTE01 = SHLA1, GRTE05 = NORO1, GRTE06 = CORA1, GRTE16 = GSLA1, GRTE17 = AMLA1
colterbay_sites <- c("AMLA1", "CORA1", "CRNR1", "DALO1", "GSLA1", "NORO1", "SHLA1")
data_raw <- data_raw %>%
  mutate(site = toupper(site),
         site = dplyr::recode(site,
                              "GRTE01" = "SHLA1",
                              "GRTE05" = "NORO1",
                              "GRTE06" = "CORA1",
                              "GRTE16" = "GSLA1",
                              "GRTE17" = "AMLA1")) %>%
  filter(site %in% colterbay_sites)

cat("After site filter:", nrow(data_raw), "rows across",
    dplyr::n_distinct(data_raw$site), "sites;",
    dplyr::n_distinct(paste(data_raw$site, data_raw$date)), "site-nights.\n")

write_csv(data_raw, "glmm_ready_data.csv")


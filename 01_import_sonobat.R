#| warning: false
#| message: false

library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(purrr)

files_all <- list.files(
  path = "/Users/nanderson/Library/CloudStorage/GoogleDrive-nigel_anderson@brown.edu/.shortcut-targets-by-id/1RxI5D6hPL6E8DyVwvePE0g8a1CvV1L9D/grandteton_colterbay",
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
  
  df <- df %>%
    filter(Prob >= 0.9)
  
  df <- df %>%
    mutate(
      
      Prob = as.numeric(Prob),
      
      file_name_clean = 
        tools::file_path_sans_ext(Filename),
      
      site = 
        stringr::word(file_name_clean, 1, sep = "_"),
      
      date_string = 
        stringr::str_extract(file_name_clean, "\\d{8}"),
      
      time_string = 
        stringr::str_extract(file_name_clean, "\\d{6}$"),
      
      date = 
        lubridate::ymd(date_string),
      
      year = 
        lubridate::year(date),
      
      month = 
        lubridate::month(date),
      
      day = 
        lubridate::day(date),
      
      jd = lubridate::yday(date),
      jd2 = jd^2
      
    ) %>%
    
    group_by(site, year, date, jd, jd2, SppAccp) %>%
    
    summarise(
      
      detections = n(),
      
      weighted_detections = 
        sum(Prob, na.rm = TRUE),
      
      mean_confidence = 
        mean(Prob, na.rm = TRUE),
      
      .groups = 'drop'
      
    ) %>%
    
    rename(species = SppAccp) %>%
    
    mutate(
      source_file = basename(file)
    )
  
  
}


data_raw <- map_dfr(files, process_file)

write_csv(
  all_counts,
  "glmm_ready_data.csv"
)


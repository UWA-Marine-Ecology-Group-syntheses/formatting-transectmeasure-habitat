library(dplyr)
library(tidyr)
library(readr)
library(CheckEM)
library(googlesheets4)
library(stringr)

# HABITAT -----
metadata <- read_metadata(here::here("data/2021-08_PtCloates_stereo-BRUVs/")) %>%
  dplyr::select(campaignid, sample, longitude_dd, latitude_dd, date_time, location, site, depth_m, #observer_count, observer_length,
                successful_count, successful_length) %>%
  glimpse()

# read in forwards annotations
forwards <- read.delim("data/2021-08_PtCloates_stereo-BRUVs/2021-08_PtCloates_stereo-BRUVs_Forwards_Dot Point Measurements.txt", 
                       header = T, skip = 4, stringsAsFactors = FALSE, 
                       colClasses = "character", na.strings = "") %>%
  clean_names()

# read in forwards annotations
backwards <- read.delim("data/2021-08_PtCloates_stereo-BRUVs/2021-08_PtCloates_stereo-BRUVs_Backwards_Dot Point Measurements.txt", 
                       header = T, skip = 4, stringsAsFactors = FALSE, 
                       colClasses = "character", na.strings = "") %>%
  clean_names() %>%
  dplyr::mutate(filename = str_replace_all(filename, c("PCB18.JPG" = "PCBP18")))

habitat_with_schema <- bind_rows(forwards, backwards) %>%
  dplyr::select(filename, starts_with("x")) %>%
  dplyr::rename(caab_code = x_6) %>%
  dplyr::mutate(caab_code = as.numeric(caab_code)) %>%
  dplyr::left_join(CheckEM::catami) %>%
  # dplyr::select(-c(starts_with("x"))) %>%
  separate(filename, into = c("sample", "extra"), sep = "_") %>%
  dplyr::mutate(sample = str_replace_all(sample, c(".JPG"= "", ".jpg" = "")))

distinct_hab_types <- habitat_with_schema %>%
  select(starts_with("x"), starts_with("level"), family, genus, species, caab_code) %>%
  distinct()

missing_caab_code <- habitat_with_schema %>%
  dplyr::filter(is.na(caab_code)) # good

unique(habitat_with_schema$sample)

num.points <- 20

wrong_points_habitat <- habitat_with_schema %>%
  group_by(sample) %>%
  summarise(points.annotated = n()) %>%
  left_join(metadata) %>%
  # dplyr::mutate(expected = case_when(
  #   successful_habitat_forward %in% "Yes" & successful_habitat_backward %in% "Yes" ~ num.points * 2, 
  #   successful_habitat_forward %in% "Yes" & successful_habitat_backward %in% "No" ~ num.points * 1, 
  #   successful_habitat_forward %in% "No" & successful_habitat_backward %in% "Yes" ~ num.points * 1, 
  #   successful_habitat_forward %in% "No" & successful_habitat_backward %in% "No" ~ num.points * 0)) %>%
  # dplyr::filter(!points.annotated == expected) %>%
  glimpse()

habitat.missing.metadata <- anti_join(habitat_with_schema, metadata, by = c("sample")) %>%
  glimpse()

metadata.missing.habitat <- anti_join(metadata, habitat_with_schema, by = c("sample")) %>%
  glimpse()

tidy_habitat <- habitat_with_schema %>%
  dplyr::mutate(number = 1) %>%                                     
  dplyr::mutate(campaignid = "2021-08_PtCloates_stereo-BRUVs") %>%
  dplyr::select(campaignid, sample, number, starts_with("level"), family, genus, species) %>%
  dplyr::filter(!level_2 %in% c("","Unscorable", NA)) %>%  
  group_by(campaignid, sample, across(starts_with("level")), family, genus, species) %>%
  dplyr::tally(number, name = "count") %>%
  ungroup() %>%                                                     
  dplyr::select(campaignid, sample, level_1, everything()) %>%
  glimpse()

write_csv(tidy_habitat, "data/to upload/2021-08_PtCloates_stereo-BRUVs_benthos.csv")


# RELIEF ----
# read in forwards annotations
forwards_relief <- read.delim("data/2021-08_PtCloates_stereo-BRUVs/2021-08_PtCloates_stereo-BRUVS_Forwards_Relief_Dot Point Measurements.txt", 
                       header = T, skip = 4, stringsAsFactors = FALSE, 
                       colClasses = "character", na.strings = "") %>%
  clean_names()

# read in forwards annotations
backwards_relief <- read.delim("data/2021-08_PtCloates_stereo-BRUVs/2021-08_PtCloates_stereo-BRUVS_Backwards_Relief_Dot Point Measurements.txt", 
                        header = T, skip = 4, stringsAsFactors = FALSE, 
                        colClasses = "character", na.strings = "") %>%
  clean_names() %>%
  dplyr::mutate(filename = str_replace_all(filename, c("PCB18.JPG" = "PCBP18")))

relief_with_schema <- bind_rows(forwards_relief, backwards_relief) %>%
  dplyr::select(filename, starts_with("x")) %>%
  dplyr::rename(caab_code = x_6) %>%
  dplyr::mutate(caab_code = as.numeric(caab_code)) %>%
  dplyr::left_join(CheckEM::catami) %>%
  dplyr::select(-c(starts_with("x"))) %>%
  separate(filename, into = c("sample", "extra"), sep = "_") %>%
  dplyr::mutate(sample = str_replace_all(sample, c(".JPG"= "", ".jpg" = "")))

relief.missing.metadata <- anti_join(relief_with_schema, metadata, by = c("sample")) %>%
  glimpse()

metadata.missing.relief <- anti_join(metadata, relief_with_schema, by = c("sample")) %>%
  glimpse()

tidy_relief <- relief_with_schema %>%
  dplyr::mutate(number = 1) %>%                                     
  dplyr::mutate(campaignid = "2021-08_PtCloates_stereo-BRUVs") %>%
  dplyr::select(campaignid, sample, number, starts_with("level"), family, genus, species) %>%
  dplyr::filter(!level_2 %in% c("","Unscorable", NA)) %>%  
  group_by(campaignid, sample, across(starts_with("level")), family, genus, species) %>%
  dplyr::tally(number, name = "count") %>%
  ungroup() %>%                                                     
  dplyr::select(campaignid, sample, level_1, everything()) %>%
  glimpse()

write_csv(tidy_relief, "data/to upload/2021-08_PtCloates_stereo-BRUVs_relief.csv")

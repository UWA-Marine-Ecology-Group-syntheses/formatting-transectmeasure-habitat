library(dplyr)
library(tidyr)
library(readr)
library(CheckEM)
library(googlesheets4)
library(stringr)

# HABITAT -----
metadata <- read_metadata(here::here("data/2022-05_PtCloates_stereo-BRUVS/")) %>%
  dplyr::select(campaignid, sample, longitude_dd, latitude_dd, date_time, location, site, depth_m, #observer_count, observer_length,
                successful_count, successful_length) %>%
  glimpse()

# read in forwards annotations
forwards <- read.delim("data/2022-05_PtCloates_stereo-BRUVS/2022-05_PtCloates_stereo-BRUVS_Forwards_Dot Point Measurements.txt", 
                       header = T, skip = 4, stringsAsFactors = FALSE, 
                       colClasses = "character", na.strings = "") %>%
  clean_names()

# read in forwards annotations
backwards <- read.delim("data/2022-05_PtCloates_stereo-BRUVS/2022-05_PtCloates_stereo-BRUVS_Backwards_Dot Point Measurements.txt", 
                       header = T, skip = 4, stringsAsFactors = FALSE, 
                       colClasses = "character", na.strings = "") %>%
  clean_names() %>%
  separate(filename, into = c("opcode","extra"), sep = ".JPG")

unique(backwards$opcode)

habitat_with_schema <- bind_rows(forwards, backwards) %>%
  dplyr::select(opcode, period, code, broad, morphology, type) %>%
  dplyr::rename(caab_code = code) %>%
  dplyr::mutate(caab_code = as.numeric(caab_code)) %>%
  
  dplyr::mutate(caab_code = case_when(
    broad %in% c("Unknown", "Open Water") ~ 1,
    broad %in% "Invertebrate Complex" ~ 2,
    
    morphology %in% "Bryozoa / Cnidaria Matrix" ~ 2,
    
    type %in% "Thalassodendrum sp." ~ 63618905, # fix incorrect caab code
    type %in% "Thalassodendrum sp. with epiphytes algae" ~ 63618905, # fix incorrect caab code
    type %in% "Ecklonia radiata" ~ 54079009, # fix incorrect caab code
    
    caab_code %in% 90300910 ~ 80300910, # fix incorrect caab code
    
    .default = caab_code
  )) %>%
  
  dplyr::left_join(CheckEM::catami) %>%
  dplyr::rename(sample = opcode) 

distinct_hab_types <- habitat_with_schema %>%
  select(broad, morphology, type, starts_with("level"), family, genus, species, caab_code) %>%
  distinct()

missing_caab_code <- habitat_with_schema %>%
  dplyr::filter(is.na(caab_code)) # good

unique(habitat_with_schema$opcode)

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
  dplyr::mutate(campaignid = "2022-05_PtCloates_stereo-BRUVS") %>%
  dplyr::select(campaignid, sample, number, starts_with("level"), family, genus, species) %>%
  dplyr::filter(!level_2 %in% c("","Unscorable", NA)) %>%  
  group_by(campaignid, sample, across(starts_with("level")), family, genus, species) %>%
  dplyr::tally(number, name = "count") %>%
  ungroup() %>%                                                     
  dplyr::select(campaignid, sample, level_1, everything()) %>%
  glimpse()

write_csv(tidy_habitat, "data/to upload/2022-05_PtCloates_stereo-BRUVS_benthos.csv")


# RELIEF ----
# read in forwards annotations
forwards_relief <- read.delim("data/2022-05_PtCloates_stereo-BRUVS/2022-05_PtCloates_stereo-BRUVS_Forwards_Relief_Dot Point Measurements.txt", 
                       header = T, skip = 4, stringsAsFactors = FALSE, 
                       colClasses = "character", na.strings = "") %>%
  clean_names()

# read in forwards annotations
backwards_relief <- read.delim("data/2022-05_PtCloates_stereo-BRUVS/2022-05_PtCloates_stereo-BRUVS_Backwards_Relief_Dot Point Measurements.txt", 
                        header = T, skip = 4, stringsAsFactors = FALSE, 
                        colClasses = "character", na.strings = "") %>%
  clean_names() %>%
  separate(filename, into = c("opcode","extra"), sep = ".JPG")

relief_with_schema <- bind_rows(forwards_relief, backwards_relief) %>%
  dplyr::filter(!is.na(relief)) %>%
  dplyr::mutate(level_5 = str_sub(relief, 2, 2)) %>%
  dplyr::filter(!level_5 %in% "n") %>%
  dplyr::left_join(catami) %>%
  dplyr::rename(sample = opcode) 

relief.missing.metadata <- anti_join(relief_with_schema, metadata, by = c("sample")) %>%
  glimpse()

metadata.missing.relief <- anti_join(metadata, relief_with_schema, by = c("sample")) %>%
  glimpse()

tidy_relief <- relief_with_schema %>%
  dplyr::mutate(number = 1) %>%                                     
  dplyr::mutate(campaignid = "2022-05_PtCloates_stereo-BRUVS") %>%
  dplyr::select(campaignid, sample, number, starts_with("level"), family, genus, species) %>%
  dplyr::filter(!level_2 %in% c("","Unscorable", NA)) %>%  
  group_by(campaignid, sample, across(starts_with("level")), family, genus, species) %>%
  dplyr::tally(number, name = "count") %>%
  ungroup() %>%                                                     
  dplyr::select(campaignid, sample, level_1, everything()) %>%
  glimpse()

write_csv(tidy_relief, "data/to upload/2022-05_PtCloates_stereo-BRUVS_relief.csv")

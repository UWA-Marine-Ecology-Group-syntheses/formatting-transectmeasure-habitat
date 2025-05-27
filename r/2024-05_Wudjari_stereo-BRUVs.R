library(dplyr)
library(tidyr)
library(readr)
library(CheckEM)
library(googlesheets4)

metadata <- read_metadata(here::here("data/2024-05_Wudjari_stereo-BRUVs/")) %>%
  dplyr::select(campaignid, sample, longitude_dd, latitude_dd, date_time, location, site, depth_m, successful_count, successful_length, successful_habitat_forwards, successful_habitat_backwards) %>%
  glimpse()

# read in forwards annotations
forwards <- read.delim("data/2024-05_Wudjari_stereo-BRUVs/2024-05_Wudjari_stereo-BRUVs_forwards_Dot Point Measurements.txt", 
                       header = T, skip = 4, stringsAsFactors = FALSE, 
                       colClasses = "character", na.strings = "") %>%
  clean_names()

# read in forwards annotations
backwards <- read.delim("data/2024-05_Wudjari_stereo-BRUVs/2024-05_Wudjari_stereo-BRUVs_backwards_Dot Point Measurements.txt", 
                       header = T, skip = 4, stringsAsFactors = FALSE, 
                       colClasses = "character", na.strings = "") %>%
  clean_names()

names(forwards)

combined <- bind_rows(forwards, backwards) %>%
  dplyr::select(campaignid, opcode, level_2, level_3, level_4, level_5, scientific) %>%
  dplyr::rename(sample = opcode)

num.points <- 20

wrong_points_habitat <- combined %>%
  group_by(sample) %>%
  summarise(points.annotated = n()) %>%
  left_join(metadata) %>%
  dplyr::mutate(expected = case_when(
    successful_habitat_forwards %in% "Yes" & successful_habitat_backwards %in% "Yes" ~ num.points * 2, 
    successful_habitat_forwards %in% "Yes" & successful_habitat_backwards %in% "No" ~ num.points * 1, 
    successful_habitat_forwards %in% "No" & successful_habitat_backwards %in% "Yes" ~ num.points * 1, 
    successful_habitat_forwards %in% "No" & successful_habitat_backwards %in% "No" ~ num.points * 0)) %>%
  dplyr::filter(!points.annotated == expected) %>%
  glimpse()

habitat.missing.metadata <- anti_join(combined, metadata, by = c("sample")) %>%
  glimpse()

metadata.missing.habitat <- anti_join(metadata, combined, by = c("sample")) %>%
  glimpse()

tidy_habitat <- combined %>%
  separate(scientific, into = c("genus", "species")) %>%
  dplyr::mutate(number = 1) %>%                                     
  left_join(catami) %>%
  dplyr::mutate(campaignid = "2024-05_Wudjari_stereo-BRUVs") %>%
  dplyr::select(campaignid, sample, number, starts_with("level"), family, genus, species) %>%
  dplyr::filter(!level_2 %in% c("","Unscorable", NA)) %>%  
  group_by(campaignid, sample, across(starts_with("level")), family, genus, species) %>%
  dplyr::tally(number, name = "count") %>%
  ungroup() %>%                                                     
  dplyr::select(campaignid, sample, level_1, everything()) %>%
  glimpse()

write_csv(tidy_habitat, "data/to upload/2024-05_Wudjari_stereo-BRUVs_benthos.csv")

# RELIEF ----
# read in forwards annotations
forwards_relief <- read.delim("data/2024-05_Wudjari_stereo-BRUVs/2024-05_Wudjari_stereo-BRUVs_forwards_relief_Dot Point Measurements.txt", 
                              header = T, skip = 4, stringsAsFactors = FALSE, 
                              colClasses = "character", na.strings = "") %>%
  clean_names()

# read in forwards annotations
backwards_relief <- read.delim("data/2024-05_Wudjari_stereo-BRUVs/2024-05_Wudjari_stereo-BRUVs_backwards_relief_Dot Point Measurements.txt", 
                               header = T, skip = 4, stringsAsFactors = FALSE, 
                               colClasses = "character", na.strings = "") %>%
  clean_names() #%>%
#separate(filename, into = c("opcode","extra"), sep = ".JPG")

relief_with_schema <- bind_rows(forwards_relief, backwards_relief) %>%
  dplyr::left_join(catami) %>%
  dplyr::rename(sample = opcode) 

relief.missing.metadata <- anti_join(relief_with_schema, metadata, by = c("sample")) %>%
  glimpse()

unique(relief.missing.metadata$filename)

metadata.missing.relief <- anti_join(metadata, relief_with_schema, by = c("sample")) %>%
  glimpse()

tidy_relief <- relief_with_schema %>%
  dplyr::mutate(number = 1) %>%                                     
  dplyr::mutate(campaignid = "2024-05_Wudjari_stereo-BRUVs") %>%
  dplyr::select(campaignid, sample, number, starts_with("level"), family, genus, species) %>%
  dplyr::filter(!level_2 %in% c("","Unscorable", NA)) %>%  
  group_by(campaignid, sample, across(starts_with("level")), family, genus, species) %>%
  dplyr::tally(number, name = "count") %>%
  ungroup() %>%                                                     
  dplyr::select(campaignid, sample, level_1, everything()) %>%
  glimpse()

write_csv(tidy_relief, "data/to upload/2024-05_Wudjari_stereo-BRUVs_relief.csv")

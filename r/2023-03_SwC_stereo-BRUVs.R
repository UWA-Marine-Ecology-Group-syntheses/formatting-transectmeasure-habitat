library(dplyr)
library(tidyr)
library(readr)
library(CheckEM)
library(googlesheets4)
library(stringr)

schema <- CheckEM::catami%>%
  dplyr::mutate(caab_code = as.numeric(caab_code))%>%
  select(-qualifiers)

# HABITAT -----
metadata <- read_metadata(here::here("data/2023-03_SwC_stereo-BRUVs/")) %>%
  dplyr::select(campaignid, sample, longitude_dd, latitude_dd, date_time, location, site, depth_m, #observer_count, observer_length,
                successful_count, successful_length) %>%
  glimpse()

# read in forwards annotations
forwards <- read.delim("data/2023-03_SwC_stereo-BRUVs/2023-03_SwC_stereo-BRUVs_Forwards_Dot Point Measurements.txt", 
                       header = T, skip = 4, stringsAsFactors = FALSE, 
                       colClasses = "character", na.strings = "") %>%
  clean_names() %>%
  dplyr::filter(!filename %in% "IO333.jpg") %>%
  dplyr::mutate(filename = str_replace_all(filename, "take 2", ""))

# read in forwards annotations
backwards <- read.delim("data/2023-03_SwC_stereo-BRUVs/2023-03_SwC_stereo-BRUVs_Backwards_Dot Point Measurements.txt", 
                        header = T, skip = 4, stringsAsFactors = FALSE, 
                        colClasses = "character", na.strings = "") %>%
  clean_names()

names(forwards)
names(backwards)

habitat_with_schema <- bind_rows(forwards, backwards) %>%
  dplyr::mutate(caab_code = as.numeric(caab_code)) %>%
  dplyr::mutate(caab_code = dplyr::if_else(caab_code == 2, 99900044, caab_code)) %>%
  dplyr::select(-level_2, -level_3, -level_4, -level_5, -scientific, -qualifiers) %>%
  dplyr::left_join(schema) %>%
  dplyr::rename(sample = opcode)

# check immediately, before any further mutates
habitat_with_schema %>% dplyr::filter(is.na(level_2)) %>% distinct(caab_code)

missing_caab_code <- habitat_with_schema %>%
  dplyr::filter(is.na(level_1)) %>%
  distinct(caab_code)

missing_caab_code

names(habitat_with_schema)

distinct_hab_types <- habitat_with_schema %>%
  select(starts_with("level"), family, genus, species, caab_code) %>%
  distinct()

missing_caab_code <- habitat_with_schema %>%
  dplyr::filter(is.na(level_1)) %>%
  distinct(caab_code, level_2, level_3)

missing_caab_code_raw <- habitat_with_schema %>%
  dplyr::filter(is.na(caab_code)) %>%
  distinct(sample, filename)

unique(habitat_with_schema$sample) %>% sort()

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

tidy_habitat <- habitat_with_schema %>%
  dplyr::mutate(sample = str_trim(sample))%>%
  dplyr::mutate(number = 1) %>%                                     
  dplyr::mutate(campaignid = "2023-03_SwC_stereo-BRUVs") %>%
  dplyr::select(campaignid, sample, number, starts_with("level"), family, genus, species, caab_code) %>%
  dplyr::filter(!level_2 %in% c("","Unscorable", NA)) %>%  
  group_by(campaignid, sample, across(starts_with("level")), family, genus, species, caab_code) %>%
  dplyr::tally(number, name = "count") %>%
  ungroup() %>%                                                     
  dplyr::select(campaignid, sample, level_1, everything()) %>%
  glimpse()

metadata.missing.habitat <- anti_join(
  metadata %>% dplyr::filter(successful_count == "Yes" | successful_length == "Yes"),
  tidy_habitat,
  by = c("campaignid", "sample")
) %>%
  glimpse()

write_csv(tidy_habitat %>%
            dplyr::rename(opcode = sample),"data/to upload/2023-03_SwC_stereo-BRUVs_benthos-count.csv")


# RELIEF ----
# read in forwards annotations
forwards_relief <- read.delim("data/2023-03_SwC_stereo-BRUVs/2023-03_SwC_stereo-BRUVs_Forwards_Relief_Dot Point Measurements.txt", 
                              header = T, skip = 4, stringsAsFactors = FALSE, 
                              colClasses = "character", na.strings = "") %>%
  clean_names()%>%
  dplyr::mutate(filename = str_replace_all(filename, "take 2", ""))

# read in forwards annotations
backwards_relief <- read.delim("data/2023-03_SwC_stereo-BRUVs/2023-03_SwC_stereo-BRUVs_Backwards_Relief_Dot Point Measurements.txt", 
                               header = T, skip = 4, stringsAsFactors = FALSE, 
                               colClasses = "character", na.strings = "") %>%
  clean_names() 

names(forwards_relief)
names(backwards_relief)

forwards_relief %>%
  dplyr::select(filename, relief_annotated, level_2, level_3, level_4, level_5, caab_code, scientific) %>%
  head(10)

unique(forwards_relief$relief_annotated)

relief_with_schema <- bind_rows(forwards_relief, backwards_relief) %>%
  dplyr::mutate(caab_code = as.numeric(caab_code)) %>%
  dplyr::filter(!is.na(level_5)) %>%
  dplyr::rename(sample = opcode)

unique(relief_with_schema$level_5)

relief.missing.metadata <- anti_join(relief_with_schema, metadata, by = c("sample")) %>%
  glimpse()

tidy_relief <- relief_with_schema %>%
  dplyr::mutate(sample = str_trim(sample)) %>%
  dplyr::mutate(number = 1) %>%                                     
  dplyr::mutate(campaignid = "2023-03_SwC_stereo-BRUVs") %>%
  dplyr::mutate(level_1 = "Physical") %>%
  dplyr::select(campaignid, sample, number, starts_with("level"), caab_code) %>%
  dplyr::filter(!level_2 %in% c("","Unscorable", NA)) %>%  
  group_by(campaignid, sample, across(starts_with("level")), caab_code) %>%
  dplyr::tally(number, name = "count") %>%
  ungroup() %>%                                                     
  dplyr::select(campaignid, sample, level_1, everything()) %>%
  glimpse()

metadata.missing.relief <- anti_join(metadata %>% dplyr::filter(successful_count == "Yes" | successful_length == "Yes"),
                                     tidy_relief,
                                     by = c("campaignid", "sample")
) %>% 
  glimpse()

write_csv(tidy_relief %>%
            dplyr::rename(opcode = sample), "data/to upload/2023-03_SwC_stereo-BRUVs_benthos-relief.csv")

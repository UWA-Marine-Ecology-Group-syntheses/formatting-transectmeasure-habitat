library(dplyr)
library(tidyr)
library(readr)
library(CheckEM)
library(googlesheets4)
library(tibble)

schema <- CheckEM::catami%>%
  dplyr::mutate(caab_code = as.numeric(caab_code))

metadata <- read_metadata(here::here("data/2022-11_Investigator_stereo-BRUVs/")) %>%
  dplyr::select(campaignid, sample, longitude_dd, latitude_dd, date_time, location, site, depth_m, successful_count, successful_length, successful_habitat_forward, successful_habitat_backward) %>%
  glimpse()

# read in forwards annotations
forwards <- read.delim("data/2022-11_Investigator_stereo-BRUVs/2022-11_Investigator_stereo-BRUVs_Forwards_Dot Point Measurements.txt", 
                       header = T, skip = 4, stringsAsFactors = FALSE, 
                       colClasses = "character", na.strings = "") %>%
  clean_names()%>%
  mutate(direction = "forwards") %>%
  rename(catami_l2_l3 = level_2, 
         catami_l4 = level_3,
         catami_l5 = level_4) %>%
  glimpse

# read in forwards annotations
backwards <- read.delim("data/2022-11_Investigator_stereo-BRUVs/2022-11_Investigator_stereo-BRUVs_Backwards_Dot Point Measurements.txt", 
                       header = T, skip = 4, stringsAsFactors = FALSE, 
                       colClasses = "character", na.strings = "") %>%
  clean_names() %>%
  mutate(direction = "backwards") %>%
  rename(catami_l2_l3 = level_2, 
         catami_l4 = level_3,
         catami_l5 = level_4) %>%
  glimpse

missing_opcode <- bind_rows(forwards, backwards) %>%
  filter(is.na(opcode)) %>%
  distinct(filename, direction)

combined <- bind_rows(forwards, backwards) %>%
  dplyr::select(filename, opcode, period, catami_l2_l3, catami_l4, catami_l5)


unique_class <- combined %>%
  distinct(catami_l2_l3, catami_l4, catami_l5)

# write_csv(unique_class, "data/2022-11_Investigator_stereo-BRUVs/unique_classes.csv")

# Have saved these to a google sheet and then by hand converted them into the correct classes in the new schema :(

# Read in google sheet rosetta stone ----
url <- "https://docs.google.com/spreadsheets/d/1SudVix9KYmkVe7B6eYEMx2nbWTeQSddz9_NAUgiKd-k/edit?usp=sharing"

rosetta <- read_sheet(url) %>%
  distinct()

# # Combine to data
# catami_cols <- c("level_1" = NA,
#                  "level_2" = NA,
#                  "level_3" = NA,
#                  "level_4" = NA,
#                  "level_5" = NA,
#                  "level_6" = NA,
#                  "level_7" = NA,
#                  "level_8" = NA,
#                  "family" = NA,
#                  "genus" = NA,
#                  "species" = NA)

habitat_with_schema <- left_join(combined, rosetta) %>%
  # dplyr::select(!c(catami_l2_l3, catami_l4, catami_l5, qualifiers)) %>%
  dplyr::rename(sample = opcode) %>%
  glimpse


missing_schema <- habitat_with_schema %>%
  dplyr::filter(is.na(caab_code)) %>%
  distinct()

num.points <- 20

wrong_points_habitat <- habitat_with_schema %>%
  group_by(sample) %>%
  summarise(points.annotated = n()) %>%
  left_join(metadata) %>%
  dplyr::mutate(expected = case_when(
    successful_habitat_forward %in% "Yes" & successful_habitat_backward %in% "Yes" ~ num.points * 2, 
    successful_habitat_forward %in% "Yes" & successful_habitat_backward %in% "No" ~ num.points * 1, 
    successful_habitat_forward %in% "No" & successful_habitat_backward %in% "Yes" ~ num.points * 1, 
    successful_habitat_forward %in% "No" & successful_habitat_backward %in% "No" ~ num.points * 0)) %>%
  dplyr::filter(!points.annotated == expected) %>%
  glimpse()

habitat.missing.metadata <- anti_join(habitat_with_schema, metadata, by = c("sample")) %>%
  glimpse()

metadata.missing.habitat <- anti_join(metadata, habitat_with_schema, by = c("sample")) %>%
  glimpse()

tidy_habitat <- habitat_with_schema %>%
  dplyr::mutate(caab_code = as.character(caab_code),number=1) %>%
  left_join(catami) %>%
  dplyr::mutate(campaignid = "2022-11_Investigator_stereo-BRUVs") %>%
  dplyr::select(campaignid, sample, number, starts_with("level"), family, genus, species) %>%
  dplyr::filter(!level_2 %in% c("","Unscorable", NA)) %>%  
  group_by(campaignid, sample, across(starts_with("level")), family, genus, species) %>%
  dplyr::tally(number, name = "count") %>%
  ungroup() %>%                                                     
  dplyr::select(campaignid, sample, level_1, everything()) %>%
  rename(opcode = sample) %>%
  left_join(catami %>% select(-qualifiers)) %>%
  glimpse()

write_csv(tidy_habitat, "data/to upload/2022-11_Investigator_stereo-BRUVs_benthos-count.csv")


# RELIEF ----
# read in forwards annotations
forwards_relief <- read.delim("data/2022-11_Investigator_stereo-BRUVs/2022-11_Investigator_stereo-BRUVs_forwards_relief_Dot Point Measurements.txt", 
                              header = T, skip = 4, stringsAsFactors = FALSE, 
                              colClasses = "character", na.strings = "") %>%
  clean_names()

# read in forwards annotations
backwards_relief <- read.delim("data/2022-11_Investigator_stereo-BRUVs/2022-11_Investigator_stereo-BRUVs_backwards_relief_Dot Point Measurements.txt", 
                               header = T, skip = 4, stringsAsFactors = FALSE, 
                               colClasses = "character", na.strings = "") %>%
  clean_names() #%>%
  #separate(filename, into = c("opcode","extra"), sep = ".JPG")

relief_with_schema <- bind_rows(forwards_relief, backwards_relief) %>%
  dplyr::left_join(catami) %>%
  dplyr::rename(sample = opcode) 

relief.missing.metadata <- anti_join(relief_with_schema, metadata, by = c("sample")) %>%
  glimpse()

metadata.missing.relief <- anti_join(metadata, relief_with_schema, by = c("sample")) %>%
  glimpse()

tidy_relief <- relief_with_schema %>%
  dplyr::mutate(number = 1) %>%                                     
  dplyr::mutate(campaignid = "2022-11_Investigator_stereo-BRUVs") %>%
  dplyr::select(campaignid, sample, number, starts_with("level"), family, genus, species) %>%
  dplyr::filter(!level_2 %in% c("","Unscorable", NA)) %>%  
  group_by(campaignid, sample, across(starts_with("level")), family, genus, species) %>%
  dplyr::tally(number, name = "count") %>%
  ungroup() %>%                                                     
  dplyr::select(campaignid, sample, level_5, count) %>%
  rename(opcode = sample) %>%
  left_join(catami %>% select(-qualifiers)) %>%
  glimpse()

write_csv(tidy_relief, "data/to upload/2022-11_Investigator_stereo-BRUVs_benthos-relief.csv")

relief_samples <- tidy_relief %>% 
  distinct(campaignid, opcode) %>%
  group_by(campaignid) %>%
  summarise(relief_sample = n(), .groups = "drop")

benthos_samples <- tidy_habitat %>%
  distinct(campaignid, opcode) %>%
  group_by(campaignid) %>%
  summarise(benthos_sample = n(), .groups = "drop")

sample_summary <- full_join(
  relief_samples,
  benthos_samples,
  by = c("campaignid")
)

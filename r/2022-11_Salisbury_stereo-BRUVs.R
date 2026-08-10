library(dplyr)
library(tidyr)
library(readr)
library(CheckEM)
library(googlesheets4)
library(stringr)
schema <- CheckEM::catami%>%
  dplyr::mutate(caab_code = as.numeric(caab_code))

metadata <- read_metadata(here::here("data/2022-11_Salisbury_stereo-BRUVs/")) %>%
  dplyr::select(campaignid, sample, longitude_dd, latitude_dd, date_time, location, site, depth_m, successful_count, successful_length, successful_habitat_forward, successful_habitat_backward) %>%
  glimpse()

# read in forwards annotations
forwards <- read.delim("data/2022-11_Salisbury_stereo-BRUVs/2022-11_Salisbury_stereo-BRUVs_Forwards_Dot Point Measurements.txt", 
                       header = T, skip = 4, stringsAsFactors = FALSE, 
                       colClasses = "character", na.strings = "") %>%
  clean_names()  %>%
  #dplyr::mutate(caab_code = as.numeric(code)) %>%
  dplyr::mutate(direction = "forwards") %>%
  glimpse

# read in forwards annotations
backwards <- read.delim("data/2022-11_Salisbury_stereo-BRUVs/2022-11_Salisbury_stereo-BRUVs_Backwards_Dot Point Measurements.txt", 
                       header = T, skip = 4, stringsAsFactors = FALSE, 
                       colClasses = "character", na.strings = "") %>%
  clean_names() %>%
  #dplyr::mutate(caab_code = as.numeric(code))%>%
  dplyr::mutate(direction = "backwards")

combined <- bind_rows(forwards, backwards) %>%
  dplyr::select(filename, opcode, period, catami_l2_l3, catami_l4, catami_l5) %>%
  dplyr::filter(!str_detect(catami_l2_l3, regex("Drift Algae", ignore_case = TRUE)))


unique_class <- combined %>%
  distinct(catami_l2_l3, catami_l4, catami_l5)


# Have saved these to a google sheet and then by hand converted them into the correct classes in the new schema :(

# Read in google sheet rosetta stone ----
url <- "https://docs.google.com/spreadsheets/d/1SudVix9KYmkVe7B6eYEMx2nbWTeQSddz9_NAUgiKd-k/edit?usp=sharing"

rosetta <- read_sheet(url) %>%
  distinct()

# Combine to data

habitat_with_schema <- left_join(combined, rosetta) %>%
  # dplyr::select(!c(catami_l2_l3, catami_l4, catami_l5, qualifiers)) %>%
  dplyr::rename(sample = opcode) %>%
  glimpse

missing_schema <- habitat_with_schema %>%
  dplyr::filter(is.na(caab_code)) %>%
  dplyr::filter(!is.na(catami_l2_l3)) %>%
  distinct(catami_l2_l3, catami_l4, catami_l5)

write_csv(missing_schema, "data/2022-11_Salisbury_stereo-BRUVs/unique_classes.csv")

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
  dplyr::mutate(number = 1) %>%  
  dplyr::mutate(caab_code = as.character(caab_code)) %>%
  left_join(catami) %>%
  dplyr::mutate(campaignid = "2022-11_Salisbury_stereo-BRUVs") %>%
  dplyr::select(campaignid, sample, number, starts_with("level"), family, genus, species, caab_code) %>%
  dplyr::filter(!level_2 %in% c("","Unscorable", NA)) %>%  
  group_by(campaignid, sample, across(starts_with("level")), family, genus, species, caab_code) %>%
  dplyr::tally(number, name = "count") %>%
  ungroup() %>%                                                     
  dplyr::select(campaignid, sample, level_1, everything(), -caab_code) %>%
  left_join(catami %>% select(-qualifiers)) %>%
  dplyr::rename(opcode = sample)%>%
  glimpse()

missing_from_schema <- anti_join(tidy_habitat, catami)


write_csv(tidy_habitat, "data/to upload/2022-11_Salisbury_stereo-BRUVs_benthos-count.csv")

# RELIEF ----
# read in forwards annotations
forwards_relief <- read.delim("data/2022-11_Salisbury_stereo-BRUVs/2022-11_Salisbury_stereo-BRUVs_forwards_relief_Dot Point Measurements.txt", 
                              header = T, skip = 4, stringsAsFactors = FALSE, 
                              colClasses = "character", na.strings = "") %>%
  clean_names()

# read in forwards annotations
backwards_relief <- read.delim("data/2022-11_Salisbury_stereo-BRUVs/2022-11_Salisbury_stereo-BRUVs_backward_relief_Dot Point Measurements.txt", 
                               header = T, skip = 4, stringsAsFactors = FALSE, 
                               colClasses = "character", na.strings = "") %>%
  clean_names() #%>%
#separate(filename, into = c("opcode","extra"), sep = ".JPG")

relief_with_schema <- bind_rows(forwards_relief, backwards_relief) %>%
  filter(!is.na(level_5)) %>%
  dplyr::select(campaignid, opcode, level_5)%>%
  dplyr::left_join(catami) %>%
  dplyr::rename(sample = opcode) 

relief.missing.metadata <- anti_join(relief_with_schema, metadata, by = c("sample")) %>%
  glimpse()

unique(relief.missing.metadata$filename)

metadata.missing.relief <- anti_join(metadata, relief_with_schema, by = c("sample")) %>%
  glimpse()

tidy_relief <- relief_with_schema %>%
  dplyr::mutate(number = 1) %>%                                     
  dplyr::mutate(campaignid = "2022-11_Salisbury_stereo-BRUVs") %>%
  dplyr::select(campaignid, sample, number, starts_with("level"), family, genus, species, caab_code) %>%
  dplyr::filter(!level_2 %in% c("","Unscorable", NA)) %>%  
  group_by(campaignid, sample, across(starts_with("level")), family, genus, species, caab_code) %>%
  dplyr::tally(number, name = "count") %>%
  ungroup() %>%                                                     
  dplyr::select(campaignid, sample, level_1, everything()) %>%
  dplyr::rename(opcode = sample)%>%
  glimpse()
  
test <- tidy_relief %>%
  filter(is.na(caab_code))

write_csv(tidy_relief, "data/to upload/2022-11_Salisbury_stereo-BRUVs_benthos-relief.csv")

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


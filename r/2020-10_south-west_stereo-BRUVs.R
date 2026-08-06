library(dplyr)
library(tidyr)
library(readr)
library(CheckEM)
library(googlesheets4)
library(stringr)

schema <- CheckEM::catami %>%
  dplyr::mutate(caab_code = as.numeric(caab_code)) %>%
  select(-qualifiers)

# HABITAT -----
metadata <- read_metadata(here::here("data/2020-10_south-west_stereo-BRUVs/")) %>%
  dplyr::select(campaignid, sample, longitude_dd, latitude_dd, date_time, location, site, depth_m, #observer_count, observer_length,
                successful_count, successful_length, successful_habitat_forward, successful_habitat_backward) %>%
  glimpse()

# read in forwards annotations
forwards <- read.delim("data/2020-10_south-west_stereo-BRUVs/2020-10_south-west_stereo-BRUVs_random-points_forwards_Dot Point Measurements.txt",
                       header = T, skip = 4, stringsAsFactors = FALSE,
                       colClasses = "character", na.strings = "") %>%
  clean_names() %>%
  dplyr::filter(!filename %in% "IO333.jpg") %>%
  dplyr::mutate(filename = str_replace_all(filename, "take 2", ""))

# read in backwards annotations
backwards <- read.delim("data/2020-10_south-west_stereo-BRUVs/2020-10_south-west_stereo-BRUVs_random-points_backwards_Dot Point Measurements.txt",
                        header = T, skip = 4, stringsAsFactors = FALSE,
                        colClasses = "character", na.strings = "") %>%
  clean_names()

habitat_with_schema <- bind_rows(forwards, backwards) %>%
  dplyr::rename(caab_code = qualifiers) %>%                          # FIX: was `code`, raw column is `qualifiers`
  dplyr::mutate(caab_code = as.numeric(caab_code)) %>%
  dplyr::mutate(caab_code = case_when(
    level_2 %in% c("Unknown", "Open Water") ~ 1,                     # FIX: was `broad`
    level_2 %in% "Invertebrate Complex" ~ 99900044,                  # FIX: was `~ 2` (not a real CAAB code) -> "Sessile invertebrates" in schema
    
    level_4 %in% "Thalassodendrum sp." ~ 63618905,                   # FIX: was `type`, fix incorrect caab code
    level_4 %in% "Ecklonia radiata" ~ 54079009,                      # FIX: was `type`, fix incorrect caab code
    
    caab_code %in% 90300910 ~ 80300910, # fix incorrect caab code
    
    .default = caab_code
  )) %>%
  dplyr::mutate(sample = str_replace_all(filename, c(".JPG"= "", ".jpg" = "")) %>% str_trim()) %>%
  dplyr::rename(raw_level_2 = level_2, raw_level_3 = level_3,        # FIX: keep the annotator's typed text under raw_* names,
                raw_level_4 = level_4, raw_level_5 = level_5) %>%    #      purely for QC below - not used in the final data
  dplyr::left_join(                                                  # FIX: this join was missing entirely -
    schema %>% dplyr::select(caab_code, level_1, level_2, level_3, level_4, level_5, family, genus, species), # FIX: all levels now come from schema, not raw
    by = "caab_code"
  )

distinct_hab_types <- habitat_with_schema %>%
  select(starts_with("level"), family, genus, species, caab_code) %>%   # FIX: dropped broad/morphology/type (now = level_2/3/4)
  distinct()

missing_caab_code <- habitat_with_schema %>%
  dplyr::filter(is.na(caab_code)) %>%
  distinct(level_2, level_3, level_4) # good                            # FIX: was broad, morphology, type

unique(habitat_with_schema$sample) %>% sort()

unmatched_categories <- habitat_with_schema %>%
  distinct(raw_level_2, raw_level_3, raw_level_4, raw_level_5, caab_code, level_1, level_2, level_3, level_4, level_5, family, genus, species) %>%
  dplyr::filter(is.na(level_1)) %>%
  glimpse()

if (nrow(unmatched_categories) > 0) {
  warning(nrow(unmatched_categories), " distinct habitat categories did not match `schema` - see `unmatched_categories`")
}

missing_caab_code <- habitat_with_schema %>%
  dplyr::filter(is.na(caab_code)) %>%
  distinct(raw_level_2, raw_level_3, raw_level_4) # good                # FIX: was broad, morphology, type; now raw_level_* since level_2-4 come from schema


num.points <- 40

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

tidy_habitat <- habitat_with_schema %>%
  dplyr::mutate(sample = str_trim(sample))%>%
  dplyr::mutate(number = 1) %>%                                     
  dplyr::mutate(campaignid = "2020-10_south-west_stereo-BRUVs") %>%
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
            dplyr::rename(opcode = sample),"data/to upload/2020-10_south-west_stereo-BRUVs_benthos-count.csv")


# RELIEF ----
# read in forwards annotations
forwards_relief <- read.delim("data/2020-10_south-west_stereo-BRUVs/2020-10_south-west_stereo_BRUVs_Habitat_grid_forwards_Dot Point Measurements.txt",
                              header = T, skip = 4, stringsAsFactors = FALSE,
                              colClasses = "character", na.strings = "") %>%
  clean_names()%>%
  dplyr::filter(!filename %in% "IO333.jpg") %>%
  dplyr::mutate(filename = str_replace_all(filename, "take 2", ""))

# read in backwards annotations
backwards_relief <- read.delim("data/2020-10_south-west_stereo-BRUVs/2020-10_south-west_stereo_BRUVs_Habitat_grid_backwards_Dot Point Measurements.txt",
                               header = T, skip = 4, stringsAsFactors = FALSE,
                               colClasses = "character", na.strings = "") %>%
  clean_names() 

relief_with_schema <- bind_rows(forwards_relief, backwards_relief) %>%
  dplyr::select(filename, relief = scientific) %>%                  # FIX: was `relief` (didn't exist), raw column is `scientific`
  dplyr::mutate(sample = str_replace_all(filename, c(".JPG"= "", ".jpg" = "")) %>% str_trim()) %>%
  dplyr::filter(!is.na(relief)) %>%
  dplyr::mutate(level_5 = str_sub(relief, 2, 2)) %>%
  dplyr::filter(!level_5 %in% "n") %>%
  dplyr::left_join(catami) 

unique(relief_with_schema$level_5)

relief.missing.metadata <- anti_join(relief_with_schema, metadata, by = c("sample")) %>%
  glimpse()

unmatched_relief_categories <- relief_with_schema %>%
  distinct(relief, level_5, level_1) %>%
  dplyr::filter(is.na(level_1)) %>%
  glimpse()

if (nrow(unmatched_relief_categories) > 0) {
  warning(nrow(unmatched_relief_categories), " distinct relief categories did not match `catami` - see `unmatched_relief_categories`")
}

tidy_relief <- relief_with_schema %>%
  dplyr::mutate(sample = str_trim(sample))%>%
  dplyr::mutate(number = 1) %>%                                     
  dplyr::mutate(campaignid = "2020-10_south-west_stereo-BRUVs") %>%
  dplyr::select(campaignid, sample, number, starts_with("level"), family, genus, species, caab_code) %>%
  dplyr::filter(!level_2 %in% c("","Unscorable", NA)) %>%  
  group_by(campaignid, sample, across(starts_with("level")), family, genus, species, caab_code) %>%
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
            dplyr::rename(opcode = sample), "data/to upload/2020-10_south-west_stereo-BRUVs_benthos-relief.csv")

names(tidy_habitat)
names(metadata)

unique(tidy_habitat$campaignid)
unique(metadata$campaignid)
setdiff(unique(tidy_habitat$campaignid), unique(metadata$campaignid))

anti_join(
  tidy_habitat %>% distinct(campaignid, sample),
  metadata %>% distinct(campaignid, sample),
  by = c("campaignid", "sample")
)

class(tidy_habitat$sample)
class(metadata$sample)

dplyr::full_join(
  tidy_habitat %>% distinct(campaignid, sample) %>% dplyr::mutate(in_habitat = TRUE),
  metadata %>% distinct(campaignid, sample) %>% dplyr::mutate(in_metadata = TRUE),
  by = c("campaignid", "sample")
) %>%
  dplyr::filter(is.na(in_habitat) | is.na(in_metadata))


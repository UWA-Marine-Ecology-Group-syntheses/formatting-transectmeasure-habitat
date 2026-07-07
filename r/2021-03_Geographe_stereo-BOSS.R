library(dplyr)
library(tidyr)
library(readr)
library(CheckEM)
library(janitor)
library(stringr)

# Small helper: guarantee these columns exist (as NA) even if the schema
# join doesn't produce every CATAMI level - avoids "column doesn't exist"
# errors later in select() if your data doesn't reach level_6/7/8.
ensure_cols <- function(df, cols) {
  df <- dplyr::ungroup(df)
  missing <- setdiff(cols, names(df))
  if (length(missing) > 0) {
    df[missing] <- NA_character_
  }
  df
}

schema <- CheckEM::catami %>%
  dplyr::mutate(caab_code = as.numeric(caab_code)) %>%
  dplyr::select(-qualifiers)

# HABITAT -----
metadata <- read_metadata(here::here("data/2021-03_Geographe_BOSS//"), method = "BOSS") %>%
  dplyr::select(campaignid, sample, longitude_dd, latitude_dd, date_time,
                location, site, depth_m, successful_count, successful_length) %>%
  glimpse()

# read in panoramic annotations
panoramic <- read.delim(
  here::here("data/2021-03_Geographe_BOSS/2021-03_Geographe_BOSS_Dot Point Measurements.txt"),
  header = TRUE, skip = 4, stringsAsFactors = FALSE,
  colClasses = "character", na.strings = ""
) %>%
  clean_names() %>%
  dplyr::mutate(sample = str_remove(filename, "\\.jpg$"))

# read in downwards annotations
# downwards <- read.delim(
#   here::here("data/2021-03_Geographe_BOSS/2021-03_Geographe_BOSS_Downwards_Dot Point Measurements.txt"),
#   header = TRUE, skip = 4, stringsAsFactors = FALSE,
#   colClasses = "character", na.strings = ""
# ) %>%
#   clean_names() %>%
#   dplyr::mutate(sample = str_remove(filename, "\\.jpg$"))

names(panoramic)
# names(downwards)

habitat_with_schema <- panoramic %>%
  dplyr::mutate(caab_code = as.numeric(code)) %>%   # raw "CODE" column holds the CATAMI code
  dplyr::left_join(schema, by = "caab_code") %>%
  dplyr::mutate(
    level_1 = dplyr::if_else(caab_code == 2, "Biota", level_1)
  ) %>%
  ensure_cols(c("level_1", "level_2", "level_3", "level_4", "level_5",
                "level_6", "level_7", "level_8", "family", "genus", "species"))

missing_caab_code <- habitat_with_schema %>%
  dplyr::filter(is.na(level_1)) %>%
  distinct(caab_code)

missing_caab_code

names(habitat_with_schema)

distinct_hab_types <- habitat_with_schema %>%
  select(starts_with("level"), family, genus, species, caab_code) %>%
  distinct()

missing_caab_code_raw <- habitat_with_schema %>%
  dplyr::filter(is.na(caab_code)) %>%
  distinct(sample, filename)

unique(habitat_with_schema$sample) %>% sort()

num.points <- 80

wrong_points_habitat <- habitat_with_schema %>%
  group_by(sample) %>%
  summarise(points.annotated = n()) %>%
  left_join(metadata, by = "sample") %>%
  glimpse()

habitat.missing.metadata <- anti_join(habitat_with_schema, metadata, by = c("sample")) %>%
  glimpse()

tidy_habitat <- habitat_with_schema %>%
  dplyr::mutate(sample = str_trim(sample)) %>%
  dplyr::mutate(number = 1) %>%
  dplyr::mutate(campaignid = "2021-03_Geographe_BOSS") %>%
  ensure_cols(c("level_1", "level_2", "level_3", "level_4", "level_5",
                "level_6", "level_7", "level_8", "family", "genus", "species")) %>%
  dplyr::select(campaignid, sample, number, starts_with("level"), family, genus, species, caab_code) %>%
  dplyr::filter(!level_2 %in% c("", "Unscorable", NA)) %>%
  group_by(campaignid, sample, across(starts_with("level")), family, genus, species, caab_code) %>%
  dplyr::tally(number, name = "count") %>%
  ungroup() %>%
  dplyr::rename(opcode = sample) %>%
  dplyr::select(campaignid, opcode,
                level_1, level_2, level_3, level_4, level_5, level_6, level_7, level_8,
                family, genus, species, caab_code, count) %>%
  glimpse()

metadata.missing.habitat <- anti_join(
  metadata %>% dplyr::filter(successful_count == "Yes" | successful_length == "Yes") %>%
    dplyr::rename(opcode = sample),
  tidy_habitat,
  by = c("campaignid", "opcode")
) %>%
  glimpse()

write_csv(tidy_habitat, here::here("data/to upload/2021-03_Geographe_BOSS_benthos-count.csv"))


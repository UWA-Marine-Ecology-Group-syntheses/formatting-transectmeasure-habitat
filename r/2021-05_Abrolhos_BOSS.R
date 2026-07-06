library(dplyr)
library(tidyr)
library(readr)
library(CheckEM)
library(janitor)
library(stringr)

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
metadata <- read_metadata(here::here("data/2021-05_Abrolhos_BOSS/"), method = "BOSS") %>%
  dplyr::select(campaignid, sample, longitude_dd, latitude_dd, date_time,
                location, site, depth_m, successful_count, successful_length) %>%
  glimpse()

# read in panoramic annotations
panoramic <- read.delim(
  here::here("data/2021-05_Abrolhos_BOSS/2021-05_Abrolhos_BOSS_Dot Point Measurements.txt"),
  header = TRUE, skip = 4, stringsAsFactors = FALSE,
  colClasses = "character", na.strings = ""
) %>%
  clean_names() %>%
  dplyr::mutate(sample = str_remove(filename, "\\.jpg$"))

names(panoramic)
head(panoramic[, c("scientific", "qualifiers", "check")], 10)

habitat_with_schema <- panoramic %>%
  rename(caab_code = qualifiers) %>%
  mutate(
    caab_code = as.numeric(caab_code),
    level_1 = case_when(
      is.na(level_2) ~ NA_character_,
      level_2 == "Unknown" ~ "Unknown",
      TRUE ~ "Biota"
    )
  ) %>%
  ensure_cols(c(
    "level_1", "level_2", "level_3", "level_4", "level_5",
    "level_6", "level_7", "level_8",
    "family", "genus", "species"
  ))

missing_caab_code_raw <- panoramic %>%
  filter(is.na(suppressWarnings(as.numeric(qualifiers)))) %>%
  distinct(qualifiers)
missing_caab_code_raw

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

num.points <- 20

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
  dplyr::mutate(campaignid = "2021-05_Abrolhos_BOSS") %>%
  ensure_cols(c("level_1", "level_2", "level_3", "level_4", "level_5",
                "level_6", "level_7", "level_8", "family", "genus", "species")) %>%
  dplyr::select(campaignid, sample, number, starts_with("level"), family, genus, species, caab_code) %>%
  dplyr::filter(!level_2 %in% c("", "Unscorable", NA)) %>%
  group_by(campaignid, sample, across(starts_with("level")), family, genus, species, caab_code) %>%
  dplyr::tally(number, name = "count") %>%
  ungroup() %>%
  dplyr::rename(period = sample) %>%
  dplyr::select(campaignid, period,
                level_1, level_2, level_3, level_4, level_5, level_6, level_7, level_8,
                family, genus, species, caab_code, count) %>%
  glimpse()

metadata.missing.habitat <- anti_join(
  metadata %>% dplyr::filter(successful_count == "Yes" | successful_length == "Yes") %>%
    dplyr::rename(period = sample),
  tidy_habitat,
  by = c("campaignid", "period")
) %>%
  glimpse()

write_csv(tidy_habitat, here::here("data/to upload/2021-05_Abrolhos_BOSS_benthos-count.csv"))



# RELIEF ----
# read in forwards annotations
# RELIEF (BOSS - panoramic only) ----
relief_file <- read.delim(
  here::here("data/2021-05_Abrolhos_BOSS/2021-05_Abrolhos_BOSS_Relief_Dot Point Measurements.txt"),
  header = T, skip = 4, stringsAsFactors = FALSE,
  colClasses = "character", na.strings = ""
) %>%
  clean_names() %>%
  glimpse()

relief_with_schema <- relief_file %>%
  dplyr::select(filename, relief = scientific) %>%          # BOSS "scientific" = BRUVs "Relief"
  dplyr::mutate(sample = str_replace_all(filename, c(".JPG" = "", ".jpg" = ""))) %>%
  dplyr::filter(!is.na(relief)) %>%
  dplyr::mutate(level_5 = str_sub(relief, 2, 2)) %>%
  dplyr::filter(!level_5 %in% "n") %>%
  dplyr::left_join(catami) %>%
  glimpse()

unique(relief_with_schema$level_5)

relief.missing.metadata <- anti_join(relief_with_schema, metadata, by = c("sample")) %>%
  glimpse()

metadata.missing.relief <- anti_join(metadata, relief_with_schema, by = c("sample")) %>%
  glimpse()

tidy_relief <- relief_with_schema %>%
  dplyr::mutate(number = 1) %>%
  dplyr::mutate(campaignid = "2021-05_Abrolhos_BOSS") %>%
  dplyr::select(campaignid, sample, number, starts_with("level"), family, genus, species, caab_code) %>%
  dplyr::filter(!level_2 %in% c("", "Unscorable", NA)) %>%
  group_by(campaignid, sample, across(starts_with("level")), family, genus, species, caab_code) %>%
  dplyr::tally(number, name = "count") %>%
  ungroup() %>%
  dplyr::select(campaignid, sample, level_1, everything()) %>%
  dplyr::rename(period = sample) %>%
  glimpse()

write_csv(tidy_relief, "data/to upload/2021-05_Abrolhos_BOSS_benthos-relief.csv")

relief_samples <- tidy_relief %>%
  distinct(campaignid, period) %>%
  group_by(campaignid) %>%
  summarise(relief_sample = n(), .groups = "drop")

benthos_samples <- tidy_habitat %>%
  distinct(campaignid, opcode) %>%
  group_by(campaignid) %>%
  summarise(benthos_sample = n(), .groups = "drop")

sample_summary <- full_join(relief_samples, benthos_samples, by = c("campaignid"))
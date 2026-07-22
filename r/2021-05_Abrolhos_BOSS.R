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
  mutate(
    level_2 = case_when(
      level_2 == "Invertebrate Complex" & level_3 == "Complex 1" ~ "Sessile invertebrates",
      TRUE ~ level_2
    ),
    level_3 = case_when(
      level_2 == "Sessile invertebrates" ~ NA_character_,
      level_2 == "Macroalgae" & level_3 == "Drift" ~ NA_character_,
      level_3 == "Erect course branching" ~ "Erect coarse branching",
      TRUE ~ level_3
    ),
    # Turf mat / Veneer are texture notes on Consolidated substrate, not their own category
    level_4 = case_when(
      level_2 == "Consolidated" & level_4 %in% c("Turf mat", "Veneer") ~ NA_character_,
      TRUE ~ level_4
    )
  ) %>%
  ensure_cols(c(
    "level_1", "level_2", "level_3", "level_4", "level_5",
    "level_6", "level_7", "level_8",
    "family", "genus", "species"
  )) %>%
  # Ecklonia: split the species name out of level_4 into family/genus/species properly
  mutate(
    family  = if_else(level_4 == "Ecklonia radiata", "Lessoniaceae", family),
    genus   = if_else(level_4 == "Ecklonia radiata", "Ecklonia", genus),
    species = if_else(level_4 == "Ecklonia radiata", "radiata", species),
    level_4 = if_else(level_4 == "Ecklonia radiata", "Brown", level_4)
  )

flat_codes <- schema %>%
  dplyr::filter(is.na(level_4), is.na(family), is.na(genus), is.na(species)) %>%
  dplyr::distinct(level_1, level_2, level_3, flat_caab_code = caab_code)

habitat_with_schema_filtered <- habitat_with_schema %>%
  dplyr::left_join(flat_codes, by = c("level_1", "level_2", "level_3")) %>%
  dplyr::mutate(
    caab_code = dplyr::case_when(
      genus == "Ecklonia" & species == "radiata" ~ 54079009,
      is.na(level_4) & !is.na(flat_caab_code)     ~ flat_caab_code,
      TRUE                                        ~ caab_code
    ),
    caab_code = if_else(caab_code == 90300910, 80300910, caab_code)  # raw typo
  ) %>%
  dplyr::select(-flat_caab_code)

level_relabel <- tibble::tribble(
  ~caab_code, ~level_1_schema, ~level_2_schema, ~level_3_schema,          ~level_4_schema,      ~level_5_schema,
  82001003,   "Physical",      "Substrate",     "Consolidated (hard)",   "Boulders",            NA_character_,
  82001004,   "Physical",      "Substrate",     "Consolidated (hard)",   "Cobbles",             NA_character_,
  82001002,   "Physical",      "Substrate",     "Consolidated (hard)",   "Rock",                NA_character_,
  82001007,   "Physical",      "Substrate",     "Unconsolidated (soft)", "Pebble / gravel",     "Biologenic",
  82001011,   "Physical",      "Substrate",     "Unconsolidated (soft)", "Pebble / gravel",     "Gravel (2-10mm)",
  82001012,   "Physical",      "Substrate",     "Unconsolidated (soft)", "Pebble / gravel",     "Pebble (10-64mm)",
  82001014,   "Physical",      "Substrate",     "Unconsolidated (soft)", "Sand / mud (<2mm)",   "Coarse sand (with shell fragments)",
  82001015,   "Physical",      "Substrate",     "Unconsolidated (soft)", "Sand / mud (<2mm)",   "Fine sand (no shell fragments)",
  11168902,   NA_character_,   "Cnidaria",      "Corals",                "Black & Octocorals",  "Branching (3D)",
  11168912,   NA_character_,   "Cnidaria",      "Corals",                "Black & Octocorals",  "Fan (2D)",
  11001000,   NA_character_,   "Cnidaria",      "Hydroids",              NA_character_,         NA_character_,
  10000909,   NA_character_,   "Sponges",       "Cup-likes",             NA_character_,         NA_character_,
  11168917,   NA_character_,   "Cnidaria",      "Corals",                "Black & Octocorals",  "Whip",
  11168918,   NA_character_,   "Cnidaria",      "Corals",                "Black & Octocorals",  "Quill (seapen)",
  80300930,   NA_character_,   NA_character_,   "Filamentous / filiform", NA_character_,         NA_character_
)

habitat_with_schema_clean <- habitat_with_schema_filtered %>%
  dplyr::left_join(level_relabel, by = "caab_code") %>%
  dplyr::mutate(
    level_1 = dplyr::coalesce(level_1_schema, level_1),
    level_2 = dplyr::coalesce(level_2_schema, level_2),
    level_3 = dplyr::coalesce(level_3_schema, level_3),
    level_4 = dplyr::coalesce(level_4_schema, level_4),
    level_5 = dplyr::coalesce(level_5_schema, level_5)
  ) %>%
  dplyr::select(-level_1_schema, -level_2_schema, -level_3_schema, -level_4_schema, -level_5_schema)

missing_caab_code_raw <- panoramic %>%
  filter(is.na(suppressWarnings(as.numeric(qualifiers)))) %>%
  distinct(qualifiers)
missing_caab_code_raw

missing_caab_code <- habitat_with_schema_clean %>%
  dplyr::filter(is.na(level_1)) %>%
  distinct(caab_code)
missing_caab_code

names(habitat_with_schema_clean)

distinct_hab_types <- habitat_with_schema_clean %>%
  select(starts_with("level"), family, genus, species, caab_code) %>%
  distinct()

missing_caab_code_raw <- habitat_with_schema_clean %>%
  dplyr::filter(is.na(caab_code)) %>%
  distinct(sample, filename)

unique(habitat_with_schema_clean$sample) %>% sort()

num.points <- 80

wrong_points_habitat <- habitat_with_schema_clean %>%
  group_by(sample) %>%
  summarise(points.annotated = n()) %>%
  left_join(metadata, by = "sample") %>%
  glimpse()

habitat.missing.metadata <- anti_join(habitat_with_schema_clean, metadata, by = c("sample")) %>%
  glimpse()

tidy_habitat <- habitat_with_schema_clean %>%
  dplyr::mutate(sample = str_trim(sample)) %>%
  dplyr::mutate(number = 1) %>%
  dplyr::mutate(campaignid = "2021-05_Abrolhos_BOSS") %>%
  ensure_cols(c("level_1", "level_2", "level_3", "level_4", "level_5",
                "level_6", "level_7", "level_8", "family", "genus", "species")) %>%
  dplyr::select(campaignid, sample, number, starts_with("level"), family, genus, species, caab_code) %>%
  dplyr::filter(!level_2 %in% c("", "Unknown", "Open Water", NA)) %>%   # <-- added "Open Water"
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

schema %>%
  dplyr::filter(level_2 == "Macroalgae", level_3 == "Erect coarse branching") %>%
  dplyr::select(level_3, level_4, caab_code)

schema %>%
  dplyr::filter(level_2 == "Macroalgae")

habitat_with_schema %>% filter(str_detect(level_4, "Ecklonia")) %>% distinct(level_3, level_4)

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
  distinct(campaignid, period) %>%
  group_by(campaignid) %>%
  summarise(benthos_sample = n(), .groups = "drop")

sample_summary <- full_join(relief_samples, benthos_samples, by = c("campaignid"))

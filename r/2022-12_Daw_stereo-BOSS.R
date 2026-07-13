library(dplyr)
library(tidyr)
library(readr)
library(CheckEM)
library(googlesheets4)

schema <- CheckEM::catami%>%
  dplyr::mutate(caab_code = as.numeric(caab_code))

metadata <- read_metadata(here::here("data/2022-12_Daw_stereo-BOSS/")) %>%
  rename(sample = period) %>%
  select(
    campaignid,
    sample,
    longitude_dd,
    latitude_dd,
    date_time,
    location,
    site,
    depth_m,
    successful_count,
    successful_length,
    successful_habitat_panoramic
  )
names(schema)


# read in forwards annotations
forwards <- read.delim(
  "data/2022-12_Daw_stereo-BOSS/2022-12_Daw_stereo-BOSS_Dot Point Measurements.txt",
  header = TRUE,
  skip = 4,
  stringsAsFactors = FALSE,
  colClasses = "character",
  na.strings = ""
) %>%
  clean_names()

forwards <- forwards %>%
  separate(
    level_2,
    into = c("level_2", "new_level_3"),
    sep = " > ",
    fill = "right"
  ) %>%
  mutate(
    level_5 = level_4,
    level_4 = level_3,
    level_3 = new_level_3
  ) %>%
  select(-new_level_3)

combined <- forwards %>%
  transmute(
    campaignid = "2022-12_Daw_stereo-BOSS",
    sample = period,
    level_2,
    level_3,
    level_4,
    level_5,
    scientific,
    qualifiers
  )

num.points <- 50

wrong_points_habitat <- combined %>%
  group_by(sample) %>%
  summarise(points.annotated = n(), .groups = "drop") %>%
  left_join(metadata, by = "sample") %>%
  mutate(
    expected = if_else(successful_habitat_panoramic == "Yes",
                       num.points,
                       0)
  ) %>%
  filter(points.annotated != expected)

habitat.missing.metadata <- anti_join(
  combined,
  metadata,
  by = "sample"
)

metadata.missing.habitat <- anti_join(
  metadata,
  combined,
  by = "sample"
)


tidy_habitat <- combined %>%
  separate(scientific, into = c("genus", "species")) %>%
  dplyr::mutate(number = 1) %>%  
  left_join(catami) %>%
  dplyr::mutate(caab_code = as.character(caab_code)) %>%
  #left_join(catami) %>%
  dplyr::mutate(campaignid = "2022-12_Daw_stereo-BOSS") %>%
  dplyr::select(campaignid, sample, number, starts_with("level"), family, genus, species, caab_code) %>%
  dplyr::filter(!level_2 %in% c("","Unscorable", NA)) %>%  
  group_by(campaignid, sample, across(starts_with("level")), family, genus, species, caab_code) %>%
  dplyr::tally(number, name = "count") %>%
  ungroup() %>%                                                     
  dplyr::select(campaignid, sample, level_1, everything()) %>%
  rename(period = sample) %>%
  glimpse()

write_csv(tidy_habitat, "data/to upload/2022-12_Daw_stereo-BOSS_benthos-count.csv")



combined <- combined %>%
  left_join(
    schema %>%
      select(
        caab_code,
        family,
        genus,
        species,
        level_1,
        level_2,
        level_3,
        level_4,
        level_5,
        level_6,
        level_7,
        level_8,
        qualifiers
      ),
    by = c(
      "level_2",
      "level_3",
      "level_4",
      "level_5",
      "qualifiers"
    )
  )




benthos_samples <- tidy_habitat %>%
  distinct(campaignid, opcode) %>%
  group_by(campaignid) %>%
  summarise(benthos_sample = n(), .groups = "drop")



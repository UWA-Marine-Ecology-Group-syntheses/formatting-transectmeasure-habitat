library(dplyr)
library(tidyr)
library(readr)
library(CheckEM)
library(googlesheets4)
library(stringr)

schema <- CheckEM::catami%>%
  dplyr::mutate(caab_code = as.numeric(caab_code))%>%
  select(-qualifiers)


head(schema)

metadata <- read_metadata(here::here("data/2014-12_Geographe.Bay_stereo-BRUVs//")) %>%
  dplyr::select(campaignid, sample, longitude_dd, latitude_dd, date_time, location, site, depth_m, 
                #successful_count, successful_length, successful_habitat_forwards, successful_habitat_backwards
                ) %>%
  rename(opcode = sample) %>%
  glimpse()

# read in forwards annotations
forwards <- read.delim("data/2014-12_Geographe.Bay_stereo-BRUVs/2014-12_Geographe.Bay_stereoBRUVs_habitat.txt", 
                       header = T, stringsAsFactors = FALSE, 
                       colClasses = "character", na.strings = "") %>%
  clean_names() %>%
  glimpse


benthos <- forwards %>%
  select(
    sample,
    campaignid,
    starts_with("biota_")) %>%
 rename(opcode = sample)


benthos_long <- benthos %>%
  pivot_longer(
    cols = starts_with("biota_"),
    names_to = "benthos_category",
    values_to = "count"
  ) %>%
  mutate(count = as.numeric(count))


benthos_filtered <- benthos_long %>%
  filter(count > 0)

head(benthos_filtered)

schema %>%
  filter(level_1 == "Biota") %>%
  distinct(level_2) %>%
  arrange(level_2)

unique(benthos_filtered$benthos_category)

benthos_lookup <- tibble::tribble(
  ~benthos_category,       ~level_2,
  "biota_macroalgae",      "Macroalgae",
  "biota_seagrasses",      "Seagrasses",
  "biota_sponges",         "Sponges",
  "biota_cnidaria",        "Cnidaria",
  "biota_crustacea",       "Crustacea",
  "biota_echinoderms",     "Echinoderms",
  "biota_fishes",          "Fishes",
  "biota_molluscs",        "Molluscs",
  "biota_worms",           "Worms",
  "biota_bryozoa",         "Bryozoa",
  "biota_ascidians",       "Ascidians",
  "biota_octocoral_black", "Cnidaria",
  "biota_stony_corals",    "Cnidaria",
  "biota_unconsolidated",  "Substrate",
  "biota_consolidated",  "Substrate")

benthos_lookup_level3 <- tibble::tribble(
  ~benthos_category,       ~level_3,
  "biota_unconsolidated",  "Unconsolidated (soft)",
  "biota_consolidated",    "Consolidated (hard)")

benthos_caab <- benthos_long %>% 
  left_join(benthos_lookup, by = "benthos_category") %>%
  left_join(benthos_lookup_level3, by = "benthos_category") %>%
  mutate(level_4 = NA) %>%
  filter(count > 0)

benthos_caab_joined <- benthos_caab %>% left_join( schema )

benthos_summary <- benthos_caab_joined %>%
  group_by(
    campaignid,
    opcode,
    level_1,
    level_2,
    level_3,
    level_4,
    level_5,
    level_6,
    level_7,
    level_8,
    family,
    genus,
    species,
    caab_code
  ) %>%
  mutate(campaignid = "2014-12_Geographe-bay_stereo-BRUVs") %>%
  summarise(
    count = sum(count, na.rm = TRUE),
    .groups = "drop") %>%
  semi_join(metadata) %>%
  #dplyr::filter(!level_2 %in% c("","Unscorable", NA))
  glimpse

unique(benthos_summary$level_2)
unique(benthos_summary$level_3)

write_csv(benthos_summary, "data/to upload/2014-12_Geographe.Bay_stereo-BRUVs_benthos-count.csv")


#RELIEF

relief <- forwards %>%
  select(
    sample,
    campaignid,
    starts_with("relief_"))%>%
  rename(opcode = sample)

relief_long <- relief %>%
  pivot_longer(
    cols = starts_with("relief_"),
    names_to = "relief_category",
    values_to = "count") %>%
  mutate(count = as.numeric(count))

relief_filtered <- relief_long %>%
  filter(count > 0)



names(relief)

schema %>%
  filter(level_2 == "Relief") %>%
  select(caab_code, level_1, level_2, level_3, level_4)

relief_lookup <- tibble::tribble(
  ~relief_category, ~caab_code,
  "relief_0_flat_substrate_sandy_rubble_with_few_features_0_substrate_slope_", 82003001,
  "relief_1_some_relief_features_amongst_mostly_flat_substrate_sand_rubble_45_degree_substrate_slope_", 82003003,
  "relief_2_mostly_relief_features_amongst_some_flat_substrate_or_rubble_45_substrate_slope_", 82003004,
  "relief_3_good_relief_structure_with_some_overhangs_45_substrate_slope_", 82003006,
  "relief_4_high_structural_complexity_fissures_and_caves_vertical_wall_90_substrate_slope_", 82003007
)

relief_with_schema <- relief_filtered %>%
  left_join(relief_lookup, by = "relief_category") %>%
  left_join(schema, by = "caab_code")

schema %>%
  filter(level_2 == "Relief") %>%
  select(caab_code, level_3, level_4)

relief_summary <- relief_with_schema %>%
  select(
    campaignid,
    opcode,
    level_1,
    level_2,
    level_3,
    level_4,
    level_5,
    level_6,
    level_7,
    level_8,
    family,
    genus,
    species,
    caab_code,
    count
  ) %>%
  mutate(campaignid = "2014-12_Geographe-bay_stereo-BRUVs") %>%
  semi_join(metadata)

 write_csv(relief_summary, "data/to upload/2014-12_Geographe.Bay_stereo-BRUVs_benthos-relief.csv")

relief_samples <- relief_summary %>% 
  distinct(campaignid, opcode) %>%
  group_by(campaignid) %>%
  summarise(relief_sample = n(), .groups = "drop")

benthos_samples <- benthos_summary %>%
  distinct(campaignid, opcode) %>%
  group_by(campaignid) %>%
  summarise(benthos_sample = n(), .groups = "drop")

sample_summary <- full_join(
  relief_samples,
  benthos_samples,
  by = c("campaignid")
)

# metadata <- metadata %>%
#   rename(opcode = sample)

habitat.missing.metadata <- anti_join(benthos_summary, metadata, by = c("opcode")) %>%
  glimpse()

metadata.missing.habitat <- anti_join(metadata, benthos_summary, by = c("opcode")) %>%
  glimpse()

relief.missing.metadata <- anti_join(relief_summary, metadata, by = c("opcode")) %>%
  glimpse()

metadata.missing.relief <- anti_join(metadata, relief_summary, by = c("opcode")) %>%
  glimpse()

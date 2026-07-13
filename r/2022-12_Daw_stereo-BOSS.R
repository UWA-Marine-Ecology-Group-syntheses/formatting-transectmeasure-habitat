library(dplyr)
library(tidyr)
library(readr)
library(CheckEM)
library(googlesheets4)
library(stringr)
library(janitor)

#----------------------------
# CATAMI schema
#----------------------------

schema <- CheckEM::catami %>%
  mutate(caab_code = as.numeric(caab_code)) %>%
  select(-qualifiers)

schema_lookup <- schema %>%
  arrange(caab_code) %>%
  distinct(
    level_1,
    level_2,
    level_3,
    level_4,
    level_5,
    .keep_all = TRUE
  )

#----------------------------
# Metadata
#----------------------------

metadata <- read.csv(
  here::here("data/uploads/Eastern-Recherche-Marine-Park-BOSS_metadata.csv"),
  stringsAsFactors = FALSE
) %>%
  clean_names()

#----------------------------
# TransectMeasure habitat file
#----------------------------

panoramic <- read.delim(
  "data/raw/2022-12_Daw_stereo-BOSS_Dot Point Measurements.txt",
  header = TRUE,
  skip = 4,
  stringsAsFactors = FALSE,
  colClasses = "character",
  na.strings = ""
) %>%
  clean_names() %>%
  filter(filename != "IO333.jpg") %>%
  mutate(
    filename = str_replace_all(filename, "take 2", "")
  )

#----------------------------
# Convert TransectMeasure hierarchy
# to CATAMI hierarchy
#----------------------------

panoramic_clean <- panoramic %>%
  separate(
    level_2,
    into = c("level_1","level_2"),
    sep = " > ",
    fill = "right",
    extra = "merge"
  ) %>%
  mutate(
    
    is_biota = level_1 %in% c(
      "Bryozoa",
      "Cnidaria",
      "Macroalgae",
      "Seagrasses",
      "Sponges"
    ),
    
    is_substrate = level_1 == "Substrate"
    
  ) %>%
  
  mutate(
    
    level_5 = case_when(
      is_biota ~ level_4,
      is_substrate ~ level_4,
      TRUE ~ level_5
    ),
    
    level_4 = case_when(
      is_biota ~ level_3,
      is_substrate ~ level_3,
      TRUE ~ level_4
    ),
    
    level_3 = case_when(
      is_biota ~ level_2,
      is_substrate ~ level_2,
      TRUE ~ level_3
    ),
    
    level_2 = case_when(
      is_biota ~ level_1,
      is_substrate ~ "Substrate",
      TRUE ~ level_2
    ),
    
    level_1 = case_when(
      is_biota ~ "Biota",
      is_substrate ~ "Physical",
      TRUE ~ level_1
    )
    
  ) %>%
  select(-is_biota, -is_substrate)

#----------------------------
# Add CATAMI information
#----------------------------

#----------------------------
# Add CATAMI information
#----------------------------

habitat_with_schema <- panoramic_clean %>%
  select(-scientific, -qualifiers) %>%
  left_join(
    schema_lookup,
    by = c(
      "level_1",
      "level_2",
      "level_3",
      "level_4",
      "level_5"
    )
  ) %>%
  
  # Populate taxonomy for species-level entries that don't match the hierarchy
  mutate(
    
    family = case_when(
      level_5 == "Ecklonia radiata" ~ "Lessoniaceae",
      level_5 == "Scytothalia dorycarpa" ~ "Seirococcaceae",
      level_4 == "Posidonia sp. (Caab 63600903)" ~ "Posidoniaceae",
      level_4 == "Zostera sp. (Caab 63600903)" ~ "Zosteraceae",
      TRUE ~ family
    ),
    
    genus = case_when(
      level_5 == "Ecklonia radiata" ~ "Ecklonia",
      level_5 == "Scytothalia dorycarpa" ~ "Scytothalia",
      level_4 == "Posidonia sp. (Caab 63600903)" ~ "Posidonia",
      level_4 == "Zostera sp. (Caab 63600903)" ~ "Zostera",
      TRUE ~ genus
    ),
    
    species = case_when(
      level_5 == "Ecklonia radiata" ~ "radiata",
      level_5 == "Scytothalia dorycarpa" ~ "dorycarpa",
      level_4 == "Posidonia sp. (Caab 63600903)" ~ "spp",
      level_4 == "Zostera sp. (Caab 63600903)" ~ "spp",
      TRUE ~ species
    )
  )

#----------------------------
# Fill missing CAAB codes using taxonomy
#----------------------------

taxonomy_lookup <- schema %>%
  filter(!is.na(genus)) %>%
  select(caab_code, family, genus, species)

habitat_with_schema_clean <- habitat_with_schema %>%
  left_join(
    taxonomy_lookup,
    by = c("family", "genus", "species"),
    suffix = c("", "_tax")
  ) %>%
  mutate(
    caab_code = coalesce(caab_code, caab_code_tax)
  ) %>%
  select(-caab_code_tax)
#----------------------------
# Check unmatched habitat types
#----------------------------

missing_caab_code <- habitat_with_schema_clean %>%
  filter(is.na(caab_code)) %>%
  distinct(
    level_1,
    level_2,
    level_3,
    level_4,
    level_5
  )

print(missing_caab_code)

#----------------------------
# Check annotation counts
#----------------------------

num.points <- 80

wrong_points_habitat <- habitat_with_schema_clean %>%
  group_by(period) %>%
  summarise(points.annotated = n(), .groups = "drop") %>%
  left_join(metadata, by = "period")

#----------------------------
# Metadata missing habitat
#----------------------------

habitat.missing.metadata <- anti_join(
  habitat_with_schema_clean,
  metadata,
  by = "period"
)

#----------------------------
# Create upload table
#----------------------------

tidy_habitat <- habitat_with_schema_clean %>%
  mutate(
    period = str_trim(period),
    number = 1,
    campaignid = "2022-12_Daw_stereo-BOSS"
  ) %>%
  filter(
    !level_2 %in% c("", "Unscorable", NA),
    !level_1 %in% c("Matrix", "Unscorable")
  ) %>%
  select(
    campaignid,
    period,
    number,
    starts_with("level"),
    family,
    genus,
    species,
    caab_code
  ) %>%
  group_by(
    campaignid,
    period,
    across(starts_with("level")),
    family,
    genus,
    species,
    caab_code
  ) %>%
  tally(number, name = "count") %>%
  ungroup()

#----------------------------
# Check for metadata records
# with no habitat annotations
#----------------------------

metadata.missing.habitat <- anti_join(
  metadata %>%
    filter(
      successful_count == "Yes" |
        successful_length == "Yes"
    ),
  tidy_habitat,
  by = c("campaignid", "period")
)

#----------------------------
# Export
#----------------------------

write_csv(
  tidy_habitat,
  "data/uploads/2022-12_Daw_stereo-BOSS_benthos-count.csv"
)

names(habitat_with_schema_clean)

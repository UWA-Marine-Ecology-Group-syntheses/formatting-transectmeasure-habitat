
### Secure access to EventMeasure or generic stereo-video annotations from Campaigns, Projects and Collaborations within GlobalArchive

### OBJECTIVES ###
# 1. use an API token to access Projects and Collaborations shared with you.
# 2. securely download any number of Campaigns within a Workgroup 
# 3. combine multiple Campaigns into single Metadata, MaxN and Length files for subsequent validation and data analysis.

### Please forward any updates and improvements to tim.langlois@uwa.edu.au & brooke.gibbons@uwa.edu.au or raise an issue in the "globalarchive-query" GitHub repository

rm(list=ls()) # Clear memory

## Load Libraries ----
# To connect to GlobalArchive
library(devtools)
install_github("UWAMEGFisheries/GlobalArchive", dependencies = TRUE) # to check for updates
library(GlobalArchive)
library(httr)
library(jsonlite)
library(R.utils)
# To connect to GitHub
library(RCurl)
# To tidy data
library(plyr)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(stringr)
library(CheckEM)
library(googlesheets4)

ga.read.files_csv <- function(flnm) {
  read_csv(flnm,col_types = cols(.default = "c"))%>%
    ga.clean.names() %>%
    dplyr::select(-c(any_of(c("campaignid")))) %>%
    dplyr::mutate(campaign.naming=str_replace_all(flnm,paste(download.dir,"/",sep=""),""))%>%
    tidyr::separate(campaign.naming,into=c("project","campaignid"),sep="/", extra = "drop", fill = "right")%>%
    plyr::rename(., replace = c(opcode="sample"),warn_missing = FALSE)
}

## Set Study Name ----
# Change this to suit your study name. This will also be the prefix on your final saved files.
study <- "australia-synthesis_new" 

## Set your working directory ----
working.dir <- "data/GA" # to directory of current file - or type your own

## Save these directory names to use later----
to.be.checked.dir<-paste(working.dir,"Data to be checked",sep="/") 
download.dir<-paste(working.dir,"Downloads",sep="/")
tidy.dir<-paste(working.dir,"Tidy data",sep="/")

## Query from GlobalArchive----
# Load default values from GlobalArchive ----
source("https://raw.githubusercontent.com/UWAMEGFisheries/GlobalArchive/master/values.R")

# An API token allows R to communicate with GlobalArchive
# Add your personal API user token ----
API_USER_TOKEN <- "993ba5c4267b9f8cd21de73b0434c95bc72f518a4f6e725226986022"

## Download data ----
# takes 21 minutes to run - turn on again to refresh the data
# ga.get.campaign.list(API_USER_TOKEN, process_campaign_object,
#                      q = ga.query.workgroup("2024+Australian+BRUV+synthesis"))

# Combine all downloaded data----
# Your data is now downloaded into many folders within the 'Downloads' folder. (You can open File Explorer or use the Files Pane to check)
# The below code will go into each of these folders and find all files that have the same ending (e.g. "_Metadata.csv") and bind them together.
# The end product is three data frames; metadata, maxn and length.

metadata <- ga.list.files("_Metadata.csv") %>% # list all files ending in "_Metadata.csv"
  purrr::map_df(~ga.read.files_csv(.)) %>% # combine into dataframe
  dplyr::select(project, campaignid, sample, latitude, longitude, date, time, location, status, site, depth, observer, successful.count, successful.length, comment) %>% # This line ONLY keep the 15 columns listed. Remove or turn this line off to keep all columns (Turn off with a # at the front).
  glimpse()

unique(metadata$project) %>% sort() # 42 projects - correct.
unique(metadata$campaignid)  %>% sort() # 75 campaigns - should be 75

write.csv(metadata, paste0("data/GA/Data to be checked/", study, "_metadata.csv"), row.names = FALSE)
saveRDS(metadata, paste0("data/GA/Data to be checked/", study, "_metadata.RDS"))

# Start cleaning habitat data manually -----
dot_point_measurements <- ga.list.files("_Dot Point Measurements.txt") %>% # list all files ending in ""_Dot Point Measurements"
  purrr::map_df(~ga.read.files_txt(.))

unique(dot_point_measurements$campaignid)

jac_rosetta_hab <- read_sheet("https://docs.google.com/spreadsheets/d/1k8OMwKaQiGRikxk8fiTAGkENOwEY8iOXTLIMZcqvHqc/edit?gid=1082941936#gid=1082941936",
                              sheet = "jac_hab")

b_m_t_hab <- read_sheet("https://docs.google.com/spreadsheets/d/1k8OMwKaQiGRikxk8fiTAGkENOwEY8iOXTLIMZcqvHqc/edit?gid=1082941936#gid=1082941936",
                              sheet = "b_m_t")

jac_rosetta_relief <- read_sheet("https://docs.google.com/spreadsheets/d/1k8OMwKaQiGRikxk8fiTAGkENOwEY8iOXTLIMZcqvHqc/edit?gid=1082941936#gid=1082941936",
                              sheet = "jac_relief")

relief_sch <- jac_rosetta_relief %>%
  dplyr::rename(relief = catami_l5)

# 2013-05_Aidans_Hons_pilchards_stereoBRUVs ----

aidans_hons_metadata <- metadata %>%
  dplyr::filter(campaignid %in% c("2013-05_Aidans_Hons_pilchards_stereoBRUVs")) 

aidans_hons_hab <- dot_point_measurements %>%
  dplyr::filter(campaignid %in% c("2013-05_Aidans_Hons_pilchards_stereoBRUVs")) %>%
  CheckEM::clean_names() %>%
  dplyr::select(campaignid, filename, # TODO need to sort these out 
                image_row, image_col, 
                starts_with("catami")) %>%
  dplyr::mutate(id = 1:nrow(.)) %>%
  left_join(jac_rosetta_hab) %>%
  dplyr::select(campaignid, filename, id, 
                starts_with("level"), caab_code) %>%
  dplyr::mutate(number = 1) %>% # Add a count column to summarise the number of points
  dplyr::filter(!level_2 %in% c("", "Unscorable", NA)) %>%  
  dplyr::select(campaignid, filename, number, caab_code) %>% # family, genus, species, 
  dplyr::group_by(campaignid, filename, caab_code) %>% #family, genus, species, 
  dplyr::tally(number, name = "number") %>%
  dplyr::ungroup() %>%
  dplyr::left_join(CheckEM::catami) %>%
  dplyr::select(-qualifiers) %>%
  dplyr::mutate(sample = str_replace_all(filename, c("_left" = "",
                                                     "_Left" = "",
                                                     ".avi" = "",
                                                     " \\(2\\)" = "",
                                                     "_right" = "",
                                                     "_Right" = "",
                                                     "Inside_2_25_Pilchards" = "INSIDE_2_25_PILCHARDS",
                                                     "Outside_1_5_Pilchards" = "Outside_2_15_Pilchards"))) %>%
  dplyr::select(-filename) %>%
  dplyr::select(campaignid, sample, everything()) %>%
  glimpse()

write_csv(aidans_hons_hab, "data/to upload/2013-05_Aidans_Hons_pilchards_stereoBRUVs_benthos.csv")


unique(aidans_hons_hab$sample)

missing_metadata <- anti_join(aidans_hons_hab, aidans_hons_metadata)
# There was one missing habitat and one missing metadata - assume these are meant to be the same thing

missing_habitat <- anti_join(aidans_hons_metadata, aidans_hons_hab)

# distinct_classes <- aidans_hons_hab %>%
#   select(starts_with("catami")) %>%
#   dplyr::select(-c(catami_l4, catami_l6, catami_sp_code, catami_l5)) %>%
#   distinct() %>%
#   dplyr::glimpse()
# 
# write_csv(distinct_classes, "Jacs_classes.csv")
# write_csv(schema, "uwa-classes.csv")
# 
# unique(aidans_hons_hab$catami_l1)
# 
# schema <- CheckEM::catami
# unique(schema$level_2)

aidans_hons_relief <- dot_point_measurements %>%
  dplyr::filter(campaignid %in% c("2013-05_Aidans_Hons_pilchards_stereoBRUVs")) %>%
  CheckEM::clean_names() %>%
  dplyr::select(campaignid, filename, # TODO need to sort these out 
                image_row, image_col, 
                catami_l5) %>%
  dplyr::mutate(id = 1:nrow(.)) %>%
  left_join(jac_rosetta_relief) %>%
  dplyr::select(campaignid, filename, id, 
                starts_with("level"), caab_code) %>%
  dplyr::mutate(number = 1) %>% # Add a count column to summarise the number of points
  dplyr::filter(!level_2 %in% c("", "Unscorable", NA)) %>%  
  dplyr::select(campaignid, filename, number, caab_code) %>% # family, genus, species, 
  dplyr::group_by(campaignid, filename, caab_code) %>% #family, genus, species, 
  dplyr::tally(number, name = "number") %>%
  dplyr::ungroup() %>%
  dplyr::left_join(CheckEM::catami) %>%
  dplyr::select(-qualifiers) %>%
  dplyr::mutate(sample = str_replace_all(filename, c("_left" = "",
                                                     "_Left" = "",
                                                     ".avi" = "",
                                                     " \\(2\\)" = "",
                                                     "_right" = "",
                                                     "_Right" = "",
                                                     "Inside_2_25_Pilchards" = "INSIDE_2_25_PILCHARDS",
                                                     "Outside_1_5_Pilchards" = "Outside_2_15_Pilchards"))) %>%
  dplyr::select(-filename) %>%
  dplyr::select(campaignid, sample, everything()) %>%
  glimpse()

# relief_classes <- aidans_hons_relief %>%
#   distinct(catami_l5)
# 
# write_csv(relief_classes, "Jacs_relief_classes.csv")

write_csv(aidans_hons_relief, "data/to upload/2013-05_Aidans_Hons_pilchards_stereoBRUVs_relief.csv")


# 2012-08_Flinders_CMR_BRUV -----
flinders_cmr_metadata <- metadata %>%
  dplyr::filter(campaignid %in% c("2012-08_Flinders_CMR_BRUV")) 

flinders_cmr_hab <- dot_point_measurements %>%
  dplyr::filter(campaignid %in% c("2012-08_Flinders_CMR_BRUV")) %>%
  CheckEM::clean_names() %>%
  dplyr::select(campaignid, filename, # TODO need to sort these out 
                image_row, image_col, 
                starts_with("catami")) %>%
  dplyr::mutate(id = 1:nrow(.)) %>%
  left_join(jac_rosetta_hab) 

# missing_classes <- flinders_cmr_hab %>%
#   dplyr::filter(is.na(level_1)) %>%
#   dplyr::distinct(catami_l1, catami_l2, catami_l3)
# 
# write_csv(missing_classes, "missing_classes.csv")

flinders_cmr_hab <- flinders_cmr_hab %>%
  dplyr::select(campaignid, filename, id, 
                starts_with("level"), caab_code) %>%
  dplyr::mutate(number = 1) %>% # Add a count column to summarise the number of points
  dplyr::filter(!level_2 %in% c("", "Unscorable", NA)) %>%  
  dplyr::select(campaignid, filename, number, caab_code) %>% # family, genus, species, 
  dplyr::group_by(campaignid, filename, caab_code) %>% #family, genus, species, 
  dplyr::tally(number, name = "number") %>%
  dplyr::ungroup() %>%
  dplyr::left_join(CheckEM::catami) %>%
  dplyr::select(-qualifiers) %>%
  dplyr::mutate(sample = str_replace_all(filename, c(".avi" = ""))) %>%
  dplyr::select(-filename) %>%
  dplyr::select(campaignid, sample, everything()) %>%
  glimpse()

unique(flinders_cmr_hab$sample)
missing_metadata <- anti_join(flinders_cmr_hab, flinders_cmr_metadata)
missing_habitat <- anti_join(flinders_cmr_metadata, flinders_cmr_hab)

write_csv(flinders_cmr_hab, "data/to upload/2012-08_Flinders_CMR_BRUV_benthos.csv") # TODO fix the opcodes in this

flinders_cmr_relief <- dot_point_measurements %>%
  dplyr::filter(campaignid %in% c("2012-08_Flinders_CMR_BRUV")) %>%
  CheckEM::clean_names() %>%
  dplyr::select(campaignid, filename, # TODO need to sort these out 
                image_row, image_col, 
                catami_l5) %>%
  dplyr::mutate(id = 1:nrow(.)) %>%
  left_join(jac_rosetta_relief) %>%
  dplyr::select(campaignid, filename, id, 
                starts_with("level"), caab_code) %>%
  dplyr::mutate(number = 1) %>% # Add a count column to summarise the number of points
  dplyr::filter(!level_5 %in% c("", "Unscorable", NA)) %>%  
  dplyr::select(campaignid, filename, number, caab_code) %>% # family, genus, species, 
  dplyr::group_by(campaignid, filename, caab_code) %>% #family, genus, species, 
  dplyr::tally(number, name = "number") %>%
  dplyr::ungroup() %>%
  dplyr::left_join(CheckEM::catami) %>%
  dplyr::select(-qualifiers) %>%
  dplyr::mutate(sample = str_replace_all(filename, c(".avi" = ""))) %>%
  dplyr::select(-filename) %>%
  dplyr::select(campaignid, sample, everything()) %>%
  glimpse()

# relief_classes <- flinders_cmr_relief %>%
#   distinct(catami_l5)
# 
# write_csv(relief_classes, "Jacs_relief_classes.csv")

write_csv(flinders_cmr_relief, "data/to upload/2012-08_Flinders_CMR_BRUV_relief.csv") # TODO fix the opcodes in this


# 2014-04_Tasman_Fracture_CMR_stereoBRUVs ----
tas_frac_metadata <- metadata %>%
  dplyr::filter(campaignid %in% c("2014-04_Tasman_Fracture_CMR_stereoBRUVs")) 

tas_frac_hab <- dot_point_measurements %>%
  dplyr::filter(campaignid %in% c("2014-04_Tasman_Fracture_CMR_stereoBRUVs")) %>%
  CheckEM::clean_names() %>%
  dplyr::select(campaignid, filename, # TODO need to sort these out 
                image_row, image_col, 
                starts_with("catami")) %>%
  dplyr::mutate(id = 1:nrow(.)) %>%
  left_join(jac_rosetta_hab) %>%

# missing_classes <- tas_frac_hab %>%
#   dplyr::filter(is.na(level_1)) %>%
#   dplyr::distinct(catami_l1, catami_l2, catami_l3)

# write_csv(missing_classes, "missing_classes.csv")

# tas_frac_hab <- tas_frac_hab %>%
  dplyr::select(campaignid, filename, id, 
                starts_with("level"), caab_code) %>%
  dplyr::mutate(number = 1) %>% # Add a count column to summarise the number of points
  dplyr::filter(!level_2 %in% c("", "Unscorable", NA)) %>%  
  dplyr::select(campaignid, filename, number, caab_code) %>% # family, genus, species, 
  dplyr::group_by(campaignid, filename, caab_code) %>% #family, genus, species, 
  dplyr::tally(number, name = "number") %>%
  dplyr::ungroup() %>%
  dplyr::left_join(CheckEM::catami) %>%
  dplyr::select(-qualifiers) %>%
  mutate(sample = sub(".*_", "", filename)) %>%
  dplyr::mutate(sample = str_replace_all(sample, c(".avi" = ""))) %>%
  mutate(sample = gsub("^0+", "", sample)) %>%
  dplyr::select(-filename) %>%
  dplyr::select(campaignid, sample, everything()) %>%
  glimpse()

unique(tas_frac_hab$sample)
missing_metadata <- anti_join(tas_frac_hab, tas_frac_metadata)
missing_habitat <- anti_join(tas_frac_metadata, tas_frac_hab)

write_csv(tas_frac_hab, "data/to upload/2014-04_Tasman_Fracture_CMR_stereoBRUVs_benthos.csv") # TODO fix the opcodes in this

tas_frac_relief <- dot_point_measurements %>%
  dplyr::filter(campaignid %in% c("2014-04_Tasman_Fracture_CMR_stereoBRUVs")) %>%
  CheckEM::clean_names() %>%
  dplyr::select(campaignid, filename, # TODO need to sort these out 
                image_row, image_col, 
                catami_l5) %>%
  dplyr::mutate(id = 1:nrow(.)) %>%
  left_join(jac_rosetta_relief) %>%
  dplyr::select(campaignid, filename, id, 
                starts_with("level"), caab_code) %>%
  dplyr::mutate(number = 1) %>% # Add a count column to summarise the number of points
  dplyr::filter(!level_5 %in% c("", "Unscorable", NA)) %>%  
  dplyr::select(campaignid, filename, number, caab_code) %>% # family, genus, species, 
  dplyr::group_by(campaignid, filename, caab_code) %>% #family, genus, species, 
  dplyr::tally(number, name = "number") %>%
  dplyr::ungroup() %>%
  dplyr::left_join(CheckEM::catami) %>%
  dplyr::select(-qualifiers) %>%
  mutate(sample = sub(".*_", "", filename)) %>%
  dplyr::mutate(sample = str_replace_all(sample, c(".avi" = ""))) %>%
  mutate(sample = gsub("^0+", "", sample)) %>%
  dplyr::select(-filename) %>%
  dplyr::select(campaignid, sample, everything()) %>%
  glimpse()

# relief_classes <- tas_frac_relief %>%
#   distinct(catami_l5)
# 
# write_csv(relief_classes, "Jacs_relief_classes.csv")

write_csv(tas_frac_relief, "data/to upload/2014-04_Tasman_Fracture_CMR_stereoBRUVs_relief.csv") # TODO fix the opcodes in this

# Tasman_Fracture_202103 ----
tas_frac_metadata1 <- metadata %>%
  dplyr::filter(campaignid %in% c("Tasman_Fracture_202103")) 

names(tas_frac_hab1)

tas_frac_hab1 <- dot_point_measurements %>%
  dplyr::filter(campaignid %in% c("Tasman_Fracture_202103")) %>%
  CheckEM::clean_names() %>%
  select(where(~ !all(is.na(.)))) %>%
  dplyr::select(campaignid, filename, 
                image_row, image_col, 
                broad, morphology, type) %>%
  dplyr::mutate(id = 1:nrow(.)) %>%

# new_classes <- tas_frac_hab1 %>%
#   distinct(broad, morphology, type)
# 
# write_csv(new_classes, "missing_classes.csv")

  left_join(b_m_t_hab) %>%
  dplyr::select(campaignid, filename, id, 
                starts_with("level"), caab_code) %>%
  dplyr::mutate(number = 1) %>% # Add a count column to summarise the number of points
  dplyr::filter(!level_2 %in% c("", "Unscorable", NA)) %>%  
  dplyr::select(campaignid, filename, number, caab_code) %>% # family, genus, species, 
  dplyr::group_by(campaignid, filename, caab_code) %>% #family, genus, species, 
  dplyr::tally(number, name = "number") %>%
  dplyr::ungroup() %>%
  dplyr::left_join(CheckEM::catami) %>%
  dplyr::select(-qualifiers) %>%
  dplyr::mutate(sample = str_replace_all(filename, c(".png" = ""))) %>%
  mutate(sample = str_extract(sample, "(?<=_)[0-9]+(?=_)")) %>%
  # mutate(sample = sub(".*_", "", filename)) %>%

  # mutate(sample = gsub("^0+", "", sample)) %>%
  # dplyr::select(-filename) %>%
  # dplyr::select(campaignid, sample, everything()) %>%
  glimpse()

unique(tas_frac_hab1$sample)
missing_metadata <- anti_join(tas_frac_hab1, tas_frac_metadata1)
missing_habitat <- anti_join(tas_frac_metadata1, tas_frac_hab1)

write_csv(tas_frac_hab1, "data/to upload/Tasman_Fracture_202103_benthos.csv") # TODO fix the opcodes in this

tas_frac_relief1 <- dot_point_measurements %>%
  dplyr::filter(campaignid %in% c("Tasman_Fracture_202103")) %>%
  CheckEM::clean_names() %>%
  select(where(~ !all(is.na(.)))) %>%
  dplyr::mutate(id = 1:nrow(.)) %>%
  left_join(relief_sch) %>%
  dplyr::select(campaignid, filename, id, 
                starts_with("level"), caab_code) %>%
  dplyr::mutate(number = 1) %>% # Add a count column to summarise the number of points
  dplyr::filter(!level_5 %in% c("", "Unscorable", NA)) %>%  
  dplyr::select(campaignid, filename, number, caab_code) %>% # family, genus, species, 
  dplyr::group_by(campaignid, filename, caab_code) %>% #family, genus, species, 
  dplyr::tally(number, name = "number") %>%
  dplyr::ungroup() %>%
  dplyr::left_join(CheckEM::catami) %>%
  dplyr::select(-qualifiers) %>%
  dplyr::mutate(sample = str_replace_all(filename, c(".png" = ""))) %>%
  mutate(sample = str_extract(sample, "(?<=_)[0-9]+(?=_)")) %>%
  dplyr::select(-filename) %>%
  dplyr::select(campaignid, sample, everything()) %>%
  glimpse()

# relief_classes <- tas_frac_relief1 %>%
#   distinct(catami_l5)
# 
# write_csv(relief_classes, "Jacs_relief_classes.csv")

write_csv(tas_frac_relief1, "data/to upload/Tasman_Fracture_202103_relief.csv") # TODO fix the opcodes in this

# Start of Tim's campaigns ----
# 2020-06_south-west_stereo-BRUVs
swc_2020_06_metadata <- metadata %>%
  dplyr::filter(campaignid %in% c("2020-06_south-west_stereo-BRUVs")) 

names(swc_2020_06_metadata)

swc_2020_06_hab <- dot_point_measurements %>%
  dplyr::filter(campaignid %in% c("2020-06_south-west_stereo-BRUVs")) %>%
  CheckEM::clean_names() %>%
  select(where(~ !all(is.na(.)))) %>%
  dplyr::select(campaignid, filename, 
                image_row, image_col, 
                broad, morphology, type) %>%
  dplyr::mutate(id = 1:nrow(.)) 

# Ky has done something weird in here for relief?
# Come back on a day that isn't a Friday

# same has happened with 2020-10_south-west_stereo-BRUVs
unique(dot_point_measurements$campaignid)

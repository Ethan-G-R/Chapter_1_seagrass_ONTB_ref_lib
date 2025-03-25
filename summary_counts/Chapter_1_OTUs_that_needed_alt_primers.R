# 22.11.24
# Ethan Ross

# In this script I want to know how many OTUs required COI 
# degen or tax specific primers

library(writexl)
library(readxl)
library(dplyr)
library(worms)
library(ggplot2)
library(stringr)
library(gridExtra)
library(tidyr)

All_barcode_data_by_OTU <- read_xlsx("summary_counts/Chapter_1_summary_condenced.xlsx", sheet = 3)

names(All_barcode_data_by_OTU)

# I need to remove all the records where COI PCR failed
All_barcode_data_by_OTU <- All_barcode_data_by_OTU %>% 
  filter(!is.na(stage_COI)) %>%
  filter(Contamination_COI == "N") %>%
  select(FINAL_ID, SpecimenID, Contamination_COI)

total_COI <- All_barcode_data_by_OTU %>% 
  distinct(FINAL_ID) %>% nrow()

#y <- All_barcode_data_by_OTU %>% 
#  distinct(FINAL_ID)
#write_xlsx(y, "unique_COI_OTUs_condenced.xlsx")

str(All_barcode_data_by_OTU)

# Filter for SpecimenIDs that end in "jg", "ech", or "poly"
matching_ids <- All_barcode_data_by_OTU %>%
  filter(grepl("jg$|ech$|poly$", SpecimenID)) %>%
  distinct(FINAL_ID)

# Find FINAL_IDs with no matching SpecimenIDs
no_matching_ids <- All_barcode_data_by_OTU %>%
  filter(!(FINAL_ID %in% matching_ids$FINAL_ID)) %>%
  distinct(FINAL_ID)

# Count the unique FINAL_IDs
no_matching_count <- nrow(no_matching_ids)

# 103 OTUs did not have any degen / tax specific primer PCRs
no_matching_count

# 29 OTUs did have degen / tax specifc primer PCRs
total_COI - no_matching_count

# What about OTUs that needed the degen / tax specfic primers?


# Filter FINAL_IDs where no SpecimenID violates the condition
exclusive_ids <- All_barcode_data_by_OTU %>%
  group_by(FINAL_ID) %>%
  filter(all(grepl("jg$|ech$|poly$", SpecimenID) & !is.na(SpecimenID))) %>%
  distinct(FINAL_ID)

# Count the unique FINAL_IDs
exclusive_count <- nrow(exclusive_ids)

# 23 of the OTUs exclusively had sequences using degen / tax specific primers
exclusive_count

# so 109 OTUs did not require and degen / tax specific primers
total_COI - exclusive_count






# 21.10.24
# Ethan Ross

# Okay in this script I will get all my chapter 2 summary data in one place


library(writexl)
library(readxl)
library(dplyr)
library(worms)
library(ggplot2)

All_barcode_data <- read_excel("summary_counts/Chapter_1_summary_condenced.xlsx", sheet = 1)

names(All_barcode_data)

# How many unique specimens are in my dataset? 316 (335 with full dataset)

All_barcode_data %>%
  distinct(SpecimenID_OG) %>% nrow()

# How many unique OTUs are in my dataset? 150

All_barcode_data %>% distinct(FINAL_ID) %>% nrow()

################################################################################

# How many OTUs did I recover COI DNA barcodes for? 132 

All_barcode_data %>% filter(Contamination_COI == "N") %>%
  distinct(FINAL_ID) %>% nrow()

#x <- All_barcode_data %>% filter(Contamination_COI == "N") %>%
#  distinct(FINAL_ID)

#write_xlsx(x, "unique_COI_OTUs.xlsx")

# What percentage of OTUs were COI DNA barcodes recovered for? 88%
All_barcode_data %>% filter(Contamination_COI == "N") %>%
  distinct(FINAL_ID) %>% nrow() / 150 * 100

# How many OTUs did I recover 18S DNA barcodes for? 131

All_barcode_data %>% filter(Contamination_18S == "N") %>%
  distinct(FINAL_ID) %>% nrow()

# What percentage of OTUs were 18S DNA barcodes recovered for? 88%
All_barcode_data %>% filter(Contamination_18S == "N") %>%
  distinct(FINAL_ID) %>% nrow() / 150 * 100


############################### Side Quest #####################################
# Here I want to know which specimens I have that did get recovered from 
# either the COI or 18S runs
# All_barcode_data <- read_excel("../Chapter_1_summary.xlsx", sheet = 1)

# x <- All_barcode_data %>% filter(is.na(RUN_COI)) %>% filter(is.na(RUN_18S))

# write_xlsx(x, "Specimens_with_no_recovered_barcodes.xlsx")

# Quest completed 22.10.24 - I created a subset of my data that only contains
# the specimens which I attempted to recover DNA barcodes from)
################################################################################

# Taxonomic summary

names(All_barcode_data)
All_barcode_data %>% distinct(phylum)
All_barcode_data %>% distinct(phylum) %>% nrow()
All_barcode_data %>% distinct(class) %>% nrow()
All_barcode_data %>% distinct(order) %>% nrow()
All_barcode_data %>% distinct(family) %>% nrow()
All_barcode_data %>% distinct(genus) %>% nrow()
All_barcode_data %>% filter(rank == "Species") %>% distinct(FINAL_ID) %>% nrow()

#install.packages("ggalluvial")

library(ggalluvial)
library(ggplot2)

################################################################################

# Create a distinct sorted dataframe
All_barcode_tax_data <- All_barcode_data %>% 
  select(FINAL_ID, Final_rank, phylum, class, order, family, genus) %>%
  distinct()

str(All_barcode_tax_data)


# Add the species column based on the condition
All_barcode_tax_data <- All_barcode_tax_data %>%
  mutate(species = ifelse(Final_rank == "Species", FINAL_ID, NA))

str(All_barcode_tax_data)


################################# Specimens ####################################


# How many specimens did I recover COI DNA barcodes for? 268 

All_barcode_data %>% filter(Contamination_COI == "N") %>%
  distinct(SpecimenID_OG) %>% nrow()

# What percentage of specimens were COI DNA barcodes recovered for? 84.8%
All_barcode_data %>% filter(Contamination_COI == "N") %>%
  distinct(SpecimenID_OG) %>% nrow() / 316 * 100

# How many specimens did I recover 18S DNA barcodes for? 209 

All_barcode_data %>% filter(Contamination_18S == "N") %>%
  distinct(SpecimenID_OG) %>% nrow()

# What percentage of specimens were COI DNA barcodes recovered for? 93.3%
All_barcode_data %>% filter(Contamination_18S == "N") %>%
  distinct(SpecimenID_OG) %>% nrow() / 224 * 100

############################# Ranks by method counts ###########################
library(stringr)

All_barcode_data <- All_barcode_data %>%
  mutate(Molecular_ID_COI = str_replace_all(Molecular_ID_COI, "[\r\n]", ""))

All_barcode_data <- All_barcode_data %>%
  mutate(Molecular_ID_18S = str_replace_all(Molecular_ID_18S, "[\r\n]", ""))

All_barcode_data <- All_barcode_data %>%
  mutate(Morphological_ID = str_replace_all(Morphological_ID, "[\r\n]", ""))


############################# Ranks by method counts v2 ########################

All_barcode_data_per_OTU <- read_excel("summary_counts/Chapter_1_summary_condenced.xlsx", sheet = 4)

str(All_barcode_data_per_OTU)

All_barcode_data_per_OTU %>%
  filter(Contamination_COI == "N") %>%
  group_by(Rank_COI) %>%
  dplyr::summarise(count = n())

All_barcode_data_per_OTU %>%
  filter(Contamination_18S == "N") %>%
  group_by(Rank_18S) %>%
  dplyr::summarise(count = n())


All_barcode_data_per_OTU %>%
  group_by(Rank_Morph) %>%
  dplyr::summarise(count = n())


############################## Congruence ######################################
names(All_barcode_data)

# 133
All_barcode_data_per_OTU %>%
  filter(Contamination_COI == "N") %>%
  filter(!is.na(Congruence_COI_morph)) %>% nrow()

# 101 agreed
All_barcode_data_per_OTU %>%
  filter(Contamination_COI == "N") %>%
  filter(Congruence_COI_morph == "Y") %>% nrow()

All_barcode_data_per_OTU %>%
  filter(Contamination_COI == "N") %>%
  filter(Congruence_COI_morph == "N") %>%
  select(order)

# 132
All_barcode_data_per_OTU %>%
  filter(Contamination_18S == "N") %>%
  filter(!is.na(Congruence_18S_morph)) %>% nrow()

# 108 agreed
All_barcode_data_per_OTU %>%
  filter(Contamination_18S == "N") %>%
  filter(Congruence_18S_morph == "Y") %>% nrow()


# 119
All_barcode_data_per_OTU %>%
  filter((Contamination_18S == "N" | is.na(Contamination_18S)) &
           (Contamination_COI == "N" | is.na(Contamination_COI))) %>%
  filter(!is.na(Congruence_18S_COI)) %>% nrow()

# 116 agreed
All_barcode_data_per_OTU %>%
  filter((Contamination_18S == "N" | is.na(Contamination_18S)) &
           (Contamination_COI == "N" | is.na(Contamination_COI))) %>%
  filter(Congruence_18S_COI == "Y") %>% nrow()

All_barcode_data_per_OTU %>%
  filter((Contamination_18S == "N" | is.na(Contamination_18S)) &
           (Contamination_COI == "N" | is.na(Contamination_COI))) %>%
  filter(Congruence_18S_COI == "N")

str(All_barcode_data_per_OTU_congruence)


#################### How many morphological species IDs were revoked? ##########

# For 44 OTUs, the FINAL_ID was the same as the species level morphological ID
All_barcode_data_per_OTU %>%
  filter(Rank_Morph == "Species") %>%
  filter(Morphological_ID == FINAL_ID) %>% nrow()

# For 55 OTUs, the FINAL_ID was different to the species level morphological ID
All_barcode_data_per_OTU %>%
  filter(Rank_Morph == "Species") %>%
  filter(Morphological_ID != FINAL_ID) %>% nrow()

################### Which OTUs were not recovered ##############################

##################### COI
All_barcode_data_per_OTU %>%
  filter(Contamination_COI == "Y") %>%
  select(FINAL_ID, phylum, class, order, family, genus)
  
All_barcode_data_per_OTU %>%
  filter(is.na(Contamination_COI)) %>%
  select(FINAL_ID, phylum, class, order, family, genus)

COI_failures <- All_barcode_data_per_OTU %>%
  filter(Contamination_COI == "Y" | is.na(Contamination_COI)) %>%
  select(SpecimenID_OG, FINAL_ID, phylum, class, order, family, genus, Contamination_COI)
 
# write_xlsx(COI_failures, "COI_failures.xlsx")

# For this I will need to take into account all the sequences which have been
# generated for each OTU

All_barcode_data <- read_excel("summary_counts/Chapter_1_summary_condenced.xlsx", sheet = 1)

failed_COI_OTUs <- COI_failures$FINAL_ID

# Okay lets first get all the sequneces generated from specimens belonging to 
# these OTUs

failed_COI_sequences <- All_barcode_data %>% filter(FINAL_ID %in% failed_COI_OTUs) %>%
  select(SpecimenID_OG, SpecimenID, FINAL_ID, phylum, class, order, family, genus, Contamination_COI)

failed_COI_sequences

# Now lets get the PCR information for these sequences

PCR_record_COI <- read_xlsx("summary_counts/Working_data/flongle_loading_data/COI_extraction_PCR_Loading_record.xlsx")

PCR_record_COI <- PCR_record_COI %>% select(SpecimenID_OG, SpecimenID, PCR_product, Loaded_into_Flongle)

failed_COI_sequences <- left_join(failed_COI_sequences, PCR_record_COI, by = c("SpecimenID_OG", "SpecimenID"))

failed_COI_sequences <- failed_COI_sequences %>%
  mutate(Recovered_from_Flongle = ifelse(!is.na(Contamination_COI) & Loaded_into_Flongle == "Y", "Y", "N")) %>%
  mutate(PCR_attempted = ifelse(is.na(PCR_product), "N", "Y"))

names(failed_COI_sequences)

# reorder
failed_COI_sequences <- failed_COI_sequences %>%
  select(SpecimenID_OG, SpecimenID, FINAL_ID, phylum, class, order, family, genus, PCR_attempted,
         PCR_product, Loaded_into_Flongle, Recovered_from_Flongle, Contamination_COI)

write_xlsx(failed_COI_sequences, "summary_counts/Working_data/sequencing_failure/failed_COI_sequences.xlsx")


##################### 18S

failures_18S <- All_barcode_data_per_OTU %>%
  filter(Contamination_18S == "Y" | is.na(Contamination_18S)) %>%
  select(SpecimenID_OG, FINAL_ID, phylum, class, order, family, genus, Contamination_18S)

# Where these these 18S OTUs actually PCRed?

PCR_record_18S <- read_xlsx("summary_counts/Working_data/flongle_loading_data/18S_extraction_PCR_Loading_record.xlsx")

names(PCR_record_18S)

PCR_record_18S <- PCR_record_18S %>% select(SpecimenID_OG, PCR_product, Loaded_into_Flongle)

failures_18S <- left_join(failures_18S, PCR_record_18S, by = "SpecimenID_OG")

str(failures_18S)

failures_18S <- failures_18S %>%
  mutate(Recovered_from_Flongle = ifelse(!is.na(Contamination_18S) & Loaded_into_Flongle == "Y", "Y", "N")) %>%
  mutate(PCR_attempted = ifelse(is.na(PCR_product), "N", "Y"))
  
failures_18S

# reorder
failures_18S <- failures_18S %>%
  select(SpecimenID_OG, FINAL_ID, phylum, class, order, family, genus, PCR_attempted,
         PCR_product, Loaded_into_Flongle, Recovered_from_Flongle, Contamination_18S)

write_xlsx(failures_18S, "summary_counts/Working_data/sequencing_failure/failed_18S_sequences.xlsx")

####################### Which OTUs required manual consensus ###################
# COI
str(All_barcode_data)

All_mapped_COI <- All_barcode_data %>%
  filter(Contamination_COI == "N") %>%
  filter(stage_COI %in% c("Consensus_by_length_1%_rerun", 	
                       "Consensus_by_similarity_1%_rerun",
                       "Consensus by similarity_contamination_rerun",
                       "Consensus_by_length_contamination_rerun"))

All_mapped_COI_OTUs <- All_mapped_COI %>% 
  select(FINAL_ID, SpecimenID_OG, SpecimenID, phylum, class, order, family, genus)

# FINAL_IDs that have mapped sequences
All_mapped_COI_OTUs_IDs <- All_mapped_COI_OTUs$FINAL_ID

# All sequence records belonging to OTUs which have at least one mapped sequence
All_mapped_COI_OTUs <- All_barcode_data %>% filter(FINAL_ID %in% All_mapped_COI_OTUs_IDs)


All_mapped_COI_OTUs <- All_mapped_COI_OTUs %>% 
  select(FINAL_ID, SpecimenID_OG, SpecimenID, phylum, class, order, 
         family, genus, stage_COI)

write_xlsx(All_mapped_COI_OTUs, "summary_counts/Working_data/OTUs_with_mapped_sequences/All_OTUs_with_at_least_1_mapped_COI_sequence.xlsx")


str(All_mapped_COI_OTUs)

stages_of_interest <- c("Consensus_by_length_1%_rerun", 
                        "Consensus_by_similarity_1%_rerun", 
                        "Consensus by similarity_contamination_rerun", 
                        "Consensus_by_length_contamination_rerun")

# These OTUs have other COI sequences which didn't need mapping
# 17 OTUs
All_mapped_COI_OTUs %>% 
  filter(!is.na(stage_COI)) %>% 
  group_by(FINAL_ID) %>%
  filter(!all(stage_COI %in% stages_of_interest)) %>%
  ungroup() %>%
  select(FINAL_ID) %>%
  distinct()

OTUs_unmandatory_mapping_COI <- All_mapped_COI_OTUs %>% 
  filter(!is.na(stage_COI)) %>% 
  group_by(FINAL_ID) %>%
  filter(!all(stage_COI %in% stages_of_interest)) %>%
  ungroup()

# These OTUs required mapping to recover COI
# 14 OTUs
All_mapped_COI_OTUs %>% 
  filter(!is.na(stage_COI)) %>% 
  group_by(FINAL_ID) %>%
  filter(all(stage_COI %in% stages_of_interest)) %>%
  ungroup() %>%
  select(FINAL_ID) %>%
  distinct()

################################## Mandatory COI mapping #######################

OTUs_mandatory_mapping_COI <- All_mapped_COI_OTUs %>% 
  filter(!is.na(stage_COI)) %>% 
  group_by(FINAL_ID) %>%
  filter(all(stage_COI %in% stages_of_interest)) %>%
  ungroup()

PCR_record_COI <- PCR_record_COI %>% select(SpecimenID_OG, SpecimenID, PCR_product, Loaded_into_Flongle)

OTUs_mandatory_mapping_COI <- left_join(OTUs_mandatory_mapping_COI, PCR_record_COI, by = c("SpecimenID_OG", "SpecimenID"))

OTUs_mandatory_mapping_COI <- OTUs_mandatory_mapping_COI %>%
  mutate(Recovered_from_Flongle = ifelse(!is.na(stage_COI) & Loaded_into_Flongle == "Y", "Y", "N")) %>%
  mutate(PCR_attempted = ifelse(is.na(PCR_product), "N", "Y"))

# reorder
OTUs_mandatory_mapping_COI <- OTUs_mandatory_mapping_COI %>%
  select(SpecimenID_OG, SpecimenID, FINAL_ID, phylum, class, order, family, genus, PCR_attempted,
         PCR_product, Loaded_into_Flongle, Recovered_from_Flongle, stage_COI)

OTUs_mandatory_mapping_COI <- OTUs_mandatory_mapping_COI %>% distinct()

write_xlsx(OTUs_mandatory_mapping_COI, "summary_counts/Working_data/OTUs_with_mapped_sequences/OTUs_mandatory_mapping_COI.xlsx")

################################## Un-mandatory COI mapping ####################

OTUs_unmandatory_mapping_COI <- left_join(OTUs_unmandatory_mapping_COI, PCR_record_COI, by = c("SpecimenID_OG", "SpecimenID"))

OTUs_unmandatory_mapping_COI <- OTUs_unmandatory_mapping_COI %>%
  mutate(Recovered_from_Flongle = ifelse(!is.na(stage_COI) & Loaded_into_Flongle == "Y", "Y", "N")) %>%
  mutate(PCR_attempted = ifelse(is.na(PCR_product), "N", "Y"))

# reorder
OTUs_unmandatory_mapping_COI <- OTUs_unmandatory_mapping_COI %>%
  select(SpecimenID_OG, SpecimenID, FINAL_ID, phylum, class, order, family, genus, PCR_attempted,
         PCR_product, Loaded_into_Flongle, Recovered_from_Flongle, stage_COI)

OTUs_unmandatory_mapping_COI <- OTUs_unmandatory_mapping_COI %>% distinct()

write_xlsx(OTUs_unmandatory_mapping_COI, "summary_counts/Working_data/OTUs_with_mapped_sequences/OTUs_unmandatory_mapping_COI.xlsx")



########################################### 18s ################################

All_mapped_18S <- All_barcode_data %>%
  filter(Contamination_18S == "N") %>%
  filter(stage_18S %in% c("Consensus_by_length_1%_rerun", 	
                          "Consensus_by_similarity_1%_rerun",
                          "Consensus by similarity_contamination_rerun",
                          "Consensus_by_length_contamination_rerun"))

All_mapped_18S_OTUs <- All_mapped_18S %>% 
  select(FINAL_ID, SpecimenID_OG, SpecimenID, phylum, class, order, family, genus)

# FINAL_IDs that have mapped sequences
All_mapped_18S_OTUs_IDs <- All_mapped_18S_OTUs$FINAL_ID

# All sequence records belonging to OTUs which have at least one mapped sequence
All_mapped_18S_OTUs <- All_barcode_data %>% filter(FINAL_ID %in% All_mapped_18S_OTUs_IDs)

All_mapped_18S_OTUs <- All_mapped_18S_OTUs %>% 
  select(FINAL_ID, SpecimenID_OG, SpecimenID, phylum, class, order, 
         family, genus, stage_18S)

stages_of_interest <- c("Consensus_by_length_1%_rerun", 
                        "Consensus_by_similarity_1%_rerun", 
                        "Consensus by similarity_contamination_rerun", 
                        "Consensus_by_length_contamination_rerun")

# These OTUs have other 18S sequences which didn't need mapping
# 1 OTUs
All_mapped_18S_OTUs %>% 
  filter(!is.na(stage_18S)) %>% 
  group_by(FINAL_ID) %>%
  filter(!all(stage_18S %in% stages_of_interest)) %>%
  ungroup() %>%
  select(FINAL_ID) %>%
  distinct()

# These OTUs required mapping to recover 18S
# 6 OTUs
All_mapped_18S_OTUs %>% 
  filter(!is.na(stage_18S)) %>% 
  group_by(FINAL_ID) %>%
  filter(all(stage_18S %in% stages_of_interest)) %>%
  ungroup() %>%
  select(FINAL_ID) %>%
  distinct()

################################## Mandatory 18s mapping #######################

OTUs_mandatory_mapping_18S <- All_mapped_18S_OTUs %>% 
  filter(!is.na(stage_18S)) %>% 
  group_by(FINAL_ID) %>%
  filter(all(stage_18S %in% stages_of_interest)) %>%
  ungroup()

PCR_record_18S <- PCR_record_18S %>% select(SpecimenID_OG, PCR_product, Loaded_into_Flongle)

OTUs_mandatory_mapping_18S <- left_join(OTUs_mandatory_mapping_18S, PCR_record_18S, by = "SpecimenID_OG")

OTUs_mandatory_mapping_18S <- OTUs_mandatory_mapping_18S %>%
  mutate(Recovered_from_Flongle = ifelse(!is.na(stage_18S) & Loaded_into_Flongle == "Y", "Y", "N")) %>%
  mutate(PCR_attempted = ifelse(is.na(PCR_product), "N", "Y"))

# reorder
OTUs_mandatory_mapping_18S <- OTUs_mandatory_mapping_18S %>%
  select(SpecimenID_OG, FINAL_ID, phylum, class, order, family, genus, PCR_attempted,
         PCR_product, Loaded_into_Flongle, Recovered_from_Flongle, stage_18S)

OTUs_mandatory_mapping_18S <- OTUs_mandatory_mapping_18S %>% distinct()

write_xlsx(OTUs_mandatory_mapping_18S, "summary_counts/Working_data/OTUs_with_mapped_sequences/OTUs_mandatory_mapping_18S.xlsx")

################################## Un-mandatory 18s mapping ####################

OTUs_unmandatory_mapping_18S <- All_mapped_18S_OTUs %>% 
  filter(!is.na(stage_18S)) %>% 
  group_by(FINAL_ID) %>%
  filter(!all(stage_18S %in% stages_of_interest)) %>%
  ungroup()

OTUs_unmandatory_mapping_18S <- left_join(OTUs_unmandatory_mapping_18S, PCR_record_18S, by = "SpecimenID_OG")

OTUs_unmandatory_mapping_18S <- OTUs_unmandatory_mapping_18S %>%
  mutate(Recovered_from_Flongle = ifelse(!is.na(stage_18S) & Loaded_into_Flongle == "Y", "Y", "N")) %>%
  mutate(PCR_attempted = ifelse(is.na(PCR_product), "N", "Y"))

# reorder
OTUs_unmandatory_mapping_18S <- OTUs_unmandatory_mapping_18S %>%
  select(SpecimenID_OG, FINAL_ID, phylum, class, order, family, genus, PCR_attempted,
         PCR_product, Loaded_into_Flongle, Recovered_from_Flongle, stage_18S)

OTUs_unmandatory_mapping_18S <- OTUs_unmandatory_mapping_18S %>% distinct()

write_xlsx(OTUs_unmandatory_mapping_18S, "summary_counts/Working_data/OTUs_with_mapped_sequences/OTUs_unmandatory_mapping_18S.xlsx")



################################################################################
# How many COI IDs were not to species level?

All_barcode_data_per_OTU <- read_excel("summary_counts/Chapter_1_summary_condenced.xlsx", sheet = 4)

# 47 OTUs were not identified to Species level using the COI barcodes
All_barcode_data_per_OTU %>% 
  filter(Contamination_COI == "N") %>%
  filter(Rank_COI != "Species") %>% nrow()

# 86 OTUs were identified to Species level using the COI barcodes
All_barcode_data_per_OTU %>% 
  filter(Contamination_COI == "N") %>%
  filter(Rank_COI == "Species") %>% nrow()

# 94 OTUs were identified to species level by taking into account all markers and 
# morphology 
All_barcode_data_per_OTU %>% 
  filter(Final_rank == "Species") %>% nrow()

# 9 OTUs were identified to Species level using Morphology 
All_barcode_data_per_OTU %>% 
  filter(Final_rank == "Species") %>% nrow() - All_barcode_data_per_OTU %>% 
  filter(Contamination_COI == "N") %>%
  filter(Rank_COI == "Species") %>% nrow()

All_barcode_data_per_OTU %>% 
  filter(Rank_COI != "Species" & Final_rank == "Species") %>%
  nrow()

All_barcode_data_per_OTU %>% 
  filter(Rank_COI != "Species" & Final_rank == "Species") %>%
  select(Molecular_ID_COI, Morphological_ID, FINAL_ID)

All_barcode_data_per_OTU %>% 
  filter(Rank_COI == "Genus" & Final_rank == "Genus") %>%
  select(Molecular_ID_COI, Morphological_ID, FINAL_ID)

All_barcode_data_per_OTU %>% 
  filter(Rank_COI != "Genus" & Final_rank == "Genus") %>%
  select(Molecular_ID_COI, Morphological_ID, FINAL_ID)

################################################################################











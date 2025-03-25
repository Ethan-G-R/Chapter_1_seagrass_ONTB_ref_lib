# 01.11.24
# Ethan Ross

# In this script I will combine the PCR information with the recovery information 


library(writexl)
library(readxl)
library(dplyr)
library(worms)
library(ggplot2)
library(stringr)
library(tidyr)
library(scales)

All_barcode_data <- read_excel("../Chapter_1_summary.xlsx", sheet = 2)

################################## COI #########################################
PCR_record_COI <- read_xlsx("C:/Users/r01er21/OneDrive - University of Aberdeen/Chapter 1 Invert ID results/Chapter_1_rerun_post_16.05.24/PCR_and_sequencing_records/COI_extraction_PCR_Loading_record.xlsx")

names(PCR_record_COI)

PCR_record_COI <- PCR_record_COI %>% select(-Specimen_ID_OLD, -ONT_RUN)

PCR_record_COI <- PCR_record_COI %>%
  dplyr::rename(PCR_product_COI = PCR_product) %>%
  dplyr::rename(Tag_combo_COI = Tag_combo) %>%
  dplyr::rename(Primers_COI = Primers) %>%
  dplyr::rename(Gel_COI = Gel) %>%
  dplyr::rename(Loaded_into_Flongle_COI = Loaded_into_Flongle)
#  dplyr::rename(ONT_RUN_COI = ONT_RUN)

################################# SIDE QUEST ###################################
# There needs to be some name adjustments 
# since I haven't renamed the Specimen IDs for _a and _b 
# Also the _LCOech1a tags must be removed from the end of LE 34 and LE 39

# All_barcode_data %>% 
#   select(SpecimenID_OG, SpecimenID,RUN_COI) %>%
#   filter(str_ends(SpecimenID, "_a"))
# 
# All_barcode_data %>% 
#   select(SpecimenID_OG, SpecimenID, RUN_COI) %>%
#   filter(str_ends(SpecimenID, "_b"))
################################ COMPLETE ######################################

################################ COI Combine ###################################
names(All_barcode_data)

All_barcode_data <- left_join(All_barcode_data, PCR_record_COI, by = c("SpecimenID_OG","SpecimenID")) %>%
 relocate(Extracted_DNA, PCR_product_COI, Tag_combo_COI, Primers_COI, Gel_COI, Loaded_into_Flongle_COI, .after = SpecimenID)

################################## 18S #########################################

PCR_record_18S <- read_xlsx("C:/Users/r01er21/OneDrive - University of Aberdeen/Chapter 1 Invert ID results/Chapter_1_rerun_post_16.05.24/PCR_and_sequencing_records/18S_extraction_PCR_Loading_record.xlsx")

names(PCR_record_18S)

PCR_record_18S <- PCR_record_18S %>% select(-ONT_RUN, -Extracted_DNA)

PCR_record_18S <- PCR_record_18S %>%
  dplyr::rename(PCR_product_18S = PCR_product) %>%
  dplyr::rename(Tag_combo_18S = Tag_combo) %>%
  dplyr::rename(Primers_18S = Primers) %>%
  dplyr::rename(Loaded_into_Flongle_18S = Loaded_into_Flongle)

All_barcode_data <- left_join(All_barcode_data, PCR_record_18S, by = c("SpecimenID_OG")) %>%
  relocate(PCR_product_18S, Tag_combo_18S, Primers_18S, Loaded_into_Flongle_18S, .after = Congruence_COI_morph)

names(All_barcode_data)

write_xlsx(All_barcode_data, "Chapter_1_summary_with_PCRs.xlsx")






















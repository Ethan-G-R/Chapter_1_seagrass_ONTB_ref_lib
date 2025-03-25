# 20.11.24
# Ethan Ross

# In this script I will look at what the contaminatino for the Bivalves was

library(writexl)
library(readxl)
library(dplyr)
library(worms)
library(ggplot2)
library(stringr)
library(gridExtra)

All_ONTB_COI_contamination <- read_excel("summary_counts/Working_data/contamination/COI_contamination_record_v3.xlsx", sheet = 1)
names(All_ONTB_COI_contamination)

All_ONTB_COI_contamination <- All_ONTB_COI_contamination %>% 
  select(Run, SpecimenID_OG, SpecimenID, Contamination_COI_pre_mapping, Type) %>%
  filter(Type == "External")

All_pre_mapping_hits <- read_excel("BLAST_BOLD_outputs/All_COI_BOLD_hits.xlsx", sheet = 3)
names(All_pre_mapping_hits)

All_pre_mapping_hits <- All_pre_mapping_hits %>% 
  select(Run, SpecimenID, Top_BOLD_hit) %>%
  mutate(SpecimenID_OG = SpecimenID)


All_ONTB_COI_contamination <- left_join(All_ONTB_COI_contamination, 
                                        All_pre_mapping_hits,
                                        by = c("Run", "SpecimenID_OG",
                                               "SpecimenID"))

# Which of them are Bivalves?

All_barcode_data <- read_excel("summary_counts/Chapter_1_summary_with_PCRs.xlsx")

# All_barcode_data_bivalvia <- All_barcode_data %>% filter(class == "Bivalvia")

names(All_barcode_data)

All_barcode_data <- All_barcode_data %>%
  select(RUN_COI, SpecimenID_OG, SpecimenID, class) %>%
  mutate(Run = RUN_COI) %>%
  select(-RUN_COI)

x <- inner_join(All_ONTB_COI_contamination, All_barcode_data, 
           by = c("Run", "SpecimenID_OG", "SpecimenID"))

write_xlsx(x, "summary_counts/Working_data/contamination/Chapter_1_external_contamination_hits.xlsx")




# 22.03.24
# Ethan Ross

# In this script I will get counts for the number of ambiguous bases in my
# ONTBarcoder2 raw sequences

library(dplyr)
library(writexl)
library(readxl)
library(stringr)

## N count function 

# Function to count characters that are not "A", "G", "C", or "T"
Conut_non_AGCT <- function(barcode) {
  return(str_count(barcode, "[^AGCT]"))
}

###################################### COI #####################################

## Get raw COI data

ONTB2_raw_COI_seqs <- read_xlsx("ONTB2_outputs/ONTBarcoder2_COI_run_summary_and_output_raw.xlsx", sheet = 1)

# Remove rows with no sequences 
ONTB2_raw_COI_seqs_N_counts <- ONTB2_raw_COI_seqs %>% filter(barcode != "NA")

# make sure length is a numeric value
ONTB2_raw_COI_seqs_N_counts$length <- as.numeric(ONTB2_raw_COI_seqs_N_counts$length)

# Add a new column with counts of characters that are not "A", "G", "C", or "T"
ONTB2_raw_COI_seqs_N_counts <- ONTB2_raw_COI_seqs_N_counts %>%
  mutate(Conut_non_AGCT = sapply(barcode, Conut_non_AGCT))

# Add a new column with percentage of ambiguous bases
ONTB2_raw_COI_seqs_N_counts <- ONTB2_raw_COI_seqs_N_counts %>%
  mutate(Perc_non_AGCT = 100*Conut_non_AGCT/length)

## Save

write_xlsx(ONTB2_raw_COI_seqs_N_counts, "ONTB2_outputs/ONTBarcoder2_COI_run_summary_raw_N_counts.xlsx")

# Get COI sequences with Ns

ONTB2_raw_COI_seqs_Ns <- ONTB2_raw_COI_seqs_N_counts %>% filter(Conut_non_AGCT > 0)

## Save

write_xlsx(ONTB2_raw_COI_seqs_Ns, "mapped_reruns/ONTBarcoder2_COI_N_seqs_for_mapping.xlsx")

###################################### 18S #####################################
## Get raw 18S data

ONTB2_raw_18S_seqs <- read_xlsx("ONTB2_outputs/ONTBarcoder2_18S_run_summary_and_output_raw.xlsx", sheet = 1)

# Remove rows with no sequences 
ONTB2_raw_18S_seqs_N_counts <- ONTB2_raw_18S_seqs %>% filter(barcode != "NA")

# make sure length is a numeric value
ONTB2_raw_18S_seqs_N_counts$length <- as.numeric(ONTB2_raw_18S_seqs_N_counts$length)

# Add a new column with counts of characters that are not "A", "G", "C", or "T"
ONTB2_raw_18S_seqs_N_counts <- ONTB2_raw_18S_seqs_N_counts %>%
  mutate(Conut_non_AGCT = sapply(barcode, Conut_non_AGCT))

# Add a new column with percentage of ambiguous bases
ONTB2_raw_18S_seqs_N_counts <- ONTB2_raw_18S_seqs_N_counts %>%
  mutate(Perc_non_AGCT = 100*Conut_non_AGCT/length)

## Save

write_xlsx(ONTB2_raw_18S_seqs_N_counts, "ONTB2_outputs/ONTBarcoder2_18S_run_summary_raw_N_counts.xlsx")

# Get COI sequences with Ns

ONTB2_raw_18S_seqs_Ns <- ONTB2_raw_18S_seqs_N_counts %>% filter(Conut_non_AGCT > 0)

## Save

write_xlsx(ONTB2_raw_18S_seqs_Ns, "mapped_reruns/ONTBarcoder2_18S_N_seqs_for_mapping.xlsx")

################################################################################

# COI_mapped <- read_xlsx("mapped_reruns/ONTBarcoder2_COI_mapped_seqs.xlsx")
# 
# # Remove rows with no sequences 
# COI_mapped_N_counts <- COI_mapped %>% filter(barcode != "NA")
# 
# # make sure length is a numeric value
# COI_mapped_N_counts$length <- as.numeric(COI_mapped_N_counts$length)
# 
# # Add a new column with counts of characters that are not "A", "G", "C", or "T"
# COI_mapped_N_counts <- COI_mapped_N_counts %>%
#   mutate(Conut_non_AGCT = sapply(barcode, Conut_non_AGCT))
# 
# # Add a new column with percentage of ambiguous bases
# COI_mapped_N_counts <- COI_mapped_N_counts %>%
#   mutate(Perc_non_AGCT = 100*Conut_non_AGCT/length)
# 
# write_xlsx(COI_mapped_N_counts, "mapped_reruns/ONTBarcoder2_COI_mapped_seqs.xlsx")

# mapped_18S <- read_xlsx("mapped_reruns/ONTBarcoder2_18S_mapped_seqs.xlsx")
# # 
# # # Remove rows with no sequences 
# mapped_18S_N_counts <- mapped_18S %>% filter(barcode != "NA")
# # 
# # # make sure length is a numeric value
# mapped_18S_N_counts$length <- as.numeric(mapped_18S_N_counts$length)
# # 
# # # Add a new column with counts of characters that are not "A", "G", "C", or "T"
# mapped_18S_N_counts <- mapped_18S_N_counts %>%
#    mutate(Conut_non_AGCT = sapply(barcode, Conut_non_AGCT))
# # 
# # # Add a new column with percentage of ambiguous bases
# mapped_18S_N_counts <- mapped_18S_N_counts %>%
#    mutate(Perc_non_AGCT = 100*Conut_non_AGCT/length)
# # 
# write_xlsx(mapped_18S_N_counts, "mapped_reruns/ONTBarcoder2_18S_mapped_seqs.xlsx")


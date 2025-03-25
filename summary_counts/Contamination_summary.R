# 04.09.24
# Ethan Ross

# In this script I will get all the contaminated sequences before and after
# mapping

library(writexl)
library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
library(gridExtra)
library(tidyr)

############################# COI ##############################################
All_ONTB_COI_post_mapping <- read_excel("summary_counts/Chapter_1_summary_condenced.xlsx", sheet = 1)
names(All_ONTB_COI_post_mapping)

# Lets get all the records of specimens which didn't have contaminated barcodes
#All_ONTB_COI_post_mapping_target <- All_ONTB_COI_post_mapping %>% select(SpecimenID_OG, Contamination_COI) %>%
#  filter(Contamination_COI == "N")

# Lets get rid of specimens for which no COI sequences were recovered
All_ONTB_COI_post_mapping <- All_ONTB_COI_post_mapping %>% select(SpecimenID_OG, FINAL_ID, Contamination_COI) %>%
  filter(!is.na(Contamination_COI))

# 332 COI sequences were recovered
nrow(All_ONTB_COI_post_mapping)

# Now lets combine any rows that belong to the same specimen
# In cases, any specimens with at least one non non contaminated sequence will
# count as having no contaminated sequences
All_ONTB_COI_post_mapping_per_specimen <- All_ONTB_COI_post_mapping %>%
  group_by(SpecimenID_OG, FINAL_ID) %>%
  dplyr::summarise(
    Contamination_COI = case_when(
      any(Contamination_COI == "N") ~ "N",  # Prioritize "N"
      any(Contamination_COI == "Y") ~ "Y",  # Only if there are no "N"
      TRUE ~ NA_character_  # Handle cases with neither "N" nor "Y"
    ),
    .groups = 'drop'  # Ungroup after summarising
  )

# 286 specimens had consensus sequences recovered
All_ONTB_COI_post_mapping_per_specimen %>% nrow()

# 268 specimens had non contaminated DNA barcodes generated 
All_ONTB_COI_post_mapping_per_specimen %>% filter(Contamination_COI == "N") %>% nrow()

# 18 specimens had contaminated DNA barcodes generated
All_ONTB_COI_post_mapping_per_specimen %>% filter(Contamination_COI == "Y") %>% nrow()

##################################### by OTU ###################################
All_ONTB_COI_post_mapping_per_OTU <- All_ONTB_COI_post_mapping %>%
  group_by(FINAL_ID) %>%
  dplyr::summarise(
    Contamination_COI = case_when(
      any(Contamination_COI == "N") ~ "N",  # Prioritize "N"
      any(Contamination_COI == "Y") ~ "Y",  # Only if there are no "N"
      TRUE ~ NA_character_  # Handle cases with neither "N" nor "Y"
    ),
    .groups = 'drop'  # Ungroup after summarising
  )

# 138 OTUs had COI barcodes recovered
All_ONTB_COI_post_mapping_per_OTU %>% nrow()

# 133 OTUs had non contaminated DNA barcodes recovered
All_ONTB_COI_post_mapping_per_OTU %>% filter(Contamination_COI == "N") %>% nrow()

# 5 OTUs had non contaminated DNA barcodes recovered
All_ONTB_COI_post_mapping_per_OTU %>% filter(Contamination_COI == "Y") %>% nrow()



############################# 18S ##############################################

All_ONTB_18S_post_mapping <- read_excel("summary_counts/Chapter_1_summary_condenced.xlsx", sheet = 1)
names(All_ONTB_18S_post_mapping)

# Lets get all the records of specimens which didn't have contaminated barcodes
#All_ONTB_18S_post_mapping_target <- All_ONTB_18S_post_mapping %>% select(SpecimenID_OG, Contamination_18S) %>%
#  filter(Contamination_18S == "N")

# Lets get rid of specimens for which no 18S sequences were recovered
All_ONTB_18S_post_mapping <- All_ONTB_18S_post_mapping %>% select(SpecimenID_OG, FINAL_ID, Contamination_18S) %>%
  filter(!is.na(Contamination_18S))

# 249 18S sequences were recovered
nrow(All_ONTB_18S_post_mapping)

# Now lets combine any rows that belong to the same specimen
# In cases, any specimens with at least one non non contaminated sequence will
# count as having no contaminated sequences
All_ONTB_18S_post_mapping_per_specimen <- All_ONTB_18S_post_mapping %>%
  group_by(SpecimenID_OG, FINAL_ID) %>%
  dplyr::summarise(
    Contamination_18S = case_when(
      any(Contamination_18S == "N") ~ "N",  # Prioritize "N"
      any(Contamination_18S == "Y") ~ "Y",  # Only if there are no "N"
      TRUE ~ NA_character_  # Handle cases with neither "N" nor "Y"
    ),
    .groups = 'drop'  # Ungroup after summarising
  )

# 213 specimens had consensus sequneces reocvered 
All_ONTB_18S_post_mapping_per_specimen %>% nrow()

# 209 specimens had non contaminated DNA barcodes generated 
All_ONTB_18S_post_mapping_per_specimen %>% filter(Contamination_18S == "N") %>% nrow()

# 4 specimens had contaminated DNA barcodes generated
All_ONTB_18S_post_mapping_per_specimen %>% filter(Contamination_18S == "Y") %>% nrow()

##################################### by OTU ###################################
All_ONTB_18S_post_mapping_per_OTU <- All_ONTB_18S_post_mapping %>%
  group_by(FINAL_ID) %>%
  dplyr::summarise(
    Contamination_18S = case_when(
      any(Contamination_18S == "N") ~ "N",  # Prioritize "N"
      any(Contamination_18S == "Y") ~ "Y",  # Only if there are no "N"
      TRUE ~ NA_character_  # Handle cases with neither "N" nor "Y"
    ),
    .groups = 'drop'  # Ungroup after summarising
  )

# 133 OTUs had 18S barcodes recovered
All_ONTB_18S_post_mapping_per_OTU %>% nrow()

# 131 OTUs had non contaminated DNA barcodes recovered
All_ONTB_18S_post_mapping_per_OTU %>% filter(Contamination_18S == "N") %>% nrow()

# 2 OTUs had non contaminated DNA barcodes recovered
All_ONTB_18S_post_mapping_per_OTU %>% filter(Contamination_18S == "Y") %>% nrow()

############################# Summary ##########################################

data_list <- list(
  "COI_by_specimen" = All_ONTB_COI_post_mapping_per_specimen,  # Replace with your data frames
  "COI_by_OTU" = All_ONTB_COI_post_mapping_per_OTU,
  "18S_by_specimen" = All_ONTB_18S_post_mapping_per_specimen,
  "18S_by_OTU" = All_ONTB_18S_post_mapping_per_OTU
)

write_xlsx(data_list, "summary_counts/Working_data/contamination/Chapter_1_contamination_summary.xlsx")


############################# COI contamination ################################

All_ONTB_COI_pre_mapping <- read_excel("BLAST_BOLD_outputs/All_COI_BOLD_hits.xlsx", sheet = 3)

names(All_ONTB_COI_pre_mapping)

All_ONTB_COI_pre_mapping <- All_ONTB_COI_pre_mapping %>%
  mutate(SpecimenID_OG = SpecimenID)

All_ONTB_COI_pre_mapping_contam <- All_ONTB_COI_pre_mapping %>% filter(Contamination == "Y")

names(All_ONTB_COI_pre_mapping_contam)

All_ONTB_COI_pre_mapping_contam <- All_ONTB_COI_pre_mapping_contam %>% select(Run, SpecimenID_OG, SpecimenID, Contamination)
All_ONTB_COI_pre_mapping_contam <- All_ONTB_COI_pre_mapping_contam %>% dplyr::rename(Contamination_COI_pre_mapping = Contamination)

All_ONTB_COI_post_mapping <- read_excel("summary_counts/Chapter_1_summary_condenced.xlsx", sheet = 1)

names(All_ONTB_COI_post_mapping)

All_ONTB_COI_post_mapping_contam <- All_ONTB_COI_post_mapping %>% filter(Contamination_COI == "Y")

names(All_ONTB_COI_post_mapping_contam)

All_ONTB_COI_post_mapping_contam <- All_ONTB_COI_post_mapping_contam %>% select(RUN_COI, SpecimenID_OG, SpecimenID, Contamination_COI) %>%
  dplyr::rename(Run = RUN_COI) %>% dplyr::rename(Contamination_COI_post_mapping = Contamination_COI)

All_ONTB_COI_contamination <- full_join(All_ONTB_COI_pre_mapping_contam, All_ONTB_COI_post_mapping_contam, 
               by = c("Run", "SpecimenID_OG", "SpecimenID"))
names(All_ONTB_COI_contamination)

All_ONTB_COI_contamination <- All_ONTB_COI_contamination %>%
  mutate(Contamination_COI_post_mapping = ifelse(is.na(Contamination_COI_post_mapping), "N", Contamination_COI_post_mapping))

# write_xlsx(All_ONTB_COI_contamination, "COI_contamination_record_v3.xlsx")

############################### 18S contamination ##############################

All_ONTB_18S_pre_mapping <- read_excel("BLAST_BOLD_outputs/All_18S_BLAST_hits.xlsx", sheet = 3)

names(All_ONTB_18S_pre_mapping)

All_ONTB_18S_pre_mapping_contam <- All_ONTB_18S_pre_mapping %>% filter(Contamination_18S == "Y")

All_ONTB_18S_pre_mapping_contam <- All_ONTB_18S_pre_mapping_contam %>% select(Run, SpecimenID, Contamination_18S)

All_ONTB_18S_pre_mapping_contam <- All_ONTB_18S_pre_mapping_contam %>% dplyr::rename(Contamination_18S_pre_mapping = Contamination_18S)

All_ONTB_18S_post_mapping <- read_excel("summary_counts/Chapter_1_summary_condenced.xlsx", sheet = 1)

All_ONTB_18S_post_mapping_contam <- All_ONTB_18S_post_mapping %>% filter(Contamination_18S == "Y")

head(All_ONTB_18S_post_mapping_contam)

All_ONTB_18S_post_mapping_contam <- All_ONTB_18S_post_mapping_contam %>% select(RUN_18S, SpecimenID, SpecimenID_OG, Contamination_18S) %>%
  dplyr::rename(Run = RUN_18S) %>% dplyr::rename(Contamination_18S_post_mapping = Contamination_18S)

head(All_ONTB_18S_post_mapping_contam)

All_ONTB_18S_contamination <- full_join(All_ONTB_18S_pre_mapping_contam, All_ONTB_18S_post_mapping_contam, 
                                        by = c("Run", "SpecimenID"))

head(All_ONTB_18S_contamination)

All_ONTB_18S_contamination <- All_ONTB_18S_contamination %>%
  mutate(Contamination_18S_post_mapping = ifelse(is.na(Contamination_18S_post_mapping), "N", Contamination_18S_post_mapping))

# write_xlsx(All_ONTB_18S_contamination, "18S_contamination_record_v3.xlsx")


########################### Which specimens needed mapping for 18S? ############

All_ONTB_18S_contamination <- read_excel("summary_counts/Working_data/contamination/18S_contamination_record_v3.xlsx", sheet = 1)

# This data frame is from earlier in the script 
head(All_ONTB_18S_post_mapping_per_specimen)
head(All_ONTB_18S_contamination)

x <- left_join(All_ONTB_18S_post_mapping_per_specimen, All_ONTB_18S_contamination,
                                                    by = "SpecimenID_OG")

names(x)

# There are no differences between these contamination columns
# This means there were no other consensus sequences generated for these 
# specimens without manual mapping (which makes sense since I only ran a single 
# primer pair and in most cases only a sinlge specimens per OTU)

x <- x %>% filter(!is.na(Contamination_18S_post_mapping)) %>% 
  select(SpecimenID_OG, FINAL_ID, Contamination_18S, Contamination_18S_post_mapping)

x

# 7 specimens required manual consensus to get a target sequence
x %>% filter(Contamination_18S == "N") %>% nrow()

# 4 specimens were mapped but this didn't recover a target sequence
x %>% filter(Contamination_18S == "Y") %>% nrow()

# 7 OTUs required manual consensus to get a target sequence
x %>% filter(Contamination_18S == "N") %>% distinct(FINAL_ID) %>% nrow()

# 3 OTUs required manual consensus to get a target sequence
x %>% filter(Contamination_18S == "Y") %>% distinct(FINAL_ID) %>% nrow()

########################### Which specimens needed mapping for COI? ############

All_ONTB_COI_contamination <- read_excel("summary_counts/Working_data/contamination/COI_contamination_record_v3.xlsx", sheet = 1)

# This data frame is from earlier in the script 
head(All_ONTB_COI_post_mapping_per_specimen)

head(All_ONTB_COI_contamination)

# There are some cases of different primer amplicons from the same specimen 
# needing mapping so I need to condense All_ONTB_COI_contamination by specimen
# before I proceed 

All_ONTB_COI_contamination_per_specimen <- All_ONTB_COI_contamination %>%
  group_by(SpecimenID_OG, Contamination_COI_pre_mapping, Type) %>%
  dplyr::summarise(
    Contamination_COI_post_mapping = case_when(
      any(Contamination_COI_post_mapping == "N") ~ "N",  # Prioritize "N"
      any(Contamination_COI_post_mapping == "Y") ~ "Y",  # Only if there are no "N"
      TRUE ~ NA_character_  # Handle cases with neither "N" nor "Y"
    ),
    .groups = 'drop'  # Ungroup after summarising
  )

# Okay great we can see there are 50 specimens which required mapping 
All_ONTB_COI_contamination_per_specimen %>% nrow()

# For 30 specimens, mapping resulted in a target consensus sequence
All_ONTB_COI_contamination_per_specimen %>% 
  filter(Contamination_COI_post_mapping == "N") %>% nrow()


# For 20 specimens, mapping still resulted in contamination 
All_ONTB_COI_contamination_per_specimen %>% 
  filter(Contamination_COI_post_mapping == "Y") %>% nrow()

# But now I need to check if another combination of primers resulted in a target
# consensus sequence 

y <- left_join(All_ONTB_COI_post_mapping_per_specimen, All_ONTB_COI_contamination_per_specimen,
               by = "SpecimenID_OG")

y <- y %>% filter(!is.na(Contamination_COI_post_mapping)) %>% 
  select(SpecimenID_OG, FINAL_ID, Contamination_COI, Contamination_COI_post_mapping)

print(y)

# Identify rows where the values differ and print those rows
differences <- y[y$Contamination_COI != y$Contamination_COI_post_mapping, ]

# Print mismatches to the console
if (nrow(differences) > 0) {
  print("Rows with differing values:")
  print(differences)
} else {
  print("No differences found.")
}

# Okay from this we can see there are differences.
# 011_LC2_3_7 Nicolea zostericola had jg and folmer amplicons 
# the folmer amplicons I mapped whereas the jg amplicon didn't need mapping

# 027_WB_20_6 Fabulina fabula also had a jg and folmer amplicon. Again the jg
# amplicon didn't need mapping, while the folmer one did


# 31 specimens required manual consensus to get a target sequence
y %>% filter(Contamination_COI == "N") %>% nrow()

# 18 specimens were mapped but this didn't recover a target sequence
y %>% filter(Contamination_COI == "Y") %>% nrow()

# 25 OTUs required manual consensus to get a target sequence
y %>% filter(Contamination_COI == "N") %>% distinct(FINAL_ID) %>% nrow()

# 15 OTUs required manual consensus to get a target sequence
y %>% filter(Contamination_COI == "Y") %>% distinct(FINAL_ID) %>% nrow()

##################################### Plot #####################################

All_ONTB_contam_summary <- read_excel("summary_counts/Working_data/contamination/Chapter_1_contamination_summary.xlsx", sheet = 4)

str(All_ONTB_contam_summary)

# Calculate the total Specimen_Count per Marker for x-positioning and
# Calculate the recovery percentage for each Marker
All_ONTB_contam_summary <- All_ONTB_contam_summary %>%
  group_by(Marker) %>%
  mutate(
    Total_Specimen_Count = sum(Specimen_Count),
    Recovery_Percentage = 100 * sum(Specimen_Count[Category %in% c("Target", "Target_mapped")]) / Total_Specimen_Count
  )

All_ONTB_contam_summary$Category <- factor(All_ONTB_contam_summary$Category, levels = c("Contamination", "Target_mapped", "Target"))

# Updated plot code with percentage labels at the end of each bar
p3 <- ggplot(All_ONTB_contam_summary, aes(x = Specimen_Count, y = Marker, fill = Category)) +
  geom_bar(stat = "identity", color = "black", width = 0.45, linewidth  = 1.5) +
  geom_text(data = distinct(All_ONTB_contam_summary, Marker, .keep_all = TRUE), 
            aes(x = Total_Specimen_Count + 3, y = Marker, label = paste0(round(Recovery_Percentage, 0), "%")), 
            hjust = 0, size = 16, color = "black") +  # Adjust `size` as needed
  theme_minimal() +
  labs(x = "Number of Specimens", y = "Marker", title = "") +
  scale_fill_manual(values = c("#FF654B", "#7C7C7C", "#D9D9D9"), 
                    labels = c(" Contamination ", " Target (mapped) ", " Target")) +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 45),
    axis.title.y = element_text(size = 45),
    axis.text.x = element_text(size = 45),
    axis.text.y = element_text(size = 45),
    legend.text = element_text(size = 40),
    legend.title = element_text(size = 45),
    text = element_text(size = 40),
    legend.position = "top",
    legend.justification = c("center", "top"),
    legend.key.height = unit(1.5, "cm"),
    legend.key.width = unit(1.5, "cm")
  ) +
  coord_cartesian(xlim = c(0, max(All_ONTB_contam_summary$Total_Specimen_Count) + 20)) +  # Adjust x-axis limits
  guides(fill = guide_legend(title = NULL))

p3

ggsave("Plots/Contamination_summary_per_specimen_v2.tiff", plot = p3, width = 30, height = 8, bg = "white")

############################ OLD  ###############################################
names(All_ONTB_contamination)

# First, pivot the data to a long format to facilitate stacking
All_ONTB_contam_long <- All_ONTB_contamination %>%
  pivot_longer(cols = c(Target, Target_manual, Contam), 
               names_to = "Molecular_ID", 
               values_to = "Count")

# Specify the desired order of primers
desired_order <- c("polyLCO/polyHCO", "echLCO/HCO", "jgLCO/jgHCO", "LCO/HCO", "Uni18S/Uni18R")

# Convert the Primers column to a factor with the specified order
All_ONTB_contam_long$Primers <- factor(All_ONTB_contam_long$Primers, levels = desired_order)

# Specify the desired order of primers
desired_order_ID <- c("Contam", "Target_manual", "Target")

# Convert the Primers column to a factor with the specified order
All_ONTB_contam_long$Molecular_ID <- factor(All_ONTB_contam_long$Molecular_ID, levels = desired_order_ID)



p3 <- ggplot(All_ONTB_contam_long, aes(x = Count, y = Primers, fill = Molecular_ID)) +
  geom_bar(stat = "identity", color = "black", aes(group = Molecular_ID), width = 0.45) +
  theme_minimal() +
  labs(x = "Number of Specimens", y = "Primer Pair", title = "") +
  scale_fill_manual(values = c("#F22300","#EBCC2A", "#3C9AB2"), 
                    labels = c(" Contamination ", " Target (mapped) ", " Target ")) +
  theme_bw() +
  theme(
    # Increase size for axis titles
    axis.title.x = element_text(size = 45),
    axis.title.y = element_text(size = 45),
    
    # Increase size for axis text (tick labels)
    axis.text.x = element_text(size = 45),
    axis.text.y = element_text(size = 45),
    
    # Increase size for legend text and title
    legend.text = element_text(size = 40),
    legend.title = element_text(size = 45),
    
    # Increase size for general text in the plot
    text = element_text(size = 40),
    
    # Position the legend at the top of the plot
    legend.position = "top",
    legend.justification = c("center", "top"),
    
    # Ensure legend keys are square
    legend.key.height = unit(1.5, "cm"),
    legend.key.width = unit(1.5, "cm")
  ) +
  coord_cartesian(xlim = c(0, 320)) +
  guides(fill = guide_legend(title = NULL))
p3

# Save the combined plot as a single TIFF file
ggsave("Plots/Contamination_summary.tiff", plot = p3, width = 30, height = 10, bg = "white")


